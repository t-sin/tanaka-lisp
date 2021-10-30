(in-package :tanaka-lisp)

;;;; object system

(defun make-object (name parent)
  (let ((meta (hash :name name :parent parent)))
    (let ((obj (hash :*meta* meta)))
      obj)))

(defun object-p (obj)
  (cl:and (cl:hash-table-p obj)
          (cl:gethash :*meta* obj nil)
          (cl:hash-table-p (cl:gethash :*meta* obj))))

(defun find-message (name object)
  (cl:unless (object-p object)
    (error (cl:format nil "~s is not an object" object)))
  (cl:unless (cl:keywordp name)
    (error (cl:format nil "~s is not a keyword" name)))
  (let ((method (cl:gethash name object)))
    (if method
           (cl:values method object)
           (let ((meta (get object :*meta*)))
             (let ((parent (get meta :parent)))
               (if parent
                   (find-message name parent)
                   nil))))))

;; NOTE: this OVERRIDE same message of its parent
(cl:defmacro define-message (name obj (&rest args) cl:&body body)
  `(cl:setf (cl:gethash (cl:intern (cl:symbol-name ',name) :keyword) ,obj)
            (lambda (self ,@args)
              (cl:declare (cl:ignorable self))
              ,@body)))

(cl:defmethod cl:print-object ((ht cl:hash-table) stream)
  (if (object-p ht)
      (cl:format stream "~a" (send :to-lisp-string ht))
      (%print-hash-table ht stream)))

(cl:define-condition unknown-message (cl:error)
  ((msg) (args)))

(defun send (message object &rest args)
  (cl:unless (object-p object)
    (error (cl:format nil "~s is not an object" object)))
  (bind (msg-fn obj)
      (find-message message object)
    (cl:declare (cl:ignore obj))
    (if msg-fn
        (cl:apply msg-fn object args)
        (bind (msg-fn obj)
            (find-message :unknown-message object)
          (cl:declare (cl:ignore obj))
          (if msg-fn
              (cl:funcall #'send :unknown-message object message args)
              (error (cl:make-condition 'unknown-message :msg message :args args)))))))

(cl:defparameter *archetype* #{})

(cl:eval-when (:compile-toplevel)
  (defun %name (symbol)
    (cl:intern (cl:format nil "TYPE/~a" (cl:symbol-name symbol)) :keyword))

  (cl:defmacro define-object (name parent)
    (let ((name (%name name)))
      `(cl:setf (cl:gethash ,name *archetype*) (make-object ,name ,parent))))

  (cl:defmacro type (name)
    `(cl:gethash ,(%name name) *archetype*)))

;;;; root object

(define-object object nil)

(define-message name (type object) ()
  (get (get self :*meta*) :name))

(define-message parent (type object) ()
  (get (get self :*meta*) :parent))

(define-message to-lisp-string (type object) ()
  (format nil "#<{}>" (cl:symbol-name (send :name self))))

(define-message to-string (type object) ()
  (send :construct (type string)
        (send :to-lisp-string self)))

(define-message print (type object) ()
  (format t "{}" (send :to-lisp-string self))
  (cl:values))

(define-message descendant? (type object) (ancestor)
  (let ((parent (send :parent self)))
  (if (cl:null parent)
      (send :false (type boolean))
      (if (cl:eq parent ancestor)
          (send :true (type boolean))
          (send :descendant? parent ancestor)))))

(define-message eval (type object) ()
  self)

;;;; boolean

(define-object boolean (type object))

(define-message to-lisp-string (type boolean) ()
  (format nil "{}" (cl:symbol-name (send :name self))))

(define-message true (type boolean) ()
  (let ((true (get self :*true*)))
    (if true
        true
        (let ((true (make-object :true (type boolean))))
          (cl:setf (cl:gethash :*true* self) true)
          true))))

(define-message false (type boolean) ()
  (let ((false (get self :*false*)))
    (if false
        false
        (let ((false (make-object :false (type boolean))))
          (cl:setf (cl:gethash :*false* self) false)
          false))))

;;;; integer

(define-object integer (type object))

(define-message construct (type integer) (lisp-val)
  (let ((i (make-object :integer (type integer))))
    (cl:setf (cl:gethash :lisp-value i) lisp-val)
    i))

(define-message to-lisp-string (type integer) ()
  (let ((v (get self :lisp-value)))
    (if v
        (format nil "{}" (get self :lisp-value))
        (send :name self))))

(define-message + (type integer) (other)
  (if (send :descendant? other (type integer))
      (send :construct (type integer)
            (cl:+ (get self :lisp-value)
               (get other :lisp-value)))
      (cl:error "~s is not an integer" other)))

;;;; string

(define-object string (type object))

(define-message construct (type string) (lisp-string)
  (let ((s (make-object :string (type string))))
    (cl:setf (cl:gethash :lisp-value s) lisp-string)
    s))

(define-message to-lisp-string (type string) ()
  (format nil "{}" (get self :lisp-value)))

(define-message format (type string) (&rest args)
  (let ((lisp-str (cl:apply #'format nil (send :to-lisp-string self)
                            (cl:loop
                              :for arg :in args
                              :collect (send :eval arg)))))
    (send :construct (type string) lisp-str)))

;;;; symbol

(define-object symbol (type object))

(define-message construct (type symbol) (string)
  (let ((s (define-object symbol self))
        (id (let ((%id (get self :id)))
              (if (cl:null %id)
                  (do (cl:setf (cl:gethash :id self) 0)
                      0)
                  (cl:incf (cl:gethash :id self))))))
    (cl:setf (cl:gethash :name s) string)
    (cl:setf (cl:gethash :id s) id)
    s))

(define-message equal (type symbol) (other)
  (cl:eq (get self :id) (get other :id)))

(define-message defmsg (type symbol) (receiver args &rest body)
  ;; やること:
  ;;
  ;; 1. 引数と本体からprocオブジェクトを生成する
  ;; 2. receiverにself.nameをキーとして、生成したprocオブジェクトを設定する
  ;;
  ;; ただしまだsendがprocオブジェクトに対応してないのでどげんかせんといけん
  (let ((proc (send :construct (type proc) args body)))
    ))

;;;; input stream

(define-object output-stream (type object))

(define-message construct (type output-stream) (lisp-stream)
  (let ((s (define-object %output-stream (type output-stream))))
    (cl:setf (cl:gethash :lisp-stream s) lisp-stream)
    s))

(define-message write (type output-stream) (object)
  (cl:write-string (send :to-lisp-string object)
                   (get self :lisp-stream))
  (cl:values))

;;;; runtime object

(define-object runtime (type object))

(define-message stdout (type runtime) ()
  (let ((stdout (get self :*stdout*)))
    (if (cl:null stdout)
        (let ((stdout (send :construct (type output-stream) cl:*standard-output*)))
          (cl:setf (cl:gethash :*stdout* self) stdout)
          stdout)
        stdout)))
