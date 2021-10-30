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
    (cl:setf (cl:gethash :value i) lisp-val)
    i))

(define-message to-lisp-string (type integer) ()
  (let ((v (get self :lisp-value)))
    (if v
        (format nil "{}" (get self :lisp-value))
        (send :name self))))

(define-message + (type integer) (other)
  (if (send :descendant? other (type integer))
      (send :construct (type integer)
            (cl:+ (get self :value)
               (get other :value)))
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

