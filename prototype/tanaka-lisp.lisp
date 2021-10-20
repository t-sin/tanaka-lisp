(defpackage :tanaka-lisp
  (:use)
  (:import-from :cl
                :>
                :<
                :=
                :/
                :1+
                :nil
                :t
                :let
                :defun
                :cond
                :if
                :lambda
                :setq
                :&rest)
  (:export))
(in-package :tanaka-lisp)

(cl:defun get (hash-table key)
  (cl:gethash key hash-table))

(cl:defmacro set (form value)
  `(cl:setf ,form ,value))

;;;; formatted output

(cl:defun %parse-format-string (str)
  (cl:with-output-to-string (out)
    (cl:with-input-from-string (in str)
      (cl:flet ((peek1 () (cl:peek-char nil in nil :eof))
                (read1 () (cl:read-char in)))
        (cl:loop :named input-loop
          :for ch := (peek1)
          :do (cl:case ch
                (:eof (cl:return-from input-loop))
                (#\{ (read1)
                     (if (cl:eq (peek1) #\})
                         (cl:progn
                           (read1)
                           (cl:format out "~~a"))
                         (error (cl:format nil "malformed format string: ~s" str))))
                (#\\ (read1)
                     (cl:ecase (peek1)
                       (#\\ (cl:write-char (peek1) out))
                       (#\n (read1)
                            (cl:write-char #\newline out))))
                (t (cl:write-char (read1) out))))))))

(cl:defun format (stream str &rest args)
  (let ((fmtstr (%parse-format-string str)))
    ;;    (cl:format t "fmtstr = ~s~%" fmtstr)
    (cl:apply #'cl:format stream fmtstr args)))
;;;; control flow

(cl:defmacro do (cl:&body body)
  `(cl:progn ,@body))

(cl:defmacro loop (&rest args)
  `(cl:loop
     :while ,(let ((while-clause (cl:getf args :while)))
               (if while-clause
                   while-clause
                   t))
     :do ,(cl:getf args :do)))

;;;; error handling

(cl:define-condition tanaka/condition (cl:condition)
  ((obj :initarg :obj
        :accessor tanaka/condition-obj)))

(cl:defun error (obj)
  (cl:error (cl:make-condition 'tanaka/condition :obj obj)))

(cl:defmacro try (form cl:&body catches)
  (let ((e (cl:gensym))
        (obj (cl:gensym)))
    `(cl:handler-case
         ,form
       (tanaka/condition (,e)
         (let ((,obj (tanaka/condition-obj ,e)))
           (cl:case (get (get ,obj :*meta*) :name)
             ,@(cl:loop
                 :for catch :in catches
                 :collect `(',(cl:intern (cl:symbol-name (cl:first catch)) :keyword)
                            (let ((,(cl:car (cl:second catch)) ,obj))
                              ,@(cl:cddr catch))))
             (t (cl:error ,e)))))
       (cl:condition (,e) (cl:error ,e)))))

;;;; utilities

(cl:defmacro bind ((cl:&body values) form cl:&body body)
  `(cl:multiple-value-call
       (cl:lambda (cl:&optional ,@values) ,@body)
     ,form))

;;;; hash tables

(cl:defun hash (&rest contents)
  (cl:loop
    :with ht := (cl:make-hash-table)
    :for (key val) :on contents :by #'cl:cddr
    :do (cl:setf (cl:gethash key ht) val)
    :finally (cl:return ht)))

(cl:defun read-hash-table (stream ch arg)
  (cl:declare (cl:ignore ch arg))
  (cl:let ((contents (cl:read-delimited-list #\} stream cl:t)))
    `(hash ,@contents)))

(cl:set-dispatch-macro-character #\# #\{ 'read-hash-table)
(cl:set-macro-character #\} (cl:get-macro-character #\)) cl:nil)

(cl:defun %print-hash-table (ht stream)
  (cl:format stream "#{")
  (cl:loop
    :with first-p := cl:t
    :for k :being :the :hash-key :of ht :using (:hash-value v)
    :do (cl:if first-p (cl:setf first-p cl:nil) (cl:princ " " stream))
    :do (cl:format stream "~s" k) (cl:format stream " ~s" v))
  (cl:format stream "}"))

;;;; Object oriented programming

(cl:defun make-object (name parent)
  (cl:let* ((meta (hash :name name :parent parent))
            (obj (hash :*meta* meta)))
    obj))

(cl:defun object-p (obj)
  (cl:and (cl:hash-table-p obj)
          (cl:gethash :*meta* obj nil)
          (cl:hash-table-p (cl:gethash :*meta* obj))))

(cl:defun find-message (name object)
  (cl:unless (object-p object)
    (error (cl:format cl:nil "~s is not an object" object)))
  (cl:unless (cl:keywordp name)
    (error (cl:format cl:nil "~s is not a keyword" name)))
  (cl:let ((method (cl:gethash name object)))
    (cl:if method
           (cl:values method object)
           (cl:let* ((meta (get object :*meta*))
                     (parent (get meta :parent)))
             (cl:if parent
                    (find-message name parent)
                    nil)))))

;; NOTE: this OVERRIDE same message of its parent
(cl:defmacro define-message (name obj (&rest args) cl:&body body)
  `(cl:setf (cl:gethash (cl:intern (cl:symbol-name ',name) :keyword) ,obj)
            (lambda (self ,@args)
              (cl:declare (cl:ignorable self))
              ,@body)))

(cl:defmethod cl:print-object ((ht cl:hash-table) stream)
  (if (object-p ht)
      (cl:format stream "~a" (send :to-string ht))
      (%print-hash-table ht stream)))

(cl:define-condition unknown-message (cl:error)
  ((msg) (args)))

(cl:defun send (message object &rest args)
  (cl:unless (object-p object)
    (error (cl:format cl:nil "~s is not an object" object)))
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

;;;; ---- pure object world ----

(cl:defparameter *archetype* #{})
(cl:defun %name (symbol)
  (cl:intern (cl:format nil "TYPE/~a" (cl:symbol-name symbol)) :keyword))
(cl:defmacro define-object (name parent)
  (let ((name (%name name)))
    `(cl:setf (cl:gethash ,name *archetype*) (make-object ,name ,parent))))
(cl:defmacro type (name)
  `(cl:gethash ,(%name name) *archetype*))

;; root object
(define-object object nil)
(define-message name (type object) ()
  (get (get self :*meta*) :name))
(define-message parent (type object) ()
  (get (get self :*meta*) :parent))
(define-message to-string (type object) ()
  (format nil "#<{}>" (cl:symbol-name (get (get self :*meta*) :name))))
(define-message print (type object) ()
  (format t "{}" (send :to-string self))
  (cl:values))
(define-message eval (type object) ()
  self)

;; integer
(define-object integer (type object))
(define-message construct (type integer) (lisp-val)
  (let ((i (make-object :integer (type integer))))
    (cl:setf (cl:gethash :value i) lisp-val)
    i))
(define-message to-string (type integer) ()
  (let ((v (get self :value)))
    (if v
        (format nil "{}" (get self :value))
        (send :name self))))
