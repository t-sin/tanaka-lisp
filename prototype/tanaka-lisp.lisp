(defpackage :tanaka-lisp
  (:use)
  (:import-from :cl
                :>
                :=
                :/
                :nil
                :t
                :defun
                :if
                :lambda
                :setq
                :&rest)
  (:export))
(in-package :tanaka-lisp)

(cl:defun get (hash-table key)
  (cl:gethash key hash-table))

(cl:defun println (str &rest args)
  (cl:apply #'cl:format cl:t str args)
  (cl:values))

(cl:defun error (msg)
  (cl:error msg))

(cl:defmacro try (form cl:&body catches)
  `(cl:handler-case ,form ,@catches))

(cl:defmacro bind ((cl:&body values) form cl:&body body)
  `(cl:multiple-value-call
       (cl:lambda (,@values) ,@body)
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
            (lambda (self ,@args) ,@body)))

(cl:defmethod cl:print-object ((ht cl:hash-table) stream)
  (if (object-p ht)
      (cl:format stream "#<object:~a>" (get (get ht :*meta*) :name))
      (%print-hash-table ht stream)))

(cl:define-condition unknown-message (cl:error)
  ((msg) (args)))

(cl:defun send (message object &rest args)
  (cl:unless (object-p object)
    (error (cl:format cl:nil "~s is not an object" object)))
  (bind (msg-fn obj)
      (find-message message object)
    (if msg-fn
        (cl:apply msg-fn obj args)
        (bind (msg-fn obj)
            (find-message :unknown-message object)
          (if msg-fn
              (cl:apply #'send :unknown-message obj args)
              (error (cl:make-condition 'unknown-message :msg message :args args)))))))
