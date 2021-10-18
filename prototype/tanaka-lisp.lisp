(defpackage :tanaka-lisp
  (:use)
  (:import-from :cl
                :>
                :=
                :/
                :defun
                :if
                :lambda
                :setq
                :gethash)
  (:export))
(in-package :tanaka-lisp)

(cl:defun println (str cl:&rest args)
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

(cl:defun hash (cl:&rest contents)
  (cl:loop
    :with ht := (cl:make-hash-table)
    :for (key val) :on contents :by #'cl:cddr
    :do (cl:setf (gethash key ht) val)
    :finally (cl:return ht)))

(cl:defun read-hash-table (stream ch arg)
  (cl:declare (cl:ignore ch arg))
  (cl:let ((contents (cl:read-delimited-list #\} stream cl:t)))
    `(hash ,@contents)))

(cl:set-dispatch-macro-character #\# #\{ 'read-hash-table)
(cl:set-macro-character #\} (cl:get-macro-character #\)) cl:nil)

(cl:defmethod cl:print-object ((ht cl:hash-table) stream)
  (cl:format stream "#{")
  (cl:loop
    :with first-p := cl:t
    :for k :being :the :hash-key :of ht :using (:hash-value v)
    :do (cl:if first-p (cl:setf first-p cl:nil) (cl:princ " " stream))
    :do (cl:format stream "~s" k) (cl:format stream " ~s" v))
  (cl:format stream "}"))

(cl:defun object-p (obj)
  (cl:and (cl:hash-table-p obj)
       (gethash :*meta* obj)
       (cl:hash-table-p (gethash :*meta* obj))))

(cl:defun send (message object cl:&rest args)
  (cl:unless (object-p object)
    (error (cl:format cl:nil "~s is not an object" object)))
  (cl:let ((method (gethash message object)))
    (cl:if method
        (cl:apply method object args)
        (cl:apply #'send message (gethash :*meta* object) args))))
