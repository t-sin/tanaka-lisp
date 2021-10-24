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
