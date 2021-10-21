(in-package :tanaka-lisp)

(cl:defparameter *archetype* #{})

(cl:eval-when (:compile-toplevel)
  (cl:defun %name (symbol)
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

(define-message to-string (type object) ()
  (format nil "#<{}>" (cl:symbol-name (get (get self :*meta*) :name))))

(define-message print (type object) ()
  (format t "{}" (send :to-string self))
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

(define-message to-string (type boolean) ()
  (format nil "{}" (cl:symbol-name (get (get self :*meta*) :name))))

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

(define-message to-string (type integer) ()
  (let ((v (get self :value)))
    (if v
        (format nil "{}" (get self :value))
        (send :name self))))

(define-message + (type integer) (other)
  (if (send :descendant? other (type integer))
      (send :construct (type integer)
            (cl:+ (get self :value)
               (get other :value)))
      (cl:error "~s is not an integer" other)))
