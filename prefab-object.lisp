(defpackage :prefab-object
  (:use :common-lisp)
  (:export :*prefab-table*
           :register-prefab :defprefab
           :make-prefab-object))

(in-package :prefab-object)

(defvar *prefab-table* (make-hash-table :test #'equal))

(defun register-prefab (type-name function)
  (setf (gethash type-name *prefab-table*) function))

(defmacro defprefab (fname lambda-list &rest body)
  `(register-prefab (symbol-name ',fname) (lambda ,lambda-list
                              ,@body)))

(defmacro make-prefab-object (type &rest params)
  `(let ((function (gethash (symbol-name ,type) *prefab-table*)))
     (when function
           (funcall function ,@params))))
