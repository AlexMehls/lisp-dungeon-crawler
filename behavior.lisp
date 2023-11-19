(defpackage :behavior
  (:use :common-lisp)
  (:export :behavior
           :behavior-update))

(in-package :behavior)

(defclass behavior ()
    ())

(defmethod behavior-update ((behavior behavior) delta-time game-object))