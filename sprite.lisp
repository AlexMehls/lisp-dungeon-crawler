(defpackage :sprite
  (:use :common-lisp :render-object)
  (:export :sprite
           :sprite-position :sprite-layer :sprite-size :sprite-rotation
           :sprite-update-model-matrix))

(in-package :sprite)

(defclass sprite (render-object)
    ((pos :initarg :position
          :accessor sprite-position
          :initform (3d-vectors:vec2 0 0))
     (layer :initarg :layer
            :accessor sprite-layer
            :initform 0)
     (size :initarg :size
           :accessor sprite-size
           :initform (3d-vectors:vec2 1 1))
     (rot :initarg :rotation
           :accessor sprite-rotation
           :initform 0)))

(defmethod sprite-model-matrix ((obj sprite))
  (let ((mat (3d-matrices:mtranslation (3d-vectors:v+ (3d-vectors:vxy_ (sprite-position obj)) (3d-vectors:vec3 0 0 (sprite-layer obj))))))
    (3d-matrices:nmscale mat (3d-vectors:v+ (3d-vectors:vxy_ (sprite-size obj)) (3d-vectors:vec3 0 0 1)))
    (3d-matrices:nmrotate mat 3d-vectors:+vz+ (sprite-rotation obj))
    mat))

(defmethod sprite-update-model-matrix ((obj sprite))
  (render-object-update-matrix obj (sprite-model-matrix obj)))
