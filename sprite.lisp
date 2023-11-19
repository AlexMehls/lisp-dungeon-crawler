(defpackage :sprite
  (:use :common-lisp :textures)
  (:export :sprite
           :sprite-position :sprite-layer :sprite-size :sprite-rotation :sprite-texture
           :sprite-draw :sprites-draw))

(in-package :sprite)

(defclass sprite ()
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
           :initform 0)
     (texture :initarg :texture
              :reader sprite-texture
              :initform *missing-texture*)))

(defmethod sprite-model-matrix ((obj sprite))
  (let ((mat (3d-matrices:mtranslation (3d-vectors:v+ (3d-vectors:vxy_ (sprite-position obj)) (3d-vectors:vec3 0 0 (sprite-layer obj))))))
    (3d-matrices:nmscale mat (3d-vectors:v+ (3d-vectors:vxy_ (sprite-size obj)) (3d-vectors:vec3 0 0 1)))
    (3d-matrices:nmrotate mat 3d-vectors:+vz+ (sprite-rotation obj))
    mat))

(defmethod sprite-draw ((obj sprite) vp-matrix)
  (texture-draw (sprite-texture obj) (sprite-model-matrix obj) vp-matrix))

(defun sprites-draw (sprites vp-matrix)
  (loop for sprite being the hash-values of sprites
          do (sprite-draw sprite vp-matrix)))