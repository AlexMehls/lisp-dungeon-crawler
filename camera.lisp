(defpackage :camera
  (:use :common-lisp)
  (:export :*active-camera* :*window-w* :*window-h*
           
           :camera
           :camera-position :camera-screen-ratio :camera-screen-size
           
           :camera-view-projection-matrix))

(in-package :camera)

(defvar *active-camera* NIL)
(defvar *window-w* 1)
(defvar *window-h* 1)

(defclass camera ()
    ((pos :initarg :position
          :accessor camera-position
          :initform (3d-vectors:vec2 0 0))
     (screen-ratio :initform 1
                   :accessor camera-screen-ratio)
     (screen-size :initarg :screen-size
                  :accessor camera-screen-size
                  :initform 1)))

(defmethod camera-view-projection-matrix ((obj camera))
  (let* ((half-w (* (camera-screen-ratio obj) (camera-screen-size obj) 0.5))
         (half-h (* (camera-screen-size obj) 0.5))
         (mat (3d-matrices:mortho (- half-w) half-w (- half-h) half-h 0.1 1000))
         (pos (3d-vectors:v+ (3d-vectors:vxy_ (camera-position obj)) (3d-vectors:vec3 0 0 100))))
    (3d-matrices:nmlookat mat pos (3d-vectors:v- pos 3d-vectors:+vz+) 3d-vectors:+vy+)
    mat))
