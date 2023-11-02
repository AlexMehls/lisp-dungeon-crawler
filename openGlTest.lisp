(require :asdf)
(push "./" asdf:*central-registry*)
(asdf:load-system :textures)

(ql:quickload :png-read)
(ql:quickload :cl-opengl)
(ql:quickload :cl-cffi-gtk)
(ql:quickload :3d-vectors)
(ql:quickload :3d-matrices)

(defpackage :opengl-test
(:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp :textures))

(in-package :opengl-test)

(defclass sprite ()
    ((pos :initarg :position
          :accessor sprite-position
          :initform (3d-vectors:vec 0 0))
     (layer :initarg :layer
            :accessor sprite-layer
            :initform 0)
     (size :initarg :size
           :accessor sprite-size
           :initform (3d-vectors:vec 1 1))
     (rot :initarg :rotation
           :accessor sprite-rotation
           :initform 0)
     (texture :initarg :texture
              :reader sprite-texture
              :initform *missing-texture*)))

(defmethod sprite-model-matrix ((obj sprite))
  (let ((mat (3d-matrices:mtranslation (3d-vectors:v- (3d-vectors:vxy_ (sprite-position obj)) (3d-vectors:vec 0 0 (sprite-layer obj))))))
    (3d-matrices:nmscale mat (3d-vectors:v+ (3d-vectors:vxy_ (sprite-size obj)) (3d-vectors:vec 0 0 1)))
    (3d-matrices:nmrotate mat 3d-vectors:+vz+ (sprite-rotation obj))
    mat))

(defmethod sprite-draw ((obj sprite) vp-matrix)
  (texture-draw (sprite-texture obj) (sprite-model-matrix obj) vp-matrix))

(defclass camera ()
    ((pos :initarg :position
          :accessor camera-position
          :initform (3d-vectors:vec 0 0))
     (screen-ratio :initform 1
                   :reader camera-screen-ratio)
     (screen-size :initarg :screen-size
           :reader camera-screen-size
           :initform 1)))

(defmethod camera-view-projection-matrix ((obj camera))
  (let* ((half-w (* (camera-screen-ratio obj) (camera-screen-size obj) 0.5))
         (half-h (* (camera-screen-size obj) 0.5))
         (mat (3d-matrices:mortho (- half-w) half-w (- half-h) half-h 0.001 10000))
         (pos (3d-vectors:v+ (3d-vectors:vxy_ (camera-position obj)) (3d-vectors:vec 0 0 1))))
    (3d-matrices:nmlookat mat pos (3d-vectors:v- pos 3d-vectors:+vz+) 3d-vectors:+vy+)
    mat))

(defclass tile ()
    ((tile-type :initarg :tile-type
                :reader tile-type
                :initform 'tile-floor)
     (layer :initarg :layer
            :accessor tile-layer
            :initform 0)
     (texture :initarg :texture
              :reader tile-texture
              :initform *missing-texture*)))

(defmethod tile-model-matrix ((obj tile) pos)
  (3d-matrices:mtranslation (3d-vectors:v- (3d-vectors:vxy_ pos) (3d-vectors:vec 0 0 (tile-layer obj)))))

(defmethod tile-draw ((obj tile) pos vp-matrix)
  (texture-draw (tile-texture obj) (tile-model-matrix obj pos) vp-matrix))

(defclass tile-array ()
    ((tiles :initarg :tiles
            :accessor tile-array-tiles
            :initform (make-array '(0 0) :element-type 'tile))
     (offset :initarg :offset
             :reader tile-array-offset
             :initform (3d-vectors:vec 0 0))))

(defmethod tile-array-draw ((obj tile-array) vp-matrix)
  (let ((tiles (tile-array-tiles obj)))
    (destructuring-bind (x y) (array-dimensions tiles)
      (dotimes (i x)
        (dotimes (j y)
          (let ((tile (aref tiles i j))
                (pos (3d-vectors:v+ (3d-vectors:vec i j) (tile-array-offset obj))))
            (tile-draw tile pos vp-matrix)))))))

(defvar *default-camera* (make-instance 'camera :position (3d-vectors:vec 0 0) :screen-size 7))

(defun example-gl-area ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (area (make-instance 'gtk-gl-area))
          vao
          (test-sprite (make-instance 'sprite
                         :position (3d-vectors:vec 0 0)
                         :size (3d-vectors:vec 1 1)
                         :rotation 0
                         :texture *test-texture2*))
          (test-tiles (make-instance 'tile-array
                        :tiles (make-array '(7 5) :initial-element (make-instance 'tile))
                        :offset (3d-vectors:vec -3 -2))))
      (gtk-container-add window area)
      (g-signal-connect area "realize"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-gl-area-make-current area)
                          (gtk-gl-area-get-error area)

                          (setf vao (gl:gen-vertex-array))
                          (gl:bind-vertex-array vao)
                          
                          (setup-opengl)
                          (load-textures)))
      (g-signal-connect area "unrealize"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-gl-area-make-current area)
                          (gtk-gl-area-get-error area)
                          (gl:delete-vertex-arrays (list vao))
                          (delete-textures)
                          (shutdown-opengl)))
      (g-signal-connect area "render"
                        (lambda (area context)
                          (declare (ignore context))

                          (gl:clear-color 0.5 0.5 0.5 1.0)
                          (gl:clear :color-buffer :depth-buffer)

                          (let ((w (gtk-widget-get-allocated-width area))
                                (h (gtk-widget-get-allocated-height area)))
                            (when (and (> h 0) (> w 0))
                                (setf (slot-value *default-camera* 'screen-ratio) (/ w h))))
                          
                          (let ((vp-mat (camera-view-projection-matrix *default-camera*)))
                            (tile-array-draw test-tiles vp-mat)
                            (sprite-draw test-sprite vp-mat))
                          
                          NIL))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show-all window)))
  (sb-thread:release-foreground) ; For better debug output
  (gtk:join-gtk-main))

(example-gl-area)
;(sb-ext:save-lisp-and-die "openGlTest.exe"
;                   :toplevel #'example-gl-area
;                   :executable t)