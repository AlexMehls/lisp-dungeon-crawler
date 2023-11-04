(require :asdf)
(push "./" asdf:*central-registry*)
(asdf:load-system :textures)

(ql:quickload :png-read)
(ql:quickload :cl-opengl)
(ql:quickload :cl-cffi-gtk)
(ql:quickload :3d-vectors)
(ql:quickload :3d-matrices)
(ql:quickload :local-time)
(ql:quickload :queues)
(asdf:oos 'asdf:load-op :queues.simple-queue)

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

(defvar *keys-held* NIL)
(defvar *keys-pressed* NIL)

(defmacro keyval (name)
  (gdk-keyval-from-name name)) ; Only needs to be evaluated once -> macro

(defmacro get-key-press (name)
  `(member (keyval ,name) *keys-pressed*))

(defmacro get-key-hold (name)
  `(member (keyval ,name) *keys-held*))

(defun main ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (overlay (gtk-overlay-new))
          (area (make-instance 'gtk-gl-area :auto-render T)) ; maybe render manually?
          (fixed-container (gtk-fixed-new))
          (fps-counter (gtk-label-new "FPS:"))
          vao
          (test-sprite (make-instance 'sprite
                         :position (3d-vectors:vec 0 0)
                         :size (3d-vectors:vec 1 1)
                         :rotation 0
                         :texture *test-texture2*))
          (test-tiles (make-instance 'tile-array
                        :tiles (make-array '(7 5) :initial-element (make-instance 'tile))
                        :offset (3d-vectors:vec -3 -2)))
          (prev-time (local-time:now))
          (curr-time (local-time:now))
          (fps-vals (queues:make-queue :simple-queue)))
      (gtk-container-add window overlay)
      (gtk-container-add overlay area)
      (gtk-overlay-add-overlay overlay fixed-container)
      (gtk-fixed-put fixed-container fps-counter 0 0)
      ;(gtk-overlay-add-overlay overlay fps-counter)
      (gtk-widget-add-events window :key-press-mask)
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
                          (setf curr-time (local-time:now))
                          ;(format t "Time since last render: ~ams~%" (* 1000 (local-time:timestamp-difference curr-time prev-time)))
                          ;(format t "FPS: ~a~%" (/ 1 (local-time:timestamp-difference curr-time prev-time)))

                          (let* ((delta-time (local-time:timestamp-difference curr-time prev-time))
                                 (move-dist (* 2 delta-time)))
                            (queues:qpush fps-vals (/ 1 delta-time))
                            (when (> (queues:qsize fps-vals) 60)
                                  (queues:qpop fps-vals))
                            (let ((avg-fps 0))
                              (queues:map-queue
                                (lambda (element)
                                  (setf avg-fps (+ avg-fps element)))
                                fps-vals)
                              (setf avg-fps (/ avg-fps (queues:qsize fps-vals)))
                              (gtk-label-set-text fps-counter (concatenate 'string "FPS: " (write-to-string (round avg-fps)))))
                          
                            ;(when *keys-pressed*
                            ;      (format t "Pressed: ~a~%" *keys-pressed*))
                            (when (get-key-hold "w")
                                  (setf (sprite-position test-sprite) (3d-vectors:v+ (sprite-position test-sprite) (3d-vectors:vec 0 move-dist))))
                            (when (get-key-hold "a")
                                  (setf (sprite-position test-sprite) (3d-vectors:v+ (sprite-position test-sprite) (3d-vectors:vec (- move-dist) 0))))
                            (when (get-key-hold "s")
                                  (setf (sprite-position test-sprite) (3d-vectors:v+ (sprite-position test-sprite) (3d-vectors:vec 0 (- move-dist)))))
                            (when (get-key-hold "d")
                                  (setf (sprite-position test-sprite) (3d-vectors:v+ (sprite-position test-sprite) (3d-vectors:vec move-dist 0)))))

                          (gl:clear-color 0.5 0.5 0.5 1.0)
                          (gl:clear :color-buffer :depth-buffer)
                          
                          (let ((vp-mat (camera-view-projection-matrix *default-camera*)))
                            (tile-array-draw test-tiles vp-mat)
                            (sprite-draw test-sprite vp-mat))
                          
                          (setf *keys-pressed* NIL)
                          (setf prev-time curr-time)
                          (gtk-gl-area-queue-render area) ; maybe render manually?
                          NIL))
      (g-signal-connect area "resize"
                        (lambda (area width height)
                          (declare (ignore area))
                          (when (and (> height 0) (> width 0))
                              (setf (slot-value *default-camera* 'screen-ratio) (/ width height)))
                          NIL))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect window "key_press_event"
                        (lambda (widget event)
                          (declare (ignore widget))
                          (let ((keyval (gdk-event-key-keyval event)))
                            (unless (member keyval *keys-held*)
                              (setf *keys-pressed* (adjoin keyval *keys-pressed*)))
                            (setf *keys-held* (adjoin keyval *keys-held*)))
                          ;(format t "Event: ~a~%" event)
                          ;(format t "Keys: ~a~%" *keys-held*)
                          ))
      (g-signal-connect window "key_release_event"
                        (lambda (widget event)
                          (declare (ignore widget))
                          (setf *keys-held* (set-difference *keys-held* `(,(gdk-event-key-keyval event))))
                          ;(format t "Event: ~a~%" event)
                          ;(when (= (gdk-event-key-keyval event))
                          ;    (format t "pressed space~%"))
                          ;(format t "Keys: ~a~%" *keys-held*)
                          ))
      (gtk-widget-show-all window)))
  (sb-thread:release-foreground) ; For better debug output
  (gtk:join-gtk-main))

(main)
;(sb-ext:save-lisp-and-die "openGlTest.exe"
;                   :toplevel #'main
;                   :executable t)