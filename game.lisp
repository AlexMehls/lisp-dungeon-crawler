(defpackage :game
(:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp :textures))

(in-package :game)

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

(defun make-room (pos)
  (let* ((w 11)
         (h 9)
         (tiles (make-array `(,w ,h) :initial-element (make-instance 'tile)))
         (floor-tile (make-instance 'tile :texture *test-floor-texture*))
         (wall-tile  (make-instance 'tile :texture *test-wall-texture*)))
    (dotimes (x w)
      (dotimes (y h)
        (if (or (= x 0) (= x (1- w)) (= y 0) (= y (1- h)))
            (setf (aref tiles x y) wall-tile)
            (setf (aref tiles x y) floor-tile))))
    
    (make-instance 'tile-array
      :tiles tiles
      :offset pos)))

(defclass collider ()
    ((pos :initarg :position
          :accessor collider-position
          :initform (3d-vectors:vec 0 0))))

(defclass circle-collider (collider)
    ((radius :initarg :radius
             :accessor circle-collider-radius
             :initform 0.5)))

(defclass aabb-collider (collider)
    ((size :initarg :size
           :accessor rectangle-collider-size
           :initform (3d-vectors:vec 1 1))))

(defclass rectangle-collider (collider)
    ((size :initarg :size
           :accessor rectangle-collider-size
           :initform (3d-vectors:vec 1 1))
     (rot :initarg :rotation
          :accessor rectangle-collider-rotation
          :initform 0)))

(defclass rectangle-points ()
    ((A :initarg :A)
     (B :initarg :B)
     (C :initarg :C)
     (D :initarg :D)))

(defun make-rotated-rectangle-points (pos size rot)
  (let* ((cos-rot (cos rot))
         (sin-rot (sin rot))
         (rot-mat (3d-matrices:mat2 `(,cos-rot ,(- sin-rot) ,sin-rot ,cos-rot)))
         (half-size (3d-vectors:v/ size 2))
         (A (3d-vectors:v+ pos (3d-matrices:m* rot-mat half-size)))
         (B (3d-vectors:v+ pos (3d-matrices:m* rot-mat (3d-vectors:v* half-size (3d-vectors:vec2 1 -1)))))
         (C (3d-vectors:v+ pos (3d-matrices:m* rot-mat (3d-vectors:v* half-size (3d-vectors:vec2 -1 -1)))))
         (D (3d-vectors:v+ pos (3d-matrices:m* rot-mat (3d-vectors:v* half-size (3d-vectors:vec2 -1 1))))))
    (make-instance 'rectangle-points :A A :B B :C C :D D)))

(defun make-rectangle-points (pos size)
  (let* ((half-size (3d-vectors:v/ size 2))
         (A (3d-vectors:v+ pos half-size))
         (B (3d-vectors:v+ pos (3d-vectors:v* half-size (3d-vectors:vec2 1 -1))))
         (C (3d-vectors:v+ pos (3d-vectors:v* half-size (3d-vectors:vec2 -1 -1))))
         (D (3d-vectors:v+ pos (3d-vectors:v* half-size (3d-vectors:vec2 -1 1)))))
    (make-instance 'rectangle-points :A A :B B :C C :D D)))

(defmethod collider-get-collision ((col1 circle-collider) (col2 circle-collider))
  (< (3d-vectors:vdistance (collider-position col1) (collider-position col2)) (+ (circle-collider-radius col1) (circle-collider-radius col2))))

; TODO: maybe optimise -> early return
(defmethod collider-get-collision ((col1 circle-collider) (col2 aabb-collider))
  (with-slots ((circle-pos pos) radius) col1
    (with-slots ((aabb-pos pos) size) col2
      (with-slots (A B C D) (make-rectangle-points aabb-pos size)
        (let* ((circle-x (3d-vectors:vx circle-pos))
               (circle-y (3d-vectors:vy circle-pos))
               (aabb-x (3d-vectors:vx aabb-pos))
               (aabb-y (3d-vectors:vy aabb-pos))
               (half-w (/ (3d-vectors:vx size) 2))
               (half-h (/ (3d-vectors:vy size) 2))
               (right (+ aabb-x half-w))
               (left (- aabb-x half-w))
               (top (+ aabb-y half-h))
               (bottom (- aabb-y half-h)))
          (not (or (>= circle-x (+ right radius))
                   (<= circle-x (- left radius))
                   (>= circle-y (+ top radius))
                   (<= circle-y (- bottom radius))
                   (and (>= circle-x right)
                        (>= circle-y top)
                        (>= (3d-vectors:vdistance A circle-pos) radius))
                   (and (>= circle-x right)
                        (<= circle-y bottom)
                        (>= (3d-vectors:vdistance B circle-pos) radius))
                   (and (<= circle-x left)
                        (<= circle-y bottom)
                        (>= (3d-vectors:vdistance C circle-pos) radius))
                   (and (<= circle-x left)
                        (>= circle-y top)
                        (>= (3d-vectors:vdistance D circle-pos) radius)))))))))

(defmethod point-in-rectangle (point (rectangle rectangle-points))
  (with-slots (A B C D) rectangle
    (let* ((AP (3d-vectors:v- point A))
           (AB (3d-vectors:v- B A))
           (AD (3d-vectors:v- D A))
           (prod1 (3d-vectors:v. AP AB))
           (prod2 (3d-vectors:v. AP AD)))
      (and (<= 0 prod1) (<= prod1 (3d-vectors:v. AB AB)) (<= 0 prod2) (<= prod2 (3d-vectors:v. AD AD))))))

;(defun vector-projection (v1 v2)
;  (3d-vectors:v* (* (3d-vectors:v2norm v1) (cos (3d-vectors:vangle v1 v2))) (3d-vectors:vunit v2)))

(defun vector-projection (v1 v2)
  (let ((k (/ (3d-vectors:v. v1 v2) (3d-vectors:v. v2 v2))))
    (3d-vectors:vec2 (* k (3d-vectors:vx v2)) (* k (3d-vectors:vy v2)))))

(defmethod line-intersect-circle ((circle circle-collider) A B)
  (with-slots (pos radius) circle
    (let* ((AC (3d-vectors:v- pos A))
           (AB (3d-vectors:v- B A))
           (D (3d-vectors:v+ (vector-projection AC AB) A))
           (AD (3d-vectors:v- D A))
           (k (if (> (abs (3d-vectors:vx AB)) (abs (3d-vectors:vy AB)))
                  (/ (3d-vectors:vx AD) (3d-vectors:vx AB))
                  (/ (3d-vectors:vy AD) (3d-vectors:vy AB)))))
      (if (<= k 0)
          (< (3d-vectors:vdistance pos A) radius)
          (if (>= k 1)
              (< (3d-vectors:vdistance pos B) radius)
              (< (3d-vectors:vdistance pos D) radius))))))

(defmethod collider-get-collision ((col1 circle-collider) (col2 rectangle-collider))
  (let* ((rect-size (rectangle-collider-size col2))
         (rectangle (make-rotated-rectangle-points (collider-position col2)
                                                   rect-size
                                                   (rectangle-collider-rotation col2))))
    (with-slots (A B C D) rectangle
      (or (point-in-rectangle (collider-position col1) rectangle)
          (line-intersect-circle col1 A B)
          (line-intersect-circle col1 B C)
          (line-intersect-circle col1 C D)
          (line-intersect-circle col1 D A)))))

(defmethod collider-get-collision ((col1 aabb-collider) (col2 circle-collider))
  (collider-get-collision col2 col1))

(defmethod collider-get-collision ((col1 aabb-collider) (col2 aabb-collider))
  (with-slots ((pos1 pos) (size1 size)) col1
    (with-slots ((pos2 pos) (size2 size)) col2
      (let ((x1 (3d-vectors:vx pos1))
            (y1 (3d-vectors:vy pos1))
            (w1 (3d-vectors:vx size1))
            (h1 (3d-vectors:vy size1))
            (x2 (3d-vectors:vx pos2))
            (y2 (3d-vectors:vy pos2))
            (w2 (3d-vectors:vx size2))
            (h2 (3d-vectors:vy size2)))
        (and (> (+ x1 w1) x2) (< x1 (+ x2 w2)) (> (+ y1 h1) y2) (< y1 (+ y2 h2)))))))

(defmethod collider-get-collision ((col1 aabb-collider) (col2 rectangle-collider))
  (format t "Collision not implemented~%"))

(defmethod collider-get-collision ((col1 rectangle-collider) (col2 circle-collider))
  (collider-get-collision col2 col1))

(defmethod collider-get-collision ((col1 rectangle-collider) (col2 aabb-collider))
  (collider-get-collision col2 col1))

(defmethod collider-get-collision ((col1 rectangle-collider) (col2 rectangle-collider))
  (format t "Collision not implemented~%"))

(defvar *keys-held* NIL)
(defvar *keys-pressed* NIL)

(defmacro gdk-keyval (name)
  (gdk-keyval-from-name name)) ; Only needs to be evaluated once -> macro

(defmacro get-key-press (name)
  `(member (gdk-keyval ,name) *keys-pressed*))

(defmacro get-key-hold (name)
  `(member (gdk-keyval ,name) *keys-held*))

(defun main ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (overlay (gtk-overlay-new))
          (area (make-instance 'gtk-gl-area :auto-render T)) ; maybe render manually?
          (fixed-container (gtk-fixed-new))
          (box (gtk-box-new :vertical 1))
          (fps-counter (gtk-label-new "FPS:"))
          (debug-display (gtk-label-new ""))
          vao
          (prev-time (local-time:now))
          (curr-time (local-time:now))
          (fps-vals (queues:make-queue :simple-queue))
          (camera (make-instance 'camera :position (3d-vectors:vec 0 0) :screen-size 11))
          (test-sprite (make-instance 'sprite
                         :position (3d-vectors:vec 0 0)
                         :size (3d-vectors:vec 1 1)
                         :rotation 0
                         ;:texture *test-texture2*
                         :texture *test-circle*))
          (move-speed 1)
          ;(sprite-collider (make-instance 'aabb-collider))
          (sprite-collider (make-instance 'circle-collider))

          (test-collider1 (make-instance 'aabb-collider :position (3d-vectors:vec 2 2)))
          (collider-sprite1 (make-instance 'sprite :position (3d-vectors:vec 2 2)))
          (test-collider2 (make-instance 'circle-collider :position (3d-vectors:vec -2 2)))
          (collider-sprite2 (make-instance 'sprite :position (3d-vectors:vec -2 2) :texture *test-circle*))
          (test-collider3 (make-instance 'rectangle-collider :position (3d-vectors:vec 2 -2) :rotation 1))
          (collider-sprite3 (make-instance 'sprite :position (3d-vectors:vec 2 -2) :rotation 1))
          (test-tiles (make-room (3d-vectors:vec -5 -4)))
          (is-fullscreen NIL))
      (gtk-container-add window overlay)
      (gtk-container-add overlay area)
      (gtk-overlay-add-overlay overlay fixed-container)
      (gtk-fixed-put fixed-container box 0 0)
      ;(gtk-fixed-put fixed-container fps-counter 0 0)
      (gtk-box-pack-start box fps-counter)
      (gtk-box-pack-start box debug-display)
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
                          (setf curr-time (local-time:now)) ; is this accurate? (process or system time?)
                          ;(format t "Time since last render: ~ams~%" (* 1000 (local-time:timestamp-difference curr-time prev-time)))
                          ;(format t "FPS: ~a~%" (/ 1 (local-time:timestamp-difference curr-time prev-time)))

                          (let* ((delta-time (local-time:timestamp-difference curr-time prev-time)))
                            (when (= delta-time 0)
                                (setf delta-time (/ 1 60)))
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
                            
                            (when (get-key-press "F11")
                                  (if is-fullscreen
                                      (gtk-window-unfullscreen window)
                                      (gtk-window-fullscreen window))
                                  (setf is-fullscreen (not is-fullscreen)))

                            (let* ((delta-time (min delta-time (/ 1 30))) ; game starts to slow down below 30 fps
                                    (move-dist (* move-speed delta-time)))
                              (when (get-key-hold "w")
                                    (setf (sprite-position test-sprite) (3d-vectors:v+ (sprite-position test-sprite) (3d-vectors:vec 0 move-dist))))
                              (when (get-key-hold "a")
                                    (setf (sprite-position test-sprite) (3d-vectors:v+ (sprite-position test-sprite) (3d-vectors:vec (- move-dist) 0))))
                              (when (get-key-hold "s")
                                    (setf (sprite-position test-sprite) (3d-vectors:v+ (sprite-position test-sprite) (3d-vectors:vec 0 (- move-dist)))))
                              (when (get-key-hold "d")
                                    (setf (sprite-position test-sprite) (3d-vectors:v+ (sprite-position test-sprite) (3d-vectors:vec move-dist 0))))
                          
                              ;(when *keys-pressed*
                              ;      (format t "Pressed: ~a~%" *keys-pressed*))
                              ))
                          
                          (setf (camera-position camera) (sprite-position test-sprite))
                          (setf (collider-position sprite-collider) (sprite-position test-sprite))

                          (gtk-label-set-text debug-display "")
                          (when (collider-get-collision sprite-collider test-collider1)
                                (gtk-label-set-text debug-display "Collision AABB"))
                          (when (collider-get-collision sprite-collider test-collider2)
                                (gtk-label-set-text debug-display "Collision Circle"))
                          (when (collider-get-collision sprite-collider test-collider3)
                                (gtk-label-set-text debug-display "Collision Rectangle"))

                          (gl:clear-color 0.5 0.5 0.5 1.0)
                          (gl:clear :color-buffer :depth-buffer)
                          
                          (let ((vp-mat (camera-view-projection-matrix camera)))
                            (tile-array-draw test-tiles vp-mat)
                            (sprite-draw collider-sprite1 vp-mat)
                            (sprite-draw collider-sprite2 vp-mat)
                            (sprite-draw collider-sprite3 vp-mat)
                            (sprite-draw test-sprite vp-mat))
                          
                          (setf *keys-pressed* NIL)
                          (setf prev-time curr-time)
                          (gtk-gl-area-queue-render area) ; maybe render manually? -> gdk frame clock not working
                          NIL))
      (g-signal-connect area "resize"
                        (lambda (area width height)
                          (declare (ignore area))
                          (when (and (> height 0) (> width 0))
                              (setf (slot-value camera 'screen-ratio) (/ width height)))
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
  
  ;(sleep 0.001) ; wait until gtk loop starts
  (sb-thread:release-foreground) ; For better debug output in gtk-thread
  (gtk:join-gtk-main))
