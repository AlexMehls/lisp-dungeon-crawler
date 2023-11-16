(defpackage :game
(:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp
        :textures :collision))

(in-package :game)

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

(defclass camera ()
    ((pos :initarg :position
          :accessor camera-position
          :initform (3d-vectors:vec2 0 0))
     (screen-ratio :initform 1
                   :reader camera-screen-ratio)
     (screen-size :initarg :screen-size
           :reader camera-screen-size
           :initform 1)))

(defmethod camera-view-projection-matrix ((obj camera))
  (let* ((half-w (* (camera-screen-ratio obj) (camera-screen-size obj) 0.5))
         (half-h (* (camera-screen-size obj) 0.5))
         (mat (3d-matrices:mortho (- half-w) half-w (- half-h) half-h 0.1 1000))
         (pos (3d-vectors:v+ (3d-vectors:vxy_ (camera-position obj)) (3d-vectors:vec3 0 0 100))))
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
  (3d-matrices:mtranslation (3d-vectors:v+ (3d-vectors:vxy_ pos) (3d-vectors:vec3 0 0 (tile-layer obj)))))

(defmethod tile-draw ((obj tile) pos vp-matrix)
  (texture-draw (tile-texture obj) (tile-model-matrix obj pos) vp-matrix))

(defclass tile-array ()
    ((tiles :initarg :tiles
            :accessor tile-array-tiles
            :initform (make-array '(0 0) :element-type 'tile))
     (offset :initarg :offset
             :reader tile-array-offset
             :initform (3d-vectors:vec2 0 0))))

(defmethod tile-array-draw ((obj tile-array) vp-matrix)
  (let ((tiles (tile-array-tiles obj)))
    (destructuring-bind (x y) (array-dimensions tiles)
      (dotimes (i x)
        (dotimes (j y)
          (let ((tile (aref tiles i j))
                (pos (3d-vectors:v+ (3d-vectors:vec2 i j) (tile-array-offset obj))))
            (tile-draw tile pos vp-matrix)))))))

(defun make-room (pos)
  (let* ((w 11)
         (h 9)
         (tiles (make-array `(,w ,h) :initial-element (make-instance 'tile)))
         (floor-tile (make-instance 'tile :texture *test-floor-texture* :layer -10))
         (wall-tile  (make-instance 'tile :texture *test-wall-texture* :layer 10)))
    (dotimes (x w)
      (dotimes (y h)
        (if (or (= x 0) (= x (1- w)) (= y 0) (= y (1- h)))
            (setf (aref tiles x y) wall-tile)
            (setf (aref tiles x y) floor-tile))))
    
    (make-instance 'tile-array
      :tiles tiles
      :offset pos)))

(defclass id-generator ()
    ((prev-id :initform -1)))

(defmethod id-generator-generate (gen)
  (incf (slot-value gen 'prev-id)))

(defvar *game-object-id-generator* (make-instance 'id-generator))

(defclass game-object ()
    ((id :initarg :id
         :initform (id-generator-generate *game-object-id-generator*)
         :reader game-object-id)
     (sprite :initarg :sprite
             :initform NIL
             :reader game-object-sprite)
     (collider :initarg :collider
               :initform NIL
               :reader game-object-collider)
     (behavior :initarg :behavior
               :initform (lambda (delta-time game-object) (declare (ignore delta-time game-object)))
               :reader game-object-behavior)
     (tags :initarg :tags
           :initform '()
           :accessor game-object-tags)))

;; Also sets the parent reference for the collider
(defun make-game-object (&key sprite collider behavior tags)
  (let ((obj (make-instance 'game-object :sprite sprite :tags tags)))
    (when collider
          (setf (slot-value obj 'collider) collider)
          (setf (collider-parent collider) obj)) ; circular reference (should still be handled by garbage collector)
    (when behavior
          (setf (slot-value obj 'behavior) behavior))
    obj))

;; Additional quick access to colliders and sprites for rendereing / collision detection
;; Faster?
(defvar *game-objects* (make-hash-table))
(defvar *game-object-colliders* (make-hash-table))
(defvar *game-object-sprites* (make-hash-table))

(defmethod game-object-register ((obj game-object))
  (let ((id (game-object-id obj)))
    (setf (gethash id *game-objects*) obj)
    (setf (gethash id *game-object-colliders*) (game-object-collider obj))
    (setf (gethash id *game-object-sprites*) (game-object-sprite obj))))

(defun game-object-delete-by-id (id)
  (remhash id *game-objects*)
  (remhash id *game-object-colliders*)
  (remhash id *game-object-sprites*))

(defmethod game-object-delete ((obj game-object))
  (game-object-delete-by-id (game-object-id obj)))

(defmethod game-object-move ((obj game-object) delta-pos)
  (with-slots (sprite collider) obj
    (let ((corrected-delta-pos (collider-resolve-collisions collider *game-object-colliders* delta-pos)))
      (setf (sprite-position sprite) (3d-vectors:v+ (sprite-position sprite) corrected-delta-pos))
      (setf (collider-position collider) (3d-vectors:v+ (collider-position collider) corrected-delta-pos)))))

(defmethod game-object-update ((obj game-object) delta-time)
  (funcall (game-object-behavior obj) delta-time obj))

(defun game-objects-update (game-objects delta-time)
    (loop for obj being the hash-values of game-objects
          do (game-object-update obj delta-time)))

(defmethod game-object-has-tag ((obj game-object) tag)
  (member tag (game-object-tags obj)))

;; Gets the first object colliding with the given object and with matching tag
(defmacro get-tagged-object-collision (obj tag)
  (let ((collisions (gensym))
        (collider (gensym)))
    `(let ((,collisions (collider-get-collisions (game-object-collider ,obj) *game-object-colliders*)))
       (loop for ,collider in ,collisions
               when (game-object-has-tag (collider-parent ,collider) ,tag)
               return (collider-parent ,collider)))))

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
    (let* ((window (gtk-window-new :toplevel))
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
          (camera (make-instance 'camera :position (3d-vectors:vec2 0 0) :screen-size 11))
          (player-object (make-game-object :sprite (make-instance 'sprite
                                                     :position (3d-vectors:vec2 0 0)
                                                     :size (3d-vectors:vec2 1 1)
                                                     :rotation 0
                                                     :texture *test-texture2*)
                                           :collider (make-instance 'aabb-collider :size (3d-vectors:vec2 1 1))
                                           :behavior (lambda (delta-time game-object)
                                                       (let* ((move-speed 3)
                                                              (move-dist (* move-speed delta-time))
                                                              (input-x 0)
                                                              (input-y 0))
                                                         (when (get-key-hold "w")
                                                               (setf input-y (1+ input-y)))
                                                         (when (get-key-hold "a")
                                                               (setf input-x (1- input-x)))
                                                         (when (get-key-hold "s")
                                                               (setf input-y (1- input-y)))
                                                         (when (get-key-hold "d")
                                                               (setf input-x (1+ input-x)))
                                                         
                                                         (let ((input (3d-vectors:vec2 input-x input-y)))
                                                           (when (not (3d-vectors:v= input (3d-vectors:vec2 0 0)))
                                                                 (3d-vectors:nvunit input)
                                                                 (game-object-move game-object (3d-vectors:v* input move-dist))))))
                                           :tags '(player)))

          (test-object1 (make-game-object :sprite (make-instance 'sprite :position (3d-vectors:vec2 2 2) :layer -1)
                                          :collider (make-instance 'aabb-collider :position (3d-vectors:vec2 2 2))
                                          :behavior (lambda (delta-time obj)
                                                      (declare (ignore delta-time))
                                                      (let ((player (get-tagged-object-collision obj 'player)))
                                                        (when player
                                                              (gtk-label-set-text debug-display "Collision AABB"))))))
          (test-object2 (make-game-object :sprite (make-instance 'sprite :position (3d-vectors:vec2 -2 2) :texture *test-circle* :layer -1)
                                          :collider (make-instance 'circle-collider :position (3d-vectors:vec2 -2 2))
                                          :behavior (lambda (delta-time obj)
                                                      (declare (ignore delta-time))
                                                      (let ((player (get-tagged-object-collision obj 'player)))
                                                        (when player
                                                              (gtk-label-set-text debug-display "Collision Circle")
                                                              (game-object-delete obj))))))
          (test-object3 (make-game-object :sprite (make-instance 'sprite :position (3d-vectors:vec2 2 -2) :rotation 1 :layer -1)
                                          :collider (make-instance 'rectangle-collider :position (3d-vectors:vec2 2 -2) :rotation 1)
                                          :behavior (lambda (delta-time obj)
                                                      (declare (ignore delta-time))
                                                      (let ((player (get-tagged-object-collision obj 'player)))
                                                        (when player
                                                              (gtk-label-set-text debug-display "Collision Rectangle"))))))
          (test-object4 (make-game-object :sprite (make-instance 'sprite :position (3d-vectors:vec2 -2 -2) :layer -1)
                                          :collider (make-instance 'aabb-collider :position (3d-vectors:vec2 -2 -2) :trigger T)
                                          :behavior (lambda (delta-time obj)
                                                      (declare (ignore delta-time))
                                                      (let ((player (get-tagged-object-collision obj 'player)))
                                                        (when player
                                                              (gtk-label-set-text debug-display "Collision AABB trigger"))))))
          (test-tiles (make-room (3d-vectors:vec2 -5 -4)))
          (is-fullscreen NIL))
      
      (game-object-register player-object)
      (game-object-register test-object1)
      (game-object-register test-object2)
      (game-object-register test-object3)
      (game-object-register test-object4)

      (setf (gtk-gl-area-has-depth-buffer area) T)
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
                            (gtk-label-set-text debug-display "")

                            (let ((delta-time (min delta-time (/ 1 30)))) ; game starts to slow down below 30 fps
                              (game-objects-update *game-objects* delta-time)))
                          
                          (setf (camera-position camera) (sprite-position (game-object-sprite player-object)))

                          (gl:clear-color 0.5 0.5 0.5 1.0)
                          (gl:clear :color-buffer :depth-buffer)
                          ;(gl:cull-face :back)
                          ;(gl:enable :depth-test)
                          ;(gl:depth-func :less)
                          ;(gl:depth-mask :true)
                          
                          (let ((vp-mat (camera-view-projection-matrix camera)))
                            (tile-array-draw test-tiles vp-mat)
                            (sprites-draw *game-object-sprites* vp-mat))
                          
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
