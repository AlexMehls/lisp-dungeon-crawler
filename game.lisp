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

(defvar *active-camera* NIL)

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

(defun make-tile-array (w h offset)
  (make-instance 'tile-array
    :tiles (make-array `(,w ,h) :initial-element NIL)
    :offset offset))

(defmethod tile-array-draw ((obj tile-array) vp-matrix)
  (let ((tiles (tile-array-tiles obj)))
    (destructuring-bind (x y) (array-dimensions tiles)
      (dotimes (i x)
        (dotimes (j y)
          (let ((tile (aref tiles i j))
                (pos (3d-vectors:v+ (3d-vectors:vec2 i j) (tile-array-offset obj))))
            (when tile
                  (tile-draw tile pos vp-matrix))))))))

(defmethod tile-array-add-room ((obj tile-array) offset-x offset-y w h)
  (let ((floor-tile (make-instance 'tile :texture *test-floor-texture* :layer -10))
        (wall-tile  (make-instance 'tile :texture *test-wall-texture* :layer 10)))
    (with-slots (tiles) obj
      (destructuring-bind (tiles-w tiles-h) (array-dimensions tiles)
        (loop for y from (max offset-y 0) to (min (1- tiles-h) (1- (+ offset-y h)))
                do (loop for x from (max offset-x 0) to (min (1- tiles-w) (1- (+ offset-x w)))
                         do (if (or (= x offset-x) (= x (1- (+ offset-x w))) (= y offset-y) (= y (1- (+ offset-y h))))
                              (setf (aref tiles x y) wall-tile)
                              (setf (aref tiles x y) floor-tile))))))))

; TODO: remove?
(defun make-room (pos)
  (let* ((w 11)
         (h 9)
         (tiles (make-array `(,w ,h) :initial-element NIL))
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
     (behaviors :initarg :behaviors
                :initform '()
                :reader game-object-behaviors)
     (tags :initarg :tags
           :initform '()
           :accessor game-object-tags)))

;; Also sets the parent reference for the collider
(defun make-game-object (&key sprite collider behaviors tags)
  (let ((obj (make-instance 'game-object :sprite sprite :behaviors behaviors :tags tags)))
    (when collider
          (setf (slot-value obj 'collider) collider)
          (setf (collider-parent collider) obj)) ; circular reference (should still be handled by garbage collector)
    obj))

;; Additional quick access to colliders and sprites for rendereing / collision detection
;; Faster?
(defvar *game-objects* (make-hash-table))
(defvar *game-object-colliders* (make-hash-table))
(defvar *game-object-sprites* (make-hash-table))

(defmethod game-object-register ((obj game-object))
  (let ((id (game-object-id obj)))
    (setf (gethash id *game-objects*) obj)
    (when (game-object-collider obj)
          (setf (gethash id *game-object-colliders*) (game-object-collider obj)))
    (when (game-object-sprite obj)
          (setf (gethash id *game-object-sprites*) (game-object-sprite obj)))))

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
  (loop for behavior in (game-object-behaviors obj)
          do (behavior-update behavior delta-time obj)))

(defun game-objects-update (game-objects delta-time)
    (loop for obj being the hash-values of game-objects
          do (game-object-update obj delta-time)))

(defmethod game-object-has-tag ((obj game-object) tag)
  (member tag (game-object-tags obj)))

;; Gets the first object colliding with the given object and with matching tag
(defmacro get-tagged-object-collision (obj tag)
  `(if (game-object-collider ,obj)
       (let ((collisions (collider-get-collisions (game-object-collider ,obj) *game-object-colliders*))) 
         (loop for collider in collisions 
                 when (game-object-has-tag (collider-parent collider) ,tag) 
                 return (collider-parent collider)))
       NIL))

(defvar *keys-held* NIL)
(defvar *keys-pressed* NIL)

(defvar *buttons-held* NIL)
(defvar *buttons-pressed* NIL)

(defvar *mouse-x* 0)
(defvar *mouse-y* 0)

(defvar *window-w* 1)
(defvar *window-h* 1)

(defmacro gdk-keyval (name)
  (gdk-keyval-from-name name)) ; Only needs to be evaluated once -> macro

(defmacro get-key-press (name)
  `(member (gdk-keyval ,name) *keys-pressed*))

(defmacro get-key-hold (name)
  `(member (gdk-keyval ,name) *keys-held*))

;; Swaps mouse buttons 2 and 3 (mouse 3 should be the middle button)
(defmacro gdk-mouse-button (val)
  (case val
    (2 3)
    (3 2)
    (otherwise val)))

(defmacro get-button-press (val)
  `(member (gdk-mouse-button ,val) *buttons-pressed*))

(defmacro get-button-hold (val)
  `(member (gdk-mouse-button ,val) *buttons-held*))

(defmacro get-mouse-screen-pos ()
  `(3d-vectors:vec2 *mouse-x* *mouse-y*))

;; Returns pos relative to screen center and using in-game coordinates
(defmacro screen-pos-normalized (screen-pos)
  `(3d-vectors:v* (3d-vectors:v* (3d-vectors:v- ,screen-pos (3d-vectors:vec2 (/ *window-w* 2) (/ *window-h* 2))) (3d-vectors:vec2 1 -1)) (/ (camera-screen-size *active-camera*) *window-h*)))

(defmacro screen-pos-unnormalized (world-pos)
  `(3d-vectors:v+ (3d-vectors:v/ (3d-vectors:v/ ,world-pos (/ (camera-screen-size *active-camera*) *window-h*)) (3d-vectors:vec2 1 -1)) (3d-vectors:vec2 (/ *window-w* 2) (/ *window-h* 2))))

(defmacro get-mouse-world-pos ()
  `(screen-pos-normalized (get-mouse-screen-pos)))

(defclass behavior ()
    ())

(defmethod behavior-update ((behavior behavior) delta-time game-object))

(defclass behavior-player-movement (behavior)
    ((move-speed :initarg :move-speed
                 :initform 3
                 :accessor behavior-player-movement-move-speed)))

(defmethod behavior-update ((behavior behavior-player-movement) delta-time game-object)
  (let ((move-dist (* (behavior-player-movement-move-speed behavior) delta-time))
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

(defclass behavior-collision-test (behavior)
    ((message :initarg :message
              :initform "Collision"
              :accessor behavior-collision-test-message)
     (label :initarg :label
            :initform NIL
            :accessor behavior-collision-test-label)
     (destroy :initarg :destroy
              :initform NIL
              :accessor behavior-collision-test-destroy)))

(defmethod behavior-update ((behavior behavior-collision-test) delta-time game-object)
  (let ((player (get-tagged-object-collision game-object 'player)))
    (when player
          (when (behavior-collision-test-label behavior)
                (gtk-label-set-text (behavior-collision-test-label behavior) (behavior-collision-test-message behavior)))
          (when (behavior-collision-test-destroy behavior)
                (game-object-delete game-object)))))

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
           (camera (make-instance 'camera :position (3d-vectors:vec2 0 0) :screen-size 15))
           (player-object (make-game-object :sprite (make-instance 'sprite :texture *test-texture2*)
                                            :collider (make-instance 'aabb-collider)
                                            :behaviors (list (make-instance 'behavior-player-movement :move-speed 5))
                                            :tags '(player)))
           (test-tiles (make-tile-array 64 64 (3d-vectors:vec2 -32 -32)))
           (is-fullscreen NIL))
      
      (setf *active-camera* camera)
      (tile-array-add-room test-tiles 27 28 11 9)
      ;(tile-array-add-room test-tiles 61 61 5 5)

      (game-object-register player-object)
      (game-object-register (make-game-object :sprite (make-instance 'sprite :position (3d-vectors:vec2 2 2) :layer -1)
                                              :collider (make-instance 'aabb-collider :position (3d-vectors:vec2 2 2))
                                              :behaviors (list (make-instance 'behavior-collision-test :message "Collision AABB" :label debug-display))))
      (game-object-register (make-game-object :sprite (make-instance 'sprite :position (3d-vectors:vec2 -2 2) :texture *test-circle* :layer -1)
                                              :collider (make-instance 'circle-collider :position (3d-vectors:vec2 -2 2))
                                              :behaviors (list (make-instance 'behavior-collision-test :message "Collision Circle" :label debug-display :destroy T))))
      (game-object-register (make-game-object :sprite (make-instance 'sprite :position (3d-vectors:vec2 2 -2) :rotation 1 :layer -1)
                                              :collider (make-instance 'rectangle-collider :position (3d-vectors:vec2 2 -2) :rotation 1)
                                              :behaviors (list (make-instance 'behavior-collision-test :message "Collision Rectangle" :label debug-display))))
      (game-object-register (make-game-object :sprite (make-instance 'sprite :position (3d-vectors:vec2 -2 -2) :layer -1)
                                              :collider (make-instance 'aabb-collider :position (3d-vectors:vec2 -2 -2) :trigger T)
                                              :behaviors (list (make-instance 'behavior-collision-test :message "Collision AABB 2" :label debug-display))))

      (setf (gtk-gl-area-has-depth-buffer area) T)
      (gtk-container-add window overlay)
      (gtk-container-add overlay area)
      (gtk-overlay-add-overlay overlay fixed-container)
      (gtk-fixed-put fixed-container box 0 0)
      ;(gtk-fixed-put fixed-container fps-counter 0 0)
      (gtk-box-pack-start box fps-counter)
      (gtk-box-pack-start box debug-display)

      (gtk-widget-add-events window '(:key-press-mask :key-release-mask :button-press-mask :button-release-mask :pointer-motion-mask))

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
                            
                            ;(when (get-button-press 1)
                            ;      (format t "Left Button!~%"))
                            ;(when (get-button-press 2)
                            ;      (format t "Right Button!~%"))
                            ;(when (get-button-press 3)
                            ;      (format t "Middle Button!~%"))
                            
                            (gtk-label-set-text debug-display "")

                            ;(let ((mouse-pos (get-mouse-world-pos)))
                            ;  (gtk-label-set-text debug-display (concatenate 'string
                            ;                                      "Mouse pos: "
                            ;                                      (write-to-string (3d-vectors:vx mouse-pos))
                            ;                                      ", "
                            ;                                      (write-to-string (3d-vectors:vy mouse-pos)))))

                            (let ((delta-time (min delta-time (/ 1 30)))) ; game starts to slow down below 30 fps
                              (game-objects-update *game-objects* delta-time)))
                          
                          (setf (camera-position *active-camera*) (sprite-position (game-object-sprite player-object)))

                          (gl:clear-color 0.5 0.5 0.5 1.0)
                          (gl:clear :color-buffer :depth-buffer)
                          ;(gl:cull-face :back)
                          ;(gl:enable :depth-test)
                          ;(gl:depth-func :less)
                          ;(gl:depth-mask :true)
                          
                          (let ((vp-mat (camera-view-projection-matrix *active-camera*)))
                            (tile-array-draw test-tiles vp-mat)
                            (sprites-draw *game-object-sprites* vp-mat))
                          
                          (setf *keys-pressed* NIL)
                          (setf *buttons-pressed* NIL)
                          (setf prev-time curr-time)
                          (gtk-gl-area-queue-render area) ; maybe render manually? -> gdk frame clock not working
                          NIL))
      (g-signal-connect area "resize"
                        (lambda (area width height)
                          (declare (ignore area))
                          (when (and (> height 0) (> width 0))
                                (setf (slot-value *active-camera* 'screen-ratio) (/ width height))
                                (setf *window-w* width)
                                (setf *window-h* height))
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
      (g-signal-connect window "button_press_event"
                        (lambda (widget event)
                          (declare (ignore widget))
                          (let ((button (gdk-event-button-button event)))
                            (unless (member button *buttons-held*)
                              (setf *buttons-pressed* (adjoin button *buttons-pressed*)))
                            (setf *buttons-held* (adjoin button *buttons-held*)))
                          ;(format t "Event: ~a~%" event)
                          ))
      (g-signal-connect window "button_release_event"
                        (lambda (widget event)
                          (declare (ignore widget))
                          (setf *buttons-held* (set-difference *buttons-held* `(,(gdk-event-button-button event))))
                          ;(format t "Event: ~a~%" event)
                          ))
      (g-signal-connect window "motion_notify_event"
                        (lambda (widget event)
                          (declare (ignore widget))
                          (setf *mouse-x* (gdk-event-motion-x event))
                          (setf *mouse-y* (gdk-event-motion-y event))
                          ;(format t "Event: ~a~%" event)
                          ))
      (gtk-widget-show-all window)))
  
  ;(sleep 0.001) ; wait until gtk loop starts
  (sb-thread:release-foreground) ; For better debug output in gtk-thread
  (gtk:join-gtk-main))
