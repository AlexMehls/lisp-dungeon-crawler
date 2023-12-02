(defpackage :game
(:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp
        :textures :collision :player-input :sprite :game-object :behaviors :tiles :rooms))

(in-package :game)

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

(defun main ()
  (within-main-loop
    (let* ((window (gtk-window-new :toplevel))
           (overlay (gtk-overlay-new))
           (area (make-instance 'gtk-gl-area :auto-render T)) ; maybe render manually?
           (fixed-container (gtk-fixed-new))
           (box (gtk-box-new :vertical 1))
           (fps-counter (gtk-label-new "FPS:"))
           (debug-display (gtk-label-new ""))
           (prev-time (local-time:now))
           (curr-time (local-time:now))
           (fps-vals (queues:make-queue :simple-queue))
           (camera (make-instance 'camera :position (3d-vectors:vec2 0 0) :screen-size 15))
           (player-object (make-game-object :sprite (make-instance 'sprite :texture *test-texture2*)
                                            :collider (make-instance 'aabb-collider)
                                            :behaviors (list (make-instance 'behavior-player-movement :move-speed 5))
                                            :tags '(behaviors::player))) ; TODO: better solution for tags?
           (test-tiles (make-tile-array 64 64 (3d-vectors:vec2 -32 -32)))
           (stress-test-tiles (make-tile-array 256 256 (3d-vectors:vec2 -128 -128)))
           (is-fullscreen NIL))
      
      (setf *active-camera* camera)
      (let ((floor-tile (make-instance 'tile :tile-type 'tile-floor :texture *test-floor-texture* :layer -10)))
        (with-slots (tiles::tiles) stress-test-tiles
          (destructuring-bind (tiles-h tiles-w) (array-dimensions tiles::tiles)
            (dotimes (i tiles-h)
              (dotimes (j tiles-w)
                (setf (aref tiles::tiles i j) floor-tile))))))

      (tile-array-add-room test-tiles *room-1* 24 24)
      (tile-array-add-room test-tiles *room-2* 24 (+ 24 (array-dimension (room-tiles-layout *room-1*) 0)))
      (tile-array-add-room test-tiles *room-1* (+ 24 (array-dimension (room-tiles-layout *room-1*) 1)) 24)
      (tile-array-add-room test-tiles *room-2* (+ 24 (array-dimension (room-tiles-layout *room-1*) 1)) (+ 24 (array-dimension (room-tiles-layout *room-1*) 0)))
      (tile-array-setup-collider-objects test-tiles)

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
      ;(gtk-container-add window area)
      (gtk-container-add window overlay)
      (gtk-container-add overlay area)
      (gtk-overlay-add-overlay overlay fixed-container)
      (gtk-fixed-put fixed-container box 0 0)
      (gtk-box-pack-start box fps-counter)
      (gtk-box-pack-start box debug-display)

      (gtk-widget-add-events window '(:key-press-mask :key-release-mask :button-press-mask :button-release-mask :pointer-motion-mask))
      
      (g-signal-connect area "realize"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-gl-area-make-current area)
                          (gtk-gl-area-get-error area)

                          (setup-opengl)
                          (load-textures)))
      (g-signal-connect area "unrealize"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-gl-area-make-current area)
                          (gtk-gl-area-get-error area)
                          (delete-textures)
                          (shutdown-opengl)))
      (g-signal-connect area "render"
                        (lambda (area context)
                          (declare (ignore context))
                          (setf curr-time (local-time:now))
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
                              (gtk-label-set-text fps-counter (format nil "FPS: ~a" (round avg-fps))))
                            
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

                          (gl:finish)
                          (gl:clear-color 0.5 0.5 0.5 1.0)
                          (gl:clear :color-buffer :depth-buffer)
                          ;(gl:cull-face :back)
                          ;(gl:enable :depth-test)
                          ;(gl:depth-func :less)
                          ;(gl:depth-mask :true)
                          
                          (let ((vp-mat (camera-view-projection-matrix *active-camera*)))
                            (gl:use-program textures::*texture-shader-program*)

                            ;(tile-array-draw test-tiles vp-mat)
                            (tile-array-draw stress-test-tiles vp-mat)
                            (sprites-draw *game-object-sprites* vp-mat)

                            (send-draw-calls vp-mat)

                            (gl:use-program 0))
                          
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
