(defpackage :game
(:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp
        :textures :collision :player-input :sprite :game-object :behaviors :tiles :level-loading :camera))

(in-package :game)

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
           (base-screen-size 15)
           (zoom-strength 0.1)
           (zoom-level 0)
           (zoom-level-min -5)
           (zoom-level-max 5)
           (camera (make-instance 'camera :position (3d-vectors:vec2 0 0) :screen-size base-screen-size))
           (hp-display (gtk-label-new "HP:"))
           (player-object (make-game-object :sprite (make-instance 'sprite :texture *test-texture2* :static NIL)
                                            :collider (make-instance 'aabb-collider :trigger T)
                                            :behaviors (list (make-instance 'behavior-player-movement :move-speed 5)
                                                             (make-instance 'behavior-player-attack
                                                               :damage 1
                                                               :fire-rate 2
                                                               :pierce 1
                                                               :projectile-velocity 10
                                                               :projectile-size 0.5)
                                                             (make-instance 'behavior-destructable :hp 10)) ; TODO: change destroy behavior; Display HP in GUI
                                            :tags '(behaviors::player))) ; TODO: better solution for tags?

           (level-tiles (make-tile-array 256 256 (3d-vectors:vec2 -128 -128)))
           (is-fullscreen NIL)
           (level-generation-seed t) ; t = random, otherwise int or simple-array
           (level-generation-random-state (sb-ext:seed-random-state level-generation-seed))
           (debug-enabled NIL))
      
      (setf *active-camera* camera)

      (load-next-level player-object level-tiles level-generation-random-state)

      (setf (gtk-gl-area-has-depth-buffer area) T)
      (gtk-container-add window overlay)
      (gtk-container-add overlay area)
      (gtk-overlay-add-overlay overlay fixed-container)
      (gtk-fixed-put fixed-container box 0 0)
      (gtk-box-pack-start box fps-counter)
      (gtk-box-pack-start box hp-display)
      (gtk-box-pack-start box debug-display)

      (gtk-widget-add-events window '(:key-press-mask :key-release-mask :button-press-mask :button-release-mask :pointer-motion-mask :scroll-mask))
      
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

                          (let* ((delta-time (local-time:timestamp-difference curr-time prev-time)))
                            ;; Time and fps calculation
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
                            
                            ;; Inputs (not bound to player behavior)
                            (gtk-label-set-text debug-display "")
                            
                            (when (get-key-press "F3")
                                  (setf debug-enabled (not debug-enabled)))
                            (when debug-enabled
                                  (gtk-label-set-text debug-display "[DEBUG ENABLED]"))

                            (when (get-key-press "F11")
                                  (if is-fullscreen
                                      (gtk-window-unfullscreen window)
                                      (gtk-window-fullscreen window))
                                  (setf is-fullscreen (not is-fullscreen)))

                            ;; Debug only actions
                            (when (and (get-key-press "F1") debug-enabled)
                                  (load-next-level player-object level-tiles level-generation-random-state))
                            
                            ;; Updates (game starts to slow down below 30 fps)
                            (let ((delta-time (min delta-time (/ 1 30))))
                              (game-objects-update *game-objects* delta-time)))
                          
                          (gtk-label-set-text hp-display (format nil "HP: ~a" (behavior-destructable-hp (get-object-behavior-by-subtype player-object 'behavior-destructable))))

                          ;; Camera and zoom
                          (setf (camera-position *active-camera*) (sprite-position (game-object-sprite player-object)))
                          (let ((scroll (get-mouse-scroll)))
                            (incf zoom-level scroll)
                            (unless debug-enabled
                              (setf zoom-level (min zoom-level zoom-level-max))
                              (setf zoom-level (max zoom-level zoom-level-min)))
                            (let ((new-screen-size (* base-screen-size (exp (* (- zoom-level) zoom-strength)))))
                              (setf (camera-screen-size *active-camera*) new-screen-size)))
                          
                          ;; Rendering
                          (gl:finish)
                          (gl:clear-color 0.5 0.5 0.5 1.0)
                          (gl:clear :color-buffer :depth-buffer)
                          
                          (let ((vp-mat (camera-view-projection-matrix *active-camera*)))
                            (gl:use-program textures::*texture-shader-program*)
                            (send-draw-calls vp-mat)
                            (gl:use-program 0))
                          
                          ;; Bookkeeping
                          (setf *keys-pressed* NIL)
                          (setf *buttons-pressed* NIL)
                          (setf *scroll* 0)
                          (setf prev-time curr-time)
                          (gtk-gl-area-queue-render area)
                          NIL))
      (g-signal-connect area "resize"
                        (lambda (area width height)
                          (declare (ignore area))
                          (when (and (> height 0) (> width 0))
                                (setf (camera-screen-ratio *active-camera*) (/ width height))
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
                            (setf *keys-held* (adjoin keyval *keys-held*)))))
      (g-signal-connect window "key_release_event"
                        (lambda (widget event)
                          (declare (ignore widget))
                          (setf *keys-held* (set-difference *keys-held* `(,(gdk-event-key-keyval event))))))
      (g-signal-connect window "button_press_event"
                        (lambda (widget event)
                          (declare (ignore widget))
                          (let ((button (gdk-event-button-button event)))
                            (unless (member button *buttons-held*)
                              (setf *buttons-pressed* (adjoin button *buttons-pressed*)))
                            (setf *buttons-held* (adjoin button *buttons-held*)))))
      (g-signal-connect window "button_release_event"
                        (lambda (widget event)
                          (declare (ignore widget))
                          (setf *buttons-held* (set-difference *buttons-held* `(,(gdk-event-button-button event))))))
      (g-signal-connect window "scroll_event"
                        (lambda (widget event)
                          (declare (ignore widget))
                          (case (gdk-event-scroll-direction event)
                            (:up (incf *scroll*))
                            (:down (decf *scroll*)))))
      (g-signal-connect window "motion_notify_event"
                        (lambda (widget event)
                          (declare (ignore widget))
                          (setf *mouse-x* (gdk-event-motion-x event))
                          (setf *mouse-y* (gdk-event-motion-y event))))
      (gtk-widget-show-all window)))
  
  ;(sleep 0.001) ; wait until gtk loop starts
  (sb-thread:release-foreground) ; For better debug output in gtk-thread
  (gtk:join-gtk-main))
