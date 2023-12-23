(defpackage :behaviors
  (:use :common-lisp :game-object :behavior :player-input :collision :textures :sprite)
  (:export :behavior-update
           
           :behavior-player-movement
           :behavior-player-movement-move-speed

           :behavior-destructable
           :behavior-destructable-hp
           :behavior-destructable-damage

           :behavior-projectile
           :behavior-projectile-damage :behavior-projectile-direction :behavior-projectile-velocity

           :behavior-player-attack
           :behavior-player-attack-damage :behavior-player-attack-attack-rate :behavior-player-attack-projectile-velocity :behavior-player-attack-projectile-size
           
           :behavior-collision-test
           :behavior-collision-test-message :behavior-collision-test-label :behavior-collision-test-destroy))

(in-package :behaviors)

(defclass behavior-player-movement (behavior)
    ((move-speed :initarg :move-speed
                 :initform 3
                 :accessor behavior-player-movement-move-speed)))

(defmethod behavior-update ((behavior behavior-player-movement) delta-time game-object)
  (let ((move-dist (* (behavior-player-movement-move-speed behavior) delta-time))
        (input-x 0)
        (input-y 0))
    (when (get-key-hold "w")
          (incf input-y))
    (when (get-key-hold "a")
          (decf input-x))
    (when (get-key-hold "s")
          (decf input-y))
    (when (get-key-hold "d")
          (incf input-x))
    
    (let ((input (3d-vectors:vec2 input-x input-y)))
      (when (not (3d-vectors:v= input (3d-vectors:vec2 0 0)))
            (3d-vectors:nvunit input)
            (game-object-move game-object (3d-vectors:v* input move-dist))))))

(defclass behavior-destructable (behavior)
    ((hitpoints :initarg :hp
                :initform 1
                :accessor behavior-destructable-hp)))

(defmethod behavior-destructable-damage ((behavior behavior-destructable) game-object damage)
  (with-slots (hitpoints) behavior
    (decf hitpoints damage)
    (when (<= hitpoints 0)
          (game-object-delete game-object))))

(defclass behavior-projectile (behavior)
    ((damage :initarg :damage
             :initform 1
             :reader behavior-projectile-damage)
     (direction :initarg :direction
                :initform (3d-vectors:vec2 0 1)
                :reader behavior-projectile-direction)
     (velocity :initarg :velocity
               :initform 5
               :reader behavior-projectile-velocity)
     (lifetime :initarg :lifetime
               :initform 10.0)
     (target-tags :initarg :target-tags
                  :initform '())))

(defmethod behavior-update ((behavior behavior-projectile) delta-time game-object)
  (with-slots (damage direction velocity lifetime target-tags) behavior
    (game-object-move game-object (3d-vectors:v* direction (* velocity delta-time)))

    (let ((collisions (get-object-collisions game-object))
          (destroy NIL))
      (loop for collider in collisions do
              (let ((obj (collider-parent collider)))
                (when (game-object-has-tag obj 'tiles::tile-wall)
                      (setf destroy T))
                (when (intersection target-tags (game-object-tags obj))
                      (setf destroy T)
                      (behavior-destructable-damage (get-object-behavior-by-subtype obj 'behavior-destructable) obj damage))))
      
      (decf lifetime delta-time)
      (when (or destroy (< lifetime 0))
            (game-object-delete game-object)))))

(defclass behavior-player-attack (behavior)
    ((damage :initarg :damage
             :initform 1
             :accessor behavior-player-attack-damage)
     (attack-rate :initarg :attack-rate
                  :initform 1
                  :accessor behavior-player-attack-attack-rate)
     (projectile-velocity :initarg :projectile-velocity
                          :initform 5
                          :accessor behavior-player-attack-projectile-velocity)
     (projectile-size :initarg :projectile-size
                      :initform 0.5
                      :accessor behavior-player-attack-projectile-size)
     (cooldown :initform 0)))

(defmethod behavior-update ((behavior behavior-player-attack) delta-time game-object)
  (with-slots (damage attack-rate projectile-velocity projectile-size cooldown) behavior
    (if (> cooldown 0)
        (decf cooldown delta-time)
        (when (get-button-hold 1)
              (setf cooldown (/ 1 attack-rate))
              (let* ((spawn-position (collider-position (game-object-collider game-object))) ; Assumes that player has a collider
                     (spawn-direction (3d-vectors:vunit (3d-vectors:v- (get-mouse-world-pos) spawn-position)))
                     (spawn-rotation (atan (- (3d-vectors:vx spawn-direction)) (3d-vectors:vy spawn-direction))))
                (game-object-register (make-game-object :sprite (make-instance 'sprite
                                                                  :position spawn-position
                                                                  :rotation spawn-rotation
                                                                  :size (3d-vectors:vec2 projectile-size projectile-size)
                                                                  :texture *test-circle*
                                                                  :layer 11
                                                                  :static NIL)
                                                        :collider (make-instance 'circle-collider :position spawn-position :radius (/ projectile-size 2) :trigger T)
                                                        :behaviors (list (make-instance 'behavior-projectile
                                                                           :damage damage :target-tags '(enemy)
                                                                           :direction spawn-direction :velocity projectile-velocity))
                                                        :tags '(projectile))))))))

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
                (gtk:gtk-label-set-text (behavior-collision-test-label behavior) (behavior-collision-test-message behavior)))
          (when (behavior-collision-test-destroy behavior)
                (game-object-delete game-object)))))
