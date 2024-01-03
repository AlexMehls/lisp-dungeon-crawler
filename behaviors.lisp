(defpackage :behaviors
  (:use :common-lisp :game-object :behavior :player-input :collision :textures :sprite :level-loading :prefab-object)
  (:export :behavior-update
           
           :behavior-player-movement
           :behavior-player-movement-move-speed

           :behavior-destructable
           :behavior-destructable-hp
           :behavior-destructable-damage

           :behavior-projectile
           :behavior-projectile-damage :behavior-projectile-pierce :behavior-projectile-direction :behavior-projectile-velocity

           :behavior-player-attack
           :behavior-player-attack-damage :behavior-player-attack-fire-rate :behavior-player-attack-pierce :behavior-player-attack-projectile-velocity :behavior-player-attack-projectile-size
           
           :behavior-simple-movement
           :behavior-simple-movement-move-speed :behavior-simple-movement-stop-distance :behavior-simple-movement-target-position

           :behavior-loading-zone

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
     (pierce :initarg :pierce
             :initform 0
             :reader behavior-projectile-pierce)
     (direction :initarg :direction
                :initform (3d-vectors:vec2 0 1)
                :reader behavior-projectile-direction)
     (velocity :initarg :velocity
               :initform 5
               :reader behavior-projectile-velocity)
     (lifetime :initarg :lifetime
               :initform 10.0)
     (target-tags :initarg :target-tags
                  :initform '())
     (hit-target-ids :initform '())))

(defmethod behavior-update ((behavior behavior-projectile) delta-time game-object)
  (with-slots (damage pierce direction velocity lifetime target-tags hit-target-ids) behavior
    (game-object-move game-object (3d-vectors:v* direction (* velocity delta-time)))

    (let ((collisions (get-object-collisions game-object))
          (destroy NIL))
      (loop for collider in collisions do
              (let ((obj (collider-parent collider)))
                (when (game-object-has-tag obj 'tiles::tile-wall)
                      (setf destroy T))
                (when (and (>= pierce 0) (intersection target-tags (game-object-tags obj)) (not (member (game-object-id obj) hit-target-ids)))
                      (decf pierce)
                      (setf hit-target-ids (adjoin (game-object-id obj) hit-target-ids))
                      (behavior-destructable-damage (get-object-behavior-by-subtype obj 'behavior-destructable) obj damage))))
      
      (decf lifetime delta-time)
      (when (or destroy (< pierce 0) (< lifetime 0))
            (game-object-delete game-object)))))

(defclass behavior-player-attack (behavior)
    ((damage :initarg :damage
             :initform 1
             :accessor behavior-player-attack-damage)
     (fire-rate :initarg :fire-rate
                  :initform 1
                  :accessor behavior-player-attack-fire-rate)
     (pierce :initarg :pierce
             :initform 0
             :accessor behavior-player-attack-pierce)
     (projectile-velocity :initarg :projectile-velocity
                          :initform 5
                          :accessor behavior-player-attack-projectile-velocity)
     (projectile-size :initarg :projectile-size
                      :initform 0.5
                      :accessor behavior-player-attack-projectile-size)
     (cooldown :initform 0)))

(defmethod behavior-update ((behavior behavior-player-attack) delta-time game-object)
  (with-slots (damage fire-rate pierce projectile-velocity projectile-size cooldown) behavior
    (if (> cooldown 0)
        (decf cooldown delta-time)
        (when (get-button-hold 1)
              (setf cooldown (/ 1 fire-rate))
              (let* ((spawn-position (collider-position (game-object-collider game-object))) ; Assumes that player has a collider
                     (spawn-direction (3d-vectors:vunit (3d-vectors:v- (get-mouse-world-pos) spawn-position))))
                (game-object-register (make-prefab-object 'prefab-basic-projectile spawn-position spawn-direction projectile-size damage pierce projectile-velocity)))))))

;; Simple movement behavior (no acceleration; no pathfinding; can stop at a distance from the target)
(defclass behavior-simple-movement (behavior)
    ((move-speed :initarg :move-speed
                 :initform 3
                 :accessor behavior-simple-movement-move-speed)
     (stop-distance :initarg :stop-distance
                    :initform 0
                    :accessor behavior-simple-movement-stop-distance)
     (target-position :initarg :target-position
                      :initform NIL
                      :accessor behavior-simple-movement-target-position)))

(defmethod behavior-update ((behavior behavior-simple-movement) delta-time game-object)
  (with-slots (move-speed stop-distance target-position) behavior
    (when target-position
          (let* ((object-position (collider-position (game-object-collider game-object)))
                 (distance-vec (3d-vectors:v- target-position object-position))
                 (distance (3d-vectors:v2norm distance-vec))
                 (remaining-move-distance (- distance stop-distance))
                 (move-distance (max (min (* move-speed delta-time) remaining-move-distance) 0)))
            (when (> move-distance 0)
                  (game-object-move game-object (3d-vectors:v* (3d-vectors:vunit distance-vec) move-distance)))))))

(defclass behavior-loading-zone (behavior)
    ((level-tiles :initarg :level-tiles)
     (generation-random-state :initarg :generation-random-state)))

(defmethod behavior-update ((behavior behavior-loading-zone) delta-time game-object)
  (let ((player (get-tagged-object-collision game-object 'player)))
    (when player
          (with-slots (level-tiles generation-random-state) behavior
            (load-next-level player level-tiles generation-random-state)))))

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
