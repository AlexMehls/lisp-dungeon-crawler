(defpackage :prefab-objects
  (:use :common-lisp :prefab-object :game-object :behaviors :collision :textures :sprite))

(in-package :prefab-objects)

(defprefab prefab-basic-projectile (position direction size damage pierce velocity target-tags)
  (3d-vectors:nvunit direction)
  (make-game-object :sprite (make-instance 'sprite
                              :position position
                              :rotation (atan (- (3d-vectors:vx direction)) (3d-vectors:vy direction))
                              :size (3d-vectors:vec2 size size)
                              :texture *test-circle*
                              :layer 11
                              :static NIL)
                    :collider (make-instance 'circle-collider :position position :radius (/ size 2) :trigger T)
                    :behaviors (list (make-instance 'behavior-projectile
                                        :damage damage :pierce pierce :target-tags target-tags
                                        :direction direction :velocity velocity))
                    :tags '(behaviors::projectile)))

(defprefab prefab-basic-contact-enemy (position)
  (make-game-object :sprite (make-instance 'sprite
                              :position position
                              :rotation 0
                              :size (3d-vectors:vec2 0.75 0.75)
                              :texture *enemy-melee-texture*
                              :layer -1
                              :static NIL)
                    :collider (make-instance 'aabb-collider :position position :size (3d-vectors:vec2 0.75 0.75) :trigger T)
                    :behaviors (list (make-instance 'behavior-destructable :hp 3)
                                     (make-instance 'behavior-simple-movement :stop-distance 0)
                                     (make-instance 'behavior-enemy-contact
                                       :damage 1
                                       :attack-rate 2
                                       :vision-range 15))
                    :tags '(behaviors::enemy)))

(defprefab prefab-basic-ranged-enemy (position)
  (make-game-object :sprite (make-instance 'sprite
                              :position position
                              :rotation 0
                              :size (3d-vectors:vec2 0.75 0.75)
                              :texture *enemy-ranged-texture*
                              :layer -1
                              :static NIL)
                    :collider (make-instance 'aabb-collider :position position :size (3d-vectors:vec2 0.75 0.75) :trigger T)
                    :behaviors (list (make-instance 'behavior-destructable :hp 3)
                                     (make-instance 'behavior-simple-movement :stop-distance 10)
                                     (make-instance 'behavior-enemy-ranged
                                       :damage 1
                                       :attack-rate 1
                                       :vision-range 15
                                       :pierce 0
                                       :projectile-velocity 5
                                       :projectile-size 0.5))
                    :tags '(behaviors::enemy)))

(defprefab prefab-stairs (position level-tiles generation-random-state)
  (make-game-object :sprite (make-instance 'sprite :position position :static T)
                    :collider (make-instance 'aabb-collider :position position :trigger T)
                    :behaviors (list (make-instance 'behavior-loading-zone :level-tiles level-tiles :generation-random-state generation-random-state))))
