(defpackage :prefab-objects
  (:use :common-lisp :prefab-object :game-object :behaviors :collision :textures :sprite))

(in-package :prefab-objects)

(defprefab prefab-basic-projectile (position direction size damage pierce velocity)
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
                                        :damage damage :pierce pierce :target-tags '(behaviors::enemy)
                                        :direction direction :velocity velocity))
                    :tags '(behaviors::projectile)))
