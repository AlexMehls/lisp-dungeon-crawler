(defpackage :level-loading
  (:use :common-lisp :game-object :tiles :level-generation)
  (:export :*start-room* :*end-room* :*rooms*
           :load-next-level))

(in-package :level-loading)

(defvar *start-room*)
(defvar *end-room*)
(defvar *rooms*)

(defun load-next-level (player-obj level-tile-array &optional (random-state *random-state*))
  ; Clear all tiles (including colliders)
  (tile-array-clear level-tile-array)

  ; Delete all other game-objects (projectiles, enemies, ...)
  (game-objects-clear *game-objects*)
  (game-object-register player-obj)
  
  ; Setup new level
  (game-object-set-pos player-obj (generate-level level-tile-array *start-room* *end-room* *rooms* 20 100 random-state))
  (tile-array-register-tiles level-tile-array)
  (tile-array-setup-collider-objects level-tile-array))
