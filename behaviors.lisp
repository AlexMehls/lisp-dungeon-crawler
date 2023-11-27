(defpackage :behaviors
  (:use :common-lisp :game-object :behavior :player-input :collision)
  (:export :behavior-update
           
           :behavior-player-movement
           :behavior-player-movement-move-speed
           
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
                (setf (gtk:label-text (behavior-collision-test-label behavior)) (behavior-collision-test-message behavior)))
          (when (behavior-collision-test-destroy behavior)
                (game-object-delete game-object)))))
