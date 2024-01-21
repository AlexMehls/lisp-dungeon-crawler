(defpackage :game-object
  (:use :common-lisp :collision :sprite :render-object :behavior)
  (:export :make-game-object
           :*game-objects* :*game-object-colliders* :*game-object-sprites*
           :game-object-id :game-object-sprite :game-object-collider :game-object-behaviors :game-object-tags
           :game-object-register :game-object-delete-by-id :game-object-delete

           :game-object-move :game-object-set-pos
           :game-object-update :game-objects-update
           :game-objects-clear
           :game-object-has-tag :get-tagged-object
           :get-object-collisions :get-tagged-object-collision
           :get-objects-line-of-sight-collisions :get-tagged-objects-line-of-sight-collisions
           :get-object-behavior-by-subtype))

(in-package :game-object)

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
(defvar *game-object-sprites* (make-hash-table)) ; TODO: remove?

(defmethod game-object-register ((obj game-object))
  (let ((id (game-object-id obj)))
    (setf (gethash id *game-objects*) obj)
    (when (game-object-collider obj)
          (setf (gethash id *game-object-colliders*) (game-object-collider obj)))
    (when (game-object-sprite obj)
          (render-object-register (game-object-sprite obj))
          (sprite-update-model-matrix (game-object-sprite obj))
          (setf (gethash id *game-object-sprites*) (game-object-sprite obj)))))

(defun game-object-delete-by-id (id)
  (let ((game-obj (gethash id *game-objects*)))
    (when game-obj
          (let ((sprite-obj (game-object-sprite game-obj)))
            (when sprite-obj
                  (render-object-free sprite-obj)))))
  (remhash id *game-objects*)
  (remhash id *game-object-colliders*)
  (remhash id *game-object-sprites*))

(defmethod game-object-delete ((obj game-object))
  (game-object-delete-by-id (game-object-id obj)))

;; Moves game-object (collider and sprite), while respecting other colliders
(defmethod game-object-move ((obj game-object) delta-pos)
  (with-slots (sprite collider) obj
    (let ((corrected-delta-pos (collider-resolve-collisions collider *game-object-colliders* delta-pos)))
      (setf (sprite-position sprite) (3d-vectors:v+ (sprite-position sprite) corrected-delta-pos))
      (sprite-update-model-matrix sprite)
      (setf (collider-position collider) (3d-vectors:v+ (collider-position collider) corrected-delta-pos)))))

;; Sets a game-object's (collider and sprite) position, without respecting other colliders
(defmethod game-object-set-pos ((obj game-object) pos)
  (with-slots (sprite collider) obj
    (setf (sprite-position sprite) pos)
    (sprite-update-model-matrix sprite)
    (setf (collider-position collider) pos)))

(defmethod game-object-update ((obj game-object) delta-time)
  (loop for behavior in (game-object-behaviors obj)
          do (behavior-update behavior delta-time obj)))

(defun game-objects-update (game-objects delta-time)
  (loop for obj being the hash-values of game-objects
        do (when (eq (type-of obj) 'game-object) ; For some reason there are sometimes 0-values in the hashtable (just for one frame)
                   (game-object-update obj delta-time))))

(defun game-objects-clear (game-objects)
  (loop for id being the hash-keys of game-objects do
          (game-object-delete-by-id id)))

(defmethod game-object-has-tag ((obj game-object) tag)
  (member tag (game-object-tags obj)))

;; Gets the first object with the given tag
(defmacro get-tagged-object (tag)
  `(loop for obj being the hash-values of *game-objects*
           when (game-object-has-tag obj ,tag) 
           return obj))

;; Gets all colliders colliding with the given object
(defmacro get-object-collisions (obj)
  `(if (game-object-collider ,obj)
       (collider-get-collisions (game-object-collider ,obj) *game-object-colliders*)
       NIL))

;; Gets the first object colliding with the given object and with matching tag
(defmacro get-tagged-object-collision (obj tag)
  `(let ((collisions (get-object-collisions ,obj)))
     (loop for collider in collisions 
             when (game-object-has-tag (collider-parent collider) ,tag) 
             return (collider-parent collider))))

;; Gets all colliders blocking line-of-sight between given objects
(defmacro get-objects-line-of-sight-collisions (obj1 obj2)
  `(if (and (game-object-collider ,obj1) (game-object-collider ,obj2))
       (line-of-sight-get-collisions (collider-position (game-object-collider ,obj1))
                                     (collider-position (game-object-collider ,obj2))
                                     *game-object-colliders*)
       NIL))

;; Gets the first object blocking line-of-sight between given objects and with matching tag
(defmacro get-tagged-objects-line-of-sight-collisions (obj1 obj2 tag)
  `(let ((collisions (get-objects-line-of-sight-collisions ,obj1 ,obj2)))
     (loop for collider in collisions
             when (game-object-has-tag (collider-parent collider) ,tag) 
             return (collider-parent collider))))

;; Gets the first behavior of the given object with the matching subtype
(defmacro get-object-behavior-by-subtype (obj type)
  `(loop for behavior in (game-object-behaviors ,obj)
           when (eq (type-of behavior) ,type)
           return behavior))
