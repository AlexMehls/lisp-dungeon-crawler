(defpackage :tiles
  (:use :common-lisp :textures :game-object :collision)
  (:export :tile-type :tile-layer :tile-texture
           :tile-draw

           :make-tile-array
           :tile-array-tiles :tile-array-offset :tile-array-collider-objects
           :tile-array-draw
           :tile-array-add-room
           :tile-array-delete-collider-objects :tile-array-setup-collider-objects))

(in-package :tiles)

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
             :initform (3d-vectors:vec2 0 0))
     (collider-objects :reader tile-array-collider-objects
                       :initform '())))

(defun make-tile-array (w h offset)
  (make-instance 'tile-array
    :tiles (make-array `(,w ,h) :initial-element NIL)
    :offset offset))

(defmethod tile-array-draw ((obj tile-array) vp-matrix)
  (let ((tiles (tile-array-tiles obj)))
    (destructuring-bind (x y) (array-dimensions tiles)
      (dotimes (i x)
        (dotimes (j y)
          (let ((tile (aref tiles i j)))
            (when tile
                  (tile-draw tile (3d-vectors:v+ (3d-vectors:vec2 i j) (tile-array-offset obj)) vp-matrix))))))))

(defmethod tile-array-add-room ((obj tile-array) offset-x offset-y w h)
  (let ((floor-tile (make-instance 'tile :tile-type 'tile-floor :texture *test-floor-texture* :layer -10))
        (wall-tile  (make-instance 'tile :tile-type 'tile-wall :texture *test-wall-texture* :layer 10)))
    (with-slots (tiles) obj
      (destructuring-bind (tiles-w tiles-h) (array-dimensions tiles)
        (loop for y from (max offset-y 0) to (min (1- tiles-h) (1- (+ offset-y h)))
                do (loop for x from (max offset-x 0) to (min (1- tiles-w) (1- (+ offset-x w)))
                         do (if (or (= x offset-x) (= x (1- (+ offset-x w))) (= y offset-y) (= y (1- (+ offset-y h))))
                              (setf (aref tiles x y) wall-tile)
                              (setf (aref tiles x y) floor-tile))))))))

(defmethod tile-array-delete-collider-objects ((obj tile-array))
  (loop for col-obj in (tile-array-collider-objects obj)
          do (game-object-delete col-obj)))

(defmethod tile-array-setup-collider-objects ((obj tile-array))
  (tile-array-delete-collider-objects obj)
  ; TODO: better -> merge same tiles into bigger colliders
  (let ((tiles (tile-array-tiles obj)))
    (destructuring-bind (x y) (array-dimensions tiles)
      (dotimes (i x)
        (dotimes (j y)
          (let ((tile (aref tiles i j)))
            (when (and tile (eq (tile-type tile) 'tile-wall))
                  (game-object-register (make-game-object :collider (make-instance 'aabb-collider :position (3d-vectors:v+ (3d-vectors:vec2 i j) (tile-array-offset obj)))
                                                          :tags `(,(tile-type tile)))))))))))
