(defpackage :tiles
  (:use :common-lisp :render-object :textures :game-object :collision :rooms)
  (:export :tile
           :tile-type :tile-layer

           :make-tile-array
           :tile-array-tiles :tile-array-offset :tile-array-collider-objects
           :tile-array-register-tiles :tile-array-free-tiles
           :tile-array-delete-collider-objects :tile-array-setup-collider-objects
           :tile-array-add-room :tile-array-block-connection))

(in-package :tiles)

(defmacro indices-to-position (i j h)
  `(3d-vectors:vec2 ,j (- ,h ,i 1)))

(defclass tile (render-object)
    ((tile-type :initarg :tile-type
                :reader tile-type
                :initform 'tile-floor)
     (layer :initarg :layer
            :accessor tile-layer
            :initform 0)))

(defmethod tile-model-matrix ((obj tile) pos)
  (3d-matrices:mtranslation (3d-vectors:v+ (3d-vectors:vxy_ pos) (3d-vectors:vec3 0 0 (tile-layer obj)))))

(defmethod tile-update-model-matrix ((obj tile) pos)
  (render-object-update-matrix obj (tile-model-matrix obj pos)))

(defclass tile-array ()
    ((tiles :initarg :tiles
            :reader tile-array-tiles
            :initform (make-array '(0 0) :element-type 'tile))
     (offset :initarg :offset
             :reader tile-array-offset
             :initform (3d-vectors:vec2 0 0))
     (collider-objects :reader tile-array-collider-objects
                       :initform '())))

(defun make-tile-array (w h offset)
  (make-instance 'tile-array
    :tiles (make-array `(,h ,w) :initial-element NIL)
    :offset offset))

(defmethod tile-array-register-tiles ((obj tile-array))
  (let ((tiles (tile-array-tiles obj)))
    (destructuring-bind (h w) (array-dimensions tiles)
      (dotimes (i h)
        (dotimes (j w)
          (let ((tile (aref tiles i j)))
            (when tile
                  (render-object-register tile)
                  (tile-update-model-matrix tile (3d-vectors:v+ (indices-to-position i j h) (tile-array-offset obj))))))))))

(defmethod tile-array-free-tiles ((obj tile-array))
  (let ((tiles (tile-array-tiles obj)))
    (destructuring-bind (h w) (array-dimensions tiles)
      (dotimes (i h)
        (dotimes (j w)
          (let ((tile (aref tiles i j)))
            (when tile
                  (render-object-free tile))))))))

(defmethod tile-array-delete-collider-objects ((obj tile-array))
  (loop for col-obj in (tile-array-collider-objects obj)
          do (game-object-delete col-obj)))

(defmethod tile-array-setup-collider-objects ((obj tile-array))
  (tile-array-delete-collider-objects obj)
  ; TODO: better -> merge same tiles into bigger colliders
  (let ((tiles (tile-array-tiles obj)))
    (destructuring-bind (h w) (array-dimensions tiles)
      (dotimes (i h)
        (dotimes (j w)
          (let ((tile (aref tiles i j)))
            (when (and tile (eq (tile-type tile) 'tile-wall))
                  (game-object-register (make-game-object :collider (make-instance 'aabb-collider :position (3d-vectors:v+ (indices-to-position i j h) (tile-array-offset obj)))
                                                          :tags `(,(tile-type tile)))))))))))

(defmethod tile-array-add-room ((obj tile-array) (room-obj room-tiles) offset-x offset-y)
  (with-slots (tiles) obj
    (destructuring-bind (tiles-h tiles-w) (array-dimensions tiles)
      (loop-room-tiles room-obj i-room j-room h-room w-room tile-type-symbol
        (let ((i (+ i-room (- tiles-h h-room offset-y)))
              (j (+ j-room offset-x)))
          (when (and (>= i 0) (< i tiles-h) (>= j 0) (< j tiles-w))
                (case tile-type-symbol
                  (rooms::tile-floor (setf (aref tiles i j) (make-instance 'tile :tile-type 'tile-floor :texture *test-floor-texture* :layer -10)))
                  (rooms::tile-wall (setf (aref tiles i j) (make-instance 'tile :tile-type 'tile-wall :texture *test-wall-texture* :layer 10))))))))))

(defmethod tile-array-block-connection ((obj tile-array) (room-obj room-tiles) direction offset-x offset-y)
  (with-slots (tiles) obj
    (destructuring-bind (tiles-h tiles-w) (array-dimensions tiles)
      (let ((layout (room-tiles-layout room-obj)))
        (destructuring-bind (h w) (array-dimensions layout)
          (loop for connection in (aref (room-tiles-connections room-obj) direction) do
                  (let ((c-offset (first connection)))
                    (dotimes (c (second connection))
                      (let ((i-room)
                            (j-room))
                        (case direction
                          (0 (setf i-room 0)
                             (setf j-room (+ c-offset c)))
                          (1 (setf i-room (+ c-offset c))
                             (setf j-room (1- w)))
                          (2 (setf i-room (1- h))
                             (setf j-room (+ c-offset c)))
                          (3 (setf i-room (+ c-offset c))
                             (setf j-room 0)))
                        (let ((i (+ i-room (- tiles-h h offset-y)))
                              (j (+ j-room offset-x)))
                          (when (and (>= i 0) (< i tiles-h) (>= j 0) (< j tiles-w))
                                (setf (aref tiles i j) (setf (aref tiles i j) (make-instance 'tile :tile-type 'tile-wall :texture *test-wall-texture* :layer 10))))))))))))))
                  
