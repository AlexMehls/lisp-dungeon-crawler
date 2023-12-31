(defpackage :room
  (:use :common-lisp)
  (:export :make-hash-table-with-pairs :make-connections-array :connection-index-complement :next-indices
           :room-tiles
           :room-tiles-keys :room-tiles-layout :room-tiles-connections :room-tiles-game-objects
           :copy-room-tiles-rotated
           :loop-room-tiles :room-indices-to-position :room-tiles-find-connections :room-tiles-can-connect ))

(in-package :room)

(defmacro make-hash-table-with-pairs (pairs-list)
  `(let ((ht (make-hash-table)))
     (loop for pair in ,pairs-list do 
             (let ((key (car pair))
                   (value (car (cdr pair))))
               (setf (gethash key ht) value)))
     ht))

(defun make-connections-array (&key top right bottom left)
  (unless top
    (setf top '()))
  (unless right
    (setf right '()))
  (unless bottom
    (setf bottom '()))
  (unless left
    (setf left '()))
  (make-array 4 :initial-contents `(,top ,right ,bottom ,left)))

(defmacro connection-index-complement (index)
  `(mod (+ ,index 2) 4))

(defun next-indices (i j direction)
  (case direction
    (0 (decf i))
    (1 (incf j))
    (2 (incf i))
    (3 (decf j)))
  (values i j))

(defclass room-tiles ()
    ((keys :initarg :keys
           :reader room-tiles-keys
           :initform (make-hash-table))
     ;; Array dimensions are (h, w)
     (layout :initarg :layout
             :reader room-tiles-layout
             :initform (make-array '(0 0)))
     ;; Connections: array with 4 elements (starts with top, then clockwise): Each is list of pairs (offset, size) [x-offset/-size for top/bottom and y-offset/-size for left/right]
     (connections :initarg :connections
                  :reader room-tiles-connections
                  :initform (make-connections-array))
     ;; Game-Objects: objects that are spawned on room creation (list of (i, j, object-type)) [i and j can be floats]
     ;; "object-type" is a symbol that represents what kind of object should be spawned (e.g. a specific enemy, a generic (random) enemy, the stairs to the next level)
     (game-objects :initarg :game-objects
                   :reader room-tiles-game-objects
                   :initform '())))

(defun inverted-connection (connection size)
  (let ((new-connection (reverse connection)))
    (loop for entry in new-connection do
            (setf (first entry) (- size (first entry) (second entry))))
    new-connection))

;; Creates a rotated version of the supplied room
;; Rotation is an integer value (0 = no rotation, then rotated clockwise)
(defmethod copy-room-tiles-rotated ((room-obj room-tiles) rotation)
  (setf rotation (mod rotation 4))
  (with-slots (keys layout connections) room-obj
    (let ((new-keys (alexandria:copy-hash-table keys))
          (new-layout (make-array (array-dimensions layout)))
          (new-connections (make-array (array-dimensions connections)))
          (h (array-dimension layout 0))
          (w (array-dimension layout 1)))
      ;; Rotated copy of layout
      (dotimes (i h)
        (dotimes (j w)
          (let ((new-i i)
                (new-j j))
            (case rotation
              (1 (setf new-i j)
                 (setf new-j (- w i 1)))
              (2 (setf new-i (- h i 1))
                 (setf new-j (- w j 1)))
              (3 (setf new-i (- h j 1))
                 (setf new-j i)))
            (setf (aref new-layout new-i new-j) (aref layout i j)))))

      ;; Shifted (and possibly flipped) copy of connections
      (dotimes (i (array-dimension connections 0))
        (let ((side-length (if (or (= i 0) (= i 2)) w h)))
          (if (or (and (evenp i) (or (= rotation 2) (= rotation 3)))
                  (and (oddp i) (or (= rotation 1) (= rotation 2))))
              (setf (aref new-connections (mod (+ i rotation) 4)) (inverted-connection (aref connections i) side-length))    
              (setf (aref new-connections (mod (+ i rotation) 4)) (alexandria:copy-sequence 'list (aref connections i))))))
      
      (make-instance 'room-tiles :keys new-keys :layout new-layout :connections new-connections))))

;; Loops over "layout" array while looking up the key value in the "keys" hash-table
(defmacro loop-room-tiles (room-tiles i j h w tile-type &rest body)
  (let ((layout (gensym)))
    `(let ((,layout (room-tiles-layout ,room-tiles)))
       (destructuring-bind (,h ,w) (array-dimensions ,layout)
         (dotimes (,i ,h)
           (dotimes (,j ,w)
             (let ((,tile-type (gethash (aref ,layout ,i ,j) (room-tiles-keys ,room-tiles))))
               ,@body)))))))

(defmacro room-indices-to-position (room-tiles i j room-offset-x room-offset-y)
  `(let ((internal-offset-x ,j)
         (internal-offset-y (- (array-dimension (room-tiles-layout ,room-tiles) 0) ,i 1)))
     (3d-vectors:vec2 (+ ,room-offset-x internal-offset-x) (+ ,room-offset-y internal-offset-y))))

;; TODO: Remove? -> Unused
;; Finds all possibilities how a room can connect to a given connection
;; Returns list of pairs (room-offset, room-connection-index)
(defmethod room-tiles-find-connections ((room-obj room-tiles) connection connection-direction)
  (let* ((connection-size (second connection))
         (room-connections (aref (room-tiles-connections room-obj) (connection-index-complement connection-direction))))
    (loop for room-connection in room-connections and i from 0
            when (= (second room-connection) connection-size)
            collect (list (- (first connection) (first room-connection)) i))))

;; Determines if 2 rooms can connect directly (axis alligned) in a given direction (side of obj-1)
;; Returns T if connections on the specified side are equal
(defmethod room-tiles-can-connect ((obj-1 room-tiles) (obj-2 room-tiles) direction)
  (let* ((connections-1 (aref (room-tiles-connections obj-1) direction))
         (connections-2 (aref (room-tiles-connections obj-2) (connection-index-complement direction)))
         (len-1 (list-length connections-1))
         (len-2 (list-length connections-2)))
    (and (= len-1 len-2)
         (> len-1 0)
         (> len-2 0)
         (not (loop for c-1 in connections-1
                    for c-2 in connections-2
                      when (not (and (= (first c-1) (first c-2))
                                     (= (second c-1) (second c-2))))
                      return T)))))
