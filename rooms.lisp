(defpackage :rooms
  (:use :common-lisp)
  (:export :next-indices
           :room-tiles
           :room-tiles-keys :room-tiles-layout :room-tiles-connections
           :loop-room-tiles :room-tiles-find-connections :room-tiles-can-connect
           
           :*rooms* :*room-1* :*room-2*))

(in-package :rooms)

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
                  :initform (make-connections-array))))

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

(defvar *room-1* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W tile-wall) (F tile-floor) (O NIL)))
                   :layout (make-array '(16 16) :initial-contents '((O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (W W W W W W W F F W W W W W W W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W W W W W W W F F W W W W W W W)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)))
                   :connections (make-connections-array :top '((7 2))
                                                        :bottom '((7 2)))))

(defvar *room-2* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W tile-wall) (F tile-floor) (O NIL)))
                   :layout (make-array '(16 16) :initial-contents '((O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O W W W W F F W W W W O O O)
                                                                    (O O O W F F F F F F F F W O O O)
                                                                    (O O O W F F F F F F F F W O O O)
                                                                    (W W W W F F F F F F F F W W W W)
                                                                    (F F F F F F F F F F F F F F F F)
                                                                    (F F F F F F F F F F F F F F F F)
                                                                    (W W W W F F F F F F F F W W W W)
                                                                    (O O O W F F F F F F F F W O O O)
                                                                    (O O O W F F F F F F F F W O O O)
                                                                    (O O O W W W W F F W W W W O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)))
                   :connections (make-connections-array :top '((7 2))
                                                        :bottom '((7 2))  
                                                        :left '((7 2))
                                                        :right '((7 2)))))

(defvar *room-crossroad* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W tile-wall) (F tile-floor)))
                   :layout (make-array '(16 16) :initial-contents '((W W W W W W W F F W W W W W W W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (F F F F F F F F F F F F F F F F)
                                                                    (F F F F F F F F F F F F F F F F)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W W W W W W W F F W W W W W W W)))
                   :connections (make-connections-array :top '((7 2))
                                                        :bottom '((7 2))  
                                                        :left '((7 2))
                                                        :right '((7 2)))))

(defvar *room-t-junction-not-B* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W tile-wall) (F tile-floor)))
                   :layout (make-array '(16 16) :initial-contents '((W W W W W W W F F W W W W W W W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (F F F F F F F F F F F F F F F F)
                                                                    (F F F F F F F F F F F F F F F F)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W W W W W W W W W W W W W W W W)))
                   :connections (make-connections-array :top '((7 2))
                                                        :left '((7 2))
                                                        :right '((7 2)))))

(defvar *room-t-junction-not-L* (copy-room-tiles-rotated *room-t-junction-not-B* 1))
(defvar *room-t-junction-not-T* (copy-room-tiles-rotated *room-t-junction-not-B* 2))
(defvar *room-t-junction-not-R* (copy-room-tiles-rotated *room-t-junction-not-B* 3))

(defvar *room-vertical* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W tile-wall) (F tile-floor)))
                   :layout (make-array '(16 16) :initial-contents '((W W W W W W W F F W W W W W W W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W W W W W W W F F W W W W W W W)))
                   :connections (make-connections-array :top '((7 2))
                                                        :bottom '((7 2)))))

(defvar *room-horizontal* (copy-room-tiles-rotated *room-vertical* 1))

(defvar *room-corner-TR* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W tile-wall) (F tile-floor)))
                   :layout (make-array '(16 16) :initial-contents '((W W W W W W W F F W W W W W W W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F F)
                                                                    (W F F F F F F F F F F F F F F F)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W W W W W W W W W W W W W W W W)))
                   :connections (make-connections-array :top '((7 2))
                                                        :right '((7 2)))))

(defvar *room-corner-RB* (copy-room-tiles-rotated *room-corner-TR* 1))
(defvar *room-corner-BL* (copy-room-tiles-rotated *room-corner-TR* 2))
(defvar *room-corner-LT* (copy-room-tiles-rotated *room-corner-TR* 3))

(defvar *room-dead-end-T* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W tile-wall) (F tile-floor)))
                   :layout (make-array '(16 16) :initial-contents '((W W W W W W W F F W W W W W W W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W W W W W W W W W W W W W W W W)))
                   :connections (make-connections-array :top '((7 2)))))

(defvar *room-dead-end-R* (copy-room-tiles-rotated *room-dead-end-T* 1))
(defvar *room-dead-end-B* (copy-room-tiles-rotated *room-dead-end-T* 2))
(defvar *room-dead-end-L* (copy-room-tiles-rotated *room-dead-end-T* 3))

(defvar *rooms* (list ;*room-1*
                     *room-crossroad*
                     *room-t-junction-not-B* *room-t-junction-not-L* *room-t-junction-not-T* *room-t-junction-not-R*
                     *room-vertical* *room-horizontal*
                     *room-corner-TR* *room-corner-RB* *room-corner-BL* *room-corner-LT*
                     *room-dead-end-T* *room-dead-end-R* *room-dead-end-B* *room-dead-end-L*))
