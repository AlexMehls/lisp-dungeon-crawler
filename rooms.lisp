(defpackage :rooms
  (:use :common-lisp)
  (:export :room-tiles
           :room-tiles-keys :room-tiles-layout
           :loop-room-tiles :room-tiles-find-connections
           
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

;; Loops over "layout" array while looking up the key value in the "keys" hash-table
(defmacro loop-room-tiles (room-tiles i j h w tile-type &rest body)
  (let ((layout (gensym)))
    `(let ((,layout (room-tiles-layout ,room-tiles)))
       (destructuring-bind (,h ,w) (array-dimensions ,layout)
         (dotimes (,i ,h)
           (dotimes (,j ,w)
             (let ((,tile-type (gethash (aref ,layout ,i ,j) (room-tiles-keys ,room-tiles))))
               ,@body)))))))

;; Finds all possibilities how a room can connect to a given connection
;; Returns list of pairs (room-offset, room-connection-index)
(defmethod room-tiles-find-connections ((room-obj room-tiles) connection connection-direction)
  (let* ((connection-size (second connection))
         (room-connections (aref (room-tiles-connections room-obj) (connection-index-complement connection-direction))))
    (loop for room-connection in room-connections and i from 0
            when (= (second room-connection) connection-size)
            collect (list (- (first connection) (first room-connection)) i))))

(defvar *room-1* (make-instance 'room-tiles
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

(defvar *room-2* (make-instance 'room-tiles
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

(defvar *rooms* `(,*room-1* ,*room-2*))
