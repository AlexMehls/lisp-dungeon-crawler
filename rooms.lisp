(defpackage :rooms
  (:use :common-lisp)
  (:export :room-tiles
           :room-tiles-keys :room-tiles-layout
           :loop-room-tiles
           
           :*room-1* :*room-2*))

(in-package :rooms)

(defmacro make-hash-table-with-pairs (pairs-list)
  `(let ((ht (make-hash-table)))
     (loop for pair in ,pairs-list do 
             (let ((key (car pair))
                   (value (car (cdr pair))))
               (setf (gethash key ht) value)))
     ht))

(defclass room-tiles ()
    ((keys :initarg :keys
           :reader room-tiles-keys
           :initform (make-hash-table))
     (layout :initarg :layout
             :reader room-tiles-layout
             :initform (make-array '(0 0)))))

;; Loops over "layout" array while looking up the key value in the "keys" hash-table
(defmacro loop-room-tiles (room-tiles i j h w tile-type &rest body)
  (let ((layout (gensym)))
    `(let ((,layout (room-tiles-layout ,room-tiles)))
       (destructuring-bind (,h ,w) (array-dimensions ,layout)
         (dotimes (,i ,h)
           (dotimes (,j ,w)
             (let ((,tile-type (gethash (aref ,layout ,i ,j) (room-tiles-keys ,room-tiles))))
               ,@body)))))))

;; Array dimensions are (h, w)
(defvar *room-1* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W tile-wall) (F tile-floor)))
                   :layout (make-array '(9 11) :initial-contents '((W W W W F F F W W W W)
                                                                   (W F F F F F F F F F W)
                                                                   (W F F F F F F F F F W)
                                                                   (F F F F F F F F F F F)
                                                                   (F F F F F F F F F F F)
                                                                   (F F F F F F F F F F F)
                                                                   (W F F F F F F F F F W)
                                                                   (W F F F F F F F F F W)
                                                                   (W W W W F F F W W W W)))))

(defvar *room-2* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W tile-wall) (F tile-floor) (O NIL)))
                   :layout (make-array '(9 11) :initial-contents '((O O O W W W W W O O O)
                                                                   (O O O W F F F W O O O)
                                                                   (W W W W F F F W W W W)
                                                                   (W F F F F F F F F F W)
                                                                   (W F F F F F F F F F W)
                                                                   (W F F F F F F F F F W)
                                                                   (W W W W F F F W W W W)
                                                                   (O O O W F F F W O O O)
                                                                   (O O O W F F F W O O O)))))
