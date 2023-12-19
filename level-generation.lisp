(defpackage :level-generation
  (:use :common-lisp :tiles :rooms)
  (:export :generate-level))

(in-package :level-generation)

;; For now only supports fixed size square rooms
(defconstant +room-size+ 16)

(defmacro iota (n)
  `(loop for i from 0 below ,n collect i))

;; TODO: more efficient?
(defmacro random-indices (count &optional (random-state *random-state*))
  `(let ((initial-list (iota ,count))
         (result (list)))
     (loop for n from ,count above 0 do
             (let* ((index (random n ,random-state))
                    (element (nth index initial-list)))
               (setf result (push element result))
               (setf initial-list (delete element initial-list :start index :count 1))))
     result))

(defun expand-room (room-layout i j rooms &optional (random-state *random-state*))
  (let ((room-obj (aref room-layout i j))
        (expansion-order (random-indices 4 random-state)))
    (destructuring-bind (layout-h layout-w) (array-dimensions room-layout)
      (loop for direction in expansion-order do
              (multiple-value-bind (new-i new-j) (next-indices i j direction)
                (when (and (>= new-i 0) (< new-i layout-h) (>= new-j 0) (< new-j layout-w) (not (aref room-layout new-i new-j)))
                      (let ((candidates (loop for other-room in rooms
                                                when (room-tiles-can-connect room-obj other-room direction)
                                                collect other-room)))
                        (when candidates
                              (setf (aref room-layout new-i new-j) (nth (random (length candidates) random-state) candidates))
                              (expand-room room-layout new-i new-j rooms random-state)))))))))

(defun generate-level (tile-array starting-room rooms &optional (random-state *random-state*))
  (let ((tiles (tile-array-tiles tile-array)))
    (destructuring-bind (tiles-h tiles-w) (array-dimensions tiles)
      (let* ((layout-h (/ tiles-h +room-size+))
             (layout-w (/ tiles-w +room-size+))
             (room-layout (make-array (list layout-h layout-w) :initial-element NIL)))
        
        (let ((i (random layout-h random-state))
              (j (random layout-w random-state)))
          (setf (aref room-layout i j) starting-room)
          (expand-room room-layout i j rooms random-state))
        
        (dotimes (i layout-h)
          (dotimes (j layout-w)
            (when (aref room-layout i j)
                  (let ((x-offset (* j +room-size+))
                        (y-offset (* i +room-size+)))
                    (tile-array-add-room tile-array (aref room-layout i j) x-offset y-offset)))))))))
