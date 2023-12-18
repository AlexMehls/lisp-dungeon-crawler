(defpackage :level-generation
  (:use :common-lisp :tiles :rooms)
  (:export :generate-level))

(in-package :level-generation)

;; For now only supports fixed size square rooms
(defconstant +room-size+ 16)

(defun generate-level (tile-array starting-room rooms &optional (random-state *random-state*))
  (declare (ignore rooms))
  (let ((tiles (tile-array-tiles tile-array)))
    (destructuring-bind (tiles-h tiles-w) (array-dimensions tiles)
      (let* ((layout-h (/ tiles-h +room-size+))
             (layout-w (/ tiles-w +room-size+))
             (room-layout (make-array (list layout-h layout-w) :initial-element NIL)))
        
        (setf (aref room-layout (random layout-h random-state) (random layout-w random-state)) starting-room)
        ;; TODO: other rooms
        
        (dotimes (i layout-h)
          (dotimes (j layout-w)
            (when (aref room-layout i j)
                  (let ((x-offset (* j +room-size+))
                        (y-offset (* i +room-size+)))
                    (tile-array-add-room tile-array (aref room-layout i j) x-offset y-offset)))))))))
