(defpackage :level-generation
  (:use :common-lisp :tiles :room :prefab-object :game-object)
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

(defstruct room-layout
  layout
  room-count
  start-i
  start-j
  (dead-ends '())) ; List of triples (i, j, distance-to-start)

(defun expand-room (room-layout i j distance rooms &optional (room-count NIL) (random-state *random-state*))
  (let ((room-obj (aref (room-layout-layout room-layout) i j))
        (expansion-order (random-indices 4 random-state))
        (rooms-generated 0))
    (destructuring-bind (layout-h layout-w) (array-dimensions (room-layout-layout room-layout))
      (loop for direction in expansion-order
              when (or (not room-count) (< rooms-generated room-count)) do
              (multiple-value-bind (new-i new-j) (next-indices i j direction)
                (when (and (>= new-i 0) (< new-i layout-h) (>= new-j 0) (< new-j layout-w) (not (aref (room-layout-layout room-layout) new-i new-j)))
                      (let ((candidates (loop for other-room in rooms
                                                when (room-tiles-can-connect room-obj other-room direction)
                                                collect other-room)))
                        (when candidates
                              (setf (aref (room-layout-layout room-layout) new-i new-j) (nth (random (length candidates) random-state) candidates))
                              (incf rooms-generated)
                              (incf rooms-generated (expand-room room-layout new-i new-j (1+ distance) rooms (when room-count (- room-count rooms-generated)) random-state))))))))
    (when (= rooms-generated 0)
          (push (list i j distance) (room-layout-dead-ends room-layout)))
    rooms-generated))

(defun try-generate-room-layout (h w starting-room ending-room rooms &optional (room-count NIL) (random-state *random-state*))
  (let* ((room-layout (make-room-layout :layout (make-array (list h w) :initial-element NIL)))
         (i (random h random-state))
         (j (random w random-state)))
    (format t "Starting room at i=~a j=~a~%" i j)
    (setf (room-layout-start-i room-layout) i)
    (setf (room-layout-start-j room-layout) j)
    (setf (aref (room-layout-layout room-layout) i j) starting-room)

    (setf (room-layout-room-count room-layout) (expand-room room-layout i j 0 rooms room-count random-state))
    (format t "Rooms generated: ~a~%" (room-layout-room-count room-layout))

    (sort (room-layout-dead-ends room-layout) #'(lambda (a b) (> (third a) (third b))))
    (let ((dead-end (first (room-layout-dead-ends room-layout))))
      (format t "Ending room at i=~a j=~a~%" (first dead-end) (second dead-end))
      (setf (aref (room-layout-layout room-layout) (first dead-end) (second dead-end)) ending-room))
    room-layout))

(defun generate-room-layout (h w starting-room ending-room rooms &optional (room-count NIL) (attempts 1) (random-state *random-state*))
  (let ((best-room-layout NIL))
    (loop for i from 1 to attempts
          do (format t "Generation attempt ~a:~%" i)
             (let ((room-layout (try-generate-room-layout h w starting-room ending-room rooms room-count random-state)))
               (when (or (not best-room-layout) (> (room-layout-room-count room-layout) (room-layout-room-count best-room-layout)))
                     (setf best-room-layout room-layout)))
          when (or (not room-count) (>= (room-layout-room-count best-room-layout) room-count) (>= i attempts))
          return best-room-layout)))

(defun make-rooms-from-layout (tile-array layout)
  (destructuring-bind (layout-h layout-w) (array-dimensions layout)
    (dotimes (i layout-h)
      (dotimes (j layout-w)
        (when (aref layout i j)
              (let ((x-offset (* j +room-size+))
                    (y-offset (* (- layout-h i 1) +room-size+)))
                (tile-array-add-room tile-array (aref layout i j) x-offset y-offset)))))))

(defun block-invalid-connections (tile-array layout)
  (destructuring-bind (layout-h layout-w) (array-dimensions layout)
    (dotimes (i layout-h)
      (dotimes (j layout-w)
        (when (aref layout i j)
              (let ((x-offset (* j +room-size+))
                    (y-offset (* (- layout-h i 1) +room-size+)))
                (dotimes (direction 4)
                  (multiple-value-bind (i-other j-other) (next-indices i j direction)
                    (unless (and (>= i-other 0) (< i-other layout-h) (>= j-other 0) (< j-other layout-w)
                                 (aref layout i-other j-other)
                                 (room-tiles-can-connect (aref layout i j) (aref layout i-other j-other) direction))
                      (tile-array-block-connection tile-array (aref layout i j) direction x-offset y-offset))))))))))

(defun spawn-object (type position tile-array &optional (random-state *random-state*))
  (format t "Spawning ~a at ~a~%" type position)
  (let ((obj (case type
               (room::stairs (make-prefab-object 'prefab-stairs position tile-array random-state))
               (otherwise (format t "Unknown spawnable object: ~a~%" type)))))
    (when obj
          (game-object-register obj))))

(defun spawn-game-objects (tile-array layout &optional (random-state *random-state*))
  (destructuring-bind (layout-h layout-w) (array-dimensions layout)
    (dotimes (i layout-h)
      (dotimes (j layout-w)
        (let ((room-obj (aref layout i j)))
          (when room-obj
                (let ((x-offset (* j +room-size+))
                      (y-offset (* (- layout-h i 1) +room-size+))
                      (to-spawn (room-tiles-game-objects room-obj)))
                  (loop for spawn-info in to-spawn do
                          (format t "~a ~a ~a ~a~%" x-offset y-offset (first spawn-info) (second spawn-info))
                          (let ((spawn-type (third spawn-info))
                                (spawn-pos (3d-vectors:v+ (tile-array-offset tile-array) (room-indices-to-position room-obj (first spawn-info) (second spawn-info) x-offset y-offset))))
                            (spawn-object spawn-type spawn-pos tile-array random-state))))))))))

(defun generate-level (tile-array starting-room ending-room rooms &optional (room-count NIL) (attempts 1) (random-state *random-state*))
  (format t "Generating level...~%")
  (let ((tiles (tile-array-tiles tile-array))
        (starting-pos (3d-vectors:vec2 0 0)))
    (destructuring-bind (tiles-h tiles-w) (array-dimensions tiles)
      (let ((layout-h (/ tiles-h +room-size+))
            (layout-w (/ tiles-w +room-size+)))
        
        (let* ((room-layout (generate-room-layout layout-h layout-w starting-room ending-room rooms room-count attempts random-state))
               (i (room-layout-start-i room-layout))
               (j (room-layout-start-j room-layout))
               (offset (tile-array-offset tile-array))
               (centering-x (+ (3d-vectors:vx offset) (/ +room-size+ 2) -0.5))
               (centering-y (+ (3d-vectors:vy offset) (/ +room-size+ 2) -0.5)))
          (setf starting-pos (3d-vectors:vec2 (+ (* j +room-size+) centering-x) (+ (* (- layout-h i 1) +room-size+) centering-y)))
          (format t "Final room-count: ~a~%" (room-layout-room-count room-layout))
          (format t "Dead ends:~%")
          (loop for element in (room-layout-dead-ends room-layout) do
                  (format t "At ~a, ~a (Distance ~a)~%" (first element) (second element) (third element)))
          (make-rooms-from-layout tile-array (room-layout-layout room-layout))
          (block-invalid-connections tile-array (room-layout-layout room-layout))
          (spawn-game-objects tile-array (room-layout-layout room-layout) random-state))))
    (format t "...Done~%")
    starting-pos))
