(defpackage :collision
  (:use :common-lisp)
  (:export :circle-collider :aabb-collider :rectangle-collider
           :collider-get-collision :collider-resolve-collision :collider-get-collisions :collider-resolve-collisions
           :collider-position :collider-parent :collider-is-trigger
           :circle-collider-radius
           :aabb-collider-size
           :rectangle-collider-size :rectangle-collider-rotation))

(in-package :collision)

;; Classes

(defclass collider ()
    ((pos :initarg :position
          :accessor collider-position
          :initform (3d-vectors:vec2 0 0))
     (parent :initarg :parent
             :initform NIL
             :accessor collider-parent)
     (trigger :initarg :trigger
              :initform NIL
              :accessor collider-is-trigger)))

(defclass circle-collider (collider)
    ((radius :initarg :radius
             :accessor circle-collider-radius
             :initform 0.5)))

(defclass aabb-collider (collider)
    ((size :initarg :size
           :accessor aabb-collider-size
           :initform (3d-vectors:vec2 1 1))))

(defclass rectangle-collider (collider)
    ((size :initarg :size
           :accessor rectangle-collider-size
           :initform (3d-vectors:vec2 1 1))
     (rot :initarg :rotation
          :accessor rectangle-collider-rotation
          :initform 0)))

(defstruct (rectangle-points (:constructor create-rectangle-points (A B C D)))
  A B C D)

(defun make-rotated-rectangle-points (pos size rot)
  (let* ((cos-rot (cos rot))
         (sin-rot (sin rot))
         (rot-mat (3d-matrices:mat2 `(,cos-rot ,(- sin-rot) ,sin-rot ,cos-rot)))
         (half-size (3d-vectors:v/ size 2))
         (A (3d-vectors:v+ pos (3d-matrices:m* rot-mat half-size)))
         (B (3d-vectors:v+ pos (3d-matrices:m* rot-mat (3d-vectors:v* half-size (3d-vectors:vec2 1 -1)))))
         (C (3d-vectors:v+ pos (3d-matrices:m* rot-mat (3d-vectors:v* half-size (3d-vectors:vec2 -1 -1)))))
         (D (3d-vectors:v+ pos (3d-matrices:m* rot-mat (3d-vectors:v* half-size (3d-vectors:vec2 -1 1))))))
    (create-rectangle-points A B C D)))

(defun make-rectangle-points (pos size)
  (let* ((half-size (3d-vectors:v/ size 2))
         (A (3d-vectors:v+ pos half-size))
         (B (3d-vectors:v+ pos (3d-vectors:v* half-size (3d-vectors:vec2 1 -1))))
         (C (3d-vectors:v+ pos (3d-vectors:v* half-size (3d-vectors:vec2 -1 -1))))
         (D (3d-vectors:v+ pos (3d-vectors:v* half-size (3d-vectors:vec2 -1 1)))))
    (create-rectangle-points A B C D)))

;; Collsion helpers

;(defun vector-projection (v1 v2)
;  (3d-vectors:v* (* (3d-vectors:v2norm v1) (cos (3d-vectors:vangle v1 v2))) (3d-vectors:vunit v2)))

(defun vector-projection (v1 v2)
  (let ((k (/ (3d-vectors:v. v1 v2) (3d-vectors:v. v2 v2))))
    (3d-vectors:vec2 (* k (3d-vectors:vx v2)) (* k (3d-vectors:vy v2)))))

(defmethod point-in-rectangle (point (rectangle rectangle-points))
  (with-slots (A B C D) rectangle
    (let* ((AP (3d-vectors:v- point A))
           (AB (3d-vectors:v- B A))
           (AD (3d-vectors:v- D A))
           (prod1 (3d-vectors:v. AP AB))
           (prod2 (3d-vectors:v. AP AD)))
      (and (<= 0 prod1) (<= prod1 (3d-vectors:v. AB AB)) (<= 0 prod2) (<= prod2 (3d-vectors:v. AD AD))))))

(defmethod line-intersect-circle ((circle circle-collider) A B)
  (with-slots (pos radius) circle
    (let* ((AC (3d-vectors:v- pos A))
           (AB (3d-vectors:v- B A))
           (D (3d-vectors:v+ (vector-projection AC AB) A))
           (AD (3d-vectors:v- D A))
           (k (if (> (abs (3d-vectors:vx AB)) (abs (3d-vectors:vy AB)))
                  (/ (3d-vectors:vx AD) (3d-vectors:vx AB))
                  (/ (3d-vectors:vy AD) (3d-vectors:vy AB)))))
      (if (<= k 0)
          (< (3d-vectors:vdistance pos A) radius)
          (if (>= k 1)
              (< (3d-vectors:vdistance pos B) radius)
              (< (3d-vectors:vdistance pos D) radius))))))

;; Get collisions
;; Returns true if colliders are overlapping

(defmethod collider-get-collision ((col1 circle-collider) (col2 circle-collider))
  (< (3d-vectors:vdistance (collider-position col1) (collider-position col2)) (+ (circle-collider-radius col1) (circle-collider-radius col2))))

; TODO: maybe optimise -> early return
(defmethod collider-get-collision ((col1 circle-collider) (col2 aabb-collider))
  (with-slots ((circle-pos pos) radius) col1
    (with-slots ((aabb-pos pos) size) col2
      (with-slots (A B C D) (make-rectangle-points aabb-pos size)
        (let* ((circle-x (3d-vectors:vx circle-pos))
               (circle-y (3d-vectors:vy circle-pos))
               (aabb-x (3d-vectors:vx aabb-pos))
               (aabb-y (3d-vectors:vy aabb-pos))
               (half-w (/ (3d-vectors:vx size) 2))
               (half-h (/ (3d-vectors:vy size) 2))
               (right (+ aabb-x half-w))
               (left (- aabb-x half-w))
               (top (+ aabb-y half-h))
               (bottom (- aabb-y half-h)))
          ;; TODO: swap not-or to and
          (not (or (>= circle-x (+ right radius))
                   (<= circle-x (- left radius))
                   (>= circle-y (+ top radius))
                   (<= circle-y (- bottom radius))
                   (and (>= circle-x right)
                        (>= circle-y top)
                        (>= (3d-vectors:vdistance A circle-pos) radius))
                   (and (>= circle-x right)
                        (<= circle-y bottom)
                        (>= (3d-vectors:vdistance B circle-pos) radius))
                   (and (<= circle-x left)
                        (<= circle-y bottom)
                        (>= (3d-vectors:vdistance C circle-pos) radius))
                   (and (<= circle-x left)
                        (>= circle-y top)
                        (>= (3d-vectors:vdistance D circle-pos) radius)))))))))

(defmethod collider-get-collision ((col1 circle-collider) (col2 rectangle-collider))
  (let ((rectangle (make-rotated-rectangle-points (collider-position col2)
                                                  (rectangle-collider-size col2)
                                                  (rectangle-collider-rotation col2))))
    (with-slots (A B C D) rectangle
      (or (point-in-rectangle (collider-position col1) rectangle)
          (line-intersect-circle col1 A B)
          (line-intersect-circle col1 B C)
          (line-intersect-circle col1 C D)
          (line-intersect-circle col1 D A)))))

(defmethod collider-get-collision ((col1 aabb-collider) (col2 circle-collider))
  (collider-get-collision col2 col1))

(defmethod collider-get-collision ((col1 aabb-collider) (col2 aabb-collider))
  (with-slots ((pos1 pos) (size1 size)) col1
    (with-slots ((pos2 pos) (size2 size)) col2
      (let ((dx (- (3d-vectors:vx pos1) (3d-vectors:vx pos2)))
            (dy (- (3d-vectors:vy pos1) (3d-vectors:vy pos2)))
            (sw (/ (+ (3d-vectors:vx size1) (3d-vectors:vx size2)) 2))
            (sh (/ (+ (3d-vectors:vy size1) (3d-vectors:vy size2)) 2)))
        (and (> (+ dx sw) 0) (< (- dx sw) 0) (> (+ dy sh) 0) (< (- dy sh) 0))))))

(defmethod collider-get-collision ((col1 aabb-collider) (col2 rectangle-collider))
  ;(format t "Collision not implemented~%")
  NIL)

(defmethod collider-get-collision ((col1 rectangle-collider) (col2 circle-collider))
  (collider-get-collision col2 col1))

(defmethod collider-get-collision ((col1 rectangle-collider) (col2 aabb-collider))
  (collider-get-collision col2 col1))

(defmethod collider-get-collision ((col1 rectangle-collider) (col2 rectangle-collider))
  ;(format t "Collision not implemented~%")
  NIL)

;; Collision resolution
;; Takes two colliders as input, as well as an atempted movement (of the first collider)
;; Returns the permissible movement

(defmethod collider-resolve-collision ((col1 aabb-collider) (col2 aabb-collider) delta-pos)
  (with-slots ((pos1 pos) (size1 size)) col1
    (with-slots ((pos2 pos) (size2 size)) col2
      (let* ((new-pos (3d-vectors:v+ pos1 delta-pos))
             (dx (- (3d-vectors:vx pos1) (3d-vectors:vx pos2)))
             (new-dx (- (3d-vectors:vx new-pos) (3d-vectors:vx pos2)))
             (dy (- (3d-vectors:vy pos1) (3d-vectors:vy pos2)))
             (new-dy (- (3d-vectors:vy new-pos) (3d-vectors:vy pos2)))
             (sw (/ (+ (3d-vectors:vx size1) (3d-vectors:vx size2)) 2))
             (sh (/ (+ (3d-vectors:vy size1) (3d-vectors:vy size2)) 2)))
        (when (and (not (and (> (+ dx sw) 0) (< (- dx sw) 0)))                                            ; not already colliding in x direction
                   (and (> (+ new-dx sw) 0) (< (- new-dx sw) 0) (> (+ new-dy sh) 0) (< (- new-dy sh) 0))) ; will be colliding after move
              (if (> new-dx 0)
                  (setf new-dx sw)
                  (setf new-dx (- sw))))
        (when (and (not (and (> (+ dy sh) 0) (< (- dy sh) 0)))                                            ; not already colliding in y direction
                   (and (> (+ new-dx sw) 0) (< (- new-dx sw) 0) (> (+ new-dy sh) 0) (< (- new-dy sh) 0))) ; will be colliding after move
              (if (> new-dy 0)
                  (setf new-dy sh)
                  (setf new-dy (- sh))))
        (3d-vectors:v- (3d-vectors:v+ (3d-vectors:vec2 new-dx new-dy) pos2) pos1)))))

;; Fallback for other colliders
(defmethod collider-resolve-collision ((col1 collider) (col2 collider) delta-pos)
  delta-pos)

;; Multi collision detection / resolution
(defmethod collider-get-collisions ((col collider) colliders)
  (loop for other-collider being the hash-values of colliders
          when (and (not (eq col other-collider)) (collider-get-collision col other-collider)) collect other-collider))

(defmethod collider-resolve-collisions ((col collider) colliders delta-pos)
  (let ((min-dx (3d-vectors:vx delta-pos))
        (min-dy (3d-vectors:vy delta-pos)))
    (loop for other-collider being the hash-values of colliders
            when (not (or (eq col other-collider) (collider-is-trigger other-collider)))
            do (let* ((new-delta-pos (collider-resolve-collision col other-collider delta-pos))
                      (new-dx (3d-vectors:vx new-delta-pos))
                      (new-dy (3d-vectors:vy new-delta-pos)))
                 (when (< (abs new-dx) (abs min-dx)) (setf min-dx new-dx))
                 (when (< (abs new-dy) (abs min-dy)) (setf min-dy new-dy))))
    (3d-vectors:vec2 min-dx min-dy)))
