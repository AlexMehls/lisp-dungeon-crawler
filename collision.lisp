(defpackage :collision
  (:use :common-lisp)
  (:export :circle-collider :aabb-collider :rectangle-collider
           :collider-get-collision
           :collider-position
           :circle-collider-radius
           :aabb-collider-size
           :rectangle-collider-size :rectangle-collider-rotation))

(in-package :collision)

;; Classes

(defclass collider ()
    ((pos :initarg :position
          :accessor collider-position
          :initform (3d-vectors:vec 0 0))))

(defclass circle-collider (collider)
    ((radius :initarg :radius
             :accessor circle-collider-radius
             :initform 0.5)))

(defclass aabb-collider (collider)
    ((size :initarg :size
           :accessor aabb-collider-size
           :initform (3d-vectors:vec 1 1))))

(defclass rectangle-collider (collider)
    ((size :initarg :size
           :accessor rectangle-collider-size
           :initform (3d-vectors:vec 1 1))
     (rot :initarg :rotation
          :accessor rectangle-collider-rotation
          :initform 0)))

(defclass rectangle-points ()
    ((A :initarg :A)
     (B :initarg :B)
     (C :initarg :C)
     (D :initarg :D)))

(defun make-rotated-rectangle-points (pos size rot)
  (let* ((cos-rot (cos rot))
         (sin-rot (sin rot))
         (rot-mat (3d-matrices:mat2 `(,cos-rot ,(- sin-rot) ,sin-rot ,cos-rot)))
         (half-size (3d-vectors:v/ size 2))
         (A (3d-vectors:v+ pos (3d-matrices:m* rot-mat half-size)))
         (B (3d-vectors:v+ pos (3d-matrices:m* rot-mat (3d-vectors:v* half-size (3d-vectors:vec2 1 -1)))))
         (C (3d-vectors:v+ pos (3d-matrices:m* rot-mat (3d-vectors:v* half-size (3d-vectors:vec2 -1 -1)))))
         (D (3d-vectors:v+ pos (3d-matrices:m* rot-mat (3d-vectors:v* half-size (3d-vectors:vec2 -1 1))))))
    (make-instance 'rectangle-points :A A :B B :C C :D D)))

(defun make-rectangle-points (pos size)
  (let* ((half-size (3d-vectors:v/ size 2))
         (A (3d-vectors:v+ pos half-size))
         (B (3d-vectors:v+ pos (3d-vectors:v* half-size (3d-vectors:vec2 1 -1))))
         (C (3d-vectors:v+ pos (3d-vectors:v* half-size (3d-vectors:vec2 -1 -1))))
         (D (3d-vectors:v+ pos (3d-vectors:v* half-size (3d-vectors:vec2 -1 1)))))
    (make-instance 'rectangle-points :A A :B B :C C :D D)))

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
  (let* ((rect-size (rectangle-collider-size col2))
         (rectangle (make-rotated-rectangle-points (collider-position col2)
                                                   rect-size
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
      (let ((x1 (3d-vectors:vx pos1))
            (y1 (3d-vectors:vy pos1))
            (w1 (3d-vectors:vx size1))
            (h1 (3d-vectors:vy size1))
            (x2 (3d-vectors:vx pos2))
            (y2 (3d-vectors:vy pos2))
            (w2 (3d-vectors:vx size2))
            (h2 (3d-vectors:vy size2)))
        (and (> (+ x1 w1) x2) (< x1 (+ x2 w2)) (> (+ y1 h1) y2) (< y1 (+ y2 h2)))))))

(defmethod collider-get-collision ((col1 aabb-collider) (col2 rectangle-collider))
  (format t "Collision not implemented~%"))

(defmethod collider-get-collision ((col1 rectangle-collider) (col2 circle-collider))
  (collider-get-collision col2 col1))

(defmethod collider-get-collision ((col1 rectangle-collider) (col2 aabb-collider))
  (collider-get-collision col2 col1))

(defmethod collider-get-collision ((col1 rectangle-collider) (col2 rectangle-collider))
  (format t "Collision not implemented~%"))
