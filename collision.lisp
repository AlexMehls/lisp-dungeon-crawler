(defpackage :collision
  (:use :common-lisp)
  (:export :circle-collider :aabb-collider :rectangle-collider :line-collider
           :collider-get-collision :collider-resolve-collision :collider-get-collisions :collider-resolve-collisions
           :raycast-get-collisions :line-of-sight-get-collisions

           :collider-position :collider-parent :collider-is-trigger
           :circle-collider-radius
           :aabb-collider-size
           :rectangle-collider-size :rectangle-collider-rotation
           :line-collider-length :line-collider-rotation))

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

;; Meant to be used for line-of-sight checks (i.e. raycasting)
(defclass line-collider (collider)
    ((length :initarg :length
             :accessor line-collider-length
             :initform 1)
     ;; Default rotation is vertical
     (rot :initarg :rotation
          :accessor line-collider-rotation
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

(defstruct (line-points (:constructor create-line-points (A B)))
  A B)

(defun make-line-points (pos length rot)
  (let* ((cos-rot (cos rot))
         (sin-rot (sin rot))
         (rot-mat (3d-matrices:mat2 `(,cos-rot ,(- sin-rot) ,sin-rot ,cos-rot)))
         (half-length-vec (3d-vectors:vec2 0 (/ length 2)))
         (A (3d-vectors:v+ pos (3d-matrices:m* rot-mat half-length-vec)))
         (B (3d-vectors:v+ pos (3d-matrices:m* rot-mat (3d-vectors:v* half-length-vec (3d-vectors:vec2 1 -1))))))
    (create-line-points A B)))

;; Collsion helpers

;(defun vector-projection (v1 v2)
;  (3d-vectors:v* (* (3d-vectors:v2norm v1) (cos (3d-vectors:vangle v1 v2))) (3d-vectors:vunit v2)))

(defmacro vector-projection (v1 v2)
  `(let ((k (/ (3d-vectors:v. ,v1 ,v2) (3d-vectors:v. ,v2 ,v2))))
     (3d-vectors:vec2 (* k (3d-vectors:vx ,v2)) (* k (3d-vectors:vy ,v2)))))

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

(defmacro orthogonal-projection-parameter (point A B)
  `(let ((direction (3d-vectors:v- ,B ,A)))
     (/ (3d-vectors:v. (3d-vectors:v- ,point ,A) direction) (3d-vectors:v. direction direction))))

;; Returns T if all rojections of points onto the line lie on the same side outside the end points
; TODO: expand list as macro
(defmacro multi-projection-check-ends-of-line (A B points)
  `(let* ((point1 (car ,points))
          (projection1 (orthogonal-projection-parameter point1 ,A ,B)))
      (if (< projection1 0)  
         (let ((rest-points (cdr ,points)))
           (not (loop for point in rest-points
                        when (>= (orthogonal-projection-parameter point ,A ,B) 0)
                        return T)))
         (let ((upper-bound (3d-vectors:vdistance ,A ,B)))
           (if (> projection1 upper-bound)
               (let ((rest-points (cdr ,points)))
                 (not (loop for point in rest-points
                              when (<= (orthogonal-projection-parameter point ,A ,B) upper-bound)
                              return T)))
               NIL)))))

(defmethod rectangle-points-get-collision ((rect1 rectangle-points) (rect2 rectangle-points))
  (with-slots (A B C D) rect1
    (with-slots ((E A) (F B) (G C) (H D)) rect2
      (not (or (multi-projection-check-ends-of-line A B `(,E ,F ,G ,H))
               (multi-projection-check-ends-of-line A D `(,E ,F ,G ,H))
               (multi-projection-check-ends-of-line E F `(,A ,B ,C ,D))
               (multi-projection-check-ends-of-line E H `(,A ,B ,C ,D)))))))

(defmethod rectangle-points-line-points-get-collision ((rect rectangle-points) (line line-points))
  (with-slots (A B) line
    (with-slots ((C A) (D B) (E C) (F D)) rect
      (not (multi-projection-check-ends-of-line A B `(,C ,D ,E ,F))))))

(defmethod line-points-get-collision ((line1 line-points) (line2 line-points))
  (with-slots (A B) line1
    (with-slots ((C A) (D B)) line2
      (let* ((A-x (3d-vectors:vx A))
             (A-y (3d-vectors:vy A))
             (B-x (3d-vectors:vx B))
             (B-y (3d-vectors:vy B))
             (C-x (3d-vectors:vx C))
             (C-y (3d-vectors:vy C))
             (D-x (3d-vectors:vx D))
             (D-y (3d-vectors:vy D))
             (s1-x (- B-x A-x))
             (s1-y (- B-y A-y))
             (s2-x (- D-x C-x))
             (s2-y (- D-y C-y))
             (divisor (+ (* (- s2-x) s1-y) (* s1-x s2-y)))
             (s-val (/ (+ (* (- s1-y) (- A-x C-x))
                          (* s1-x (- A-y C-y)))
                       divisor))
             (t-val (/ (- (* s2-x (- A-y C-y))
                          (* s2-y (- A-x C-x)))
                       divisor)))
        (and (>= s-val 0) (<= s-val 1) (>= t-val 0) (<= t-val 1))))))

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

(defmethod collider-get-collision ((col1 circle-collider) (col2 line-collider))
  (let ((line (make-line-points (collider-position col2)
                                (line-collider-length col2)
                                (line-collider-rotation col2))))
    (with-slots (A B) line
      (line-intersect-circle col1 A B))))

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
  (rectangle-points-get-collision (make-rectangle-points (collider-position col1) (aabb-collider-size col1))
                                  (make-rotated-rectangle-points (collider-position col2) (rectangle-collider-size col2) (rectangle-collider-rotation col2))))

(defmethod collider-get-collision ((col1 aabb-collider) (col2 line-collider))
  (rectangle-points-line-points-get-collision (make-rectangle-points (collider-position col1) (aabb-collider-size col1))
                                              (make-line-points (collider-position col2) (line-collider-length col2) (line-collider-rotation col2))))

(defmethod collider-get-collision ((col1 rectangle-collider) (col2 circle-collider))
  (collider-get-collision col2 col1))

(defmethod collider-get-collision ((col1 rectangle-collider) (col2 aabb-collider))
  (collider-get-collision col2 col1))

(defmethod collider-get-collision ((col1 rectangle-collider) (col2 rectangle-collider))
  (rectangle-points-get-collision (make-rotated-rectangle-points (collider-position col1) (rectangle-collider-size col1) (rectangle-collider-rotation col1))
                                  (make-rotated-rectangle-points (collider-position col2) (rectangle-collider-size col2) (rectangle-collider-rotation col2))))

(defmethod collider-get-collision ((col1 rectangle-collider) (col2 line-collider))
  (rectangle-points-line-points-get-collision (make-rotated-rectangle-points (collider-position col1) (rectangle-collider-size col1) (rectangle-collider-rotation col1))
                                              (make-line-points (collider-position col2) (line-collider-length col2) (line-collider-rotation col2))))

(defmethod collider-get-collision ((col1 line-collider) (col2 circle-collider))
  (collider-get-collision col2 col1))

(defmethod collider-get-collision ((col1 line-collider) (col2 aabb-collider))
  (collider-get-collision col2 col1))

(defmethod collider-get-collision ((col1 line-collider) (col2 rectangle-collider))
  (collider-get-collision col2 col1))

(defmethod collider-get-collision ((col1 line-collider) (col2 line-collider))
  (line-points-get-collision (make-line-points (collider-position col1) (line-collider-length col1) (line-collider-rotation col1))
                             (make-line-points (collider-position col2) (line-collider-length col2) (line-collider-rotation col2))))

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

;; Splits input into x and y components and does checks separately
;; Sloves collider getting stuck on touching colliders
(defmethod collider-resolve-collisions ((col collider) colliders delta-pos)
  (let* ((original-pos (collider-position col))
         (dx (3d-vectors:vx delta-pos))
         (dy (3d-vectors:vy delta-pos))
         (min-dx dx)
         (min-dy dy))
    (let ((delta-pos (3d-vectors:vec2 dx 0)))
      (loop for other-collider being the hash-values of colliders
              when (not (or (eq col other-collider) (collider-is-trigger other-collider)))
              do (let ((new-dx (3d-vectors:vx (collider-resolve-collision col other-collider delta-pos))))
                 (when (< (abs new-dx) (abs min-dx)) (setf min-dx new-dx))))
      (setf (collider-position col) (3d-vectors:v+ original-pos (3d-vectors:vec2 min-dx 0)))) ; move in x direction
    (let ((delta-pos (3d-vectors:vec2 0 dy)))
      (loop for other-collider being the hash-values of colliders
              when (not (or (eq col other-collider) (collider-is-trigger other-collider)))
              do (let ((new-dy (3d-vectors:vy (collider-resolve-collision col other-collider delta-pos))))
                 (when (< (abs new-dy) (abs min-dy)) (setf min-dy new-dy)))))
    (setf (collider-position col) original-pos) ; reset position
    (3d-vectors:vec2 min-dx min-dy)))

;; Old version with movement-result = (min-x-movement, min-y-movement)
;(defmethod collider-resolve-collisions ((col collider) colliders delta-pos)
;  (let ((min-dx (3d-vectors:vx delta-pos))
;        (min-dy (3d-vectors:vy delta-pos)))
;    (loop for other-collider being the hash-values of colliders
;            when (not (or (eq col other-collider) (collider-is-trigger other-collider)))
;            do (let* ((new-delta-pos (collider-resolve-collision col other-collider delta-pos))
;                      (new-dx (3d-vectors:vx new-delta-pos))
;                      (new-dy (3d-vectors:vy new-delta-pos)))
;                 (when (< (abs new-dx) (abs min-dx)) (setf min-dx new-dx))
;                 (when (< (abs new-dy) (abs min-dy)) (setf min-dy new-dy))))
;    (3d-vectors:vec2 min-dx min-dy)))

;; direction is a rotation value (TODO: change to direction vector?)
(defun raycast-get-collisions (position direction colliders &optional (limit 1000))
  (let ((line-col (make-instance 'line-collider :position position :length limit :rotation direction)))
    (collider-get-collisions line-col colliders)))

(defun line-of-sight-get-collisions (point-A point-B colliders)
  (let* ((pos (3d-vectors:v/ (3d-vectors:v+ point-A point-B) 2))
         (dist (3d-vectors:vdistance point-A point-B))
         (dist-vec-unit (3d-vectors:vunit (3d-vectors:v- point-B point-A)))
         (rot (atan (- (3d-vectors:vx dist-vec-unit)) (3d-vectors:vy dist-vec-unit)))
         (line-col (make-instance 'line-collider :position pos :length dist :rotation rot)))
    (collider-get-collisions line-col colliders)))
