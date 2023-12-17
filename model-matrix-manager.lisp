(defpackage :model-matrix-manager
  (:use :common-lisp)
  (:export :simple-array-vector-data :simple-array-vector-size :simple-array-vector-capacity :simple-array-vector-type
           
           :model-matrix-manager
           :model-matrix-manager-matrices :model-matrix-manager-slot-size
           :model-matrix-manager-get-slot :model-matrix-manager-free-slot :model-matrix-manager-set-slot))

(in-package :model-matrix-manager)

;; Uses a simple-array as the data storage and provides C++-vector-like operations
(defclass simple-array-vector ()
    ((data :initarg :data
           :initform (make-array 0)
           :reader simple-array-vector-data)
     (size :initform 0
           :reader simple-array-vector-size)
     (capacity :initform 0
               :reader simple-array-vector-capacity)
     (type :initarg :type
           :initform NIL
           :reader simple-array-vector-type)))

(defun make-simple-array-vector (type)
  (make-instance 'simple-array-vector :data (make-array 0 :element-type type) :type type))

(defmethod simple-array-vector-clear ((obj simple-array-vector))
  (with-slots (size) obj
    (setf size 0)))

(defmethod simple-array-vector-reserve ((obj simple-array-vector) new-capacity)
  (with-slots (data size capacity) obj
    (when (> new-capacity capacity)
          (setf capacity new-capacity)
          (let ((new-data (make-array capacity :element-type (simple-array-vector-type obj))))
            (dotimes (i size)
              (setf (aref new-data i) (aref data i)))
            (setf data new-data)))))

(defmethod simple-array-vector-push-vector ((obj simple-array-vector) value-vec)
  (with-slots (data size capacity) obj
    (when (> (+ size (length value-vec)) capacity)
          (simple-array-vector-reserve obj (max (+ size (length value-vec)) (* 2 capacity))))
    (dotimes (i (length value-vec))
      (setf (aref data size) (aref value-vec i))
      (incf size))))

(defmethod simple-array-vector-set-vector ((obj simple-array-vector) start-index value-vec)
  (with-slots (data size capacity) obj
    (dotimes (i (length value-vec))
      (setf (aref data (+ start-index i)) (aref value-vec i)))))

(defclass model-matrix-manager ()
    ((matrices :initform (make-simple-array-vector 'single-float)
               :reader model-matrix-manager-matrices)
     (vacant-slots :initform (queues:make-queue :simple-queue))
     (slot-size :initform 16
                :reader model-matrix-manager-slot-size)))

(defmethod model-matrix-manager-set-slot ((obj model-matrix-manager) slot value-vec)
  (with-slots (matrices slot-size) obj
    (simple-array-vector-set-vector matrices (* slot slot-size) value-vec)))

(defmethod model-matrix-manager-get-slot ((obj model-matrix-manager))
  (with-slots (matrices vacant-slots slot-size) obj
    (let ((slot (queues:qpop vacant-slots)))
      (if slot
          slot
          (progn
           (simple-array-vector-push-vector matrices (make-array slot-size :initial-element 0.0))
           (1- (/ (simple-array-vector-size matrices) 16)))))))

(defmethod model-matrix-manager-free-slot ((obj model-matrix-manager) slot)
  (with-slots (vacant-slots slot-size) obj
    (queues:qpush vacant-slots slot)
    (model-matrix-manager-set-slot obj slot (make-array slot-size :initial-element 0.0))))
