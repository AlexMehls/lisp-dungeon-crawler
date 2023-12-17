(defpackage :model-matrix-manager
  (:use :common-lisp)
  (:export :simple-array-vector-data :simple-array-vector-size :simple-array-vector-capacity :simple-array-vector-type
           
           :model-matrix-manager
           :model-matrix-manager-bind-buffer :model-matrix-manager-gen-buffer :model-matrix-manager-delete-buffer

           :model-matrix-manager-static-matrices :model-matrix-manager-dynamic-matrices
           :model-matrix-manager-total-size :model-matrix-manager-object-count
           :model-matrix-manager-get-static-slot :model-matrix-manager-free-static-slot :model-matrix-manager-set-static-slot
           :model-matrix-manager-get-dynamic-slot :model-matrix-manager-free-dynamic-slot :model-matrix-manager-set-dynamic-slot))

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
    ((static-matrices :initform (make-simple-array-vector 'single-float)
                      :reader model-matrix-manager-static-matrices)
     (dynamic-matrices :initform (make-simple-array-vector 'single-float)
                       :reader model-matrix-manager-dynamic-matrices)
     (vacant-static-slots :initform (queues:make-queue :simple-queue))
     (vacant-dynamic-slots :initform (queues:make-queue :simple-queue))
     (static-needs-update :initform NIL
                          :accessor model-matrix-manager-static-needs-update)
     (slot-size :initform 16)
     
     (buffer :initform NIL)
     (buffer-size :initform 0)))

(defmethod model-matrix-manager-total-size ((obj model-matrix-manager))
  (with-slots (static-matrices dynamic-matrices) obj
    (+ (simple-array-vector-size static-matrices) (simple-array-vector-size dynamic-matrices))))

(defmethod model-matrix-manager-total-capacity ((obj model-matrix-manager))
  (with-slots (static-matrices dynamic-matrices) obj
    (+ (simple-array-vector-capacity static-matrices) (simple-array-vector-capacity dynamic-matrices))))

(defmethod model-matrix-manager-object-count ((obj model-matrix-manager))
  (with-slots (slot-size) obj
    (/ (model-matrix-manager-total-size obj) slot-size)))

(defmethod model-matrix-manager-delete-buffer ((obj model-matrix-manager))
  (with-slots (buffer buffer-size) obj
    (when buffer
          (gl:delete-buffers (list buffer)))
    (setf buffer NIL)
    (setf buffer-size 0)))

(defmethod model-matrix-manager-gen-buffer ((obj model-matrix-manager))
  (model-matrix-manager-delete-buffer obj)
  (with-slots (buffer) obj
    (setf buffer (gl:gen-buffer))))

(defmethod model-matrix-manager-bind-buffer ((obj model-matrix-manager))
  (with-slots (static-matrices dynamic-matrices static-needs-update buffer buffer-size) obj
    (gl:bind-buffer :array-buffer buffer)

    ;; TODO: get rid of warning
    (let ((reallocation-needed (> (model-matrix-manager-total-size obj) buffer-size)))
      
      (when reallocation-needed
            (let ((total-capacity (model-matrix-manager-total-capacity obj)))
              (gl:buffer-data :array-buffer :dynamic-draw (gl::make-gl-array-from-pointer (gl::null-pointer) :float total-capacity))
              (setf buffer-size total-capacity)))

      ;; Update static data if necessary
      (when (or static-needs-update reallocation-needed)
            (with-slots (data size) static-matrices
              (waaf-cffi:with-array-as-foreign-pointer (data ptr :float
                                                                  :lisp-type single-float
                                                                  :start 0
                                                                  :end size
                                                                  :copy-to-foreign T
                                                                  :copy-from-foreign NIL)
                (gl:buffer-sub-data :array-buffer (gl::make-gl-array-from-pointer ptr :float size))))
            (setf static-needs-update NIL))
            
      ;; Update dynamic data
      (with-slots (data size) dynamic-matrices
        (waaf-cffi:with-array-as-foreign-pointer (data ptr :float
                                                            :lisp-type single-float
                                                            :start 0
                                                            :end size
                                                            :copy-to-foreign T
                                                            :copy-from-foreign NIL)
          (gl:buffer-sub-data :array-buffer (gl::make-gl-array-from-pointer ptr :float size) :buffer-offset (simple-array-vector-size static-matrices)))))))

(defmethod model-matrix-manager-set-static-slot ((obj model-matrix-manager) slot value-vec)
  (with-slots (static-matrices slot-size static-needs-update) obj
    (simple-array-vector-set-vector static-matrices (* slot slot-size) value-vec)
    (setf static-needs-update T)))

(defmethod model-matrix-manager-get-static-slot ((obj model-matrix-manager))
  (with-slots (static-matrices vacant-static-slots slot-size static-needs-update) obj
    (let ((slot (queues:qpop vacant-static-slots)))
      (if slot
          slot
          (progn
           (simple-array-vector-push-vector static-matrices (make-array slot-size :initial-element 0.0))
           (setf static-needs-update T)
           (1- (/ (simple-array-vector-size static-matrices) 16)))))))

(defmethod model-matrix-manager-free-static-slot ((obj model-matrix-manager) slot)
  (with-slots (vacant-static-slots slot-size) obj
    (queues:qpush vacant-static-slots slot)
    (model-matrix-manager-set-static-slot obj slot (make-array slot-size :initial-element 0.0))))

(defmethod model-matrix-manager-set-dynamic-slot ((obj model-matrix-manager) slot value-vec)
  (with-slots (dynamic-matrices slot-size) obj
    (simple-array-vector-set-vector dynamic-matrices (* slot slot-size) value-vec)))

(defmethod model-matrix-manager-get-dynamic-slot ((obj model-matrix-manager))
  (with-slots (dynamic-matrices vacant-dynamic-slots slot-size) obj
    (let ((slot (queues:qpop vacant-dynamic-slots)))
      (if slot
          slot
          (progn
           (simple-array-vector-push-vector dynamic-matrices (make-array slot-size :initial-element 0.0))
           (1- (/ (simple-array-vector-size dynamic-matrices) 16)))))))

(defmethod model-matrix-manager-free-dynamic-slot ((obj model-matrix-manager) slot)
  (with-slots (vacant-dynamic-slots slot-size) obj
    (queues:qpush vacant-dynamic-slots slot)
    (model-matrix-manager-set-dynamic-slot obj slot (make-array slot-size :initial-element 0.0))))
