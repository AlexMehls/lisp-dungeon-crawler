(defpackage :render-object
  (:use :common-lisp :textures :model-matrix-manager)
  (:export :render-object
           :render-object-texture
           :render-object-register :render-object-free :render-object-update-matrix))

(in-package :render-object)

(defclass render-object ()
    ((texture :initarg :texture
              :reader render-object-texture
              :initform *missing-texture*)
     (model-matrix-slot :initform NIL)
     (static :initform T
             :initarg :static)))

(defmethod render-object-register ((obj render-object))
  (with-slots (texture model-matrix-slot static) obj
    (unless model-matrix-slot
      (if static
          (setf model-matrix-slot (model-matrix-manager-get-static-slot (texture-model-matrix-manager texture)))
          (setf model-matrix-slot (model-matrix-manager-get-dynamic-slot (texture-model-matrix-manager texture)))))))

(defmethod render-object-free ((obj render-object))
  (with-slots (texture model-matrix-slot static) obj
    (when model-matrix-slot
          (if static
              (model-matrix-manager-free-static-slot (texture-model-matrix-manager texture) model-matrix-slot)
              (model-matrix-manager-free-dynamic-slot (texture-model-matrix-manager texture) model-matrix-slot))
          (setf model-matrix-slot NIL))))

(defmethod render-object-update-matrix ((obj render-object) model-matrix)
  (with-slots (texture model-matrix-slot static) obj
    (when model-matrix-slot
          (if static
              (model-matrix-manager-set-static-slot (texture-model-matrix-manager texture) model-matrix-slot (3d-matrices:marr4 (3d-matrices:mtranspose model-matrix)))
              (model-matrix-manager-set-dynamic-slot (texture-model-matrix-manager texture) model-matrix-slot (3d-matrices:marr4 (3d-matrices:mtranspose model-matrix)))))))
