(defpackage :textures
  (:use :common-lisp)
  (:export :*test-texture* :*test-texture2* :*missing-texture* :*test-floor-texture* :*test-wall-texture* :*test-circle*
           :load-textures :delete-textures
           :texture-draw
           :send-draw-calls
           :setup-opengl :shutdown-opengl))

(in-package :textures)

(defvar *plane-vertex-buffer*)
(defvar *plane-index-buffer*)
(defvar *texture-shader-program*)
(defvar *vp-matrix-id*)

; defconstant causes errors
(defvar +plane-vertex-array+
        (let ((vertex-array (gl:alloc-gl-array :float 20)))
          (setf (gl:glaref vertex-array 0) -0.5) ; x
          (setf (gl:glaref vertex-array 1) -0.5) ; y
          (setf (gl:glaref vertex-array 2) 0.0)  ; z
          (setf (gl:glaref vertex-array 3) 1.0)  ; u
          (setf (gl:glaref vertex-array 4) 0.0)  ; v
               
          (setf (gl:glaref vertex-array 5) -0.5)
          (setf (gl:glaref vertex-array 6) 0.5)
          (setf (gl:glaref vertex-array 7) 0.0)
          (setf (gl:glaref vertex-array 8) 0.0)
          (setf (gl:glaref vertex-array 9) 0.0)
               
          (setf (gl:glaref vertex-array 10) 0.5)
          (setf (gl:glaref vertex-array 11) -0.5)
          (setf (gl:glaref vertex-array 12) 0.0)
          (setf (gl:glaref vertex-array 13) 1.0)
          (setf (gl:glaref vertex-array 14) 1.0)
               
          (setf (gl:glaref vertex-array 15) 0.5)
          (setf (gl:glaref vertex-array 16) 0.5)
          (setf (gl:glaref vertex-array 17) 0.0)
          (setf (gl:glaref vertex-array 18) 0.0)
          (setf (gl:glaref vertex-array 19) 1.0)
          vertex-array))

; defconstant causes errors
(defvar +plane-index-array+
        (let ((index-array (gl:alloc-gl-array :unsigned-short 6)))
          ; 1st trig
          (setf (gl:glaref index-array 0) 0)
          (setf (gl:glaref index-array 1) 1)
          (setf (gl:glaref index-array 2) 2)
          ; 2nd trig
          (setf (gl:glaref index-array 3) 3)
          (setf (gl:glaref index-array 4) 1)
          (setf (gl:glaref index-array 5) 2)
          index-array))

(defun make-shader (vertex-shader-file fragment-shader-file)
  (let ((program (gl:create-program))
        (vertex-shader (gl:create-shader :vertex-shader))
        (fragment-shader (gl:create-shader :fragment-shader)))
    (gl:shader-source vertex-shader (uiop:read-file-string vertex-shader-file))
    (gl:compile-shader vertex-shader)
    (gl:attach-shader program vertex-shader)

    (gl:shader-source fragment-shader (uiop:read-file-string fragment-shader-file))
    (gl:compile-shader fragment-shader)
    (gl:attach-shader program fragment-shader)

    (gl:link-program program)

    (gl:detach-shader program vertex-shader)
    (gl:detach-shader program fragment-shader)

    (gl:delete-shader vertex-shader)
    (gl:delete-shader fragment-shader)

    program))

(defun setup-opengl ()
  (setf *texture-shader-program* (make-shader "shaders/texture_instanced_vertex_shader.vertexshader" "shaders/texture_discard_fragment_shader.fragmentshader"))
                            
  (setf *plane-vertex-buffer* (gl:gen-buffer))
  (gl:bind-buffer :array-buffer *plane-vertex-buffer*)
  (gl:buffer-data :array-buffer :static-draw +plane-vertex-array+)

  (setf *plane-index-buffer* (gl:gen-buffer))
  (gl:bind-buffer :element-array-buffer *plane-index-buffer*)
  (gl:buffer-data :element-array-buffer :static-draw +plane-index-array+)

  (setf *vp-matrix-id* (gl:get-uniform-location *texture-shader-program* "VP"))
  ;(gl:enable :texture-2d)
  (gl:cull-face :back)
  (gl:enable :depth-test)
  (gl:depth-func :less)
  (gl:depth-mask :true)

  ; (1) Use for proper partial transparency (manual sorting of draw order required)
  ;(gl:disable :depth-test)
  ;(gl:depth-mask :false)
  ; (2) Required for partial transparency -> alternative: use shader that discards on full transparency

  ;(gl:enable :blend)
  ;(gl:blend-func :src-alpha :one-minus-src-alpha)
  )

(defun shutdown-opengl ()
  ;(gl:disable :texture-2d)
  (gl:delete-program *texture-shader-program*))

(defvar *loaded-textures* '())

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

(defclass texture ()
    ((file :initarg :file
           :reader texture-file)
     (texture-id :reader texture-texture-id)
     (vao :initform NIL
          :accessor texture-vao)
     (model-matrix-buffer :initform NIL
                          :accessor texture-model-matrix-buffer)
     (model-matrix-buffer-size :initform 0
                               :accessor texture-model-matrix-buffer-size)
     (model-matrix-array :initform (make-simple-array-vector 'single-float)
                         :accessor texture-model-matrix-array)))

;; Set up the openGL buffers for rendering
(defmethod texture-setup-buffers ((obj texture))
  (setf (texture-vao obj) (gl:gen-vertex-array))
  (gl:bind-vertex-array (texture-vao obj))

  (let ((float-size (cffi:foreign-type-size :float))
        (vec4-size (* 4 (cffi:foreign-type-size :float))))
    ; Setup vertex position and uv
    (gl:bind-buffer :array-buffer *plane-vertex-buffer*)

    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float NIL (* 5 float-size) (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 1 2 :float NIL (* 5 float-size) (cffi:inc-pointer (cffi:null-pointer) (* 3 float-size)))

    ; Setup vertex indices
    (gl:bind-buffer :element-array-buffer *plane-index-buffer*)

    ; Setup model matrix buffer for instanced rendering
    (setf (texture-model-matrix-buffer obj) (gl:gen-buffer))
    (gl:bind-buffer :array-buffer (texture-model-matrix-buffer obj))

    (gl:enable-vertex-attrib-array 2)
    (gl:vertex-attrib-pointer 2 4 :float NIL (* 4 vec4-size) (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 3)
    (gl:vertex-attrib-pointer 3 4 :float NIL (* 4 vec4-size) (cffi:inc-pointer (cffi:null-pointer) vec4-size))
    (gl:enable-vertex-attrib-array 4)
    (gl:vertex-attrib-pointer 4 4 :float NIL (* 4 vec4-size) (cffi:inc-pointer (cffi:null-pointer) (* 2 vec4-size)))
    (gl:enable-vertex-attrib-array 5)
    (gl:vertex-attrib-pointer 5 4 :float NIL (* 4 vec4-size) (cffi:inc-pointer (cffi:null-pointer) (* 3 vec4-size)))

    (%gl:vertex-attrib-divisor 2 1)
    (%gl:vertex-attrib-divisor 3 1)
    (%gl:vertex-attrib-divisor 4 1)
    (%gl:vertex-attrib-divisor 5 1))
  (gl:bind-vertex-array 0))

;; Load the texture data from the file
(defmethod texture-load-data ((obj texture))
  (labels ((array-flatten (in)
                          (let ((out (make-array (array-total-size in) :fill-pointer 0 :element-type 'unsigned-byte)))
                            (destructuring-bind (x y z) (array-dimensions in)
                              (dotimes (i x)
                                      (dotimes (j y)
                                              (dotimes (k z)
                                                      (vector-push (aref in i j k) out)))))
                            out)))
    (format t "Loading texture from file ~a~%" (texture-file obj))
    (with-slots (file texture-id) obj
      (setf texture-id (car (gl:gen-textures 1)))
      (gl:bind-texture :texture-2d texture-id)
      (gl:pixel-store :unpack-alignment 1) ; alternatively: manually align data so that each row is a multiple of 4 Byte
      (let* ((png (png-read:read-png-file file))
            (data (array-flatten (png-read:image-data png)))
            (width (png-read:width png))
            (height (png-read:height png))
            (color-type (png-read:colour-type png)))
        (format t "Color type: ~a~%" color-type)
        (format t "Data type is ~a~%" (type-of data))
        (format t "Size: ~a, ~a~%" width height)
        ;(format t "Data:~%~a~%" data)
        (case color-type
          (:truecolor (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb :unsigned-byte data)
                      (format t "RGB-mode~%"))
          (:indexed-colour (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb :unsigned-byte data)
                      (format t "RGB-mode~%"))
          (:truecolor-alpha (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte data)
                            (format t "RGBA-mode~%"))
          (otherwise (format t "Error: Color type not implemented"))))
      ;(gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
      ;(gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
      ;(gl:tex-parameter :texture-2d :texture-min-filter :nearest)
      ;(gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      ;(gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
      (gl:tex-parameter :texture-2d :texture-min-filter :nearest-mipmap-nearest)
      (gl:generate-mipmap :texture-2d)
      (gl:bind-texture :texture-2d 0))))

(defmethod texture-load ((obj texture))
  (setf *loaded-textures* (adjoin obj *loaded-textures*))
  (texture-setup-buffers obj)
  (texture-load-data obj))

(defmethod texture-delete ((obj texture))
  (setf *loaded-textures* (set-difference *loaded-textures* (list obj)))

  (gl:delete-texture (slot-value obj 'texture-id))
  (when (texture-model-matrix-buffer obj)
        (gl:delete-buffers (list (texture-model-matrix-buffer obj))))
  (when (texture-vao obj)
        (gl:delete-vertex-arrays (list (texture-vao obj)))))

;; "registers" a draw call to be executed later
(defmethod texture-draw ((obj texture) model-matrix vp-matrix)
  (simple-array-vector-push-vector (texture-model-matrix-array obj) (3d-matrices:marr4 (3d-matrices:mtranspose model-matrix))))

;; actually does a draw call using instancing
(defmethod texture-send-draw-call ((obj texture) vp-matrix)
  (when (> (simple-array-vector-size (texture-model-matrix-array obj)) 0)
        (gl:bind-vertex-array (texture-vao obj))
        (gl:uniform-matrix-4fv *vp-matrix-id* (vector (3d-matrices:marr4 vp-matrix)))

        ;; setup model matrices
        ; TODO: get rid of warning
        ; TODO: when re-allocation of buffer occurs, allocate capacity of array (not just element-count)
        (gl:bind-buffer :array-buffer (texture-model-matrix-buffer obj))
        (let* ((lisp-array (simple-array-vector-data (texture-model-matrix-array obj)))
               (element-count (simple-array-vector-size (texture-model-matrix-array obj))))
          (waaf-cffi:with-array-as-foreign-pointer (lisp-array ptr :float
                                                                   :lisp-type single-float
                                                                   :start 0
                                                                   :end element-count
                                                                   :copy-to-foreign T
                                                                   :copy-from-foreign NIL)
            (if (< (texture-model-matrix-buffer-size obj) element-count)
                (progn
                 (gl:buffer-data :array-buffer :dynamic-draw (gl::make-gl-array-from-pointer ptr :float element-count))
                 (setf (texture-model-matrix-buffer-size obj) element-count))
                (gl:buffer-sub-data :array-buffer (gl::make-gl-array-from-pointer ptr :float element-count)))))

        (gl:active-texture :texture0)
        (gl:bind-texture :texture-2d (texture-texture-id obj))

        (gl:draw-elements-instanced :triangles (gl:make-null-gl-array :unsigned-short) (/ (simple-array-vector-size (texture-model-matrix-array obj)) 16) :count 6)
        (simple-array-vector-clear (texture-model-matrix-array obj))

        (gl:bind-vertex-array 0)))

(defvar *test-texture* (make-instance 'texture
                         :file "textures/test.png"))

(defvar *test-texture2* (make-instance 'texture
                          :file "textures/test2.png"))

(defvar *missing-texture* (make-instance 'texture
                            :file "textures/missing_texture.png"))

(defvar *test-floor-texture* (make-instance 'texture
                               :file "textures/test_floor.png"))

(defvar *test-wall-texture* (make-instance 'texture
                              :file "textures/test_wall.png"))

(defvar *test-circle* (make-instance 'texture
                        :file "textures/circle.png"))

(defun load-textures ()
  (texture-load *test-texture*)
  (texture-load *missing-texture*)
  (texture-load *test-texture2*)
  (texture-load *test-floor-texture*)
  (texture-load *test-wall-texture*)
  (texture-load *test-circle*))

(defun delete-textures ()
  (texture-delete *test-texture*)
  (texture-delete *missing-texture*)
  (texture-delete *test-texture2*)
  (texture-delete *test-floor-texture*)
  (texture-delete *test-wall-texture*)
  (texture-delete *test-circle*))

(defun send-draw-calls (vp-matrix)
  (loop for texture in *loaded-textures*
          do (texture-send-draw-call texture vp-matrix)))
