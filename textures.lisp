(defpackage :textures
  (:use :common-lisp)
  (:export :*test-texture* :*test-texture2* :*missing-texture* :*test-floor-texture* :*test-wall-texture* :*test-circle*
           :load-textures :delete-textures
           :texture-draw
           :setup-opengl :shutdown-opengl))

(in-package :textures)

(defvar *plane-vertex-buffer*)
(defvar *plane-index-buffer*)
(defvar *texture-shader-program*)
(defvar *mvp-matrix-id*)

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
  (setf *texture-shader-program* (make-shader "shaders/texture_vertex_shader.vertexshader" "shaders/texture_fragment_shader.fragmentshader"))
                            
  (setf *plane-vertex-buffer* (gl:gen-buffer))
  (gl:bind-buffer :array-buffer *plane-vertex-buffer*)
  (gl:buffer-data :array-buffer :static-draw +plane-vertex-array+)

  (setf *plane-index-buffer* (gl:gen-buffer))
  (gl:bind-buffer :element-array-buffer *plane-index-buffer*)
  (gl:buffer-data :element-array-buffer :static-draw +plane-index-array+)

  (setf *mvp-matrix-id* (gl:get-uniform-location *texture-shader-program* "MVP"))
  ;(gl:enable :texture-2d)
  (gl:cull-face :back)
  (gl:enable :depth-test)
  (gl:depth-func :less)
  (gl:depth-mask :true)
  )

(defun shutdown-opengl ()
  ;(gl:disable :texture-2d)
  (gl:delete-program *texture-shader-program*))

(defclass texture ()
    ((file :initarg :file
           :reader texture-file)
     (texture-id :reader texture-texture-id)))

(defmethod texture-load ((obj texture))
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

(defmethod texture-delete ((obj texture))
  (gl:delete-texture (slot-value obj 'texture-id)))

(defmethod texture-draw ((obj texture) model-matrix vp-matrix)
  ; Todo: parameter? slot of texture?
  (gl:use-program *texture-shader-program*)

  (gl:uniform-matrix-4fv *mvp-matrix-id* (vector (3d-matrices:marr4 (3d-matrices:m* vp-matrix model-matrix))))

  (gl:enable-vertex-attrib-array 0)
  (gl:enable-vertex-attrib-array 1)

  (gl:bind-buffer :array-buffer *plane-vertex-buffer*)
  (gl:vertex-attrib-pointer 0 3 :float NIL (* 5 (cffi:foreign-type-size :float)) (cffi:null-pointer))
  (gl:vertex-attrib-pointer 1 2 :float NIL (* 5 (cffi:foreign-type-size :float)) (cffi:inc-pointer (cffi:null-pointer) (* 3 (cffi:foreign-type-size :float))))

  (gl:bind-buffer :element-array-buffer *plane-index-buffer*)

  ; (1) Use for proper partial transparency (manual sorting of draw order required)
  ;(gl:disable :depth-test)
  ;(gl:depth-mask :false)
  ; (2) Required for partial transparency -> alternative: use shader that discards on full transparency
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (texture-texture-id obj))

  ; null-array -> uses currently bound element-array-buffer
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6)

  (gl:disable-vertex-attrib-array 0)
  (gl:disable-vertex-attrib-array 1)

  ; Use according to (1)
  ;(gl:enable :depth-test)
  ;(gl:depth-mask :true)
  ; (2)
  (gl:disable :blend)

  (gl:use-program 0)
  (gl:flush))

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
