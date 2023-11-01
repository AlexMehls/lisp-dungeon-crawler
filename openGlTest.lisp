(ql:quickload :png-read)
(ql:quickload :cl-opengl)
(ql:quickload :cl-cffi-gtk)
(ql:quickload :3d-vectors)
(ql:quickload :3d-matrices)

(defpackage :opengl-test
(:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :opengl-test)

(defclass texture ()
    ((file :initarg :file
           :reader texture-file)
     (texture-id :reader texture-texture-id)))

(defmethod texture-load ((obj texture))
  ; TODO: replace with nested loops?
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

(defvar *test-texture* (make-instance 'texture
                                 :file "textures/test.png"))

(defvar *test-texture2* (make-instance 'texture
                                 :file "textures/test2.png"))

(defvar *missing-texture* (make-instance 'texture
                                 :file "textures/missing_texture.png"))

(defvar *plane-vertex-buffer*)
(defvar *plane-uv-buffer*)
(defvar *texture-shader-program*)
(defvar *mvp-matrix-id*)

(defclass sprite ()
    ((pos :initarg :position
          :accessor sprite-position
          :initform (3d-vectors:vec 0 0))
     (layer :initarg :layer
            :accessor sprite-layer
            :initform 0)
     (size :initarg :size
           :accessor sprite-size
           :initform (3d-vectors:vec 1 1))
     (rot :initarg :rotation
           :accessor sprite-rotation
           :initform 0)
     (texture :initarg :texture
              :reader sprite-texture
              :initform *missing-texture*)))

(defclass camera ()
    ((pos :initarg :position
          :accessor camera-position
          :initform (3d-vectors:vec 0 0))
     (screen-ratio :initform 1
                   :reader camera-screen-ratio)
     (screen-size :initarg :screen-size
           :reader camera-screen-size
           :initform 1)))

(defvar *default-camera* (make-instance 'camera :position (3d-vectors:vec 0 0) :screen-size 5))

(defmethod sprite-model-matrix ((obj sprite))
  (let ((mat (3d-matrices:mtranslation (3d-vectors:v- (3d-vectors:vxy_ (sprite-position obj)) (3d-vectors:vec 0 0 (sprite-layer obj))))))
    (3d-matrices:nmscale mat (3d-vectors:v+ (3d-vectors:vxy_ (sprite-size obj)) (3d-vectors:vec 0 0 1)))
    (3d-matrices:nmrotate mat 3d-vectors:+vz+ (sprite-rotation obj))
    mat))

(defmethod camera-view-projection-matrix ((obj camera))
  (let* ((half-w (* (camera-screen-ratio obj) (camera-screen-size obj) 0.5))
         (half-h (* (camera-screen-size obj) 0.5))
         (mat (3d-matrices:mortho (- half-w) half-w (- half-h) half-h 0.001 10000))
         (pos (3d-vectors:v+ (3d-vectors:vxy_ (camera-position obj)) (3d-vectors:vec 0 0 1))))
    (3d-matrices:nmlookat mat pos (3d-vectors:v- pos 3d-vectors:+vz+) 3d-vectors:+vy+)
    mat))

(defmethod sprite-draw ((obj sprite) vp-matrix)
  ; Todo: program member of sprite?
  (gl:use-program *texture-shader-program*)

  ; TODO: view-projection shouldn't be calculated multiple times

  (gl:uniform-matrix-4fv *mvp-matrix-id* (vector (3d-matrices:marr4 (3d-matrices:m* vp-matrix (sprite-model-matrix obj)))))

  ; TODO: Render unsing indices

  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer *plane-vertex-buffer*)
  (gl:vertex-attrib-pointer 0 3 :float NIL 0 (cffi:null-pointer))

  (gl:enable-vertex-attrib-array 1)
  (gl:bind-buffer :array-buffer *plane-uv-buffer*)
  (gl:vertex-attrib-pointer 1 2 :float NIL 0 (cffi:null-pointer))

  ; (1) Use for proper partial transparency (manual sorting of draw order required)
  ;(gl:disable :depth-test)
  ;(gl:depth-mask :false)
  ; (2) Required for partial transparency -> alternative: use shader that discards on full transparency
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (texture-texture-id (sprite-texture obj)))
  ;(format t "Drawing sprite at ~a~%" (sprite-position obj))

  (gl:draw-arrays :triangles 0 6)

  (gl:disable-vertex-attrib-array 0)
  (gl:disable-vertex-attrib-array 1)

  ; Use according to (1)
  ;(gl:enable :depth-test)
  ;(gl:depth-mask :true)
  ; (2)
  (gl:disable :blend)

  (gl:use-program 0)
  (gl:flush))

(defconstant +plane-vertex-array+
        (let ((vertex-array (gl:alloc-gl-array :float 18)))
          ; 1st trig
          (setf (gl:glaref vertex-array 0) -0.5)
          (setf (gl:glaref vertex-array 1) -0.5)
          (setf (gl:glaref vertex-array 2) 0.0)
          (setf (gl:glaref vertex-array 3) -0.5)
          (setf (gl:glaref vertex-array 4) 0.5)
          (setf (gl:glaref vertex-array 5) 0.0)
          (setf (gl:glaref vertex-array 6) 0.5)
          (setf (gl:glaref vertex-array 7) -0.5)
          (setf (gl:glaref vertex-array 8) 0.0)
          ; 2nd trig
          (setf (gl:glaref vertex-array 9) 0.5)
          (setf (gl:glaref vertex-array 10) 0.5)
          (setf (gl:glaref vertex-array 11) 0.0)
          (setf (gl:glaref vertex-array 12) -0.5)
          (setf (gl:glaref vertex-array 13) 0.5)
          (setf (gl:glaref vertex-array 14) 0.0)
          (setf (gl:glaref vertex-array 15) 0.5)
          (setf (gl:glaref vertex-array 16) -0.5)
          (setf (gl:glaref vertex-array 17) 0.0)
          vertex-array))

(defconstant +plane-uv-array+
        (let ((uv-array (gl:alloc-gl-array :float 12)))
          ; 1st trig
          (setf (gl:glaref uv-array 0) 1.0)
          (setf (gl:glaref uv-array 1) 0.0)
          (setf (gl:glaref uv-array 2) 0.0)
          (setf (gl:glaref uv-array 3) 0.0)
          (setf (gl:glaref uv-array 4) 1.0)
          (setf (gl:glaref uv-array 5) 1.0)
          ; 2nd trig
          (setf (gl:glaref uv-array 6) 0.0)
          (setf (gl:glaref uv-array 7) 1.0)
          (setf (gl:glaref uv-array 8) 0.0)
          (setf (gl:glaref uv-array 9) 0.0)
          (setf (gl:glaref uv-array 10) 1.0)
          (setf (gl:glaref uv-array 11) 1.0)
          uv-array))

(defun load-textures ()
  (texture-load *test-texture*)
  (texture-load *missing-texture*)
  (texture-load *test-texture2*))

(defun delete-textures ()
  (texture-delete *test-texture*)
  (texture-delete *missing-texture*)
  (texture-delete *test-texture2*))

(defun setup-opengl ()
  ;(gl:enable :texture-2d)
  (gl:cull-face :back)
  (gl:depth-func :less)
  (load-textures))

(defun shutdown-opengl ()
  (delete-textures)
  ;(gl:disable :texture-2d)
  )

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

(defun example-gl-area ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (area (make-instance 'gtk-gl-area))
          vao
          (test-sprite (make-instance 'sprite
                         :position (3d-vectors:vec 0 0)
                         :size (3d-vectors:vec 1 1)
                         :rotation 0
                         :texture *test-texture2*)))
      (gtk-container-add window area)
      (g-signal-connect area "realize"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-gl-area-make-current area)
                          (gtk-gl-area-get-error area)

                          (setf vao (gl:gen-vertex-array))
                          (gl:bind-vertex-array vao)

                          (setf *texture-shader-program* (make-shader "shaders/texture_vertex_shader.vertexshader" "shaders/texture_fragment_shader.fragmentshader"))
                            
                          (setf *plane-vertex-buffer* (gl:gen-buffer))
                          (gl:bind-buffer :array-buffer *plane-vertex-buffer*)
                          (gl:buffer-data :array-buffer :static-draw +plane-vertex-array+)

                          (setf *plane-uv-buffer* (gl:gen-buffer))
                          (gl:bind-buffer :array-buffer *plane-uv-buffer*)
                          (gl:buffer-data :array-buffer :static-draw +plane-uv-array+)

                          (setf *mvp-matrix-id* (gl:get-uniform-location *texture-shader-program* "MVP"))
                          
                          (setup-opengl)))
      (g-signal-connect area "unrealize"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-gl-area-make-current area)
                          (gtk-gl-area-get-error area)
                          (gl:delete-vertex-arrays (list vao))
                          (gl:delete-program *texture-shader-program*)
                          (shutdown-opengl)))
      (g-signal-connect area "render"
                        (lambda (area context)
                          (declare (ignore context))
                          (gl:clear-color 0.5 0.5 0.5 1.0)
                          (gl:clear :color-buffer :depth-buffer)
                          (let ((w (gtk-widget-get-allocated-width area))
                                (h (gtk-widget-get-allocated-height area)))
                            (when (and (> h 0) (> w 0))
                                (setf (slot-value *default-camera* 'screen-ratio) (/ w h))))
                          (let ((vp-mat (camera-view-projection-matrix *default-camera*)))
                            (sprite-draw test-sprite vp-mat))
                          NIL))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show-all window)))
  (sb-thread:release-foreground) ; For better debug output
  (gtk:join-gtk-main))

(example-gl-area)
;(sb-ext:save-lisp-and-die "openGlTest.exe"
;                   :toplevel #'example-gl-area
;                   :executable t)