(require :asdf)
(push "./" asdf:*central-registry*)
(asdf:load-system :textures)

(ql:quickload :png-read)
(ql:quickload :cl-opengl)
(ql:quickload :cl-cffi-gtk)
(ql:quickload :3d-vectors)
(ql:quickload :3d-matrices)

(defpackage :opengl-test
(:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp :textures))

(in-package :opengl-test)

(defvar *plane-vertex-buffer*)
(defvar *plane-index-buffer*)
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

  (gl:uniform-matrix-4fv *mvp-matrix-id* (vector (3d-matrices:marr4 (3d-matrices:m* vp-matrix (sprite-model-matrix obj)))))

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
  (gl:bind-texture :texture-2d (texture-texture-id (sprite-texture obj)))

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

(defconstant +plane-vertex-array+
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

(defconstant +plane-index-array+
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

                          (setf *plane-index-buffer* (gl:gen-buffer))
                          (gl:bind-buffer :element-array-buffer *plane-index-buffer*)
                          (gl:buffer-data :element-array-buffer :static-draw +plane-index-array+)

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