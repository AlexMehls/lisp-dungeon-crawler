(defpackage :player-input
  (:use :common-lisp)
  (:export :*active-camera* :*window-w* :*window-h*
           
           :*keys-held* :*keys-pressed*
           :*buttons-held* :*buttons-pressed*
           :*mouse-x* :*mouse-y*
           
           :gdk-keyval :get-key-press :get-key-hold
           :gdk-mouse-button :get-button-press :get-button-hold
           :get-mouse-screen-pos :screen-pos-normalized :screen-pos-unnormalized :get-mouse-world-pos))

(in-package :player-input)

; TODO: put somewhere else
(defvar *active-camera* NIL)
(defvar *window-w* 1)
(defvar *window-h* 1)

(defvar *keys-held* NIL)
(defvar *keys-pressed* NIL)

(defvar *buttons-held* NIL)
(defvar *buttons-pressed* NIL)

(defvar *mouse-x* 0)
(defvar *mouse-y* 0)

(defmacro gdk-keyval (name)
  (declare (ignore name))
  0) ; Only needs to be evaluated once -> macro

(defmacro get-key-press (name)
  `(member (gdk-keyval ,name) *keys-pressed*))

(defmacro get-key-hold (name)
  `(member (gdk-keyval ,name) *keys-held*))

;; Swaps mouse buttons 2 and 3 (mouse 3 should be the middle button)
(defmacro gdk-mouse-button (val)
  (case val
    (2 3)
    (3 2)
    (otherwise val)))

(defmacro get-button-press (val)
  `(member (gdk-mouse-button ,val) *buttons-pressed*))

(defmacro get-button-hold (val)
  `(member (gdk-mouse-button ,val) *buttons-held*))

(defmacro get-mouse-screen-pos ()
  `(3d-vectors:vec2 *mouse-x* *mouse-y*))

;; Returns pos relative to screen center and using in-game coordinates
(defmacro screen-pos-normalized (screen-pos)
  `(3d-vectors:v* (3d-vectors:v* (3d-vectors:v- ,screen-pos (3d-vectors:vec2 (/ *window-w* 2) (/ *window-h* 2))) (3d-vectors:vec2 1 -1)) (/ (camera-screen-size *active-camera*) *window-h*)))

(defmacro screen-pos-unnormalized (world-pos)
  `(3d-vectors:v+ (3d-vectors:v/ (3d-vectors:v/ ,world-pos (/ (camera-screen-size *active-camera*) *window-h*)) (3d-vectors:vec2 1 -1)) (3d-vectors:vec2 (/ *window-w* 2) (/ *window-h* 2))))

(defmacro get-mouse-world-pos ()
  `(screen-pos-normalized (get-mouse-screen-pos)))
