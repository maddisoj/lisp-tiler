(require "~/quicklisp/setup.lisp")

(ql:quickload "cl-opengl")
(ql:quickload "cl-glu")
(ql:quickload "cl-glut")

(require "keyboard.lisp")
(require "squarify.lisp")
(require "rectangle.lisp")

(defconstant *window-width* 800)
(defconstant *window-height* 600)
(defconstant *areas* (list 10 20 30 40 50 60 70 80 90 100))
(defparameter *rects* (squarify *areas* (create-rectangle 0 0 *window-width* *window-height*)))

(defclass tiler-window (glut:window)
  ()
  (:default-initargs :title "Tiler"
                     :width *window-width*
                     :height *window-height*
                     :mode '(:double :rgb :depth)))

(defmethod glut:display-window :before ((window tiler-window))
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:load-identity)
  (init-2D))

(defmethod glut:display-window ((window tiler-window))
  (gl:clear :color-buffer :depth-buffer)
  (gl:load-identity)
  
  (dolist (rect *rects*)
    (draw-rectangle rect))

  (glut:swap-buffers))

(defmethod glut:keyboard ((window tiler-window) key x y)
    (on-key-press key x y))

(defun init-2D ()
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:viewport 0 0 *window-width* *window-height*)
  (gl:ortho 0 *window-width* *window-height* 0 0 1)
  (gl:matrix-mode :modelview))

(defun main()
  (dolist (rect *rects*)
    (print-object rect t)
    (format t "~%"))
  (glut:display-window (make-instance 'tiler-window)))

(main)
