(load #P"quicklisp/setup.lisp")

(ql:quickload "cl-opengl")
(ql:quickload "cl-glu")
(ql:quickload "cl-glut")

(require "keyboard.lisp")

(defclass tiler-window (glut:window)
  ()
  (:default-initargs :title "Tiler"
                     :width 800
                     :height 600
                     :mode '(:double :rgb :depth)))

(defmethod glut:display-window :before ((window tiler-window))
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:load-identity))

(defmethod glut:display-window ((window tiler-window))
  (gl:clear :color-buffer :depth-buffer)
  (gl:load-identity)

  (glut:swap-buffers))

(defmethod glut:keyboard ((window tiler-window) key x y)
    (on-key-press key x y))

(defun main()
  (glut:display-window (make-instance 'tiler-window)))

(main)
