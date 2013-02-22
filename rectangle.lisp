(require "~/quicklisp/setup.lisp")

(ql:quickload "cl-opengl")

(defclass rectangle ()
  ((x :initarg :x
      :accessor x
      :initform 0)
   (y :initarg :y
      :accessor y
      :initform 0)
   (width :initarg :width
          :accessor width
          :initform 0)
   (height :initarg :height
           :accessor height
           :initform 0)))

(defun create-rectangle (x y width height)
  (make-instance 'rectangle :x x :y y :width width :height height))

(defmethod print-object ((rect rectangle) stream)
  (format stream "(~a, ~a, ~a, ~a)" (x rect) (y rect) (width rect) (height rect)))

(defun draw-rectangle ((rect rectangle))
  (let ((x (x rect))
        (y (y rect))
        (w (width rect))
        (h (height rect)))
    (gl:with-primitives :quads
      (gl:color 0.0 0.0 0.0)
      (gl:vertex x y 0.0) 
      (gl:vertex (+ x w) y 0.0)
      (gl:vertex (+ x w) (+ y h) 0.0) 
      (gl:vertex x (+ y h) 0.0))

    (incf x)
    (incf y)
    (decf w)
    (decf h)

    (gl:with-primitives :quads
      (gl:color 0.0 0.0 0.0)
      (gl:vertex x y 0.0) 
      (gl:vertex (+ x w) y 0.0)
      (gl:vertex (+ x w) (+ y h) 0.0) 
      (gl:vertex x (+ y h) 0.0))))
