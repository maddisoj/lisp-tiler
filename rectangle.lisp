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

(defmacro create-rectangle (x y width height)
  `(make-instance 'rectangle :x ,x :y ,y :width ,width :height ,height))

(defmethod print-object ((rect rectangle) stream)
  (format stream "(~a, ~a, ~a, ~a)" (x rect) (y rect) (width rect) (height rect)))
