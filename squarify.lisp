 (require "rectangle.lisp")

(defclass row ()
  ((area :initarg :area
         :initform 0
         :accessor area)
   (min-area :initarg :min-area
             :initform nil
             :accessor min-area)
   (max-area :initarg :max-area
             :initform nil
             :accessor max-area)))

(defun worst ((r row) smallest-side)
  (let ((row-area (area r))
        (smallest-sq (expt smallest-side 2)))
    (if (eq row-area 0)
      (return-from worst)
      (progn
        (setf row-area (expt row-area 2))
        (max (/ (* (max-area r) smallest-sq) row-area)
             (/ row-area (* (min-area r) smallest-sq)))))))

(defmacro scale (areas scale)
  `(dotimes (n (list-length ,areas))
     (setf (nth n ,areas) (* (nth n ,areas) ,scale)))) 

(defun sum (L)
  (apply '+ L))

(defun squarify (areas (rect rectangle))
  (let ((total-area (sum areas))
        (scale (/ (* (width rect) (height rect)) total-area)))
    (format t "total-area: ~a~%scale: ~a~%" total-area scale)))

(let ((areas (list 1 2 3)))
  (scale areas 2)
  (print areas)
  (squarify areas (create-rectangle 0 0 100 100)))
