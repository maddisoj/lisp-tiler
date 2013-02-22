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
             :accessor max-area)
   (areas :initarg :areas
          :initform '()
          :accessor areas)))

(defun add-to-row (area (r row))
  (push area (areas r))
  (if (eql (min-area r) nil)
    (setf (min-area r) area)
    (setf (min-area r) (min (min-area r) area)))
  (if (eql (max-area r) nil)
    (setf (max-area r) area)
    (setf (max-area r) (max (max-area r) area)))
  (incf (area r) area))

(defun recalc-min-max-area ((r row)))

(defun remove-last ((r row))
  (let ((area (pop (areas r))))
    (when (or (eql (min-area r) area)
              (eql (max-area r) area)
      (recalc-min-max-area r)))
    (decf (area r) area)))

(defun worst ((r row) smallest-side)
  (let ((row-area (area r))
        (smallest-sq (expt smallest-side 2)))
    (if (eq row-area 0)
      (return-from worst)
      (progn
        (setf row-area (expt row-area 2))
        (max (/ (* (max-area r) smallest-sq) row-area)
             (/ row-area (* (min-area r) smallest-sq)))))))

(defun scale (areas scale)
  (do ((tail areas (cdr tail)))
      ((endp tail))
      (setf (car tail)
            (* (car tail) scale))))

(defun sum (L)
  (reduce '+ L))

(defun layout ((r row) (rect rectangle))
  (let ((draw-vertically (> (width rect) (height rect)))
        (len (/ (area r) (min (width rect) (height rect))))
        (x (x rect))
        (y (y rect))
        (rects '())
        (current-rect (make-instance 'rectangle)))

    (if draw-vertically
      (progn
        (when (> len (width rect))
          (setf len (width rect)))
        (decf (width rect) len))
      (progn  
        (when (> len (height rect))
          (setf len (height rect)))
        (decf (height rect) len)))

      (dolist (area (areas r))
        (setf (x current-rect) x)
        (setf (y current-rect) y)
        (setf (width current-rect) (if draw-vertically len (/ area len)))
        (setf (height current-rect) (if draw-vertically (/ area len) len))
        (if draw-vertically
          (incf y (height current-rect))
          (incf x (width current-rect)))
        (push current-rect rects)
        (setf current-rect (make-instance 'rectangle)))
    (return-from layout rects)))

(defun squarify (areas (rect rectangle))
  (let ((total-area (sum areas))
        (scale-factor (* (width rect) (height rect)))
        (smallest-side (min (width rect) (height rect)))
        (r (make-instance 'row))
        (best-aspect nil)
        (current-aspect nil)
        (result-rects '()))

    (setf scale-factor (/ scale-factor total-area))
    (scale areas scale-factor)

    (dolist (area areas)
      (add-to-row area r)
      
      (when (eql best-aspect nil)
        (setf best-aspect (worst r smallest-side))
        (go end))

      (setf current-aspect (worst r smallest-side))
      (when (<= current-aspect best-aspect)
        (setf best-aspect current-aspect)
        (go end))
      
      (remove-last r)
      (setf result-rects (nconc result-rects (layout r rect)))
      (setf smallest-side (min (width rect) (height rect)))
      (setf r (make-instance 'row))
      (add-to-row area r)
      (setf best-aspect (worst r smallest-side))
      
      finally (setf result-rects (nconc result-rects (layout r rect)))
              (return-from squarify result-rects)
      end)))

(let ((areas (list 1 2 3)))
  (scale areas 2)
  (dolist (rect (squarify areas (create-rectangle 0 0 100 100)))
    (print-object rect t)))
