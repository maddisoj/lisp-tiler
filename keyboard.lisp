(defvar *key-bindings* '())

(defclass key-binding ()
  ((key
    :initarg :key
    :initform 0)
   (callback
    :initarg :callback
    :initform nil)))

(defmacro create-key-binding (key callback)
  `(make-instance 'key-binding :key ,key :callback ,callback))

(defun add-key-binding ((kb key-binding))
  (push kb *key-bindings*))

(defun on-key-press (key x y)
  (loop for kb in *key-bindings* do
    (when (eq key (slot-value kb 'key))
      (funcall (slot-value kb 'callback) key x y))))
