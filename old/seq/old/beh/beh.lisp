(defclass signal ()
  ((env :initarg :env))
  (:documentation "base signal class"))

(defgeneric sig-init (sig)
  "initialize sig XXX there must be a default CLOS way of doing this")


(defun make-signal (class env)
  (let ((o (make-instance class env)))
    (sig-init o)
    o))

(defmethod sig-setenv ((sig signal) k v)
  (setf (gethash k (slot-value sig 'env)) v))
(defmethod sig-getenv ((sig signal) k)
  (gethash k (slot-value sig 'env)))

(defgeneric sig-read (sig n)
  "calculate and return 'n' elements")
(defgeneric sig-get-block (sig)
  "internal method to get next block")

(defclass sine (signal)
  )

(defun make-sine 

(defmethod sig-get-block ((sig sine))
  (format nil "sine-wave-at-~F-hz"
