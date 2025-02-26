(in-package :yggdrasil)

;;percentage
(defun %-from-total (value total)
  "get what percentage an value is from another value"
  (float (* (/ value total) 100)))

(defun %-of-value (percentage value)
  "get the percentage of a value"
  (* (/ percentage 100) value))


(defgeneric vector+ (vector1 vector2))
(defmethod vector+ ((vector1 array) (vector2 array))
  (vector (+ (aref vector1 0) (aref vector2 0))
	  (+ (aref vector1 1) (aref vector2 1))))

(defgeneric vector- (vector1 vector2))
(defmethod vector- ((vector1 array) (vector2 array))
  (vector (- (aref vector1 0) (aref vector2 0))
	  (- (aref vector1 1) (aref vector2 1))))

(defgeneric vector* (vector1 vector2))
(defmethod vector* ((vector1 array) (vector2 array))
  (vector (* (aref vector1 0) (aref vector2 0))
	  (* (aref vector1 1) (aref vector2 1))))

(defgeneric vector/ (vector1 vector2))
(defmethod vector/ ((vector1 array) (vector2 array))
  (vector (/ (aref vector1 0) (aref vector2 0))
	  (/ (aref vector1 1) (aref vector2 1))))

(defgeneric vector= (vector1 vector2))
(defmethod vector= ((vector1 array) (vector2 array))
  (and (= (aref vector1 0) (aref vector2 0))
       (= (aref vector1 1) (aref vector2 1))))

(defgeneric vector/= (vector1 vector2))
(defmethod vector/= ((vector1 array) (vector2 array))
  (or (/= (aref vector1 0) (aref vector2 0))
      (/= (aref vector1 1) (aref vector2 1))))

(defgeneric scalar-mult (vector Scalar))
(defmethod scalar-mult ((vector array) Scalar)
  (map 'vector (lambda (x) (* x scalar)) vector))

(defgeneric dot (vector1 vector2))
(defmethod dot ((vector1 array) (vector2 array))
  (+ (* (aref vector1 0) (aref vector2 0))
     (* (aref vector1 1) (aref vector2 1))))

(defgeneric Magnitude (vector))
(defmethod Magnitude ((vector array))
  (sqrt (dot vector vector)))

(defgeneric magnitude-squared (vector))
(defmethod magnitude-squared ((vector array))
  (dot vector vector))

(defgeneric distance (point1 point2))
(defmethod distance ((point1 array) (point2 array))
  (magnitude (vector- point1 point2)))

;;; Note to future me, we do not implement normalized, because normalize and normalized is the same.
;;; In the cookbook, normalize modify the vector, we don't want to modify the vector and always return.
(defgeneric normalize (vector))
(defmethod normalize ((vector array))
  (* vector (/ 1.0 magnitude (vector))))

(defgeneric angle (vector1 vector2))
(defmethod angle ((vector1 array) (vector2 array))
  (acos (/ (dot vector1 vector2)
	   (sqrt (* (magnitude-squared vector1)
		    (magnitude-squared vector2))))))


(defgeneric project (length direction))
(defmethod project ((length array) (direction array))
  (scalar-mult direction
	       (/ (dot length direction)
		  (magnitude-squared direction))))

(defgeneric perpendicular (length direction))
(defmethod perpendicular ((length array) (direction array))
  (vector- length (project length direction)))
