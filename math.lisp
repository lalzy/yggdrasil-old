(in-package :yggdrasil)

;;percentage
(defun %-from-total (value total)
  "get what percentage an value is from another value"
  (float (* (/ value total) 100)))

(defun %-of-value (percentage value)
  "get the percentage of a value"
  (* (/ percentage 100) value))


(defun vector-add (vector1 vector2)
  (vector (+ (x vector1) (x vector2)) (+ (y vector1) (y vector2))))

(defun scalar-mult (vector Scalar)
  (vector (* (x vector) scalar) (* (y vector) scalar)))

(defun component-product (vector1 vector2)
  (vector (* (x vector1) (x vector2)) (* (y vector1) (y vector2))))

(defun dot (vector1 vector2)
  (+ (* (x vector1) (x vector2)) (* (y vector1) (y vector2))))

;;Implement Cross product?

(defun cross-product-2D (vector1 vector2)
  (- (* (x vector1) (y vector2)) (* (y vector1) (x vector2))))

(defun vector-length (vector)
  (sqrt (+ (expt (x vector) 2) (expt (y vector) 2))))

(defun unit-vector (vector))
