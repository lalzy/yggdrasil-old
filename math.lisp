(in-package :yggdrasil)

;;percentage
(defun %-from-total (value total)
  "get what percentage an value is from another value"
  (float (* (/ value total) 100)))

(defun %-of-value (percentage value)
  "get the percentage of a value"
  (* (/ percentage 100) value))
