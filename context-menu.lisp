(in-package :yggdrasil)

;; Created nested Context System

(defparameter *context-menu-x* nil)
(defparameter *context-menu-y* nil)
(defparameter *context-menu-text-width* nil)
(defparameter *context-menu-text-height* nil)
(defparameter *context-menu-width* nil)
(defparameter *context-menu-height* nil)
(defparameter *context-menu-items* nil)
(defparameter *context-menu-font* nil)


(defun menu-change-font (font)
  (setf *context-menu-font* font))


(defun menu-create (x y items &optional (font *default-font*))
  (setf *context-menu-x* x
	*context-menu-y* y
	*context-menu-items* items
	*context-menu-font* font)
  
  (multiple-value-bind (w h) (text-size (elt items 0) font)
    (setf *context-menu-text-width* w
	  *context-menu-text-height* h))
  
  (multiple-value-bind (w h) (get-total-text-size items font)
    (setf *context-menu-width* w
	  *context-menu-height* h)))

(defun menu-select-item ()
  (let ((adjusted-y *context-menu-y*))
    (dolist (item *context-menu-items*)
      (when (mouse-collision-check (make-instance 'rectangle :x *context-menu-x* :y adjusted-y :w *context-menu-width* :h *context-menu-text-height*))
	(return-from menu-select-item item))
      (incf adjusted-y *context-menu-text-height*))))

(defun menu-draw (&optional (color (get-color blue)))
  (draw-rectangle (vector (- *context-menu-x* 5)  *context-menu-y*  (+ *context-menu-width* 5) (+ *context-menu-height* 5)) :filled t :color (get-color gray))
  (let ((adjusted-y *context-menu-y*))
    (dolist (item *context-menu-items*)
      (if (mouse-collision-check (make-instance 'rectangle :x *context-menu-x* :y adjusted-y :w *context-menu-width* :h *context-menu-text-height*))
	  (yg:draw-string *context-menu-x* adjusted-y item :draw-type :shaded :bg-color (get-color dark-gray))
	  (yg:draw-string *context-menu-x* adjusted-y item))
      (incf adjusted-y (1+ *context-menu-text-height*)))))

