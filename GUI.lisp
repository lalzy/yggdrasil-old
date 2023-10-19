;;;; GUI.lisp

(in-package #:yggdrasil)


(defparameter *GUI* nil)

(defclass button (rectangle)
  ((function :initarg :function)
   (text :initarg :text)))


(defun add-button (function x y w h &optional frame display-text)
  (push (make-instance 'button :text display-text :function function :x x :y y :w w :h h) *GUI*))


(defun draw-GUI ()
  (dolist (element *GUI*)
    (if (equal 'button (type-of  element))
	(draw-rectangle-* (x element) (y element) (w element) (h element) :color (get-color gray) :filled nil))))
 
#|
Class:
  Frame:
    List of objects

  Button:
    Function to trigger
    Display Text

  Text:
    text


List of GUI elements:
  Loop through list:
    If is frame, check it's members and draw based on memers.
    if not frame,  draw object type.

|#
