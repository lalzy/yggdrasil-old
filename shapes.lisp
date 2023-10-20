;;;; Handles representation of shapes, classes, getters and drawing.

(in-package :yggdrasil)

(defclass pos ()
  ((x :initarg :x)
   (y :initarg :Y)))

(defclass circle (pos)
  ((radius :initarg :r)))

(defclass rectangle (pos)
  ((width :initarg :w)
   (height :initarg :h)))

(defmethod x ((object array))
  (aref object 0))

(defmethod x ((object pos))
  (slot-value object 'x))

(defmethod (setf x) (value (object array))
  (setf (aref object 0) value))

(defmethod (setf x) (value (object pos))
  (setf (slot-value object 'x) value))

(defmethod y ((object array))
  (aref object 1))

(defmethod y ((object pos))
  (slot-value object 'y))

(defmethod (setf y) (value (object array))
  (setf (aref object 1) value))

(defmethod (setf y) (value (object pos))
  (setf (slot-value object 'y) value))

(defmethod w ((object array))
  (aref object 2))

(defmethod w ((object rectangle))
  (slot-value object 'width))

(defmethod h ((object array))
  (aref object 3))

(defmethod h ((object rectangle))
  (slot-value object 'height))


(defmethod r ((object array))
  (aref object 2))

(defmethod r ((object circle))
  (slot-value object 'radius))


;;; Image getters, to be rewritten for OpenGL

(defmethod w ((object sdl:surface))
  (sdl:width object))

(defmethod h ((object sdl:surface))
  (sdl:height object))



;;; Drawing,  this is to ensure when I move to openGL I won't have to change game code later.
;;;   Any game made with the engine should still be workable after that move('if' I ever get around to that).


(defun adjust-center-position (pos old-length new-length)
  (- pos (round (- new-length old-length) 2)))


(defun draw-rectangle-* (x y w h &key (color (get-color green)) (filled nil) (angle 0))
  (unless (edge-collision-check (vector x y w h) t)
    (let ((surface (sdl:create-surface w h :pixel-alpha t)))
      
      (if filled
	  (sdl:draw-box-* 0 0 w h :color color :surface surface)
	  (sdl:draw-rectangle-* 0 0 w h :color color :surface surface))

      (when (> angle 0)
	(setf surface (sdl-gfx:rotate-surface angle :surface surface :smooth t))
	(when (or (/= angle 0) (/= angle 90) (/= angle 180) (/= angle 360))
	  (setf x (adjust-center-position x w (sdl:width surface))
		y (adjust-center-position y h (sdl:height surface)))))
      
      (sdl:draw-surface-at-* surface x y))))


(defun draw-rectangle (rectangle &key (color (get-color green)) (filled nil) (angle 0))
  (if (sdl:video-init-p)
      (draw-rectangle-* (x rectangle) (y rectangle) (w rectangle) (h rectangle) :color color :filled filled :angle angle)
      (error "SDL has not been initialized")))



(defun draw-circle-* (x y r &key (color (get-color green)) (filled nil))
    (unless (edge-collision-check (vector x y r) t)
      (let* ((size (+ (* r 2) 1))
	     (color-key (if (equalp color (get-color black)) (get-color white) (get-color black)))
	     (surface (sdl:create-surface size size :color-key color-key)))
					;(surface (sdl:create-surface size size :pixel-alpha t)))
	(sdl:draw-box-* 0 0 size size :color color-key :surface surface)
	(if filled
	    (sdl:draw-filled-circle-* r r r
				      :color color :surface surface)
	    (sdl:draw-circle-* r r r :color color :surface surface))
	(sdl:draw-surface-at-* surface (- x r) (- y r)))))

(defun draw-circle (circle &key (color (get-color green)) (filled nil))
  (if (sdl:video-init-p)
      (draw-circle-* (x circle) (y circle) (r circle))
      (error "SDL has not been initialized")))
