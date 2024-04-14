;;;; shapes.lisp
;;;; Handles representation of shapes, classes, getters and drawing.
(in-package :yggdrasil)
;;; Clean up the w\h\r methods so that they don't overwrite the accessor, should only compliment them.

(defclass pos () ;; Exists to abstract away SDL for when\if i move over to openGL
  ((x :initarg :x :accessor x)
   (y :initarg :Y :accessor y))
  (:documentation "class for x\y positioning of objects used by the engine"))

(defclass circle (pos) ;; Exists to abstract away SDL for when\if i move over to openGL
  ((radius :initarg :r :accessor r))
  (:documentation "primitive circle-shape used by the engine.")) 

(defclass rectangle (pos) ;; Exists to abstract away SDL for when\if i move over to openGL
  ((width :initarg :w :accessor w)
   (height :initarg :h :accessor h))
  (:documentation "primitive rectangle-shape used by the engine."))

(defgeneric x (object)
  (:documentation "getter\setter for x-position, either as vector or as pos object"))

(defmethod x ((object array))
  (aref object 0))

(defmethod (setf x) (value (object array))
  (setf (aref object 0) value))

(defgeneric y (object)
  (:documentation "getter\setter for y-position, either as vector or as pos object"))

(defmethod y ((object array))
  (aref object 1))

(defmethod (setf y) (value (object array))
  (setf (aref object 1) value))

(defgeneric w (object)
  (:documentation "getter for rectangle width, either as vector or as rectangle object"))

(defmethod w ((object array))
  (aref object 2))

(defmethod w ((object rectangle))
  (slot-value object 'width))

(defgeneric h (object)
  (:documentation "getter for rectangle height, either as vector or as rectangle object"))

(defmethod h ((object array))
  (aref object 3))

(defmethod h ((object rectangle))
  (slot-value object 'height))

(defgeneric r (object)
  (:documentation "getter for circle radius, either as vector or as circle object"))

(defmethod r ((object array))
  (aref object 2))

(defmethod r ((object circle))
  (slot-value object 'radius))


;;; Image getters, to be rewritten for OpenGL

(defmethod w ((object sdl:surface))
  (sdl:width object))

(defmethod h ((object sdl:surface))
  (sdl:height object))

(locally (declare (optimize (speed 3)))
  
  (defun create-circle-vector-% (&key (x 0) (y 0) (r 1))
    "Defines an cirlce of radius based on % of total screen size"
    (let* ((value (if (> *width* *height*) *width* *height*))
           (p (%-of-value r value))) ; Doesn't like call being done with the rounding
      (declare (type fixnum *width* *height* r p))
      (vector x y (round p))))
  
(defun create-rectangle-vector-% (&key (x 0) (y 0) (w 1) (h 1))
  "Defines an rectangle of size based on % of total screen size"
  ;; See function above
  (let ((w-sum (%-of-value w *width*))
        (h-sum (%-of-value h *height*)))
    (declare (type fixnum w h *width* *height*
                   w-sum h-sum))
    
    (vector x y (round w-sum) (round h-sum))))
 ; (vector x y (round (%-of-value w *width*)) (round (%-of-value h *height*)))))

(defun create-rectangle-object-* (x y w h))
(defun create-rectangle-object ())
(defun create-rectangle-object-% ())

;;; Drawing,  this is to ensure when I move to openGL I won't have to change game code later.
;;;   Any game made with the engine should still be workable after that move('if' I ever get around to that).

(defun adjust-center-position (pos old-length new-length)
  "returns the center of object (mostly for rectangles that defaults to upper-left)"
   (- pos (round (- new-length old-length) 2)))


(defun draw-line-* (x-start y-start x-end y-end &key (color (get-color white)))
  "helper to draw-line, takes absolute values instead of vector\object"
  (sdl:draw-line-* x-start y-start x-end y-end :color color))

(defun draw-line (point1 point2 &key (color (get-color white)))
  "draws a line from point1 to point2. Points are either vectors, or pos-objects.
key arguments:
color - takes an (SDL) color (default white)"
  (draw-line-* (x point1) (y point1) (x point2) (y point2) :color color))

;; Need to declare types in the functions first
(locally (declare (optimize (speed 3)))

  (defun draw-rectangle-* (x y w h &key (color (get-color green)) (filled nil) (angle 0) from-center)
    "Helper to draw-rectangle, takes absolute values instead of vector\object"
    (declare (type fixnum angle x y w h))
    
    (unless (edge-collision-check (vector x y w h) t)
      (let ((surface (sdl:create-surface w h :pixel-alpha t)))
        (unwind-protect 
             (progn 
               (when from-center
                 ;; can't optimize rounding for fixnum, so need to first divide, then round the returned float
                 (setf x (- x (round (/ w 2)))
                       y (- y (round (/ h 2)))))
               
               (if filled
	           (sdl:draw-box-* 0 0 w h :color color :surface surface)
	           (sdl:draw-rectangle-* 0 0 w h :color color :surface surface))

               ;; prevents miss-alignment with 'straight' angles.
               (when (and (> angle 0) (/= angle 90) (/= angle 180) (/ angle 360))
	         (setf surface (sdl-gfx:rotate-surface angle :surface surface :smooth t :free t))
                 
                 ;; rotate on current-drawn position, rather than placement position
                 ;;   - Unfortunately due to rounding causes minor missalignment
	         (setf x (adjust-center-position x w (sdl:width surface))
	               y (adjust-center-position y h (sdl:height surface))))
               
               (sdl:draw-surface-at-* surface x y)))
        (sdl:free surface))))

  (defun draw-rectangle (rectangle &key (color (get-color green)) (filled nil) (angle 0) from-center)
    "draws an rectangle on-screen. Takes an rectangle as either vector or object.
key arguments:
color - takes an (SDL) color (default green)
filled - if set will fill entire rectangle with the color
angle - Angle to draw\rotate the rectangle (default 0)"
    (if (sdl:video-init-p)
        (draw-rectangle-* (x rectangle) (y rectangle) (w rectangle) (h rectangle) :color color :filled filled
                                                                                  :angle angle :from-center from-center)
        (error "SDL has not been initialized")))
  
  (defun draw-circle-* (x y r &key (color (get-color green)) (filled nil))
    "helper for draw-cricle, takes absolute values instead of vector\object"
    (declare (type fixnum x y r))
    (unless (edge-collision-check (vector x y r) t)
      (let* ((size (+ (* r 2) 1))
	     (color-key (if (equalp color (get-color black)) (get-color white) (get-color black)))
	     (surface (sdl:create-surface size size :color-key color-key)))
					;(surface (sdl:create-surface size size :pixel-alpha t)))
        
        ;; optimization didn't like us doing the subtraction in the call for draw-surface-at-* so we do it here instead.
        (setf x (- x r)
              y (- y r))
        
        (unwind-protect
             (progn
	       (sdl:draw-box-* 0 0 size size :color color-key :surface surface)
	       (if filled
	           (sdl:draw-filled-circle-* r r r
				             :color color :surface surface)
	           (sdl:draw-circle-* r r r :color color :surface surface))
	       (sdl:draw-surface-at-* surface x y))
          (sdl:free surface)))))
  
  (defun draw-circle (circle &key (color (get-color green)) (filled nil))
    "draws a circle on-screen. Takes an circle as either vector or object.
key arguments:
color - takes an (SDL) color (default green)
filled - if set will fill entire rectangle with the color"
    (if (sdl:video-init-p)
        (draw-circle-* (x circle) (y circle) (r circle) :color color :filled filled)
        (error "SDL has not been initialized"))))
