(in-package :yggdrasil)

;; Create collision-check code that will redirect to correct intersection-check

(defmethod mouse-collision-check ((object rectangle) &optional (mouse (vector (sdl:mouse-x) (sdl:mouse-y))))
  (and (pixel-rect-intersection-check (x object) (y object) (w object) (h object) (x mouse) (y mouse))))

(defmethod mouse-collision-check ((object circle) &optional (mouse (vector (sdl:mouse-x) (sdl:mouse-y))))
  (and (pixel-circle-intersection-check (x object) (y object) (r object) (x mouse) (y mouse))))

(defmethod mouse-intersection-check ((object vector) &optional (mouse (vector (sdl:mouse-x) (sdl:mouse-y))))
  (if (string-equal :rectangle (rectangle-or-circle object))
      (and (pixel-rect-intersection-check (aref object 0) (aref object 1) (aref object 2) (aref object 3) (elt mouse 0) (elt mouse 1)))
      (and (pixel-circle-intersection-check (aref object 0) (aref object 1) (aref object 3) (elt mouse 0) (elt mouse 1)))))

(defun pixel-circle-intersection-check (x y r px py)
  "pixel intersection detection between pixel point and a circle"
  (let ((deltax (- x px ))
	(deltay (- y py)))
    (if (<= (+ (* deltax deltax) (* deltay deltay)) (* r r))
	t
	nil)))

(defun pixel-rect-intersection-check (x y w h px py)
  "pixel intersection detection between pixel point and a rect"
  (and (and (<= px (+ x w))
	   (>= px x)
	   (<= py (+ y h))
	   (>= py y))))

(defun rectangle-circle-intersection-check (rect circle)
  "Input: A rect and a cirlce object"
  (let ((deltax (- (x circle) (max (x rect) (min (x circle) (+ (x rect) (w rect))))))
	(deltay (- (y circle) (max (y rect) (min (y circle) (+ (y rect) (h rect)))))))
    (if (< (+ (* deltax deltax) (* deltay deltay)) (* (r circle) (r circle)))
	(list rect circle)
	nil)))

(defun rectangle-intersection-check (rect1 rect2)
  "input: two rect objects"
    (if (and (< (x rect1) (+ (x rect2) (w rect2)))
	     (> (+ (x rect1) (w rect1)) (x rect2))
	     (< (y rect1) (+ (y rect2) (h rect2)))
	     (> (+ (y rect1) (h rect1)) (y rect2)))
	(progn
	  (list rect1 rect2)
	  )
	nil))

(defun circle-intersection-check (circle1 circle2)
  "Input: two circle objects"
  (let* ((distx (- (x circle2) (x circle1)))
	 (disty (- (y circle2) (y circle1)))
	 (radius (+ (r circle1) (r circle2))))
    (if (< (+ (* distx distx) (* disty disty)) (* radius radius))
	t
	nil)))


(defmethod edge-collision-check ((object rectangle) &optional beyond)
  (edge-rect-collision object beyond))

(defmethod edge-collision-check ((object circle) &optional beyond)
  (edge-circle-collision object beyond))

(defun rectangle-or-circle (vector)
  (cond ((= (length vector) 3)
         :circle)
        ((= (length vector) 4)
         :rectangle)))

(defmethod edge-collision-check ((object array) &optional beyond)
  (let ((obj1-type (rectangle-or-circle object)))
    (cond ((equal obj1-type :none) (error "~a is not a rectangle or circle array" object))
	  ((equal obj1-type :rectangle)
	   (edge-rect-collision object beyond))
	  ((equal obj1-type :circle)
	   (edge-circle-collision object beyond)))))

(defun edge-rect-collision (rect beyond &aux (ex 0) (ey 0) (ew *width*) (eh *height*) (col nil))
  "Checks if rectangle collides with the edge of the window.
If beyond is set, will only be considered intersection if object is past window"
  (when beyond
    (setf ex (- ex (w rect))
	  ey (- ey (h rect))
	  ew (+ ew (w rect))
	  eh (+ eh (h rect))))
  
  (when (<= (x rect) ex)
    (push :left col))
  (when (>= (+ (x rect) (w rect)) ew)
    (push :right col))
  (when (<= (y rect) ey)
    (push :top col))
  (when (>= (+ (y rect) (h rect)) eh)
    (push :bottom col))
  col)

(defun edge-circle-collision (circle beyond &aux (ex 0) (ey 0) (ew *width*) (eh *height*) (col nil))
  "Checks if circle collides with the edge of the window"
  (if beyond
      (progn
	(when (<= (x circle) (- ex (* (r circle) 2)))
	  (push :left col))
	(when (>= (- (x circle) (r circle)) ew)
	  (push :right col))
	(when (<= (y circle) (- ey (r circle)))
	  (push :top col))
	(when (>= (- (y circle) (r circle)) eh)
	  (push :bottom col)))

      (progn
	(when (<= (x circle) (+ ex (r circle)))
	  (push :left col))
	(when (>= (+ (x circle) (r circle)) ew)
	  (push :right col))
	(when (<= (y circle) (+ (r circle) ey))
	  (push :top col))
	(when (>= (+ (y circle) (r circle)) eh)
	  (push :bottom col))))
  col)


(defun get-edge-dir (object dir &key (beyond nil))
  "Check if object is coliding with the chosen direction"
  (member dir (edge-collision-check object beyond) :test #'string=))






#|

(defstruct circle
  (x 0)
  (y 0)
  (radius 0))

(defstruct oriented-rectangle
  (x 0)
  (y 0)
  (width 0)
  (height 0)
  (angle 0)) ; Angle in radians

(defun rotate-point (point angle)
  "Rotate a point around the origin by a given angle in radians."
  (let* ((x (car point))
         (y (cadr point))
         (new-x (+ (* x (cos angle)) (* y (- (sin angle))))))
    (list new-x (+ (* x (sin angle)) (* y (cos angle))))))

(defun project-onto-axis (points axis)
  "Project a list of points onto a given axis."
  (let ((projections (mapcar (lambda (point) (dot-product point axis)) points)))
    (list (min projections) (max projections))))

(defun separating-axis-theorem-circle-rectangle (circle rectangle)
  "Check for collision between a circle and an oriented rectangle using the Separating Axis Theorem."
  (labels ((axis-collide-p (axis)
             (let* ((rectangle-points (list
                                        (list (x rectangle) (y rectangle))
                                        (list (+ (x rectangle) (width rectangle)) (y rectangle))
                                        (list (x rectangle) (+ (y rectangle) (height rectangle)))
                                        (list (+ (x rectangle) (width rectangle)) (+ (y rectangle) (height rectangle)))))
                    (rotated-rectangle-points (mapcar (lambda (point) (rotate-point point (angle rectangle))) rectangle-points))
                    (circle-point (list (circle-x circle) (circle-y circle)))
                    (rotated-circle-point (rotate-point circle-point (angle rectangle)))
                    (rectangle-projection (project-onto-axis rotated-rectangle-points axis))
                    (circle-projection (project-onto-axis (list rotated-circle-point) axis)))
               (not (or (< (cadr rectangle-projection) (car circle-projection))
                        (< (cadr circle-projection) (car rectangle-projection)))))))
    (and (axis-collide-p '(1 0)) ; Check overlap on the x-axis
         (axis-collide-p '(0 1)) ; Check overlap on the y-axis
         (axis-collide-p (normalize (list (- (circle-x circle) (x rectangle))
                                          (- (circle-y circle) (y rectangle))))))) ; Check overlap on the axis perpendicular to the rectangle's orientation
In this implementation, I've added a circle struct and an oriented-rectangle struct for representing circles and oriented rectangles, respectively. The rotate-point function rotates a point around the origin, and project-onto-axis projects a list of points onto a given axis. The main function separating-axis-theorem-circle-rectangle then checks for collision between a circle and an oriented rectangle using the SAT. It also checks for overlap along an axis perpendicular to the rectangle's orientation. Note that this code assumes that the normalize and dot-product functions are defined elsewhere.

|#
