(in-package :yggdrasil)

(defun rectangle-or-circle (object)
  (cond ((= (length object) 4)
	 :rectangle)
	((= (length object) 3)
	 :circle)
	(t :none)))

(defun intersection-check-helper-rectangle-and-array (rect other)
  (cond ((equal (rectangle-or-circle other) :rectangle)
	 (rectangle-intersection-check rect other))
	((equal (rectangle-or-circle other) :circle)
	 (rectangle-circle-intersection-check rect other))
	(t (error "~a is neither a rectangle nor a cricle!" other))))



;; Rewrite to better handle other shapes, and do proper collision
(defmethod collision-check ((object1 rectangle) (object2 array) &optional old-obj)
  (intersection-check-helper-rectangle-and-array object1 object2))

(defmethod collision-check ((object1 array) (object2 rectangle) &optional old-obj)
  (intersection-check-helper-rectangle-and-array object2 object1))

(defmethod collision-check ((object1 rectangle) (object2 rectangle) &optional old-obj)
  (if old-obj
      (rectangle-intersection-check (range-check-rectangle object1 old-obj) object2)
      (rectangle-intersection-check object1 object2)))

(defmethod collision-check ((object1 array) (object2 array) &optional old-obj)
  (let ((obj1-type (rectangle-or-circle object1))
	(obj2-type (rectangle-or-circle object2)))
    (cond ((equal obj1-type :none) (error "~a is not a rectangle or circle array" object1))
	  ((equal obj2-type :none) (error "~a is not a rectangle or circle array" object2))
	  ((and (equal obj1-type :rectangle) (equal obj1-type obj2-type))
	   (rectangle-intersection-check object1 object2))
	  ((and (equal obj1-type :circle) (equal obj1-type obj2-type))
	   (circle-intersection-check object1 object2))
	  (t (rectangle-circle-intersection-check object1 object2)))))

(defmethod collision-check ((object1 rectangle) (object2 circle) &optional old-obj)
  (rectangle-circle-intersection-check object1 object2))

(defmethod collision-check ((object1 circle) (object2 rectangle) &optional old-ojb)
  (rectangle-circle-intersection-check object2 object1))
  
(defmethod collision-check ((object1 circle) (object2 circle) &optional old-obj);((object1 hitbox-circle) (object2 hitbox-circle))
  (if (typep old-obj 'rect)
      (if (object-same-pos-p object1 old-obj) 
	  (circle-intersection-check object1 object2)
	  (rect-circle-intersection-check (get-range old-obj object1) object2))
      (circle-intersection-check object1 object2)))



(defun rectangle-direction-check (rect1 rect2)
  (let ((wy (* (+ (w rect1) (w rect2)) (- (y rect1) (y rect2))))
	(hx (* (+ (h rect1) (h rect2)) (- (x rect1) (x rect2)))))
    (if (> wy hx)
	(if (> wy (- hx))
	    :top
	    :left)
	(if (> wy (- hx))
	    :right
	    :bottom))))


#|
(defun rectangle-circle-intersection-check (rect circle)
  "Input: A rect and a cirlce object"
  (let ((deltax (- (x circle) (max (x rect) (min (x circle) (+ (x rect) (w rect))))))
	(deltay (- (y circle) (max (y rect) (min (y circle) (+ (y rect) (h rect)))))))
    (if (< (+ (* deltax deltax) (* deltay deltay)) (* (r circle) (r circle)))
	(list rect circle)
	nil)))
|#

(defun get-range-positions (object old-obj &aux sx ex sy ey)
  "Get the starting and ending position between the old and new object"
  (if (< (x old-obj) (x object))
      (setf sx (x old-obj) ex (x object))
      (setf ex (x old-obj) sx (x object)))
  
  (if (< (y old-obj) (y object))
      (setf sy (y old-obj) ey (y object))
      (setf ey (y old-obj) sy (y object)))
  
  ;; reduce the end position by the starting position to make the range between start\end.
  (setf ex (- ex sx)
	ey (- ey sy))
  (vector sx sy ex ey))


(defun range-check-circle (object old-obj)
  (let ((pos (get-range-positions object old-obj)))
    (vector (x pos) (y pos) (+ (* (r object) 2) (w pos)) (+ (* (r object) 2) (h pos)))))

(defun range-check-rectangle (object old-obj)
  (let ((pos (get-range-positions object old-obj)))
    (vector (x pos) (y pos) (+ (w object) (w pos)) (+ (h object) (h pos)))))

(defun object-same-pos-p (object1 object2)
  "Checks if object1 and object2 are at the same position, a.k.a, not moved"
  (if (and (= (x object1) (x object2)) (= (y object1) (y object2)))
      t
      nil))


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
