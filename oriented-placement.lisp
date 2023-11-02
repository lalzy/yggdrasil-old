(in-package :yggdrasil)

(defun get-center-rectangle-within-window (rect)
  "return a rectangle placed at the center of the game-window"
  (values (- (round *width* 2) (round (w rect) 2))
          (- (round *height* 2) (round (h rect) 2))
          (w rect)
          (h rect)))

(defgeneric center-within-window (object)
  (:documentation "places an object in the center of the game-window"))

(defmethod center-within-window ((object rectangle))
  (multiple-value-bind (x y w h) (get-center-rectangle-within-window object)
    (setf (x object) x
          (y object) y
          (w object) w
          (h object) h))
  object)

(defmethod center-within-window ((object vector))
  (multiple-value-bind (x y w h) (get-center-rectangle-within-window object)
    (setf (x object) x
          (y object) y
          (w object) w
          (h object) h))
  object)


(defun place-rectangle-inside-window (rectangle orientations offset-x offset-y offset-as-percentage)
  "helper to place-within-window"
  ;; Start by centering rectangle
  (multiple-value-bind (x y w h) (get-center-rectangle-within-window rectangle)
    ;; then orient rectangle by orientation, if any
    (unless (listp orientations)
      (setf orientations (cons orientations nil)))
    (dolist (orientation orientations)
      (case orientation
        (:left (setf x 1))
        (:right (setf x (1- (- *width* w))))
        (:top (setf y 0))
        (:bottom (setf y (1- (- *height* h))))))
    
    (if offset-as-percentage
        (setf (x rectangle) (+ x (round (yg::%-from-total offset-x *width*)))
              (y rectangle) (+ y (round (yg::%-from-total offset-x *width*)))))
        
        (setf (x rectangle) (+ x offset-x)
              (y rectangle) (+ y offset-y)))
  rectangle)

(defgeneric place-within-window (object &key orientation offset-x offset-y offset-as-percentage)
  (:documentation "places an object relative within the window
defaults to the center of the window. Orientation takes either a single, or a list of orientations (left\right, top\bottom)

offset-x\y takes an integer value
if offset-as-percentage is set, offset-x\y becomes percentages relative to the orientation (not center)"))

(defmethod place-within-window ((object rectangle) &key orientation (offset-x 0) (offset-y 0) offset-as-percentage)
  (place-rectangle-inside-window object orientation offset-x offset-y  offset-as-percentage))

(defmethod place-within-window ((object vector) &key orientation (offset-x 0) (offset-y 0) offset-as-percentage)
  (place-rectangle-inside-window object orientation offset-x offset-y  offset-as-percentage))




#|

;; Rewrite for outside and inside
(defun calculate-percentage (area percentage)
  (round (* area (/ (parse-integer percentage) 100))))

;; Check if offset is %
(defun get-offset-value (size offset)
  (cond ((numberp offset)
	 offset)
	((symbolp offset)
	 (let ((value (string offset)))
	   (case (aref value (1- (length value)))
	     (#\% (calculate-percentage size (subseq value 0 (1- (length value))))))))))

(defun filter-offset (area offsets)
  (let ((offset-x 0) (offset-y 0))
    (loop for i below (length offsets) do
      (cond ((or  (string-equal (elt offsets i) :hor)  (string-equal (elt offsets i) :horizontal) (string-equal (elt offsets i) :hori))
	     (incf i)
	     (setf offset-x (get-offset-value (yg:w area) (elt offsets i))))
	    ((or  (string-equal (elt offsets i) :vert)(string-equal (elt offsets i) :vertical))
	     (incf i)
	     (setf offset-y (get-offset-value (yg:h area) (elt offsets i))))))
    (vector offset-x offset-y)))


(defun place (area object &key (offset nil)  orientations (inside t))
  (let ((offsets (filter-offset area offset)))
    (if inside
	(place-inside area object offsets orientations)
	(place-outside area object offsets orientations))))


(defmethod center-within-rectangle ((object rectangle) (area rectangle))
  (let ((x (if (= (yg:w area) (yg:w object))
	       (yg:x area)
	       (if (> (yg:w area) (yg:w object))
		   (+ (round (+ (yg:x area) (yg:w area)) 2)
		      (round (yg:w object) 2))
		   (- (yg:x area) (round (- (yg:w object) (yg:w area)) 2)))))
	(y (if (= (yg:h area) (yg:h object))
	       (yg:y area)
	       (if (> (yg:h area) (yg:h object))
		   (+ (round (+ (yg:y area) (yg:h area)) 2)
		      (round (yg:h object) 2))
		   (- (yg:y area) (round (- (yg:h object) (yg:h area)) 2))))))

    (values x y (w rectangle) (h rectangle))))

(defmethod place-outside ((area rectangle) (object rectangle) offsets orientations)
  (multiple-value-bind (x y) (center area object )
    (dolist (orientation orientations)
      (case orientation
	(:left (setf x (1- (- (yg:x area) (yg:w object)))))
	(:right (setf x (1+ (+ (yg:x area) (yg:w area)))))
	(:top (setf y (1- (- (yg:y area) (yg:h object)))))
	(:bottom (setf y (1+ (+ (yg:y area) (yg:h area)))))))
    (setf (yg:x object) (+ x (x offsets))
	  (yg:y object) (+ y (y offsets)))))

                  ;rectangle ; rectangle
(defun place-inside (area  object offsets orientations)
  (multiple-value-bind (x y) (center area object)
    (dolist (orientation orientations)
      (case orientation
	(:left (setf x (1+ (yg:x area))))
	(:right (setf x (1- (- (+ (yg:x area) (yg:w area)) (yg:w object)))))
	(:top (setf y (1+ (yg:y area))))
	(:bottom (setf y (1- (- (+ (yg:y area) (yg:h area)) (yg:h object)))))))

    ;; Set the position
    (setf (yg:x object) (+ x (x offsets))
	  (yg:y object) (+ y (y offsets))
	  )))
|#
