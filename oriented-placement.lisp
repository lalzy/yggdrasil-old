(in-package :yggdrasil)

;;; Refactor place-within stuff to not use offset-as-percentage, but instead
;;; check if passed value is a symbol, or a number.
;;; If it's a number, handle as absolute, if it's a symbol
;;; use is-valid-symbol with % as passed value


(defun get-center-rectangle-within-window (rect)
  "return a rectangle placed at the center of the game-window"
  (values (- (round *width* 2) (round (w rect) 2))
          (- (round *height* 2) (round (h rect) 2))
          (w rect)
          (h rect)))

(defun get-center-circle-within-window (circle)
  (values (- )))

(defgeneric center-within-window (object)
  (:documentation "places an object in the center of the game-window"))

(defmethod center-within-window ((object rectangle))
  (multiple-value-bind (x y w h) (get-center-rectangle-within-window object)
    (setf (x object) x
          (y object) y))
  object)

(defmethod center-within-window ((object vector))
  (multiple-value-bind (x y w h) (get-center-rectangle-within-window object)
    (setf (x object) x
          (y object) y))
  object)

(defun offset-percentage-math (offset positional screen-dimention)
  "Helper for filter-offset-percentage with symbol"
  (round (+ positional (%-of-value offset screen-dimention))))

(defgeneric filter-offset-rectangle-percentage (offset positional screen-dimention)
  (:documentation "Helper for place-rectangle-inside-window, returns the correct calulated offset-position based on if it's an absolute pixle-value, or a percentage value."))

(defmethod filter-offset-rectangle-percentage ((offset integer) positional screen-dimention)
  (+ positional offset))

(defmethod filter-offset-rectangle-percentage ((offset symbol) positional screen-dimention)
  (let* ((string (string offset))
         (percentage-pos (position #\% string))
         (value (handler-case
                    (parse-integer (subseq string 0 percentage-pos) :junk-allowed nil)
                  (error (c)
                    (format nil "~a is not of a valid value. Only accepts either integer value, or '%' value (example: 5  or 5%)" offset)))))
    (offset-percentage-math value positional screen-dimention)))


#|  
(if (is-offset-percentage)
(setf (x rectangle) (round (+ x (yg:%-of-value offset-x *width*)))
(y rectangle) (round (+ y (yg:%-of-value offset-y *height*))))

(setf (x rectangle) (+ x offset-x)
(y rectangle) (+ y offset-y))))
rectangle)
|#

(defun place-rectangle-inside-window (rectangle orientations offset-x offset-y)
  "helper to place-within-window for rectangles"
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

    (setf (x rectangle) (filter-offset-rectangle-percentage offset-x x *width*)
          (y rectangle) (filter-offset-rectangle-percentage offset-y y *height*)))
  rectangle)



(defun place-circle-inside-window (circle orientations offset-x offset-y)
  "helper to place-within-window for circles"
  (let ((x (round *width* 2))
        (y (round *height* 2)))
    (setf (x circle) x
          (y circle) y)))

(defgeneric place-within-window (object orientation offset-x offset-y)
  (:documentation "Places {OBJECT} within the window. Defaults to centering the object within the window. {ORIENTATION} can take either a single orientation or a list of orientations, as keyed symbols (:left/:right, :top/:bottom)."))

(defmethod place-within-window ((object rectangle) orientation offset-x offset-y)
  (place-rectangle-inside-window object orientation offset-x offset-y ))

(defmethod place-within-window ((object vector) orientation offset-x offset-y )
  (cond ((= (length object) 4)
         (place-rectangle-inside-window object orientation offset-x offset-y  ))
        ((= (length object) 3)
         (place-circle-inside-window object orientation offset-x offset-y))))

(defun place-rectangle-within-object (rectangle source-object orientation offset-x offset-y)
  )


(defun place-within-helper (object source-object orientation offset-x offset-y)
  "Filter between placing within screen, or within an rectangle provided"
  (if source-object
      (place-rectangle-within-object object source-object orientation offset-x offset-y)
      (place-within-window object orientation offset-x offset-y)))

(defmacro ensure-quoted (arg)
  "Checks if what's been passed has already been quoted or not, if not, quotes it."
  `(if (listp ',arg)
       ,arg
       ',arg))


(defmacro place-within (object &key source-object orientation (offset-x 0) (offset-y 0))
  "Places {OBJECT} within {SOURCE-OBJECT}; if no {SOURCE-OBJECT} is provided, places it within the window.
{ORIENTATION} can take either a single orientation, or a list of orentations as keyed symbols (:left/:right, :top/:bottom), orientation always defaults to center of object.
{OFFSET-X} and {OFFSET-Y} can be specified as either an absolute value or a percentage (e.g., value + %, ex: 25%)."
  `(place-within-helper ,object ,source-object ,orientation (ensure-quoted ,offset-x) (ensure-quoted ,offset-y)))


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
