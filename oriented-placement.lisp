(in-package :yggdrasil)


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


(defmethod center ((area rectangle) (object rectangle))
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

    (values x y)))

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

(defmethod place-inside ((area rectangle) (object rectangle) offsets orientations)
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
