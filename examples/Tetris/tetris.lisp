;;;; breakout.lisp
(in-package #:tetris)

#|

Loop:
  If any cell will intersect on next move:
     Stop and add to tetris grid-map
  else
     move down

  check all grid lines, if any line does not have at least 1 zero:
                             remove line and append new line to top of board.

|#

(defparameter *Shapes* (vector
			;; I Shape
			(vector '((0 0 0 0)
				  (1 1 1 1))
				'((0 1)
				  (0 1)
				  (0 1)
				  (0 1)))
			;; T Shape
			(vector '((0 0 0)
				  (1 1 1)
				  (0 1 0))
				'((0 1 0)
				  (1 1 0)
				  (0 1 0))
				'((0 1 0)
				  (1 1 1))
				'((0 1 0)
				  (0 1 1)
				  (0 1 0)))
			;; Z Shape
			(vector '((1 1 0)
				  (0 1 1))
				'((0 1 0)
				  (1 1 0)
				  (1 0 0)))

			;; S Shape
			(vector '((0 1 1)
				  (1 1))
				'((0 1 0)
				  (0 1 1)
				  (0 0 1)))
			;; L Shape

			(vector '((0 0 1)
				  (1 1 1)
				  (0 0 0))
				'((0 1 0)
				  (0 1 0)
				  (0 1 1))
				'((0 0 0)
				  (1 1 1)
				  (1 0 0))
				'((1 1 0)
				  (0 1 0)
				  (0 1 0)))
			;; J Shape

			(vector '((0 0 0)
				  (1 1 1)
				  (0 0 1))
				'((0 1)
				  (0 1)
				  (1 1))
				'((1 0 0)
				  (1 1 1))
				'((0 1 1)
				  (0 1 0)
				  (0 1 0)))
			;; O Shape

			(vector '(( 1 1)
				  ( 1 1)))
			;;
			))


(defparameter *current-orientation* 0)
(defparameter *shape-index* 0)

(defparameter *map* (make-array '(22 12) :initial-element 0))
(defparameter *start-pos* #(64 64))
(defparameter *cell-size* 16)


(defun draw-map ()
  (let ((x (yg:x *start-pos*))
	(y (yg:y *start-pos*)))
    (loop :for row :below (first (array-dimensions *map*)) :do
      (loop :for column :below (second (array-dimensions *map*)) :do
	(yg:draw-rectangle (vector (+ x (* column *cell-size*)) (+ y (* row *cell-size*)) *cell-size* *cell-size*)
			   :color (case (aref *map* row column)
				    (-1 (yg:get-color gray))
				    (0 (yg:Get-color black))
				    (1 (yg:get-color green))
				    (2 (yg:get-color yellow))
				    (3 (yg:get-color cyan))
				    (4 (yg:get-color red))
				    (5 (yg:get-color orange))
				    (6 (yg:get-color indigo)))
			   :filled t)
	(yg:draw-rectangle (vector (+ x (* column *cell-size*)) (+ y (* row *cell-size*)) *cell-size* *cell-size*)
			   :color (yg:get-color black))))))


(defun create-border ()
  (let ((size (array-dimensions *map*)))
    (loop :for column :below (second size) :do
      (setf (aref *map* 0 column) -1
	    (aref *map* (1- (first size)) column) -1))
    (loop :for row :below (first size) :do
      (setf (aref *map* row 0) -1
	    (aref *map* row (1- (second size))) -1))))


;; Makes the pixel position into the actual map's position
(defun cell-position-to-map ())

;; Makes the map position into actual pixel position
(defun map-position-to-cell ())



;; Draw and Move the shape with the pixel cordinates(within bounds)

;; Check collision\intersection after translating map positions

(defun draw-shape ())

(defun main ()
  (let ((running t))
    (yg:start
	(:width 640
	 :height 480)
	(:key-down (cond ((yg:is-key :up)
			  (incf *current-orientation*))
			 ((yg:is-key :right)
			  (if (>= *shape-index* (1- (length *shapes*)))
			      (setf *Shape-index* 0)
			      (incf *shape-index*))
			  (setf *current-orientation* 0))
			 ((yg:is-key :left)
			  (if (<= *shape-index* 0)
			      (setf *shape-index* (1- (length *shapes*)))
			      (decf *Shape-index*))
			  (setf *current-orientation* 0))))
	(:init (create-border)
	 )
	(:main
	 (draw-map)))))
