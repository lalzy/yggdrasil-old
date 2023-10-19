;;;; breakout.lisp
(in-package #:breakout)
(defparameter *max-speed* 10) ;; Need to fix range-collision to increase beyond this

(defclass ball (yg:circle)
  ((speed :accessor ball-speed :initform 0)
   (move-dir :accessor ball-move-dir :initform #(nil nil))
   (attached :accessor ball-attached :initform t)))

(defun draw-items (paddle ball boxes)
  (when boxes
    (dolist (box boxes)
      (yg:draw-rectangle box :color (yg:get-color blue))))
  
  (yg:draw-rectangle paddle :color (yg:get-color green))
  (yg:draw-circle ball :color (yg:get-color yellow)))


(defun move-paddle (paddle)
  (cond ((or (yg:is-key :left) (yg:is-key #\A))
	 (decf (yg:x paddle) 5))
	((or (yg:is-key :right) (yg:is-key #\D))
	 (incf (yg:x paddle) 5))))

(defun hor-move (ball)
  (aref (ball-move-dir ball) 0))

(defun vert-move (ball)
  (aref (ball-move-dir ball) 1))

(defun move-ball (ball paddle)
  (if (ball-attached ball)
      (setf (yg:x ball) (+ (yg:x paddle) (round (yg:w paddle) 2)))
      (progn
	(if (string-equal (hor-move ball) :left)
	    (decf (yg:x ball) (ball-speed ball))
	    (incf (yg:x ball) (ball-speed ball)))
	(if (string-equal (vert-move ball) :UP)
	    (decf (yg:y ball) (ball-speed ball))
	    (incf (yg:y ball) (ball-speed ball))))))

(defun rect-collision (rect ball &optional paddle)
  (let ((collision (yg:collision-check rect ball)))
    (when collision
      (when (and paddle (< (ball-speed ball) *max-speed*))
	(incf (ball-speed ball) 1))
      (case (yg::rectangle-direction-check rect (yg::circle-to-rect ball))
	(:left (setf (aref (ball-move-dir ball) 0) :right)
	 :left)
	(:top (setf (aref (ball-move-dir ball) 1) :UP)
	 :top)
	
	(:bottom (setf (aref (ball-move-dir ball) 1) :down)
	 :bottom)
	
	(:right (setf (aref (ball-move-dir ball) 0) :LEFT)
	 :right)))))

(defun collision-check (paddle ball boxes)
  (let ((collision (yg:edge-collision-check ball)))
    (when collision
      (cond ((member :left collision)
	     (setf (aref (ball-move-dir ball) 0) :right))
	    ((member :right collision)
	     (setf (aref (ball-move-dir ball) 0) :left))
	    ((member :top collision)
	     (setf (aref (ball-move-dir ball) 1) :down))
	    ((member :bottom collision)
	     ;(reset paddle ball)
	     (setf (aref (ball-move-dir ball) 1) :up)
	     ))))

  (when boxes
    (dolist (box boxes)
      (let ((collision (rect-collision box ball)))
	(when collision
	  (return-from collision-check collision)))))
  
  (rect-collision paddle ball t)
  
  )

(defun reset (paddle ball)
  (setf (aref (ball-move-dir ball) 0) :left
	(aref (ball-move-dir ball) 1) :up
	(yg:x ball) (+ (yg:x paddle) (round (yg:w paddle) 2))
	(yg:y ball) (- (yg:y paddle) 15)
	(ball-attached ball) t
	(ball-speed ball) 1))

(defun main (&aux (width 640) (height 480) (path (asdf:system-relative-pathname :breakout "assets/")))
  (bt:make-thread
   (let* ((debug t)
	  (paddle (make-instance 'yg:rectangle :x (- (round width 2) (round 150 2)) :y (- height 50) :w 100 :h 15))
	  (ball (make-instance 'ball :x (+ (yg:x paddle) (round (yg:w paddle) 2)) :y (- (yg:y paddle) 15) :r 15))
	  (boxes)
	  pause)
     (lambda ()
       (yg:start
	   (:width width
	    :height height
	    :title "Breakout")

	   (:key-down (when (yg:is-key #\space)
			(setf (ball-attached ball) nil
			      (ball-speed ball) 3
			      (ball-move-dir ball) #(:LEFT :UP)))
		      (when (yg:is-key #\p)
			(setf pause (if pause nil t))))

	   (:mouse-down (push (make-instance 'yg:rectangle :x (sdl:mouse-x) :y (sdl:mouse-y) :w 25 :h 25) boxes))
					; (:init)
       
	   (:main
	    ;; When state running
	    (yg:draw-string 0 20 (format nil "FPS: ~a" (sdl:frame-rate)))
	    (let* (end-x
		   end-y
		   (x (yg:x ball))
		   (y (yg:y ball)))

	      (unless pause
		
		(sdl:with-timestep ()
		  (move-paddle paddle)
		  (move-ball ball paddle)
		(setf end-x (yg:x ball)
		      end-y (yg:y ball))
		  
		  (when (case (collision-check paddle ball boxes)
			  (:top (decf (yg:y ball)))
			  (:bottom (incf (yg:y ball)))
			  (:left (decf (yg:x ball)))
			  (:right (incf (yg:x ball))))
		    (setf end-x (yg:x ball)
			  end-y (yg:y ball)))
		  (unless  pause
		    (when (< x end-x)
		      (incf x (yg:r ball)))
		    (when (< y end-y)
		      (incf y (yg:r ball)))))))
	    (draw-items paddle ball boxes)

	    
	    (when debug
	      (yg:draw-string 0 0 (format nil "speed: ~a" (ball-speed ball)))
	      (yg:draw-string 0 60 (format nil "ball[~a][~a], paddle[~a][~a]" (yg:x ball) (yg:y ball) (yg:x paddle) (yg:y paddle))))
	    ))))
     :name "Breakout-main"))
		  
