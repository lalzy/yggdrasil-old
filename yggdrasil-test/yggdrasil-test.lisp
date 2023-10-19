(in-package #:yggdrasil-test)

(defparameter *map* #(
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )
#(1 1 2 2 2 2 1 1 2 2 2 2 1 1 1 )
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )
#(2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 )))

(defun draw-map (map )
  (let ((x 0)
	(y 0)
	(w 32)
	(h 32))

    (loop :for row :From 0 :to (1- (length map)) :do
      (loop :for col :from 0 :to (1- (length (aref map row))) :do
	(let ((tile (aref (aref map row) col)))
	  (cond ((= tile 1)
		 (yg::draw-rectangle-* (* col w) (* row h) w h :color (yg:get-color blue) :filled t))
		((= tile 2)
		 (yg::draw-rectangle-* (* col w) (* row h) w h :color (yg:get-color green) :filled t))
		(t
		 (yg::draw-rectangle-* (* col w) (* row h) w h :color (yg:get-color black) :filled t)))
	  (yg::draw-rectangle-* (* col w) (* row h) w h :color (yg:get-color red)))))))

(defun main (&aux (width 640) (height 480))
  (let ((player (make-instance 'yg:rectangle :x 0 :y 0 :w 25 :h 25)))
    (yg:start
	(:width width :height height)
	(:main
	 (draw-map *map*)
	 ;(yg:draw-rectangle player :color (yg:get-color blue) :filled t)
	 ))))
