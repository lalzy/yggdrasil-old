(in-package #:yggdrasil-test)

(defparameter *asset-path* (asdf:system-relative-pathname :lispbuilder-sdl "assets/";:yggdrasil "assets/"
                                                          ))



(defun main2 (&aux (width 640) (height 480))
  (let ((w 50) (h 50))
    (yg:start
      (:width width :height height
       ;:font-path *asset-path*
       ) ; necessary due to how font system work, you can't have an empty font
      (:key-down 
        (cond ((yg:is-key #\+)
		(incf w 5)
  		(incf h 5))
    	      ((yg:is-key #\-)
	   	(decf w 5)
     		(decf h 5))))
       (:draw
         (yg:draw-rectangle (vector (- (round width 2) (round w 2)) (- (round height 2) (round h 2)) w h)
                            :color (yg:get-color green) :filled t)))))


;; Test no-asset path loading lisp to see if error-handling is done correctly.
(defun main3 ()
  
  (yg:start
   (:asset-path *asset-path*)
   (:init
    (yg:load-image "lisp.png")
    (yg:set-state :game))
   (:draw
    (yg:draw-image "lisp")
    (yg:draw-image "lisp" :x 300 :y 300))))


(defun main4 ()
  (let (animation)
  (yg:start
      (:asset-path (asdf:system-relative-pathname :yggdrasil "examples/Animated-sprite/assets/"))
      (:init
       (setf animation (yg:create-animated-sprite "idle.png" (yg:allocate-cells 720 80 120 80) :interval 10 :start-playing t :auto-draw t :x 50 :y 50))
       (yg:set-state :game))
      (:draw ;(yg:play-animation animation)
             ))))


(defclass ball (yg:circle)
  ((map-pos :initarg :mappos :accessor map-pos)
   (movement-direction :initform #(:left :down) :accessor move-dir)))

(defun draw-ball (ball)
  (yg:draw-circle ball))

(defun translate-map-to-screen-pos (map)
  (values ))

(defun update-ball-screen-pos (map ball)
  (setf (yg:x ball) (round (aref (map-pos ball) 0) *scale*)
        (yg:y ball) (round (aref (map-pos ball) 1) *scale*)))

(defparameter *scale* 100)
(defparameter *movement* 10)
(defparameter *Ball* nil)

(defun collision (ball)
  (cond ((yg::get-edge-dir ball :left)
         (setf (aref (move-dir ball) 0) :right))
        ((yg::get-edge-dir ball :right)
         (setf (aref (move-dir ball) 0) :left))))

(defun movement (ball)
  (let ((vert-move (aref (move-dir ball) 0)))
    (cond ((string-equal :left vert-move)
           (decf (aref (map-pos *ball*) 0) *movement*))
          ((string-equal :right vert-move)
           (incf (aref (map-pos *ball*) 0) *movement*)))))

(defun main-loop (map ball)
  (collision ball)
  (movement ball)
  (update-ball-screen-pos map ball))

(defun main (&aux (width 640) (height 480) (title "yggdrasil-testing"))
  (bt:make-thread
   (lambda ()
     (let ((map (vector (* width *scale*) (* height *scale*))))
       (yg:start
        (:width width :height height :title "title"
         :asset-path *asset-path*
         )
        (:init
         (yg:set-state :game)
         (setf *ball* (make-instance 'ball :mappos (vector (round (aref map 0) 2) (round (aref map 1) 2)) :r 10))
         (update-ball-screen-pos map *ball*))
        (:main
         (main-loop map *ball*))
        (:draw
         (draw-ball *ball*)
         ))))
   :name title))
