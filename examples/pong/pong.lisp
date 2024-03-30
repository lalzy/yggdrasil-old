;;;; breakout.lisp
(in-package #:pong)
2
(defparameter *y* 10)
(defparameter *paddle-move-speed* 1)
(defparameter *ball-move-speed* 1)
(defparameter *rect* nil)

(defun place-paddles (paddle)
  (yg:place-within-window paddle :orientation :left :offset-x 5 :offset-as-percentage t))

;;; - Oriented-placement:
;;;         Finish place-circle-inside-window (support offsets)


;; Rewrite code to be better.


;; Fix all instances of yg::

;; Need to create placement for circle
(defun place-ball (ball)
  (yg:place-within-window ball))

(defun control-paddle (paddle)
  (let ((old-y (yg:y paddle)))
    (when (yg:is-key :down)
      (incf (yg:y paddle) (calc-relative-movement *paddle-move-speed*)))
    (when (yg:is-key :up)
      (decf (yg:y paddle) (calc-relative-movement *paddle-move-speed*)))
    
    (when (yg:edge-collision-check paddle)
      (setf (yg:y paddle) old-y))))

(defun change-ball-dir (ball ball-dir)
  (when (yg::get-edge-dir ball :left)
    (setf (aref ball-dir 0) :right))
  (when (yg::get-edge-dir ball :right)
    (setf (aref ball-dir 0) :left))
  (when (yg::get-edge-dir ball :top)
    (setf (aref ball-dir 1) :down))
  (when (yg::get-edge-dir ball :bottom)
    (setf (aref ball-dir 1) :up)))

(defun ball-move (ball paddle ball-dir)
  (change-ball-dir ball ball-dir)
  (paddle-ball-collision ball paddle ball-dir)
  (when (equal (aref ball-dir 0) :left)
    (decf (yg:x ball) (calc-relative-movement *ball-move-speed*)))
  (when (equal (aref ball-dir 0) :right)
    (incf (yg:x ball) (calc-relative-movement *ball-move-speed*)))
  (when (equal (aref ball-dir 1) :up)
    (decf (yg:y ball) (calc-relative-movement *ball-move-speed*)))
  (when (equal (aref ball-dir 1) :down)
    (incf (yg:y ball) (calc-relative-movement *ball-move-speed*))))

(defun paddle-ball-collision (ball paddle ball-dir)
  (when (yg::rectangle-circle-intersection-check paddle ball)
    (setf (aref ball-dir 0) :right)))

(defun draw-ball (ball)
  (yg:draw-circle ball :filled t))

(defun draw-paddle (paddle)
  (yg:draw-rectangle paddle :filled t))

(defun calc-relative-movement (move-speed)
  "Sets movement to be relative to window-size"
  (round (yg:%-of-value move-speed yg:*height*)))

(defun lines ()
  (let* ((wh (round yg:*width* 2))
         (hh (round yg:*height* 2))
         (p1-s (vector 0 hh))
         (p1-e (vector yg:*width* hh))
         (p2-s (vector wh 0))
         (p2-e (vector wh yg:*height*)))
    (yg:draw-line p1-s p1-e)
    (yg:draw-line p2-s p2-e)))


(defun main ()
  (let ((paddle nil)
        (ball nil)
        (ball-dir #(:left :down)))
    (bt:make-thread
     (lambda ()
       (yg:start
           (:width 400 :height 400 :resizable t)
           (:resize
            (setf paddle (yg::create-rectangle-vector-% :w 5 :h 15))
            (setf ball (yg::create-circle-vector-% :r 3))
            (place-ball ball)
            (place-paddles paddle))
           (:init
            (setf paddle (yg::create-rectangle-vector-% :w 5 :h 15))
            (setf ball (yg::create-circle-vector-% :x 150 :y 150 :r 3))
            (place-ball ball)
            (place-paddles paddle))
           (:idle
            (lines)
            
            (control-paddle paddle)
            (ball-move ball paddle ball-dir)
            (draw-paddle paddle)
            (draw-ball ball)))))
    ) :name "pong")
