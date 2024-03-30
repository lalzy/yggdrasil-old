(in-package #:yggdrasil-test)

;; Fix 'space as symbol issue with is-key

(defclass player ()
  ((idle-sprite :initarg :idle :accessor idle)
   (attack-sprite :initarg :attack :accessor attack)))

(defun main (&aux (width 640) (height 480))
  (let (player)
    
    (yg:start
     (:width width :height height)
     (:key-down
      (when (yg::check-state :game)
        (when (yg:is-key :a)
          (yg::play-animation (attack player))))
      (when (yg:is-key :space)
        (cond ((yg::check-state :menu) (yg::set-state :game))
              ((yg::check-state :game) (yg::set-state :menu)))))
     (:init
      (setf player (make-instance 'player :attack (yg::create-animated-sprite "_Attack.png" (yg:allocate-cells 720 80 120 80) :path "C:/Coding Projects/CL-Projects/aquam/assets/"))))
      
     (:main
      (yg:draw-string 0 0 "press space to pause/unpause")
      (yg::with-state :menu
        (yg:draw-string 220 30 "paused")
        (yg::update-animations))
      (yg::draw-animation (attack player))))))
