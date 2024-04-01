;;;; main.lisp
(in-package #:animated-sprite)

;;; Example for how to utilize the animated-sprite system for Yggdrasil.



(defclass player ()
  ((position :initform #(150 150) :accessor pos)
   (state :initform 'idle :accessor state)
   (direction :initform 'right :accessor direction)
   (idle-sprite :initarg :idle :accessor idle)
   (move-sprite :initarg :move :accessor move)
   (attack-sprite :initarg :attack :accessor attack)))

(defun create-player ()
  (make-instance 'player
                 ;; Creates the individual sprites that will be animated.
                 ;; The cells need to be in the order that they should be drawn.
                 :idle (yg:create-animated-sprite "idle.png" (yg:allocate-cells 1200 80 120 80))
                 :attack (yg:create-animated-sprite "attack.png" (yg:allocate-cells 720 80 120 80))
                 :move (yg:create-animated-sprite "run.png" (yg:allocate-cells 1200 80 120 80))))

(defun draw-idle (player)
  ;; Play-animations puts the animation to be automatically updated each frame (by default the 'game' state).
  ;;      Optionally you can pass a list of states of where the animations should be updated
  ;;  We place it here so that it will continually loop.
  ;;   It checks if the animation is currently running or not, if it is, it won't start playing.
  ;;   So an individual animation-object doesn't double-play, and it's safe to do this to loop-it.
  ;;      *-Only applies
  (yg:play-animation (idle player))

  ;; Draws the actual sprite with the current animation as iterated through by play-animation.
  (yg:draw-animation (idle player) :x (get-x player) :y (get-y player))) 

(defun flip (player)
  ;; This flips the image, then reverse the cell-list to match.
  ;;  Can be expensive so might want to keep it as a separate object to reference when it needs to be flipped.
  (yg:flip-animation (idle player)) 
  (yg:flip-animation (attack player))
  (yg:flip-animation (move player)))


(defun get-x (player)
  (yg:x (pos player)))

(defun get-y (player)
  (yg:y (pos player)))

(defun draw-attack (player)
  (if (yg:is-animation-playing? (attack player)) ; Helper function to check if the current animation is playing or not.
      (yg:draw-animation (attack player) :x (get-x player) :y (get-y player))
      (setf (state player) 'idle)))

(defun draw-movement (player)
  (yg:play-animation (move player))
  (yg:draw-animation (move player) :x (get-x player) :y (get-y player)))

(defun main (&aux (width 640) (height 480) (title "animated sprite example"))
  (bt:make-thread
   (lambda ()
     (unwind-protect (progn) 
       (let (player
             (speed 2))
         (yg:start
             (:width width :height height :title title
              :asset-path (asdf:system-relative-pathname :animated-sprite "assets/"))
             
             (:init
              (setf player (create-player))
              (yg:set-state :game))

             (:key-up)
             
             (:key-down
              (when (yg:is-key :space)
                (yg:set-state (if (yg:check-state :menu) :game :menu)))

              (when (yg:check-state :game)
                (unless (equal (state player) 'attack)

                  (cond ((and (yg:is-key :left)
                              (not (equal (direction player) 'left)))
                         (flip player)
                         (setf (direction player) 'left))
                        
                        ((and (yg:is-key :right)
                              (not (equal (direction player) 'right)))
                         (flip player)
                         (setf (direction player) 'right))

                        ((yg:is-key :z)
                         (setf (state player) 'attack)
                         (yg:play-animation (attack player)))))))

             (:main
              (yg:draw-string 0 0 "press space to pause/unpause")
              (yg:with-state :menu
                (yg:draw-string 150 50 "Paused"))

              
              (cond ((equal (state player) 'attack)
                     (draw-attack player))
                    ((and (equal (direction player) 'right) (yg:is-key :right))
                     (incf (yg:x (pos player)) speed)
                     (draw-movement player))
                    ((and (equal (direction player) 'left) (yg:is-key :left))
                     (incf (yg:x (pos player)) (- speed))
                     (draw-movement player))
                    (t (draw-idle player))))))
       
       ;;cleanup
       ))
   :name title))
