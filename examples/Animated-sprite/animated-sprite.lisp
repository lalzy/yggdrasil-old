;;;; main.lisp
;;; Example for how to utilize the animated-sprite system for Yggdrasil.
(defpackage #:examples-animated-sprite
  (:use #:cl)
  (:nicknames #:yg-ex-as #:ex-as)
  (:export #:main #:start))

(in-package #:examples-animated-sprite)

;; Optionally, can use the Yggdrasil Position, or rectangle class. They will allow you to use the shape-methods of Ygdrassil
;;     Such as yg:x,  yg:w,  yg:r  an so forth.
;;    While 'technically' you can use the setter\getters without the YG shape-classes, SBCL doesn't like it.
(defclass player (yg:pos) 
  ((state :initform 'idle :accessor state)
   (direction :initform 'right :accessor direction)
   (idle-sprite :initarg :idle :accessor idle)
   (move-sprite :initarg :move :accessor move)
   (attack-sprite :initarg :attack :accessor attack)))

(defun create-player ()
  (make-instance 'player
                 :x 150 :y 150
                 ;; Creates the individual sprites that will be animated.
                 ;; The cells need to be in the order that they should be drawn.
                 :idle (yg:create-animated-sprite "idle.png" (yg:allocate-cells 1200 80 120 80))
                 :attack (yg:create-animated-sprite "attack.png" (yg:allocate-cells 720 80 120 80))
                 :move (yg:create-animated-sprite "run.png" (yg:allocate-cells 1200 80 120 80))))

(defun draw-idle (player)
  ;; Play-animations puts the animation to be automatically updated each frame (by default the 'game' state).
  ;;      Optionally you can pass a list of states of where the animations should be updated
  ;;  We place it here so that it will continually loop.
  ;;   It checks if the animation is currently runnig or not, if it is, it won't start playing.
  ;;   So an individual animation-object doesn't double-play, and it's safe to do this to loop-it.
  ;;      *-Only applies
  (yg:play-animation (idle player))

  ;; Draws the actual sprite with the current animation as iterated through by play-animation.
  (yg:draw-animation (idle player) :x (yg:x player) :y (yg:y player)))


(defun flip (player &optional vertical)
  ;; This flips the image, then reverse the cell-list to match.
  ;;  Can be expensive so might want to keep it as a separate object to reference when it needs to be flipped.
  (yg:flip-animation (idle player) :horizontal (not vertical) :vertical vertical) 
  (yg:flip-animation (attack player) :horizontal (not vertical) :vertical vertical)
  (yg:flip-animation (move player) :horizontal (not vertical) :vertical vertical))

(defun draw-attack (player)
  (if (yg:is-animation-playing? (attack player)) ; Helper function to check if the current animation is playing or not.
      (yg:draw-animation (attack player) :x (yg:x player) :y (yg:y player)) ; Using the yg:x and yg:y to get the positioning data. This is actually redundant (see base-example).
      (setf (state player) 'idle)))

(defun draw-movement (player)
  (yg:play-animation (move player))
  (yg:draw-animation (move player) :x (yg:x player) :y (yg:y player)))

(defun main (&optional (asset-path (asdf:system-relative-pathname :yggdrasil-examples "animated-sprite/assets/"))
             &aux (width 640) (height 480) (title "animated sprite example - timestep"))
  (bt:make-thread
   (lambda ()
     (unwind-protect (progn) 
       (let (player
             (speed 2))
         (yg:start
             (:width width :height height :title title :fps 60 
              ;:default-font (yg:make-font font-path) ;; Font-path is throwing weird error, investigate\add proper errors.
              :asset-path asset-path) 
             
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
                        ((or (yg:is-key :up) (yg:is-key :down))
                         (flip player t))
                        ((yg:is-key :z)
                         (setf (state player) 'attack)
                         (yg:play-animation (attack player)))
                        ((yg:is-key :f)
                         (incf (sdl:frame-rate) 10))
                        ((yg:is-key :g)
                         (decf (sdl:frame-rate) 10))))))

             ;; Main loop, uses time-step, defaults to 10 DT
             (:main
              (cond ((equal (state player) 'attack)
                     (unless (yg:is-animation-playing? (attack player))
                       (setf (state player) 'idle)))
                    ((and (equal (direction player) 'right) (yg:is-key :right))
                     (setf (state player) 'movement)
                     (incf (yg:x  player) speed)) ; Here you can see the yg:x setter used)
                    ((and (equal (direction player) 'left) (yg:is-key :left))
                     (setf (state player) 'movement)
                     (incf (yg:x  player) (- speed)))
                    (t (setf (state player) 'idle))))

             ;; The default drawing-step, happens after auto-drawing. Uses FPS, defaults to 60
             (:draw
              (cond ((equal (state player) 'attack)
                     (draw-attack player))
                    ((equal (state player) 'movement)
                     (draw-movement player))
                    (t (draw-idle player))))
             
             ;;; Prioritised drawing, last step of the main-event loop right before display update
             ;;; So anything drawn here, will appear infront. Intended primarily for UI elements.
             (:draw-ui
              (yg:draw-string 0 0 "press space to pause/unpause, press f\g to increase\decrease fps")
              (yg:draw-string 0 30 (format nil "FPS = ~a, timescale = ~a" (truncate (sdl:average-fps))  (float (sdl:time-scale))))
              (yg:with-state :menu
                (yg:draw-string 150 50 "Paused")))))
       ;;cleanup
       ))
   :name title))

;; Separate function to start when using an executable
(defun start ()
  (format t "Before Main~%")
  ;; Must join thread that gets created when running as an executable
  (bt:join-thread
   (main "assets/" "Vera.ttf")) ; Sets the paths relative to the executable
  (format t "After main~%"))
