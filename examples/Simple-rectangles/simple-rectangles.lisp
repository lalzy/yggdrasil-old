(defpackage #:examples-simple-rectangles
  (:use #:cl)
  (:nicknames #:yg-ex-sr #:ex-sr)
  (:export #:main #:main2 #:main3)
  )

(in-package #:examples-simple-rectangles)


(defun main ()
  ;; Everything happens inside of start.
  (yg:start
   ;; First argument is a list of keywords (see documentation or source) that affects the initialization, such as width, height, title, or fps.
      (:width 150 :height 150 :title "simple rectangle")

   ;; Draw is a specific section within the main idle-event (loop) of SDL, specifically uses FPS-cycles, unlike the main (:idle or :main) event, which use timescale (delta 10 by default)
   (:draw
    ;; yg takes two parameters, either an Yg:rectangle object, or a vector containing #(X Y Width Height) to specify where and how to draw.
    (yg:draw-rectangle #(50 50 50 50)))))

(defun main2 ()
  (yg:start
   (:width 150 :height 150 :title "simple rectangle 2")))


(defun main3 ()
  (yg:start
   (:width 250 :height 250 :title "colored rectangle")
   
   ;; Anytime you can pass a color, you can either pass it a specific color gotten with (yg:get-color {the color}),  or a vector containing the RGB values
   (:draw (yg:draw-rectangle #(50 50 50 50) :color #(100 0 100))
          (yg:draw-rectangle #(150 150 50 50) :color (yg:get-color yellow) :filled t)))) ; Filled fills the rectangle making it into a box.


