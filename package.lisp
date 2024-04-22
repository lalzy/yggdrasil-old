;;;; package.lisp

(defpackage #:yggdrasil
  (:use #:cl #:iterate)
  (:nicknames #:yg)
  (:export
   #:start

   :*width*
   :*height*

   :pos
   :rectangle
   :circle

   #:set-path
   #:get-path
   
   #:get-aspectratio

   ;;; State
   #:check-state
   #:with-state
   #:set-state
   #:get-current-state ; mostly for debugging purposes, generally you'll want with-state or check-state
   #:add-state
   
   ;;; Math
   #:%-from-total
   #:%-of-value
   
   ;;; Controlls
   #:mouse-moved-p
   #:get-mouse-position
   #:is-mouse-dir
   #:is-mouse-key
   #:is-mouse-keys ; Checks if any of the passed mouse-buttons are pressed
   #:is-key
   #:is-keys
   #:is-all-keys
   

   ;;; Collision \ Positions
   #:mouse-collision-check
   #:collision-check ; Will have proper check between distances
   #:edge-collision-check
   #:x
   #:y
   #:w
   #:h
   #:r

   ;;; Alignment
   #:place-within
   
   ;;;Shape Drawing
   #:get-color
   #:set-color
   #:draw-line
   #:draw-rectangle
   #:draw-circle

   ;; Image-stuff
   #:load-image ; Create an image
   #:draw-image ; draws either an image, or an animation object
   #:flip-image ; flips either an image, or an animation object
   #:find-image ; find image, either through image-name, or the image-object itself.
   #:delete-image ; removing created image.
   #:add-image-to-autodraw ; adds the image to auto-draw
   
   ;; animation
   #:create-animated-sprite ; Crates an animated sprite
   #:draw-animation ; alternative to draw-image for animation-object, redirects to draw-image
   #:play-animation ; Sets the animation to be playing in game-state
   #:update-animations ; to update the animation if you're not in a game-state
   #:is-animation-playing? ; Check wether animation is running or not
   #:flip-animation ; alternative to draw-image for animation-object, redirects to draw-image
   #:pause-animation
   
   ;;; Drawing-Helper
   #:allocate-cells
   #:cell-count
   #:cell-list
   
   ;;; Context Menu
   #:init-context-menu
   #:menu-change-font
   #:menu-create
   #:menu-select-item
   #:menu-draw
   
   ;;; Text
   #:create-font
   #:draw-string
   #:draw-string-*

   ;;; Utility
   #:toggle-variable
   ))
