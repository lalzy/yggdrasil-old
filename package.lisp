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
   #:load-image
   #:draw-image
   #:flip-image
   #:find-image
   #:delete-image
   #:add-image-to-autodraw
   
   ;; animation
   #:create-animated-sprite
   #:draw-animation
   #:play-animation
   #:update-animations
   #:is-animation-playing?
   #:flip-animation
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
