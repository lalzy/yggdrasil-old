# yggdrasil

Simply put, it's a simple 2D engine built atop of Lispbuilder-sdl(SDL 1.2). Lispbuilder-sdl was chosen as SDL2-ttf is too outdated and can't be used with modern SDL2.

The aim for the engine is to abstract away from SDL, and generally 'low-level' calls, and instead automate as much as can be automated (like sprites, collisions, etc).

Very quick and simple example of how to use the engine to draw an solid coloured rectangle:
```
(defun main (&aux (width 640) (height 480))
  (let ((w 50) (h 50))
    (yg:start
      (:width width :height height
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
```

Examples for how to use the various components for the engine can be found in the examples folders. You can call the examples by calling `(ql:quickload :yggdrasil-examples)` then call the individual examples. Like so: `(animated-sprites:main)`


The current plans for the engine are in no particular order:

multi-res \ Aspect-Ratio support by default for anything drawn.
Save-system
Ability to read tile-maps
Proper error-handling (Message-Box when compiled) that works for both Windows and Linux.
An UI system (scrollbars, textfields, window-frames, buttons, etc).
Proper collisions, and\or physics engine (probably tied to Box2D)
Object-rotation (semi-implemented) + collision for said rotated objects
OpenGL for rendering (definitely no ETA, everything is designed so that if using the engine, when this switch happens it won't affect anything made with the engine)
Better sound-system (third-party sound-library, openAl?)