# yggdrasil

# This engine is deprecated and not suggested for use. It exist for historical purposes.

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


The plans for the engine were (but was abandoned in favor of it's successor, which is not public at time of writing):
- multi-res \ Aspect-Ratio support by default for anything drawn. (in successor)
- Save-system
- Ability to read tile-maps
- Proper error-handling (Message-Box when compiled) that works for both Windows and Linux.
- An UI system (scrollbars, textfields, window-frames, buttons, etc).
- Proper collisions, and\or physics engine (collision is in successor, physics is planned 0.5)
- Object-rotation (semi-implemented) + collision for said rotated objects (in successor)
- OpenGL for rendering (in successor)
- Better sound-system with third-party sound-library (planned for successor 0.3, so for public release. At time of writing successor is ~0.2)
