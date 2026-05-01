# yggdrasil

# This engine is deprecated and not suggested for use. It exist for historical purposes. It's successor is not currently public but actively worked on.

### reason for deprecation:
I watched lectures on how to build an game-engine, and in doing so also learned about ECS (Entity Component System). While this could have been made into this engine, since it requires fundemental different architecture (much of this engine is tied to CLOS in an non-trivial way), I decided to start "over". Much of the core identity of Yggdrasil-old is retained in it's successor. Such as the start macro, and the pass initialization/window-params as parameters to start.

As such, the successor was made with an ECS in mind, however I found that making the ECS an optional-feature, served th engine (and potentionally game devs) better, than enforced ECS design. I also built the concept of the engine, and ECS with SDL as the renderer, but, like with Yggdrasil-old, I ensured no direct SDL calls within the engine itself (unlike the predecessor (SHF)[https://github.com/lalzy/SHF]), and so between 0.1.3 and 0.1.5, I created my own (openGL based) renderer for it. 

The successors renderer, utilize the same calls for "immediate" draw calls, and the ECS draw calls, and the same rendering calls for both world-space, and screen space.

# Description:

Simply put, it's a simple 2D engine built atop of Lispbuilder-sdl(SDL 1.2). Lispbuilder-sdl was chosen as SDL2-ttf was too outdated and can't be used with modern SDL2 (memory-leak issues).

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
- Better sound-system with third-party sound-library (in successor when it goes public)
