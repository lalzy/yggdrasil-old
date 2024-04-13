# yggdrasil

Simply put, it's a simple 2D engine built atop of Lispbuilder-sdl(SDL 1.2). Lispbuilder-sdl was chosen as SDL2-ttf is too outdated and can't be used with modern SDL2.

Currently the engine does, simple startup of SDL through one macro(start). Allow for easy event calls. Checking which key is being pressed(through passing the char) and easy access to colors(through a pre-setup color list that can be added to during runtime), simple example showcasing this:

`(defun main (&aux (width 640) (height 480))
  (let ((w 50) (h 50))
    (yg:start
	(:width width :height height)

	(:key-down
	 (cond ((yg:is-key #\+)
		(incf w 5)
		(incf h 5))
	       
	       ((yg:is-key #\-)
		(decf w 5)
		(decf h 5))))
	
	(:main
	 (sdl:draw-box-* (- (round width 2) (round w 2)) (- (round height 2) (round h 2)) w h :color (yg:get-color green)))
	
	(:end (format t "bye!~%")))))
`
Numerous places to interject code includes, pre\post window initialization, all the event loops of SDL(including quit event) and a post-sql shutdown.


The current plans for the engine are in no particular order:

OpenGL for graphics\drawing(which is why there's a lot of redundancies, like shape objects, X\Y cordinates for them, etc, SDL's surface system dissapears with OpenGL for obvious reasons, thereby so do they).
GUI stuff(Scrollbars, TextFields, Windows, buttons, etc, etc).
Collision for objects(primarily circles and rectangles, maybe other shapes at a later date). 
Rotation of objects and collision for said rotated objects.
Simple Physics.
Potentionally replace SDLs audio library with something like OpenAL.


Originally the engine was just a few helper functions \ macros to streamline the use of SDL to avoid having to redo the exact same things over and over, but as the scope increased, it become more like an simple\small engine, than just helper functions, and so a full fledged engine it is now instead.
