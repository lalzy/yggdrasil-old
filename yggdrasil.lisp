(in-package #:yggdrasil)

#||
named-animation system, similar to named-images. with *animated-sprites*

Make interesection\collision work with image-name, and not require image-objects

Fix up\write documentation where it's missing.

make animations work in other states than just game.
       > Do this by making play-animation take an optional argument that's the state.
            possibly by redoing animation to be a key'ed list instead, containing state > list of animations (as objects) to play(?)

create ability to delete 'all' images\animations with a single call to (remove-all-images)

Option for automatic-click interaction on drawn shapes\images

Create possibility to compile an executable:
   Paths should become relative to executable.

replace loop with iter(?)

ability to loop animations without having to specify to draw it
ability to pause animations
Rewrite animated-sprite to be more 'up-to-date' and utilizing the auto-draw functionalities (when re-implemented properly)


-- Severily deprioritized.
Create an UI-System

||#

(declaim (optimize (speed 3)))

(defparameter *width* nil)
(defparameter *height* nil)

(defmacro free-image-list (list)
  "makes sure to free images from memory"
  (let ((image (gensym)))
    `(progn
       (dolist (,image ,list)
         (sdl:free (image-data ,image)))
       (setf ,list nil))))

(defun clear-globals ()
  "deletes, resets or clears out global variables"
  (setf *asset-paths* (init-asset-paths-global)
        *fonts* nil
        *state* :setup
        *animated-sprites-to-animate* nil)
  (free-image-list *images*)
  (free-image-list *auto-draw-list*)
  ;; Shares images with *animated-sprites-to-animate* so don't have to do it for both
  (free-image-list *animated-sprites*) 
  (trivial-garbage:gc :full t))

(defun init-paths (asset-path font-path image-path)
  (set-path root asset-path)
  (set-path font font-path)
  (set-path image image-path))

(defun init-globals (width height asset-path font-path image-path)
  (init-paths asset-path font-path image-path)
  (setf *width* width
	*height* height)) ; to be removed

(defun filter-events (body)
  "Filters out the synonymous keywords into a unified keyword"
  (iter (for item :in body)
    (collect
	(case (first item)
          ((:idle-start :main-start :pre-main :pre-idle)
           (list :idle-start-form (rest item)))
	  ((:time-step :game-loop  :update :main :main-loop :idle-loop)
	   (list :timestep-form (rest item)))
          ((:pre-draw :pre-auto-draw)
           (list :pre-auto-draw-form (rest item)))
          ((:post-draw :post-auto-draw :draw :drawing)
           (list :post-auto-draw-form (rest item)))
          ((:draw-ui :ui-draw :ui-event :ui :event-ui)
           (list :ui-form (rest item)))
	  ((:window-focus :window-focus-event)
	   (list :window-focus-form (rest item)))
	  ((:quit :end :quit-event)
	   (list :end-form (rest item)))
	  ((:key-down :key-down-event)
	   (list :key-down-form (rest item)))
	  ((:key-up :key-up-event)
	   (list :key-up-form (rest item)))
	  ((:mouse-move :mouse-motion-event :mouse-motion)
	   (list :mouse-move-form (rest item)))
	  ((:mouse-down :mouse-down-event :mouse-button-down-event)
	   (list :mouse-down-form (rest item)))
	  ((:mouse-up :mouse-up-event :mouse-button-up-event)
	   (list :mouse-up-form (rest item)))
          ((:video-resize-event :video-resize :resize)
           (list :VIDEO-RESIZE-EVENT (rest item)))
	  ((:pre-window-init :pre-init)
	   (list :pre-window-form (rest item)))
          ((:init :setup :initialization :initialization-step :loading)
           (list :setup-form (rest item)))
	  ((:post-window-init :pre-loop-init :pre-loop)
	   (list :post-window-form (rest item)))))))

(defun get-event-form (form-key list)
  (cadr (assoc form-key list)))

(defun initialize-font (font-filename font-extention)
  (setf *default-font*
        (handler-case 
            (name (create-font font-filename :file-extention font-extention))
          (error ()
            nil))))


(defun set-icon (icon-filename icon-path)
  (let ((icon (load-bmp icon-filename icon-path)))
    (sdl-set-icon icon 0)))

(defmacro with-window (width height title fps icon-filename icon-path dt font-filename font-extention resizable &body body)
  `(sdl:with-init ()
     (sdl:init-video)
     (sdl-ttf:init-ttf)
     (initialize-font ,font-filename ,font-extention)
     (when ,icon-filename
       (set-icon ,icon-filename ,icon-path))
     (sdl:enable-unicode)
     (sdl:window ,width ,height :title-caption ,title :fps (make-instance 'sdl:fps-mixed :dt ,dt)  :resizable ,resizable)
     (setf (sdl:frame-rate) ,fps)
     ,@body))


(defun remove-key-states (&aux new-list)
  (dolist (current-key (sdl:key-state-p))
    (let ((key (assoc current-key *keys-pressed*)))
      (when key
	(push key new-list))))
  new-list)
(defparameter *tmp1* nil)
(defparameter *tmp2* nil)

(defmacro with-events (event-forms clear-color auto-draw)
  "The SDL-Event chain"
  (alexandria:with-gensyms (previous-mouse-x previous-mouse-y) ; Used to have position-variable, unsure what it was intended for, had no use and was removed
    `(let ((,previous-mouse-x 0) (,previous-mouse-y 0))
       (unwind-protect (progn
			 (sdl:with-events ()
			   (:quit-event () ,@(get-event-form :end-form event-forms) t)
			   (:key-down-event (:unicode unicode) ;; Rewrite to properly handle special symbols
					    (dolist (key (sdl:key-state-p))
					      (unless (assoc key *keys-pressed*)
						(push (cons key (if (> unicode 0)
								    (code-char unicode)
								    (filter-special-keys key)))
						      *keys-pressed*)))
					    ,@(get-event-form :key-down-form event-forms))
			   (:key-up-event ()
					  (setf *keys-pressed* (remove-key-states))
					  ,@(get-event-form :key-up-form event-forms))
			   (:mouse-motion-event (:x x :y y)
						(setf *mouse-move-direction* (mouse-move-direction ,previous-mouse-x x ,previous-mouse-y y)
						      ,previous-mouse-x x
						      ,previous-mouse-y y)
						,@(get-event-form :mouse-move-form event-forms))

			   (:mouse-button-down-event (:button button)
						     (unless (member button *current-mouse-buttons*)
						       (push button *current-mouse-buttons*))
						     ,@(get-event-form :mouse-down-form event-forms))
			   
			   (:mouse-button-up-event (:button button)
						   (setf *current-mouse-buttons* (remove button *current-mouse-buttons*))
						   ,@(get-event-form :mouse-up-form event-forms))
                           (:video-resize-event (:w w :h h)
                                                (setf *width* w
                                                      *height* h)
                                                (sdl:resize-window w h)
                                                ,@(get-event-form :video-resize-event event-forms))
			   (:sys-wm-event ()
					  ,@(get-event-form :window-focus-form event-forms))
			   (:idle ()
                                  
				  (sdl:clear-display  (if ,clear-color (filter-color ,clear-color) (get-color black)))
                                  ;;Animation update

                                  ;; intended for loading
                                  (when (check-state :setup)
				    ,@(get-event-form :setup-form event-forms))
                                  
                                  (when (check-state :quit)
                                    (sdl:push-quit-event))

				  (sdl:with-timestep ()
				    ,@(get-event-form :timestep-form event-forms)
                                    (when (check-state :game)
                                      (update-animations)))
                                  
                                  ,@(get-event-form :pre-auto-draw-form event-forms)
                                  
				  (when ,auto-draw
				    (dolist (image *auto-draw-list*)
				      (draw-image image)))
				  
                                  ,@(get-event-form :post-auto-draw-form event-forms)
                                  
                                  ,@(get-event-form :ui-form event-forms)
                                  
                                  
                                  (sdl:update-display))
			   (sdl-ttf:quit-ttf)))
         
         ;;; Cleanup section
	 (clear-globals)))))

(defmacro start ((&key (width 640) (height 480) (title "working title") (fps 60)
		    (default-font "vera")
                    (default-font-extention "ttf")
                    resizable
                    (dt 10)
		    (auto-draw t)
                    (asset-path
                     ;; If the ASDF system is not the same as the package-name this won't work,
                     ;; so we'll default to an empty string in that case
                     (handler-case
                         (asdf:system-relative-pathname (intern (package-name *package*)) "")
                       (error (c) "")))
                    (font-path "")
                    (image-path "")
                    (icon-filename)
                    (icon-path asset-path)
		    clear-color)
                 &rest body)
  "Arguments:
:width (integer): Width of the window.
:height (integer): Height of the window.
:title (string): Title of the window.
:fps (integer): Frames per second the window should run at.
:icon (string) : Filename of icon (must be an bmp file)
:default-font (string or Pathname): filename (without the extention) of the font that will be default. Defaults to [vera]
:default-font-extention (string or pathname) : The extention for the font-file (without '.'), defaults to [ttf].
:resizable (boolean): Whether the window can be resized or not.
:auto-draw (boolean): Automatically draw loaded images or animations.
:asset-path (String or Pathname): Default path to look for any files. - Default is your-package
:font-path (string or pathname): Default path to look for font files. Will default to asset-path if not set.
:icon-filename (string) : filename without extention of the window-icon you want. Note: File must be an [BMP] file.
:icon-path (string or pathname) : path to where the icon-file is. Defaults to asset-path
:clear-color (SDL:Color): Base color used to reset the window."
  (unwind-protect (progn
		    (let ((event-forms (filter-events body)))
		      `(progn 
			 ,@(get-event-form :pre-window-form event-forms)
			 (init-globals ,width ,height ,asset-path ,font-path ,image-path)
			 (with-window ,width ,height ,title ,fps ,icon-filename ,icon-path ,dt ,default-font ,default-font-extention ,resizable
			   ,@(get-event-form :post-window-form event-forms)
			   (with-events ,event-forms ,clear-color ,auto-draw)))))))



;;; Create the ability to compile into an executable easily.
;;; Sets the asset and font-path to relative to the .executable
;;; Transfer all .DLL\Fonts and save the executable to an bin folder.
(defun make-executable ()
  "create an executable for current game/project")
