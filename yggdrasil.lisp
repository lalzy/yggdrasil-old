(in-package #:yggdrasil)

#||

What to do:
  Text Area
  Context Menu
  State System
  

||#


(defparameter *width* nil)
(defparameter *height* nil)
(defparameter *asset-path* nil)


(defun init-globals (width height asset-path)
  (setf *width* width
	*height* height
	*asset-path* asset-path))

(defun filter-events (body)
  "Filters out the synonymous keywords into a unified keyword"
  (iter (for item :in body)
	(collect
	    (case (first item)
	      ((:main :idle)
	       (list :main-form (rest item)))
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
	      ((:pre-window-init :pre-init)
	       (list :pre-window-form (rest item)))
	      ((:time-step :physics :update :update-loop)
	       (list :timestep-form (rest item)))
	      ((:post-window-init :post-init :init)
	       (list :post-window-form (rest item)))))))

(defun get-event-form (form-key list)
  (cadr (assoc form-key list)))

(defun initialize-font (font)
  (if (null font)
      (setf *default-font* (sdl:initialise-font sdl:*ttf-font-vera*))
      (setf *default-font* font)))

(defmacro with-window (width height title fps font &body body)
  `(sdl:with-init ()
     (sdl:init-video)
     (sdl-ttf:init-ttf)
     
     (initialize-font ,font)
     
     (sdl:enable-unicode)
     (sdl:window ,width ,height :title-caption ,title :fps (make-instance 'sdl:fps-mixed :dt 1))
     (setf (sdl:frame-rate) ,fps)
     ,@body))


(defun remove-key-states (&aux new-list)
  (dolist (current-key (sdl:key-state-p))
    (let ((key (assoc current-key *keys-pressed*)))
      (when key
	(push key new-list))))
  new-list)

(defmacro with-events (event-forms clear-color auto-draw)
  "The SDL-Event chain"
  (alexandria:with-gensyms (previous-mouse-x previous-mouse-y position-variable)
    `(let ((,previous-mouse-x 0) (,previous-mouse-y 0) ,position-variable)
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
			   (:key-up-event (:unicode unicode)
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
			   (:sys-wm-event ()
					  ,@(get-event-form :window-focus-form event-forms))
			   (:idle ()
				  (sdl:clear-display  (if ,clear-color ,clear-color (get-color black)))

				  (sdl:with-timestep ()
				    ,@(get-event-form :timestep-form event-forms))
				  
				  (when ,auto-draw
				    (dolist (image *auto-draw-list*)
				      (draw-image image)))
				  
				  ,@(get-event-form :main-form event-forms)

				  (sdl:update-display)))
			 
			 (sdl-ttf:quit-ttf))
	 (setf *images* (make-array 0 :adjustable t :fill-pointer 0)
	       *auto-draw-list* nil)))))

(defmacro start ((&key width height (title "working title") (fps 60)
		    default-font
		    (auto-draw t)
		    (asset-path "")
		    clear-color) &rest body)
  (unwind-protect (progn
		    (let ((event-forms (filter-events body)))
		      `(progn 
			 ,@(get-event-form :pre-window-form event-forms)
			 (init-globals ,width ,height ,asset-path)
			 (with-window ,width ,height ,title ,fps ,default-font
			   ,@(get-event-form :post-window-form event-forms)
			   (with-events ,event-forms ,clear-color ,auto-draw)))))))


