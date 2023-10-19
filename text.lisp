(in-package #:yggdrasil)
(defparameter *default-font* nil)

(defun text-size (string &optional (font *default-font*))
  (values 
   (sdl:get-font-size string :size :w  :font font)
   (sdl:get-font-size string :size :h :font font)))

(defun get-longest-width (text-list &optional (font *default-font*))
  (let ((longest 0))
    (dolist (text text-list)
      (let ((width (text-size text font)))
	(setf longest (max longest width))))
    longest))

(defun get-total-text-size (text-list &optional (font *default-font*))
  (values (get-longest-width text-list font) (* (length text-list) (nth-value 1 (text-size (elt text-list 0) font)))))

;; Created nested-size

(defun draw-string (x y string &key (font *default-font*) (color (get-color white)) draw-type (bg-color (get-color black)))
  (if (sdl:video-init-p)
    (multiple-value-bind (w h) (text-size string font)
      (unless (edge-collision-check (vector x y w h) t)
	(case draw-type
	  (:shaded (sdl:draw-string-shaded-* string x y color bg-color :font font))
	  (:blended (sdl:draw-string-blended-* string x y :color color :font font))
	  (t (sdl:draw-string-solid-* string x y :color color :font font)))	
	))
    (error "SDL has not been initialized")))
  

;; to unify making a font for when we go to OpenGL
(defun make-font (font-name &key (file-path "") (size 15))
  (sdl:initialise-font
   (make-instance 'sdl:ttf-font-definition :size size :filename (merge-pathnames font-name file-path))))
