(in-package #:yggdrasil)
(defparameter *default-font* nil) ; Default font-name that is used if no font is provided when drawing strings.

(defparameter *fonts* nil) ; List of all the fonts
(defparameter *default-font-size* 14) ; Default size when no size is specified at font-creation

(defclass font ()
  ((fonts :initarg :fonts :accessor fonts
          :documentation "vector holding all the fonts in an AList[size.font-object]") 
   (name :initarg :name :accessor name
         :documentation "the filename of the font without extention")
   (default-size :initarg :default-size :accessor default-size
                 :documentation "the size that will be drawn by default when calling the font and not specifying a size"))
  (:documentation "test"))

(defun create-font-helper-loop (sizes file)
  (loop :for size :in sizes
        :collect
        (cons size (sdl:initialise-font
                    (make-instance 'sdl:ttf-font-definition :size size :filename file)))))


(defun create-font (font-name &key (default-size *default-font-size*)
                                (file-path (get-path font))
                                ;(file-path *font-path*)
                                (file-extention "ttf") (sizes (loop :for i :from 6 :to 100 :collect i)))
  "Font-name = name of the font-file name, without extention
File-path = path to the font-file without the name
file-extention = the file's extention without '.'
default-size = what size will be used if no size is specified when drawing the font
sizes = a list of all sizes the font will support. By default supports 6 - 100 (each size is a separate font object, so more sizes = more memory)"
  (let ((font-object
          (make-instance 'font
                         :fonts (create-font-helper-loop sizes (create-file-path font-name file-path file-extention))
                         :default-size default-size
                         :name font-name)))
    (push font-object *Fonts*)
    font-object))


(defun get-font-size (font-object size)
  (when font-object
    ;; If size hasn't been specified, sets it to the default-size for this font
    (unless size
      (setf size (default-size font-object)))
    
    (or (cdr (assoc size (fonts font-object))) (error (format nil "the font ~a does not have the size [~a]" (name font-object) size)))))

(defun get-font (font-name &optional size)
  "Get the sdl-font from the *fonts* list
font-name = name as string to the font
size = the size of the font"
  (get-font-size (find font-name *fonts* :test
                       (lambda (font-name font-object)
                         (string-equal (name font-object) font-name))) size))

(defun draw-string (x y string &key (font *default-font*) size (color (get-color white)) draw-type (bg-color (get-color black)))
  (let ((sdl-font-object (or (get-font font size) (error (format nil "font ~a does not exist" font)))))
    (if (sdl:video-init-p)
        (multiple-value-bind (w h) (text-size string sdl-font-object)
          (unless (edge-collision-check (vector x y w h) t)
	    (case draw-type
	      (:shaded (sdl:draw-string-shaded-* string x y color bg-color :font sdl-font-object))
	      (:blended (sdl:draw-string-blended-* string x y :color color :font sdl-font-object))
	      (t (sdl:draw-string-solid-* string x y :color color :font sdl-font-object)))	
	    ))
        (error "SDL has not been initialized"))))


;; Test it all with new drawing before rewriting the old drawing to use new font system
(defun draw-string-new (x y string &key (font *default-font*) size)
  ;;(break (format nil "~a, ~a" *default-font* (get-font font size)))
  (unless (get-font font size)
    (error (format nil "font ~a does not exist" font)))
  (sdl:draw-string-solid-* string x y :color (get-color white) :font (get-font font size)))


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
