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

(defun filter-default-size (default-size sizes)
  (let ((return-value (find default-size sizes)))
    (if return-value
        return-value
        (car sizes))))

(defun create-font (font-name &key (default-size *default-font-size*)
                                (file-path (get-path font))
                                        ;(file-path *font-path*)
                                (file-extention "ttf") (sizes (loop :for i :from 6 :to 100 :collect i)))
  "Font-name = name of the font-file name, without extention
File-path = path to the font-file without the name
file-extention = the file's extention without '.'
default-size = what size will be used if no size is specified when drawing the font, defaults to 14, if no font of size 14 exist, defaults to first size in sizes (6 if not sizes passed)
sizes = a list of all sizes the font will support. By default supports 6 - 100 (each size is a separate font object, so more sizes = more memory)"
  ;; Removes font if it already exists.
  (setf *fonts* (remove (find-font font-name) *fonts*)) 
  
  (let ((font-object
          (make-instance 'font
                         :fonts (create-font-helper-loop sizes (create-file-path font-name file-path file-extention))
                         :default-size (filter-default-size default-size sizes)
                         :name font-name)))
    (push font-object *Fonts*)
    (unless *default-font* (setf *default-font* font-object)) ; allocate created-font as default if no default exist
    font-object))


(defun get-font-size (font-object size)
  (when font-object
    ;; If size hasn't been specified, sets it to the default-size for this font
    (unless size
      (setf size (default-size font-object)))
    
    (or (cdr (assoc size (fonts font-object))) (error (format nil "the font ~a does not have the size [~a]" (name font-object) size)))))

(defun find-font (font-name)
  (find font-name *fonts* :test
        (lambda (font-name font-object)
          (string-equal (name font-object) font-name))))

(defun get-font (font-name &optional size)
  "Get the sdl-font from the *fonts* list
font-name = name as string to the font
size = the size of the font"
  (get-font-size (find-font font-name) size))

(defun font-error-check (passed-font size)
  "Checks if the fonts exist or not, then passes the font-object back if it does. If it doesn't, will call the appropriate error."
  (let ((font (get-font passed-font size)))
    (cond ((and (null font) (null *Default-font*))
           (error "no fonts created"))
          ((null font)
           (error (format nil "font ~a does not exist" passed-font)))
          (t font))))

;; Rewrite color to 
(defun draw-string (x y string &key (font *default-font*) size (color (get-color white)) draw-type (bg-color (get-color black)))
  "Draws a string on screen.
x\y [integer] = the screen position to draw on
string [string] = the string to display
font [string] = name of file-name(without extention) of the font to draw
size [integer] = the size of the font you want to draw (as has been created with create-font. defaults to the font's default
color = either a color from (get-color), or a vector of RGB values in form of values from 0 to 255 #(R G B), ex #(255 32 0)
draw-type = The type of string to draw [shaded, blended, solid] - defaults to solid.
BG-color = background colour of the string
"
  (let ((sdl-font-object (font-error-check font size)))
    (if (sdl:video-init-p)
        (multiple-value-bind (w h) (text-size string sdl-font-object)
          (unless (edge-collision-check (vector x y w h) t)
	    (case draw-type
	      (:shaded (sdl:draw-string-shaded-* string x y (filter-color color) bg-color :font sdl-font-object))
	      (:blended (sdl:draw-string-blended-* string x y :color (filter-color color) :font sdl-font-object))
	      (t (sdl:draw-string-solid-* string x y :color (filter-color color) :font sdl-font-object)))	
	    ))
        (error "SDL has not been initialized"))))

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
