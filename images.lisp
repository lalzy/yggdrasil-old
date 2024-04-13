(in-package :yggdrasil)
;;; Double-check cells for vertical-flipping, to ensure this works correctly.
;;;  > And or fix it.


(defclass image (rectangle)
  ((image-name :initarg :name :accessor name
               :documentation "Name for this image, used in auto-drawing")
   (image-data :initarg :image :accessor image-data
               :documentation "The SDL image data") ;With openGL, this will become pixel-data
   (sprite-cells :initarg :cells :accessor cell-list
                 :documentation "Cell-list for the image")
   (flipped :initform nil :accessor flipped
            :documentation "wether the image has been flipped or not")
   (cell-count :initarg :cell-count :accessor cell-count
               :documentation "Amount of cells the image has"))
  (:documentation ""))


(defun allocate-cells (width height by-x by-y)
  "Define the cell-positions for the sprite-sheet
Width & height = resolution
Size-x\y = where the pixles will separate the cells"
  (loop :for y :from 0 :to (if (= height by-y) 0 height) :by by-y
        :append
        (loop :for x :from 0 :to (if (= width by-x) 0 (- width by-x)) :by by-x
              :collect (vector x y by-x by-y))))


;; Change to use tree
(defparameter *images* (make-array 0 :adjustable t :fill-pointer 0))
(defparameter *auto-draw-list* nil)

(defun remove-image (image-name))
(defun find-image (image-name)
  (doarray (image *images*)
           (when (string-equal (name image) image-name)
             (return-from find-image image))))


(defun remove-from-auto-draw (image-name))
(defun add-to-auto-draw (image-name)
  (push (find-image image-name) *auto-draw-list*))

(defun load-image (filename &key (path *asset-path*) (x 0) (y 0) color-key color-key-at (alpha #xFF) (image-name filename) auto-draw sprite-cells)
  (let* ((arguments `(,(merge-pathnames filename path)
		    ,@(when color-key (list :color-key color-key))
		    ,@(when color-key-at (list :color-key-at color-key-at))
		     :alpha ,alpha))
	 (surface (apply #'sdl:load-and-convert-image arguments))
	 (image (make-instance 'image :name image-name :image surface :w (sdl:width surface) :h (sdl:height surface) :x x :y y :cells sprite-cells :cell-count (length sprite-cells))))
    (vector-push-extend image *images*)

    (when sprite-cells
      (setf (sdl:cells (image-data image)) sprite-cells))
    
    (when auto-draw
      (push image *auto-draw-list*))
    image))


(defun draw-image-with-name (image-name &key x y)
  (let ((arguments
	  `( ,(find-image image-name)
	     ,@(when x (list :x x))
	     ,@(when y (list :y y)))))
    (apply #'draw-image arguments)))
;; (draw-image (find-image image-name) :x x :y y))

(defun flip-image (image &key  (horizontal t) vertical)
  "Flips the image.
Parameters:

horizontal - flips it horizontally
vertical - flips it vertically"
  (let ((flipped-image (sdl-gfx:zoom-surface (if horizontal -1 1) (if vertical -1 1) :surface (image-data image))))

    ;;(setf (flipped image) (not (flipped image)))
    (toggle-variable (flipped image))
    
    (when (cell-list image)
      (setf (sdl:cells flipped-image) (if (flipped image)
                                          (reverse (cell-list image))
                                          (cell-list image))))
    
    (setf (image-data image) flipped-image)))

;; Reduntant in SDL, not reduntant with OpenGL whenever I transition
(defun draw-image (image &key (x (x image)) (y (y image)) cell)
  ;; Ensure image cordinates is always same as drawn cordinates
  (unless (and image (edge-collision-check image t))
    (setf (x image) x
          (y image) y)
    (sdl:draw-surface-at-* (image-data image) x y :cell cell)))
