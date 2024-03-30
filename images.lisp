(in-package :yggdrasil)

(defclass image (rectangle)
  ((image-name :initarg :name :accessor name)
   (image-data :initarg :image)
   (sprite-cells :initarg :cells :accessor cell-list)
   (cell-count :initarg :cell-count :accessor cell-count)))


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
    (when (string-equal (slot-value image 'image-name) image-name)
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
      (setf (sdl:cells (slot-value image 'image-data)) sprite-cells))
    
    (when auto-draw
      (push image *auto-draw-list*))
    image))


(defun draw-image-with-name (image-name &key x y)
  (let ((arguments
	  `( ,(find-image image-name)
	     ,@(when x (list :x x))
	     ,@(when y (list :y y)))))
    (apply #'draw-image arguments)))
                                        ; (draw-image (find-image image-name) :x x :y y))

;; Reduntant in SDL, not reduntant with OpenGL
(defun draw-image (image &key (x (slot-value image 'x)) (y (slot-value image 'y)) cell)
  ;; Ensure image cordinates is always same as drawn cordinates
  (unless (and image (edge-collision-check image t))
    (setf (slot-value image 'x) x
	  (slot-value image 'y) y)
    (sdl:draw-surface-at-* (slot-value image 'image-data) x y :cell cell)))
