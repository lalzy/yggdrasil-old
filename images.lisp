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
   (vertically-flipped :initform nil :accessor vertically-flipped
                       :documentation "wether the image has been flipped or not")
   (horizontally-flipped :initform nil :accessor horizontally-flipped)
   (current-cell :initform 0 :accessor current-cell)
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
(defparameter *images* nil)
(defparameter *auto-draw-list* nil)

(defun find-image-helper (image-name in-autodraw)
  "use the image name[string] to find the image from the list of images in auto-draw"
  (let ((result (dolist (image (if in-autodraw *auto-draw-list* *images*))
                  (when (string-equal (name image) image-name)
                    (return-from find-image-helper image)))))
    (if result
        result
        (error "image named ~s does not exist" image-name))))

(defmethod find-image ((image image) &optional in-autodraw)
  (find-image-helper (name image) in-autodraw))

(defmethod find-image ((image string) &optional in-autodraw)
  (find-image-helper image in-autodraw))

(defmacro image-deleter (image list)
  "replace old list with new list that removed the object"
  `(progn
     (sdl:free (image-data image)) ;; Removes sdl-surface from memory when we delete it
     (setf ,list (remove ,image ,list))
     ;; Call\force garbage collection here.
     ))


(defun delete-image-helper (image-object auto-draw)
  "picks which list to delete image from"
  (if auto-draw
      (image-deleter image-object *auto-draw-list*)
      (image-deleter image-object *images*)))
  
(defmethod delete-image ((image image) &optional auto-draw)
  (delete-image-helper image auto-draw))

(defmethod delete-image ((image string) &optional auto-draw)
  (delete-image-helper (find-image image auto-draw) auto-draw))


(defun add-image-to-autodraw-helper (image-name)
  "Adds the passed image [string] to the auto-draw list"
  (push (find-image image-name t) *auto-draw-list*))

(defmethod add-image-to-autodraw ((image image))
  (add-image-to-autodraw-helper (name image)))

(defmethod add-image-to-autodraw ((image string))
  (add-image-to-autodraw-helper image))

(defun create-image-arguments (filename path color-key color-key-at alpha)
  `(,(create-file-path filename path)
    ,@(when color-key (list :color-key (filter-color color-key)))
    ,@(when color-key-at (list :color-key-at color-key-at))
    :alpha ,alpha))

(defun load-image (filename &key (path (get-path image)) (x 0) (y 0) color-key color-key-at (alpha #xFF) (image-name (filter-extention filename)) auto-draw sprite-cells)
  (let* ((arguments (create-image-arguments filename path color-key color-key-at alpha))
	 (surface (apply #'sdl:load-and-convert-image arguments))
	 (image (make-instance 'image :name image-name :image surface
                                      :w (sdl:width surface) :h (sdl:height surface)
                                      :x x :y y :cells sprite-cells :cell-count (length sprite-cells))))

    (when sprite-cells
      (setf (sdl:cells (image-data image)) sprite-cells))

    (push image *images*)

    (when auto-draw
      (push image *auto-draw-list*))
    image))


(defun flip-image-helper (image horizontal vertical)
  "Flips the image.
Parameters:

horizontal - flips it horizontally
vertical - flips it vertically"
  (let ((flipped-image (sdl-gfx:zoom-surface (if horizontal -1 1) (if vertical -1 1) :surface (image-data image))))
    ;;(setf (flipped image) (not (flipped image)))
    (when horizontal (toggle-variable (horizontally-flipped image)))
    (when vertical (toggle-variable (vertically-flipped image)))
    (when (cell-list image)
      (setf (sdl:cells flipped-image) (if (horizontally-flipped image)
                                          (reverse (cell-list image))
                                          (cell-list image))))
    
    (sdl:free (image-data image)) ;; Clean up old image before we set new one
    (setf (image-data image) flipped-image)))



(defmethod flip-image ((image image) &key (horizontal t) vertical)
  (flip-image-helper image horizontal vertical))

(defmethod flip-image ((image string) &key (horizontal t) vertical)
  (flip-image-helper (find-image image) horizontal vertical))

(defun calc-flipped-y-pos (y height)
  (+ y (round (/ height 2))))

(defun get-cell-h (image cell)
  (h (nth cell (cell-list image))))

;; Reduntant in SDL, not reduntant with OpenGL whenever I transition
(defun draw-image-helper (image x y cell)
  (let* ((x-pos (if x x (x image)))
         (base-y (if y y (y image)))
         ;; Image is sligthly off when flipping vertically, this is to correct that.
         (y-pos (if (vertically-flipped image)
                    (if cell
                        (calc-flipped-y-pos base-y (get-cell-h image cell))
                        (calc-flipped-y-pos base-y (h image))) 
                    base-y)))
    
    ;; Ensure image cordinates is always same as drawn cordinates
    (cond ((typep image 'image)
           (unless (and image (edge-collision-check image t))
             (sdl:draw-surface-at-* (image-data image) x-pos y-pos
                                    :cell cell)))
          ((null image)
           (error (format nil "passed variable is empty. You need to provide an image-object")))
          (t (error (format nil "~a is not of type image. You need an yggdrasil:image object" image))))))

(defmethod draw-image ((image image) &key x y (cell (current-cell image)))
  (draw-image-helper image x y cell))

(defmethod draw-image ((image string) &key x y (cell (current-cell image)))
  (draw-image-helper (find-image image) x y cell))

