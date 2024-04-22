(in-package :yggdrasil)

(defparameter *animated-sprites* nil) ; List of all the animated sprites created, so that we can reference with name
(defparameter *animated-sprites-to-animate* nil) ; sprites currently animating
;;; TODO:
;;;   Change play-animation system to work with other states than just 'game' state.
;;;   Add support for pausing animations

(defclass animation-set ()
  ((animation-name :initarg :interval :accessor name)
   (sprite :initarg :sprite :accessor sprite)
   (interval :initarg :interval :accessor animation-interval)
   (current-animation-cell :initform 0 :accessor current-animation)
   (animation-counter :initform 0 :accessor animation-counter))
  (:documentation ""))

(defun create-animated-sprite (filename sprite-cells &key (path (get-path image)) (x 0) (y 0) color-key color-key-at (alpha #xFF) (image-name filename) auto-draw
                                                       (interval 5) start-playing)
  (let ((animated-sprite (make-instance 'animation-set
                                        ;; Create image
                                        :sprite (load-image filename :path path :x x :y y :color-key color-key :color-key-at color-key-at :alpha alpha
                                                                     :image-name image-name :auto-draw auto-draw :sprite-cells sprite-cells)
                                        :interval interval)))
                                        ; Add the object to the list of objects so we can reference it later with name
    (push animated-sprite *animated-sprites*)
    
    (when start-playing
      (push animated-sprite *animated-sprites-to-animate*))
    ;; return the sprite
    animated-sprite))

;; generic defined in images.lisp
(defmethod draw-image ((animation-object animation-set) &key x y cell)
  (with-accessors ((image sprite)) animation-object
    (draw-image-helper image (if x x (x image)) (if y y (y image)) (if cell cell (current-cell image)))))

;; Can look cleaner writing 'draw-animation' rather than 'draw-image'
(defun draw-animation (animation-object &key x y)
  "Alternative to draw-image. May look clearer in code, simply redirects to draw-image.
animation-object = an instanced object of animation-set class made with #'create-animation.
x = the x-cordinate to draw from. Defaults to iamge's X
y = the y-cordinate to draw from. Defaults to image's Y"
  (draw-image animation-object :x x :y y :cell nil))

;; rewrite to optionally take a state to draw in.
(defun play-animation (animation-object)
  (unless (find animation-object *animated-sprites-to-animate*)
    (push animation-object *animated-sprites-to-animate*)))

(defun is-animation-playing? (animation-object)
  (find animation-object *animated-sprites-to-animate*))


(defun next-animation-cell (animation-object)
  "returns object when finished, otherwise returns nil"
  (with-accessors ((counter animation-counter) (interval animation-interval)) animation-object
    (with-accessors ((cell current-cell) (cell-count cell-count)) (sprite animation-object)
      (if (>= counter interval)
          (progn (if (>= cell (1- cell-count))
                     (progn
                       (setf cell 0)
                       (setf counter 0)
                       (setf *animated-sprites-to-animate* (remove animation-object *animated-sprites-to-animate*)))
                     
                     (incf cell))
                 (setf counter 0))
          (incf counter))
      nil)))

(defun update-animations ()
  (dolist (animation *animated-sprites-to-animate*)
      (next-animation-cell animation)))

;; generic defined in images.lisp
(defmethod flip-image ((animation-object animation-set) &key  (horizontal t) vertical)
  (flip-image-helper (sprite animation-object) horizontal vertical))

(defun flip-animation (animation-object &key  (horizontal t) vertical)
  "Alternative to flip-image. May look clearer in code, simply redirects to flip-image.
animation-object = an instanced object of animation-set class made with #'create-animation.
horizontal = to flip horizontally [default]
vertical = to flip vertically"
  (flip-image animation-object :horizontal horizontal :vertical vertical))

(defun pause-animation (animation-object))
