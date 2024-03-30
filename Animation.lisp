(in-package :yggdrasil)

;; Rework animation-set class
;;    > Need to keep track of amount of cells


;; 

;; Rewrite to use named system, so that it's easy to remove\delete animations
(defparameter *animated-sprites-to-animate* nil)


(defclass animation-set ()
  ((animation-name :initarg :interval :accessor name)
   (sprite :initarg :sprite :accessor sprite)
   (interval :initarg :interval :accessor animation-interval)
   (current-animation-cell :initform 0 :accessor current-animation)
   (animation-counter :initform 0 :accessor animation-counter))
  (:documentation ""))

(defun create-animated-sprite (filename sprite-cells &key (path *asset-path*) (x 0) (y 0) color-key color-key-at (alpha #xFF) (image-name filename) auto-draw
                                                       (interval 5) start-playing)
  (let ((animated-sprite (make-instance 'animation-set :sprite (load-image filename :path path :x x :y y :color-key color-key :color-key-at color-key-at :alpha alpha :image-name image-name :auto-draw auto-draw :sprite-cells sprite-cells) :interval interval)))
    (when start-playing
      (push animated-sprite *animated-sprites-to-animate*))
    animated-sprite))

(defun draw-animation (animation-object &key x y )
  (let ((sprite (sprite animation-object)))
    (draw-image (sprite animation-object) :x (if x x (x sprite)) :y (if y y (y sprite)) :cell (current-animation animation-object))))

(defun play-animation (animation-object)
  (unless (find animation-object *animated-sprites-to-animate*)
    (push animation-object *animated-sprites-to-animate*)))


(defun next-animation-cell (animation-object)
  (if (>= (animation-counter animation-object) (animation-interval animation-object))
      (progn (if (>= (current-animation animation-object) (1- (cell-count (sprite animation-object))))
                 (progn
                   (setf (current-animation animation-object) 0)
                   (setf *animated-sprites-to-animate* (remove animation-object *animated-sprites-to-animate*)))
                  
                 (incf (current-animation animation-object)))
             (setf (animation-counter animation-object) 0))
      (incf (animation-counter animation-object))))

(defun update-animations ()
  (dolist (animation *animated-sprites-to-animate*)
      (next-animation-cell animation)))
