(in-package #:yggdrasil)

;; Must be redone for OpenGL
(defparameter *SDL-colors* `((white ,(sdl:color :r 255 :g 255 :b 255))
			     (black ,(sdl:color :r 0 :g 0 :b 0))
			     (dark-gray ,(sdl:color :r 50 :g 50 :b 50))
			     (gray ,(sdl:color :r 160 :g 160 :b  160))
			     (light-gray ,(sdl:color :r 211 :g 211 :b 211))
			     (green ,(sdl:color :r 0 :g 255 :b 0))
			     (red ,(sdl:color :r 255 :g 0 :b 0))
			     (blue ,(sdl:color :r 0 :g 0 :b 255))
			     (cyan ,(sdl:color :r 0 :g 255 :b 255))
			     (orange ,(sdl:color :r 255 :g 165  :b 0))
			     (indigo ,(sdl:color :r 75 :g 0 :b 130))
			     (purple ,(sdl:color :r 128 :g 0 :b 128))
			     (yellow ,(sdl:color :r 255 :g 255 :b 0))))

(defun color-error (color)
      (error "argument:[~a] of type:[~a] is not a valid sequence for color! must be 3 numbers valued between 0 - 255" color (type-of color)))

(defun filter-color-sequence-helper (r g b)
  (sdl:color :r r :g g :b b))

(defmethod filter-color (color)
  (color-error color))

(defmethod filter-color ((color sdl:color))
  color)

(defgeneric filter-color (color)
  (:documentation "Ensures that whatever is passed is a valid color to be used for drawing.
Color must be either an SDL-object (as created by (yg:get-color)), or a sequence of 3 numbers between 0 - 255"))

(defmethod filter-color ((color list))
  (if (typep color 'valid-color)
      (filter-color-sequence-helper (first color) (second color) (third color))
      (color-error color)))

(defmethod filter-color ((color vector))
  (if (typep color 'valid-color)
      (filter-color-sequence-helper (aref color 0) (aref color 1) (aref color 2))
      (color-error color)))

;; must be rewritten for OpenGL
(defun set-color (color-symbol &key (r 0) (g 0) (b 0))
  (push (list color-symbol (sdl:color :r r :g g :b b)) *sdl-colors*))

(defun find-color (color)
  "helper function for get-color"
  (cadr (assoc color *SDL-colors* :test #'string=)))

(defmacro get-color (color)
  `(find-color ',color))

