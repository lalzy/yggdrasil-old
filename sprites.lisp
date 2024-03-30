(in-package :yggdrasil)

;;; Example sprites:
;;; 120x80

#|
(defclass sprite-sheet ()
  ((image-vector ) ;images
   (sprite-count ))
  (:documentation ""))

(defun load-sprite-sheet (filename &key (path *asset-path*) (x 0) (y 0) color-key color-key-at (alpha #xFF) (image-name filename) auto-draw)
  (make-instance 'sprite-sheet))|#
