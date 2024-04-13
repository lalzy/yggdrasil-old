(in-package #:yggdrasil-test)

(defun main (&aux (width 640) (height 480) (title "yggdrasil-testing") (asset-path (asdf:system-relative-pathname :lispbuilder-sdl "assets/")))
  (bt:make-thread
   (lambda ()
     (let (font)
       (yg:start
           (:width width :height height :title title :icon-filename "lisp" 
            :asset-path asset-path)
           (:draw
            (yg::draw-string 0 0 "default") 
            (yg::draw-string 0 50 "testing-new-font" :size 6)
            (yg::draw-string 0 100 "testing-new-font2" :size 80)))))
   :name title))
