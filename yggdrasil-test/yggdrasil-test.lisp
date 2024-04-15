(in-package #:yggdrasil-test)

(defparameter *asset-path* (asdf:system-relative-pathname :yggdrasil "assets/"))

(defun main2 (&aux (width 640) (height 480))
  (let ((w 50) (h 50))
    (yg:start
      (:width width :height height
		:font-path *asset-path*) ; necessary due to how font system work, you can't have an empty font
      (:key-down 
        (cond ((yg:is-key #\+)
		(incf w 5)
  		(incf h 5))
    	      ((yg:is-key #\-)
	   	(decf w 5)
     		(decf h 5))))
       (:draw
         (yg:draw-rectangle (vector (- (round width 2) (round w 2)) (- (round height 2) (round h 2)) w h)
                            :color (yg:get-color green) :filled t)))))

(defun main (&aux (width 640) (height 480) (title "yggdrasil-testing"))
  (bt:make-thread
   (lambda ()
     (let (image)
       (yg:start
        (:width 200 :height 200 :title "title" ; :icon-filename "lisp" 
            :asset-path *asset-path*)
           ;(:init ;(setf image (yg:load-image "lisp.bmp" :x 150
                  ;                                      :y 150))
                  ;)
           (:draw
            (yg:draw-rectangle #(50 50 50 50) :filled t)
            ;(yg:draw-image image)
           ; (yg:draw-string 0 0 "default") 
            ;(yg:draw-string 0 50 "testing-new-font" :size 6)
                                        ;(yg:draw-string 0 100 "testing-new-font2" :size 80)
            ))))
   :name title))
