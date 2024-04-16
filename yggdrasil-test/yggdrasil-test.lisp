(in-package #:yggdrasil-test)

(defparameter *asset-path* (asdf:system-relative-pathname :yggdrasil "assets/"))

(defun main2 (&aux (width 640) (height 480))
  (let ((w 50) (h 50))
    (yg:start
      (:width width :height height
       ;:font-path *asset-path*
       ) ; necessary due to how font system work, you can't have an empty font
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
     (let (image
           font)
       (yg:start
        (:width 200 :height 200 :title "title"
         :asset-path *asset-path*
         )
        (:key-down (format t "all fonts = ~a~%first font = ~a~%default size = ~a~&"  yg::*fonts* (yg::fonts (car yg::*fonts*)) (yg::default-size (car yg::*fonts*))))
        (:init

         ;; Only one font is kept.
        ; (setf font (yg:create-font "vera" :sizes '(3 10 5)))
         ;(setf font (yg:create-font "vera"))
         ;(setf font (yg:create-font "vera" :sizes '(3 10 5)))
         )
        (:draw
         (yg:draw-line #(10 10) #(40 40) :color '(133.5 0.1 3.4))
         (yg:draw-string 0 0 (format nil "~a" yg::*default-font*)
                         )
         (yg:draw-rectangle #(50 50 50 50) :filled t)
         ))))
   :name title))
