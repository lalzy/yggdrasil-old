
(asdf:defsystem #:yggdrasil-examples
  :description "Examples on how to use yggdrasil in projects"
  :author "Skn"
  :license  "MIT License"
  :serial t
  :depends-on (#:yggdrasil)
  :components (
               (:file "Simple-rectangles/simple-rectangles") ; Two simple examples to draw a rectangle
               (:file "Animated-sprite/animated-sprite") ; How to use the animation system
	       ))
