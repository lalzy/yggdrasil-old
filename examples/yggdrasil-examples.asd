
(asdf:defsystem #:yggdrasil-examples
  :description "Examples on how to use yggdrasil in projects"
  :author "Skn"
  :license  "MIT License"
  :serial t
  :depends-on (#:yggdrasil)
  :components ((:file "Animated-sprite/animated-sprite") ; How to use the animation system
	       )) 
