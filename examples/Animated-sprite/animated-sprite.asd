;;;; animated-sprite.asd

(asdf:defsystem #:animated-sprite
  :description "Simple example of how to use the animated-sprite system, and the state-system"
  :author "Skn"
  :license  "MIT License"
  :serial t
  :depends-on (#:yggdrasil)
  :components ((:file "package")
               (:file "main")))
