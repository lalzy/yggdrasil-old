;;;; breakout.asd

(asdf:defsystem #:tetris
  :description "Describe breakout here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:yggdrasil)
  :components ((:file "package")
               (:file "tetris")))
