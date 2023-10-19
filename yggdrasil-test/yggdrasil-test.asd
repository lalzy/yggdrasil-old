;;;; yggdrasil.asd

(asdf:defsystem #:yggdrasil-test
  :description "Describe yggdrasil here"
  :author "S.K.N. The Lisper"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:yggdrasil)
  :components ((:file "package")
                 (:file "yggdrasil-test")))

