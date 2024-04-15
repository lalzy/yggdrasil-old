;;;; yggdrasil.asd

(asdf:defsystem #:yggdrasil
  :description "Describe yggdrasil here"
  :author "S.K.N. The Lisper"
  :license  "BSD3"
  :version "0.0.1"
  :serial t
  :depends-on (#:lispbuilder-sdl #:lispbuilder-sdl-image  #:lispbuilder-sdl-gfx #:lispbuilder-sdl-ttf
		#:closer-mop #:cl-gamepad #:bordeaux-threads #:iterate  #:lispbuilder-sdl-ttf)
  :components ((:file "package")
	       (:file "utility")
               (:file "files") ; Move it somewhere else\lower? Also rename?
               (:file "cffi/SDL")
	       (:file "math")
	       (:file "colors")
	       (:file "shapes")
	       (:file "images")
               (:file "animation")
	       (:file "sprites")
	       (:file "text")
	       (:file "controll-input")
	       (:file "collisions")
	       (:file "oriented-placement")
	       (:file "context-menu")
	       (:file "GUI")
               (:file "states")
               (:file "yggdrasil")))
