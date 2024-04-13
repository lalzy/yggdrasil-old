(in-package #:yggdrasil)

(defun load-bmp (file path)
  "loads an bmp file, inteded for setting the window-icon"
  (lispbuilder-sdl-cffi::sdl-load-bmp (verify-file file path "bmp")))

(cffi:defcfun ("SDL_WM_SetIcon" sdl-set-icon) :void
  (Icon :pointer)
  (mask :uint8)) ; According to the SDL1.2 documentation. Lispbuilder-SDL's SetIcon uses a pointer instead.
