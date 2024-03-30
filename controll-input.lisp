(in-package #:yggdrasil)


(defparameter *keys-pressed* nil) ; List of current key pressed(first element is sdl-state-key, second is unicode key)
(defparameter *mouse-move-direction* #(none none))
(defparameter *Current-mouse-buttons* nil)
(defparameter *mouse-state* 0) ; Possibly remove (?)

(defun get-mouse-position ()
  (vector (sdl:mouse-x) (sdl:mouse-y)))


;;; Keyboard stuff

(defun filter-special-keys (key)
  "Remove the 'SDL-KEY-' portion of the key symbol"
  (let ((symbol-string (string key)))
    (intern (subseq symbol-string 8 (length symbol-string)) "KEYWORD")))

;; Loop through and check if it's part of the special-keys
(defun is-special-key (key)
  (if (symbolp key)
      (dolist (symbol special-symbols))
      nil))

;; Rewrite to work better for special keys,
;; If passed-key is kp-plus\minus
(defun is-key-helper (passed-key case-sensitive)
  (let ((test (if case-sensitive #'equal #'equalp))
	(key (rassoc passed-key *keys-pressed*)))
    
    (unless key
      (when (symbolp passed-key)
	(return-from is-key-helper nil))
      
      (unless case-sensitive
	(setf key
	      (if (upper-case-p passed-key)
		  (rassoc (char-downcase passed-key) *keys-pressed*)
		  (rassoc (char-upcase passed-key) *keys-pressed*)))))
    
    
    (and key)))
    
(defmethod is-key ((key string) &key case-sensitive)
  (if (/= (length key) 1)
      (error "input: ~a is not a single key value" key)
      (is-key-helper (aref key 0) case-sensitive)))

(defmethod is-key ((key character) &key case-sensitive)
  (is-key-helper key case-sensitive))

(defvar *special-symbols* '(:up :down :left :right :lctrl :rctrl :lshift :rshift
				  :lalt :ralt :lsuper :rsuper :menu :backspace :return
				  :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12
				  :capslock :insert :home :delete :pageup :pagedown :delete
				  :backquote :scsrolllock :pause :numlock :kp-plus :kp-minus
				  :kp1 :kp2 :kp3 :kp4 :kp5 :kp6 :kp7 :kp8 :kp9 :kp0
				  :kp-period :kp-enter :kp-multiply :kp-divide :print 
				  :rightbracket :backlash :quote :leftbracket :minus :equals))

;; Rewrite to properly handle special symbols
(defmethod is-key ((key symbol) &key case-sensitive)
  (when (string= key :space) (setf key #\space)) ; Doesn't like :space
  (is-key-helper (if (member key *special-symbols*)
                     key
                     (character key))
                 case-sensitive))

;; Checks if any of the keys are pressed
(defun is-keys (list-of-keys))

;; Checks if all keys are pressed
(defun is-all-keys (list-of-keys))


;;; Mouse stuff

(defun mouse-move-direction (old-x x old-y y)
  "Helper function to set direction mouse is moving"
  (let ((vertical (cond
		    ((= old-y y) 'none)
		    ((> old-y y) :up)
		    (t :down)))
	
	(horizontal (cond ((= old-x x) 'none)
			  ((> old-x x) :left)
			  (t :right))))
    
    (vector horizontal vertical)))

(defun mouse-moved-p ()
  (every #'(lambda (x) (not (string= x 'none))) *mouse-move-direction*))

(defun is-mouse-dir (dir)
  (doarray (direction *mouse-move-direction*)
    (if (string-equal dir direction)
	(return-from is-mouse-dir t)))
  nil)


(defun is-mouse-key (key)
  (dolist (pressed-buttons *current-mouse-buttons*)
    (case key
      ((:left 1 :mouse-1)
       (when (= pressed-buttons 1)
	 (return-from is-mouse-key t)))
      ((:middle 3 :mouse-3 :wheel )
       (when (= pressed-buttons 2)
	 (return-from is-mouse-key t)))
      ((:right 2 :mouse-2)
       (when (= pressed-buttons 3)
	 (return-from is-mouse-key t)))
      ((:wheel-up :scroll-up :up)
       (when (= pressed-buttons 4)
	 (return-from is-mouse-key t)))
      ((:wheel-down :scroll-down :down)
       (when (= pressed-buttons 5)
	 (return-from is-mouse-key t)))
      ((:x1 4 :mouse-4)
       (when (= pressed-buttons 6)
	 (return-from is-mouse-key t)))
      ((:x2 5 :mouse-5)
       (when (= pressed-buttons 7)
	 (return-from is-mouse-key t))))))


(defun is-mouse-keys (&rest keys)
  (if (listp keys)
      (dolist (key keys)
	(when (is-mouse-key key) (return t)))
      (is-mouse-key keys)))
