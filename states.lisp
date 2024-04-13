(in-package :yggdrasil)


;; Create state-system
;; Main-menu
;; Main-state
;; pause-state

;; Tie animation-system to main-state

;; Support adding new states to any of the 3 main states.


(defparameter *state* :setup) ; Default state is menu

(defparameter *states* '(:game :quit :menu :setup)) ; Various states available

(defun set-state (state)
  "Sets the game state"
  (if (member state *states*)
      (setf *state* state)))

(defun check-state (state)
  (string= state *state*))

(defmacro with-state (state &body body)
  `(when (string= ',state *state*)
     ,@body))

(defun add-state (&rest states)
  "Add all the states to *States* variable"
  (dolist (state states)
    (push state *states*)))
