(in-package :yggdrasil)


;; Create state-system
;; Main-menu
;; Main-state
;; pause-state

;; Tie animation-system to main-state

;; Support adding new states to any of the 3 main states.


(defparameter *state* :setup) ; Default state is setup, for loading assets etc

(defparameter *states* '(:game :quit :menu :setup)) ; Various states available

(defun get-current-state ()
  "returns the current state, mostly for debugging"
  *state*)

(defun set-state (state)
  "Sets the game state"
  (if (member state *states*)
      (setf *state* state)))

(defun check-state (state)
  "checks if passed state is the current state"
  (string= state *state*))

(defmacro with-state (state &body body)
  "only runs the body when the select state is running"
  `(when (string= ',state *state*)
     ,@body))

(defun add-state (&rest states)
  "creates any of the passed states"
  (dolist (state states)
    (push state *states*)))
