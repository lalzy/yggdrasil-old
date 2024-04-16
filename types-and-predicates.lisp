(in-package :yggdrasil)

(defun valid-color-p (sequence)
  (and (= (length sequence) 3)
       (every #'(lambda (number)
                  (and (numberp number)
                       (> number 0)
                       (< number #xFF))) sequence)))

(deftype valid-color ()
  `(satisfies valid-color-p))
