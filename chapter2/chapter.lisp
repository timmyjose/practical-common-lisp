;;;; Chpater 2 - A tour of the REPL

(defun factorial (n)
  (labels ((f (acc n)
	     (cond
	       ((zerop n) acc)
	       (t (f (* acc n) (- n 1))))))
    (f 1 n)))


