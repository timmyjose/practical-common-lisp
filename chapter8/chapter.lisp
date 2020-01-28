;;; Chapter 8 - Macros: Defining your own.

;;; General procedure for writing a macro:
;;;
;;; 1. Write a sample call to the macro and the code it should expand into, or vice-versa.
;;;
;;; 2. Write code that generates the handwritten expansion from the arguments in the sample call.
;;;
;;; 3. Make sure that the macro abstraction doesn't leak.
;;;

;;; sample macro: do-primes

(defun primep (number)
  (when (> number 1)
    (loop for d from 2 to (isqrt number)
       never (zerop (mod number d)))))

(defun next-prime (number)
  (loop for n from number
     when (primep n)
     return n))

;;; 1. sample call
;; (do-primes (p 0 19)
;;   (format t "~d " p))
;;

;;; 2. Sample expanded code for the call above.
;; (do ((p (next-prime 0) (next-prime (1+ p)))
;;     ((> p 19))
;;   (format t "~d " p))


;;; 3. Write the macro code and ensure that the abstraction does not leak.

;; (defmacro do-primes (var-and-range &body body)
;;   (let ((var (first var-and-range))
;; 	(start (second var-and-range))
;; 	(end (third var-and-range)))
;;     `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
;; 	 ((> ,var ,end))
;;        ,@body)))

;; ;; (defmacro do-primes ((var start end) &body body)
;; ;;   `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
;; ;;        ((> ,var ,end))
;; ;;      ,@body))

;; (defmacro do-primes ((var start end) &body body)
;;   (let ((ending-value-name (gensym)))
;;     `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
;; 	  (,ending-value-name ,end))
;; 	 ((> ,var ,ending-value-name))
;;        ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collecting `(,n (gensym)))
     ,@body))

(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))


