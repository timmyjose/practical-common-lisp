;;;; Chapter 22 - LOOP for Black Belts.

;;; iteration control

(let ((items '(mouse keyboard monitor speakers)))
  (loop
     for item in items
     for i from 1 to 10
     do (format t "~a " item)))

;;; counting loops
(assert (equalp (loop for i from 1 to 10 collecting i) '(1 2 3 4 5 6 7 8 9 10)))
(assert (equalp (loop for i from 1 upto 10 collecting i) '(1 2 3 4 5 6 7 8 9 10)))
(assert (equalp (loop for i from 10 downto 1 collecting i) '(10 9 8 7 6 5 4 3 2 1)))
(assert (equalp (loop repeat 10 collecting 1) '(1 1 1 1 1 1 1 1 1 1)))

(loop
   repeat 10
   do (format t "Hello, world~&"))

;; in - for elements of a list, on - for cons cells of a list

(assert (equalp (loop for i in (list 1 2 3 4 5) collecting i) '(1 2 3 4 5)))
(assert (equalp (loop for i on (list 1 2 3 4 5) collecting i) '((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5))))

(assert (equalp (loop for i in (list 1 2 3 4 5) by #'cddr collecting i) '(1 3 5)))
(assert (equalp (loop for i on (list 1 2 3 4 5) by #'cddr collecting i) '((1 2 3 4 5) (3 4 5) (5))))

;; for - for use with vectors/strings/bit-vectors

(assert (equalp (loop for c across "hello" collecting c) '(#\h #\e #\l #\l #\o)))
(assert (equalp (loop for x across #(1 2 3 4 5) collecting x) '(1 2 3 4 5)))

;; iterating over hash tables and packages

(let ((my-hash (make-hash-table :test #'equalp)))
  (setf (gethash 'foo my-hash) "foo")
  (setf (gethash 'bar my-hash) "bar")
  (setf (gethash 'baz my-hash) "baz")
  (setf (gethash 'quux my-hash) "quux")
  (setf (gethash 'foobar my-hash) "foobar")
  (loop for k being the hash-keys in my-hash using (hash-value v)
     do (format t "~a => ~a~%" k v))
  (loop for v being the hash-values in my-hash using (hash-key k)
     do (format t "~a <= ~a~%" v k)))

(loop for s being the symbols in  (find-package :common-lisp-user)
   do (format t "~a, " s))

(loop for s being the present-symbols in (find-package :common-lisp)
   do (format t "~a, " s))

(loop for s being the external-symbols in (find-package :common-lisp)
   do (format t "~a, " s))

;;; equals-then iteration

(with-open-file (in #p "chapter.lisp"
		    :direction :input)
  (with-standard-io-syntax
    (loop for line = (read-line in nil nil ) while line
       do (format t "~a~%" line))))

(assert (equalp (loop repeat 5
		   for x = 0 then y
		   for y = 1 then (+ x y)
		   collecting y)
		'(1 2 4 8 16)))

(assert (equalp (loop repeat 5
		   for x = 0 then y
		   and y = 1 then (+ x y)
		   collecting y)
		'(1 1 2 3 5)))

;;; destructuring

(loop for (a b) in '((1 2) (3 4) (5 6))
   do (format t "a: ~a, b: ~a~%" a b))

(assert (equalp (loop for (a nil) in '((1 2) (3 4) (5 6)) collecting a)
		'(1 3 5)))

;; note that this has to be "on" since so that we are processing
;; cons cells instead gf list items
(loop for (item . rest) on '(1 2 3 4 5)
   do (format t "~a " item)
   when rest do (format t ", "))

;;; value accumulation

(defparameter *random* (loop repeat 100 collecting (random 10000)))

(loop for i in *random*
   counting (evenp i) into evens
   counting (oddp i) into odds
   summing i into total
   maximizing i into max
   minimizing i into min
   finally (return (list min max total evens odds)))

;;; unconditional execution - do and return

(loop for i from 1 to 10 do (print i))

(block outer
  (loop for i from 0 return 100)
  (print "This will be printed")
  200)

(block foo
  (loop for i from 0 do (return-from foo 100))
  (print "This will not be printed")
  299)

;;; conditional execution - if, when, and unless

(loop for i from 1 to 10
   do (when (evenp i)
	(print i)))

(assert (= (loop for i from 1 to 10 when (evenp i) sum i) 30))
(assert (= (loop for i from 1 to 10 when (oddp i) sum i) 25))

(assert (equalp (loop for i from 1 to 10 when (evenp i) collecting i)
		'(2 4 6 8 10)))
(assert (equalp (loop for i from 1 to 10 when (oddp i) collecting i)
		'(1 3 5 7 9)))

(defparameter *hash* (make-hash-table :test #'equalp))

(setf (gethash "Bob" *hash*) 100)
(setf (gethash "Dave" *hash*) 200)
(setf (gethash "Susan" *hash*) 300)
(setf (gethash "Nate" *hash*) 400)
(setf (gethash "Carla" *hash*) 500)

(loop for key in '("Bob" "Nathan" "Susan" "David" "Carla")
   when (gethash key *hash*) collecting it)

;;; setup and teardown - initially and finally

(loop named outer for list in '(((1 2) (2 4) (2 5) (3 6) (8 10)))
   do (loop for (a b) in list
	 do (when (and (evenp a) (evenp b))
	      (return-from outer (values a b)))))

;;; termination tests - while, until, always, never, and thereis

(if (loop for i in '(2 4 6 8 10) always (evenp i))
    (print "all even"))

(if (loop for i in '(2 4 6 8 10) never (oddp i))
    (print "all even"))

(loop for c across "hello, world123" thereis (digit-char-p c))
(loop for c across "hello, world" thereis (digit-char-p c))






