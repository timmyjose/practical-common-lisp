(defun foo (&key a b c)
  (list a b c))

(assert (equal (foo :a 1 :b 2 :c 3) '(1 2 3)))
(assert (equal (foo :b 2 :c 3) '(nil 2 3)))
(assert (equal (foo :c 3) '(nil nil 3)))
(assert (equal (foo) '(nil nil nil)))

(defun bar (&key a (b 20) (c 30 c-p))
  (list a b c c-p))

(assert (equal (bar :a 1 :b 2 :c 3) '(1 2 3 t)))
(assert (equal (bar :c 3) '(nil 20 3 t)))
(assert (equal (bar :b 2) '(nil 2 30 nil)))
(assert (equal (bar) '(nil 20 30 nil)))



