;;;; Chapter 13 - Other uses for Cons Cells

;;; trees

(defparameter *tree* '((1 2) (3 4) (5 6)))

(defun tree-example ()
  (let ((tree-copy (copy-tree *tree*)))
    (assert (tree-equal *tree* tree-copy))))

(defun list-vs-tree-example ()
  (let* ((structure '((1 2) (3 4) (5 6)))
	 (list-subst (substitute 10 1 structure))
	 (tree-subst (subst 10 1 structure)))
    (format t "list-subst: ~a~%" list-subst)
    (format t "tree-subst: ~a~%" tree-subst)))

;;; sets

(defun set-example ()
  (let ((set nil))
    (loop for i from 1 to 10
       do (pushnew i set))
    (format t "set: ~a~%" set)
    (format t "Is ~d present in the set? ~a~%" 1 (member 1 set))
    (setf set (adjoin 1 set))
    (format t "Is ~d present in the set? ~a~%" 11 (member 11 set))
    (setf set (adjoin 11 set))
    (format t "Is ~d now present in the set? ~a~%" 11 (member 11 set))
    (format t "modified set: ~a~%" set)))

(defun set-operations ()
  (let ((a '(1 2 3))
	(b '(2 3 4)))
    (format t "set union: ~a~%" (union a b))
    (format t "set difference (a - b): ~a~%" (set-difference a b))
    (format t "set difference (b - a): ~a~%" (set-difference b a))
    (format t "set intersection: ~a~%" (intersection a b))
    (format t "set exclusive or (a ^ b): ~a~%" (set-exclusive-or a b))
    (format t "set exclusive or (b ^ a): ~a~%" (set-exclusive-or b a))
    (let ((c '(2 3)))
      (format t "Is a a subset of b? ~b~%" (subsetp a b))
      (format t "Is b a subset of a? ~b~%" (subsetp b a))
      (format t "Is c a subset of a? ~b~%" (subsetp c a)))))

;;; lookup tables - alists (association lists) and plists (property lists)

(defun basic-alist-example ()
  (let ((alist '((a . 1) (b . 2) (c . 3))))
    (format t "a: ~d~%" (cdr (assoc 'a alist)))
    (format t "b: ~d~%" (cdr (assoc 'b alist)))
    (format t "c: ~d~%" (cdr (assoc 'c alist)))
    (format t "d: ~d~%" (cdr (assoc 'd alist))))
  (let ((string-alist '(("bob" . 1) ("cat" . 2) ("dave" . 3))))
    (format t "Bob: ~d~%" (cdr (assoc "bob" string-alist :test #'string=)))
    (format t "Cat: ~d~%" (cdr (assoc "cat" string-alist :test #'string=)))
    (format t "Dave: ~d~%" (cdr (assoc "dave" string-alist :test #'string=)))))

(defun extended-alist-example ()
  (let ((phone-book nil))
    (setf phone-book (acons "Bob" "123-4567" phone-book))
    (setf phone-book (acons "Cat" "987-6543" phone-book))
    (setf phone-book (acons "Dave" "345-1234" phone-book))
    (format t "Bob's phone number is: ~a~%" (cdr (assoc "Bob" phone-book :test #'string=)))
    (setf phone-book (acons "Bob" "987-7654" phone-book))
    (format t "Bob's new phone number is: ~a~%" (cdr (assoc "Bob" phone-book :test #'string=)))))

(defun plist-example ()
  (let ((plist '(:name "bob" :age 42 :salary 1000000)))
    (format t "Name: ~a, age: ~d, salary: ~f~%"
	    (getf plist :name)
	    (getf plist :age)
	    (getf plist :salary 0.0))
    (setf (getf plist :salary) 2000000.0)
    (format t "Bob: ~a~%" plist)))

(defun process-properties (plist keys)
  (loop while plist do
       (multiple-value-bind (key value tail) (get-properties plist keys)
	 (format t "~a: ~a~%" key value)
	 (setf plist (cddr tail)))))

(defun process-properties-example ()
  (let ((bob '(:name "Bob"
	       :age 42
	       :salary 2000.0
	       :mood 'excellent)))
    (process-properties bob '(:name :age :mood))))

;;; destructuring bind

(destructuring-bind (x y z) (list 1 2 3)
  (list :x x :y y :z z))

(destructuring-bind (x y z) (list 1 (list 2 20) 3)
  (list :x x :y y :z z))

(destructuring-bind (x (y1 y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z))

(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z))

(destructuring-bind (&key x y z) (list :x 1 :y 2 :z 3)
  (list :x x :y y :z z))

(destructuring-bind (&key x y z) (list :z 3 :x 1 :y 2)
  (list :x x :y y :z z))

(destructuring-bind (&whole whole &key x y z) (list :z 3 :y 2 :x 1)
  (list :x x :y y :z z :whole whole))
