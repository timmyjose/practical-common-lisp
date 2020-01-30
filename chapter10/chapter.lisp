;;;; Chapter 10 - Numbers, Characters, and Strings

(assert (char= #\A #\A))
(assert (char/= #\t #\T))
(assert (char< #\x #\z))
(assert (char> #\y #\b))
(assert (char<= #\c #\c))
(assert (char<= #\d #\e))
(assert (char>= #\f #\f))
(assert (char>= #\u #\k))

(assert (char-equal #\a #\A))

(assert (string= "hello" "hello"))
(assert (string< "hello" "world"))
(assert (string<= "hello" "world"))
(assert (string> "world" "hello"))
(assert (string>= "world" "hello"))
(assert (string/= "hello" "Hello"))

(assert (string-equal "hello" "HeLLO"))
(assert (string-not-equal "hello" "WORLD"))
(assert (string-lessp "hello" "WORLD"))

(assert (string= "foobarbaz" "barbarquux" :start1 3 :end1 5 :start2 3 :end2 5))


