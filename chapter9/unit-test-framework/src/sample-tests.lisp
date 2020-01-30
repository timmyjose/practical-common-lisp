(in-package #:com-tzj-unit-test-framework)

(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -10) -11)))

(deftest test-* ()
  (check
    (= (* 1 2 3) 6)
    (= (* -1 -12) 12)
    (= (* -12 1) -12)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
