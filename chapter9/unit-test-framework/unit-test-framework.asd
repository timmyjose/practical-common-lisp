(defsystem "unit-test-framework"
  :description "A simple Unit-testing framework in Common Lisp"
  :author "Timmy Jose <zoltan.jose@gmail.com>"
  :version "0.0.1"
  :depends-on ()
  :components ((:file "packages")
	       (:file "src/unit-test-framework" :depends-on ("packages"))
	       (:file "src/sample-tests" :depends-on ("packages" "src/unit-test-framework"))))
