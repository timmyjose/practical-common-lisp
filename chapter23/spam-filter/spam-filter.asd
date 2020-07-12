(defsystem "spam-filter"
  :description "A simple spam filter"
  :author "Timmy Jose<zoltan.jose@gmail.com"
  :version "0.0.1"
  :depends-on ()
  :components ((:file "packages")
               (:file "src/spam-filter" :depends-on ("packages"))))
