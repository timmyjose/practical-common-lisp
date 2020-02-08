(defsystem "ppl"
  :description "A portable Pathname library"
  :author "Timmy Jose <zoltan.jose@gmail.com>"
  :version "0.0.1"
  :depends-on ()
  :components ((:file "packages")
               (:file "src/ppl" :depends-on ("packages"))))