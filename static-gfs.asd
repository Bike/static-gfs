(asdf:defsystem #:static-gfs
  :description "Optimize MAKE-INSTANCE and such with call site caches."
  :author "Bike <aeshtaer@gmail.com>"
  :version "0.5"
  :license "CC0"
  :components ((:file "packages")
               (:file "util" :depends-on ("packages"))
               (:file "emf" :depends-on ("packages"))
               (:file "allocate-instance" :depends-on ("packages" "emf" "util"))
               (:file "shared-initialize" :depends-on ("packages" "emf" "util"))
               (:file "initialize-instance"
                :depends-on ("shared-initialize" "emf" "util" "packages"))
               (:file "make-instance"
                :depends-on ("initialize-instance" "emf" "util" "packages"))
               (:file "compiler-macros"
                :depends-on ("make-instance" "allocate-instance" "packages"))
               (:file "debug"
                :depends-on ("make-instance" "allocate-instance" "packages"))))
