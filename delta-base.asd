(in-package :asdf)

(defsystem "delta-base"
  :name "delta-base"
  :version "0.0.0"
  :maintainer "Marcus Pemer <mpemer@gmail.com>"
  :author "Marcus Pemer <mpemer@gmail.com>"
  :description "Delta-base"
  :long-description "Lisp implementation of database schema delta calculation"
  :depends-on (:iterate
	       :postmodern
	       :lisp-unit)

  :components ((:file "test" :depends-on ("delta-base"))
	       (:file "delta-base")))
