;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem :shopper
  :version "0.1"
  :serial t
  :depends-on (:hunchentoot
	       :elephant
	       :cl-who)
  :components ((:file "package")
	       (:file "item")
               (:file "qlist")
               (:file "cart")))
