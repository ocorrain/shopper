;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(asdf:defsystem :shopper
  :version "0.1"
  :serial t
  :depends-on (:hunchentoot
	       :elephant
	       :cl-who
	       :alexandria)
  :components ((:file "package")
	       (:file "store")
	       (:file "item")
               (:file "qlist")
               (:file "cart")
	       (:file "forms")))
