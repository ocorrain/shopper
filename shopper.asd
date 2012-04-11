;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(asdf:defsystem :shopper
  :version "0.1"
  :serial t
  :depends-on (:hunchentoot
	       :elephant
	       :cl-who
	       :alexandria
	       :url-rewrite
	       :lisp-magick
	       :cl-fad)
  :components ((:file "package")
	       (:file "js")
	       (:file "widgets")
	       (:file "item")
	       (:file "images")
	       (:file "store")
               (:file "qlist")
               (:file "forms")
	       (:file "validation")
	       (:file "display")
	       (:file "tags")
	       (:file "pages")
	       (:file "cart")
	       (:file "inventory")))

