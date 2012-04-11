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
	       (:file "qlist")
               (:file "item")
	       (:file "single-item")
	       (:file "bundle")
	       (:file "images")
	       (:file "store")
               (:file "forms")
	       (:file "validation")
	       (:file "tags")
	       (:file "cart")
	       (:file "display")
	       (:file "pages")))

