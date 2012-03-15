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
	       (:file "item")
	       (:file "images")
	       (:file "store")
               (:file "qlist")
               (:file "cart")
	       (:file "forms")
	       (:file "validation")
	       (:file "pages")
	       (:file "inventory")
	       (:file "js")))
