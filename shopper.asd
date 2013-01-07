;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(asdf:defsystem :shopper
  :version "0.1"
  :serial t
  :depends-on (:restas
	       :restas-directory-publisher 
	       :hunchentoot
	       :elephant
	       :cl-who
	       :alexandria
	       :local-time
	       :url-rewrite
	       :lisp-magick
	       :cl-fad
	       :drakma
	       :md5
	       :puri
	       :cl-html-parse)
  :components ((:file "shopper")
	       (:file "utilities")
	       (:file "display-routes")
	       (:file "edit-routes")
;	       (:file "tag-routes")
	       (:file "cart-routes")
	       (:file "user")
	       (:file "user-routes")
	       (:file "js")
	       (:file "widgets")
	       (:file "item")
               (:file "qlist")
	       (:file "single-item")
	       (:file "bundle")
	       (:file "images")
	       (:file "store")
               (:file "forms")
	       (:file "tags")
	       (:file "validation")
	       (:file "cart")
	       (:file "customer")
	       (:file "display")
	       (:file "navigation")
	       (:file "pages")
	       (:file "order")
	       (:file "geo")
	       (:file "paypal")))

