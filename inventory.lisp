;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(hunchentoot:define-easy-handler (single-items-list :uri "/single-items")
    ()
  (make-page "List all single items"
	     (with-html-output-to-string (s)
	       ((:div :class "span-24")
		(:pre (fmt "~{~A~^~%~}" (hunchentoot:post-parameters*)))
		((:form :method "post" :action "/single-items")
		 (:input :type "submit" :value "Delete")
		 (:table
		  (:tr (:th "Delete")
		       (:th "SKU")
		       (:th "Item"))
		  (dolist (i (ele:get-instances-by-class 'single-item))
		    (htm 
		     (:tr (:td (:input :type "checkbox" :name (sku i)))
			  (:td ((:a :href (get-url i))
				(str (sku i))))
			  (:td (str (title i)))))))
		 (:input :type "submit" :value "Delete"))))))


