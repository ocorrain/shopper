;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defun single-item-form (stream &optional line-item)
  (with-html-output (s stream :indent t)
    ((:form :action (if line-item (get-url line-item) "/single-item/new")
	    :method :post)
     (generic-line-item-fields stream line-item)
     (textfield "weight" s "The weight (net) of the item in grams"
		(when line-item (weight line-item)))
     (textfield "price" s "The price of the item in euro cents"
		(when line-item (price line-item)))
     (submit-button "Submit" s))))

(defun bundle-form (stream &optional line-item)
  (with-html-output (s stream :indent t)
    ((:form :action (if line-item (get-url line-item) "/bundle/new")
	    :method :post)
     (generic-line-item-fields stream line-item)
     (submit-button "Next >>" s))))



(defun get-url (line-item)
  (format nil "item/~A" (sku line-item)))

(defun generic-line-item-fields (stream &optional line-item)
  (with-html-output (s stream :indent t)
    (textfield "title" stream "Name or title of this item" (when line-item (title line-item)))
    (textfield "short-description" s "A one-line description of the item"
	       (when line-item (short-description line-item)))
    (textarea "long-description" s "A paragraph length description of the item"
	      (when line-item (long-description line-item)))
    (radio-button "published" s "Is this item to be published on the web?"
		  (when line-item (published line-item)))
    (radio-button "featured" s "Is this item to be featured?"
		  (when line-item (featured line-item)))
    (textfield "packing-weight" s "The extra weight of packaging for this item: packing weight + item weight (the next field equals the total shipping weight"
	       (when line-item (packing-weight line-item)))))


(defun textfield (name stream label default-value)
  (with-html-output (s stream :indent t)
    ((:label :for name) (str label)) (:br)
    (:input :type "text" :name name
	    :value (if default-value (if (stringp default-value)
					 (escape-string-all default-value)
					 default-value)  ""))))

(defun textarea (name stream label default-value)
  (with-html-output (s stream :indent t)
    ((:label :for name) (str label)) (:br)
    ((:textarea :name name) (if default-value (str default-value) (str "")))))

(defun radio-button (name stream label default-value)
  (with-html-output (s stream :indent t)
    ((:label :for name) (str label)) (:br)
    ((:label :for "true") "Yes")
    (if default-value
	(htm (:input :type "radio" :name name :id "true" :value "true" :checked "checked"))
	(htm (:input :type "radio" :name name :id "true" :value "true")))
    ((:label :for "false") "No")
    (if default-value
	(htm (:input :type "radio" :name name :id "false" :value "true"))
	(htm (:input :type "radio" :name name :id "false" :value "true" :checked "checked")))))

(defun submit-button (label stream)
  (with-html-output (s stream :indent t)
    (:br)
    (:input :type "submit" :name "submit" :value label)))