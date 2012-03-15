;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
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

(defun image-form (stream line-item)
  (with-html-output (s stream :indent t)
    ((:form :action (get-url line-item) :method :post :enctype "multipart/form-data")
     ((:label :for "picture") "Upload an image")
     (:input :type "file" :name "picture" :class "text")
     (:input :type "submit" :value "Upload" :class "text"))))


(defun generic-line-item-fields (stream &optional line-item)
  (with-html-output (s stream :indent t)
    (textfield "title" stream "Name or title of this item" (when line-item (title line-item)))
    (textfield "short-description" s "A one-line description of the item"
	       (when line-item (short-description line-item)))
    (textarea "long-description" s "A paragraph length description of the item"
	      (when line-item (long-description line-item)))
    (checkbox "published" s "Published?"
		  (when line-item (published line-item)))
    (checkbox "featured" s "Featured?"
		  (when line-item (featured line-item)))
    (textfield "packing-weight" s "The extra weight of packaging for this item: packing weight + item weight (the next field equals the total shipping weight"
	       (when line-item (packing-weight line-item)))))



(defun textfield (name stream label default-value)
  (with-html-output (s stream :indent t)
    ((:label :for name) (str label)) (:br)
    (:input :type "text" :name name :class "text"
	    :value (if default-value (if (stringp default-value)
					 (escape-string-all default-value)
					 default-value)  ""))
    (:br)))

(defun textarea (name stream label default-value)
  (with-html-output (s stream :indent t)
    ((:label :for name) (str label)) (:br)
    ((:textarea :name name :class "text") (if default-value (str default-value) (str "")))
    (:br)))

(defun radio-button (name stream label default-value)
  (with-html-output (s stream :indent t)
    ((:label :for name) (str label)) (:br)
    ((:label :for "true") "Yes")
    (if default-value
	(htm (:input :type "radio" :name name :id "true" :value "true" :checked "checked"))
	(htm (:input :type "radio" :name name :id "true" :value "true")))
    ((:label :for "false") "No")
    (if default-value
	(htm (:input :type "radio" :name name :id "false" :value "false"))
	(htm (:input :type "radio" :name name :id "false" :value "false" :checked "checked")))
    (:br)))

(defun checkbox (name stream label default-value)
  (with-html-output (s stream :indent t)
    ((:label :for name) (str label)) 
    (if default-value
	(htm (:input :type "checkbox" :name name :value "true" :checked "checked"))
	(htm (:input :type "checkbox" :name name :value "true")))
    (:br)))

(defun submit-button (label stream)
  (with-html-output (s stream :indent t)
    (:br)
    (:input :type "submit" :name "submit" :value label :class "text")
    (:br)))
