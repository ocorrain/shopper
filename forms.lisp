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

(defun validate-generic-line-item-fields (alist)
  (flet ((assoc-val (val) (cdr (assoc val alist))))
    (let ((errors '())
	  (title (validate-as-string (assoc-val "title")))
	  (short-description (validate-as-string (assoc-val "short-description") t))
	  (long-description (validate-as-string (assoc-val "long-description") t))
	  (packing-weight (validate-number (assoc-val "packing-weight"))))
      (when (null title) (push 'title errors))
      (when (null short-description) (push 'short-description errors))
      (when (null long-description) (push 'long-description errors))
      (when (null packing-weight) (push 'packing-weight errors))
      (values title short-description long-description packing-weight published featured
	      errors))))

(defun validate-single-item-fields (alist)
  (flet ((assoc-val (val) (cdr (assoc val alist))))
    (let ((errors '())
	  (weight (validate-number (assoc-val "weight")))
	  (price (validate-number (assoc-val "price"))))
      (when (null weight) (push 'weight errors))
      (when (null price) (push 'price errors))
      (values weight price errors))))

(defun validate-as-string (thing &optional allow-empty)
  (and (stringp thing)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return #\Linefeed)
				   thing)))
	 (if (not allow-empty)
	     (unless (zerop (length trimmed))
	       trimmed)
	     trimmed))))

(defun validate-number (thing &optional allow-negative)
  (cond ((integerp thing) (if (minusp thing)
			      (when allow-negative
				thing)
			      thing))
	((stringp thing) (when-let (parsed (parse-integer thing :junk-allowed t))
			   (validate-number parsed)))
	(t nil)))



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
