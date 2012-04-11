;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(ele:defpclass line-item ()
  ((title :initarg :title :initform ""
	  :accessor title :index t
	  :documentation "Title or name of the line item" :type string)
   (short-description :initarg :short-description :initform ""
		      :accessor short-description
		      :documentation "A one-line description of the item"
		      :type string)
   (long-description :initarg :long-description :initform ""
		     :accessor long-description
		     :documentation "A long (paragraph length)
		     description of the item"
		     :type string)
   (packing-weight :initarg :packing-weight
		   :initform 0 :accessor packing-weight
		   :documentation "The extra weight of packaging:
		   ie. packing-weight + weight equals the total
		   shipping weight")
   (tags :initarg :tags :initform (ele:make-pset)
	       :accessor tags
	       :documentation "A list of categories or tags into which
	       this item falls"
	       :type list)
   (sku  :initarg :sku :initform nil :accessor sku
	:index t :documentation "Stock-keeping unit ID"
	:type string)
   (meta :initarg :meta :initform '() :accessor meta
	 :documentation "Meta tags to be added to page for HTML
	 searchability"
	 :type list)
   (featured :initarg :featured :initform nil :accessor featured
	     :type boolean :index t
	     :documentation "Is this to be published to the front-page
	     / featured page?")
   (published :initarg :published :initform nil
	      :accessor published :index t
	      :documentation "Is this to be published to the site?"
	      :type boolean)
   (images :initform '() :accessor images
	   :documentation "List of images of this item"
	   :type list)
   (image-counter :initform 0 :accessor image-counter
		  :documentation "counter for image filenames"
		  :type number)))

(defun get-item (sku)
  (ele:get-value sku (items *web-store*)))

(defgeneric get-price (item))

(defmethod get-price ((qlist quantity-list))
  (qlist-reduce (items qlist) :item-function #'get-price))

(defgeneric get-weight (item))

(defmethod get-weight ((qlist quantity-list))
  (qlist-reduce (items qlist) :item-function #'get-weight))

(defmethod get-next-image-stub ((item line-item))
  (prog1
      (format nil "~A_~A" (sku item) (image-counter item))
    (incf (image-counter item))))

(defun get-url (line-item)
  (url-rewrite:add-get-param-to-url "/item" "sku" (sku line-item)))








