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
   (weight :initarg :weight :initform 0 :accessor weight
	   :documentation "The weight of the item in grams" :type integer)
   (price :initarg :price :initform 0 :accessor price
	  :documentation "The price of the item in euro cents" :type integer)
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
		  :type number)
   ;; (children :initform '() :accessor get-children
   ;; 	     :documentation "children of this object.  Objects may not
   ;; 	     contain any reference to themselves")
   (children-qlist :initform (make-instance 'quantity-list)
		   :accessor get-children-qlist
		   :documentation "If children exists, this
			  should be a quantity list mapping each child
			  to the quantity contained in the bundle")))


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

(defmethod get-url ((line-item line-item))
  (url-rewrite:add-get-param-to-url "/item" "sku" (sku line-item)))

(defmethod get-edit-view-url ((line-item line-item))
  (format nil "/edit/item/~A/view" (sku line-item)))

(defmethod get-edit-edit-url ((line-item line-item))
  (format nil "/edit/item/~A/edit" (sku line-item)))

(defmethod get-edit-image-url ((line-item line-item))
  (format nil "/edit/item/~A/images" (sku line-item)))

(defmethod get-edit-tags-url ((line-item line-item))
  (format nil "/edit/item/~A/tags" (sku line-item)))

(defmethod get-edit-contents-url ((line-item line-item))
  (format nil "/edit/item/~A/contents" (sku line-item)))

(defun collect-items-with (func)
  (let ((result '()))
    (ele:map-btree (lambda (k v)
		     (declare (ignore k))
		     (when (funcall func v)
		       (push v result)))
		   (items *web-store*))
    result))

(defun collect-skus-with (func)
  (let ((result '()))
    (ele:map-btree (lambda (k v)
		     (when (funcall func v)
		       (push k result)))
		   (items *web-store*))
    result))

(defmethod get-weight ((item line-item))
  (weight item))

(defmethod get-price ((item line-item))
  (price item))

(defmethod get-view-url ((item line-item))
  (format nil "/view/item/~A" (sku item)))

(defun item-list->table-form (items item-input-func item-display-func action-url)
  (with-html-output-to-string (s)
    ((:table :class "table table-striped")
     ((:form :method :post :action action-url)
      (dolist (item items)
	(htm (:tr (:td (str (funcall item-display-func item)))
		  (:td (:input :type "text" :class "input-mini"
			       :name (funcall item-input-func item)
			       :value 0)))))
      ((:button :type "submit" :class "btn btn-primary") "Add to bundle")))))

(defun all-items ()
  (let ((items '()))
    (ele:map-btree (lambda (k v)
		     (declare (ignore k))
		     (push v items))
		   (items *web-store*))
    items))