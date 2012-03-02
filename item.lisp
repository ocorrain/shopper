;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(ele:defpclass line-item ()
  ((title :initarg :title :initform "" :accessor title :index t
	  :documentation "Title or name of the line item" :type string)
   (short-description :initarg :short-description :initform ""
		      :accessor short-description
		      :documentation "A one-line description of the item" :type string)
   (long-description :initarg :long-description :initform ""
		     :accessor long-description
		     :documentation "A long (paragraph length) description of the item"
		     :type string)
   (packing-weight :initarg :packing-weight :initform 0 :accessor packing-weight
		   :documentation "The extra weight of packaging:
		   ie. packing-weight + weight equals the total
		   shipping weight")
   (categories :initarg :categories :initform '() :accessor categories
	       :documentation "A list of categories or tags into which this item falls"
	       :type list)
   (sku :initarg :sku :initform nil :accessor sku :index t
	:documentation "Stock-keeping unit ID" :type string)
   (meta :initarg :meta :initform '() :accessor meta
	 :documentation "Meta tags to be added to page for HTML searchability" :type list)
   (featured :initarg :featured :initform nil :accessor featured :type boolean :index t
	     :documentation "Is this to be published to the front-page / featured page?")
   (published :initarg :published :initform nil :accessor published :index t
	      :documentation "Is this to be published to the site?" :type boolean)))

(ele:defpclass single-item (line-item)
  ((weight :initarg :weight :initform 0 :accessor weight
	   :documentation "The weight of the item in grams" :type integer)
   (price :initarg :price :initform 0 :accessor price
	  :documentation "The price of the item in euro cents" :type integer)))

(ele:defpclass bundle (line-item quantity-list)
  ((discount :initarg :discount :initform 0 :accessor discount
	     :documentation "Percentage discount for a bundle")))

(defmethod get-price ((item single-item))
  (price item))

(defmethod get-price :around ((bundle bundle))
  "Applies the bundle discount"
  (let ((initial-price (call-next-method)))
    (round (* initial-price (/ (- 100 (discount bundle)) 100)))))

(let ((sku-counter 1))
  (defun get-next-sku ()
    (prog1
	(format nil "SKU~7,'0d" sku-counter)
      (incf sku-counter)))
  (defun reset-sku-counter ()
    (setf sku-counter 1)))

(defvar *words* (make-array 98569))

(defun get-words ()
  (with-open-file (f "/etc/dictionaries-common/words")
    (do ((l (read-line f nil 'eof) (read-line f nil 'eof))
	 (l-index 0 (+ l-index 1)))
	((eq l 'eof) 'done)
      (setf (svref *words* l-index) (string-trim '(#\Newline #\Tab #\Space) l)))))

(defun random-word (arg)
  (declare (ignore arg))
  (svref *words* (random (length *words*))))

(defun random-word-list (number)
  (mapcar #'random-word (make-list number)))

(defun random-words (number)
  (format nil "~{~A~^ ~}" (random-word-list number)))

(defun flip ()
  (if (zerop (random 2))
      nil t))


(defun provision-items-test (number)
  (dotimes (i number)
    (make-instance 'single-item
		 :title (random-words 3)
		 :short-description (random-words 30)
		 :long-description (random-words 150)
		 :weight (random 2000)
		 :price (random 10000)
		 :categories (random-word-list 10)
		 :sku (get-next-sku)
		 :meta (random-word-list 10)
		 :featured (flip)
		 :published (flip))))

(defun export-items-test (filename)
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (dolist (i (ele:get-instances-by-class 'line-item))
      (format f "TITLE: ~A~%SHORT-DESCRIPTION: ~A~%LONG-DESCRIPTION: ~A~%"
	      (title i) (short-description i) (long-description i))
      (format f "WEIGHT: ~Ag~%PRICE: ~Ac~%CATEGORIES: ~S~%" (weight i) (price i) (categories i))
      (format f "SKU: ~A~%META: ~S~%FEATURED: ~A~%PUBLISHED: ~A~%~%~%"
	      (sku i) (meta i) (featured i) (published i)))))

(defun delete-items-test ()
  (ele:drop-instances (ele:get-instances-by-class 'line-item)))
