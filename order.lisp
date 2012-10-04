;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defun get-next-order (&optional (store *web-store*))
  (ele:with-transaction ()
      (prog1
	  (format nil "~A~7,'0d" (order-prefix store) (order-counter store))
	(incf (order-counter store)))))

(ele:defpclass order ()
  ((order-number :initarg :order-number :initform (get-next-order) :accessor order-number)
   (order-state :initarg :order-state :initform nil :accessor order-state)
   (order-timestamps :initarg :order-timestamps :initform nil :accessor order-timestamps)
   (customer :initarg :customer :initform nil :accessor customer
	     :documentation "Object holding customer details for this cart")
   (cart :initarg :cart :initform nil :accessor cart)
   (reified-cart :initarg :reified-cart :initform nil :accessor reified-cart)
   (gateway-ref :initarg :gateway-ref :initform nil :accessor gateway-ref)
   (order-price :initarg :order-price :initform 0 :accessor order-price)))

(defun convert-cart-to-order (cart)
  (let ((order-price (get-price cart)))
    (change-class cart 'order)
    (setf (order-price cart) order-price))
  cart)

