;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defvar *web-store* nil "Variable to hold a reference to the default web store")

(ele:defpclass web-store ()
  ((sku-prefix :initarg :sku-prefix :accessor sku-prefix)
   (sku-counter :initform 1 :accessor sku-counter)
   (order-prefix :initarg :order-prefix :accessor order-prefix)
   (order-counter :initform 1 :accessor order-counter)
   (store-name :initarg :store-name :accessor store-name)))

(defun open-web-store (dir)
  (ele:open-store `(:bdb ,dir))
  (setf *web-store* (ele:get-from-root 'web-store)))

(defun new-web-store (store-name sku-prefix order-prefix directory)
  (ensure-directories-exist directory)
  (ele:open-store `(:bdb ,directory))
  (ele:add-to-root 'web-store
		   (make-instance 'web-store
				  :sku-prefix sku-prefix
				  :order-prefix order-prefix
				  :store-name store-name))
  (setf *web-store* (ele:get-from-root 'web-store)))

(defun get-next-sku (&optional (store *web-store*))
  (prog1
      (format nil "~A~7,'0d" (sku-prefix store) (sku-counter store))
    (incf (sku-counter store))))

(defun get-next-order (&optional (store *web-store*))
  (prog1
      (format nil "~A~7,'0d" (order-prefix store) (order-counter store))
    (incf (order-counter store))))

(defun close-web-store ()
  (ele:close-store)
  (setf *web-store* nil))

