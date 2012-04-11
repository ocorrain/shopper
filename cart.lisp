;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(ele:defpclass shopping-cart (quantity-list)
  ((cookie :initarg :cookie :initform nil :accessor cookie :index t
	   :documentation "The cookie id corresponding to this shopping cart")
   (last-active :initform (get-universal-time) :accessor last-active
		:documentation "The last time this cart was accessed")
   (customer :initarg :customer :initform nil :accessor customer
	     :documentation "Object holding customer details for this cart")))



(defun random-item ()
  (random-elt (ele:get-instances-by-class 'single-item)))

(defmethod add-item :after ((item line-item) (cart shopping-cart) quantity)
  "Update the last-active field if something is added"
  (setf (last-active cart) (get-universal-time)))

(defmethod remove-item :after ((item line-item) (cart shopping-cart))
    "Update the last-active field if something is removed"
  (setf (last-active cart) (get-universal-time)))

(defmethod empty-qlist :after ((cart shopping-cart))
  "Update the last-active field if the cart is emptied"
  (setf (last-active cart) (get-universal-time)))

(defgeneric cart-widget (item))

(defmethod cart-widget ((item line-item))
  "Widget that goes on display item pages with Add to cart
  functionality"
  (lambda (stream)
    (with-html-output (s stream :indent t)
      ((:div :id "cart")
       ((:form :action "/shopping-cart" :method "post")
	((:label :for "number") "Number of items:")
	((:select :name "number" :id "number")
	 ((:option :value 1 :selected "selected") (str 1))
	 (dotimes (i 49)
	   (htm ((:option :value (+ i 2))
		 (str (+ i 2))))))
	(:input :type "hidden" :name "sku" :value (sku item))
	(:input :type "submit" :value "Add to cart"))))))


(hunchentoot:define-easy-handler (add-to-cart :uri "/shopping-cart")
    (sku number)
  (hunchentoot:log-message* :info "~A" (hunchentoot:post-parameters*))
  (case (hunchentoot:request-method*)
    (:get (display-shopping-cart))
    (:post 
; (hunchentoot:start-session)
	   (let ((item (get-item sku))
		 (quantity (validate-number number)))
	     (when (and item quantity)
	       (hunchentoot:log-message* :debug "Got item ~A~%" item)
	       (let ((cart (get-or-initialize-cart)))
		 (add-item item cart quantity))))
	   (display-shopping-cart))))

(defun get-or-initialize-cart ()
  (if-let (cart (hunchentoot:session-value :cart))
    cart
    (let ((cart (make-instance 'shopping-cart)))
      (setf (hunchentoot:session-value :cart) cart)
      cart)))

(defun display-shopping-cart ()
  (let ((cart (get-or-initialize-cart)))
    (make-page "Shopping cart"
	       (lambda (stream)
		 (print-shopping-cart cart stream)))))

(defun print-shopping-cart (cart stream)
  (with-html-output (s stream)
    (if (empty? cart)
	(htm (str "The shopping cart is empty"))
	(htm (:table (:thead
		      (:tr (:th "Quantity") (:th "Item price")
			   (:th "Subtotal") (:th "Item")))
		     (:tfoot
		      (:tr (:th " ")
			   (:th " ")
			   (:th (str (print-price (get-price cart))))
			   (:th "TOTAL")))
		     (:tbody
		      (dolist (i (items cart))
			(let* ((quantity (qlist-entry-quantity i))
			       (item (qlist-entry-item i))
			       (price (get-price item)))
			  (htm (:tr (:td (str quantity))
				    (:td (str (print-price price)))
				    (:td (str (print-price (* price quantity))))
				    (:td (str (title item)))))))))))))

