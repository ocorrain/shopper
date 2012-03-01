(in-package #:shopper)

(let ((order-counter 1))
  (defun get-next-order ()
    (prog1
	(format nil "OR~7,'0d" order-counter)
      (incf order-counter)))
  (defun reset-order-counter ()
    (setf order-counter 1)))

(ele:defpclass order (quantity-list)
  ((order-number :initarg :order-number :initform (get-next-order) :accessor order-number)
   (order-placed :initarg :placed :initform (get-universal-time) :accessor placed
		 :documentation "When the order was placed")
   (customer :initarg :customer :initform nil :accessor customer
	     :documentation "Object holding customer details for this cart")
   (order-price :initarg :order-price :initform 0 :accessor order-price)))

(defun convert-cart-to-order (cart)
  (let ((order-price (get-price cart)))
    (change-class cart 'order)
    (setf (order-price cart) order-price))
  cart)

