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

(defmethod reify ((cart shopping-cart))
  (let ((items '()))
    (dolist (i (items cart))
      (let ((line-item (qlist-entry-item i))
	    (quantity (qlist-entry-quantity i)))
	(push (list (sku line-item)
		    (title line-item)
		    quantity
		    (price line-item)
		    (weight line-item))
	      items)))
    items))


(defmethod count-items-in ((cart shopping-cart))
  (reduce #'+ (mapcar #'qlist-entry-quantity (items cart))))

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
  (with-html-output-to-string (s)
      ((:div :id "cart")
       ((:p :align "center")
       ((:form :action "/add-to-cart" :method "post")
	((:div :class "input-append")
	 ((:select :class "input-mini" :name "number" :id "number")
	  ((:option :value 1 :selected "selected") (str 1))
	  (dotimes (i 49)
	    (htm ((:option :value (+ i 2))
		  (str (+ i 2))))))
	 (:input :type "hidden" :name "sku" :value (sku item))
	 ((:button :class "btn btn-success" :type "submit") "Add to cart")))))))



(defun get-or-initialize-cart ()
  (if-let (cart (hunchentoot:session-value :cart))
    cart
    (let ((cart (make-instance 'shopping-cart)))
      (setf (hunchentoot:session-value :cart) cart)
      cart)))

(defun get-cart ()
  (hunchentoot:session-value :cart))

(defun display-shopping-cart ()
  (let ((cart (get-or-initialize-cart)))
    (standard-page "Shopping cart"
	      (lambda (stream)
		(print-shopping-cart cart stream)))))

(defmethod display-link ((item line-item))
  (with-html-output-to-string (s)
    ((:a :href (get-url item))
     (str (title item)))))

(defun print-shopping-cart (cart)
  (with-html-output-to-string (s)
    (if (empty? cart)
	(htm (str "The shopping cart is empty"))
	(htm (:table (:thead
		      (:tr (:th "Quantity") (:th "Price")
			   (:th "Item")))
		     (:tfoot
		      (:tr (:th (fmt "Total weight: ~Ag" (get-weight cart)))
			   (:th (str (print-price (get-price cart))))
			   (:th "TOTAL")))
		     (:tbody
		      (dolist (i (items cart))
			(let* ((quantity (qlist-entry-quantity i))
			       (item (qlist-entry-item i))
			       (price (get-price item)))
			  (htm (:tr (:td (str quantity))
				    (:td (str (print-price (* price quantity))))
				    (:td ((:a :href (get-view-url item))
					  (str (title item)))
					 ;; (when (typep item 'bundle)
					 ;;   (funcall (simple-bundle-list item) s))
					 )))))))))))


(defun shopping-cart-form (cart)
  (with-html-output-to-string (s)
    ((:div :class "row")
     ((:div :class "span7")
      (:h3 "Shopping cart")
      (:p "These are the items in your shopping cart.  You can edit
    the number of each item by entering the quantity beside the item
    and clicking 'Update'.  To remove an item, set its quantity to zero."))
     ((:div :class "span5")
      ((:div :class "well well-small")
       ((:dl :class "dl-horizontal")
	(:dt "Total price")
	(:dd (str (print-price (get-price cart))))
	(:dt "Weight")
	(:dd (fmt "~Ag" (get-weight cart)))))))
    
    ((:form :class "form-horizontal" :action "/shopping-cart" :method :post)
     ((:div :class "row")
      ((:button :type "submit" :class "btn btn-large pull-left") "Update")
      ((:a :href "/checkout" :class "pull-right btn btn-large btn-primary")
       "Check out")
      ((:a :href "/" :class "pull-right btn btn-large btn-success")
       "Continue shopping"))

     (:hr)

     (dolist (i (items cart))
       (destructuring-bind (item quantity) i
	 (htm ((:div :class "row")
	       ((:div :class "span2")
		(str (display-a-small-image item)))
	       ((:div :class "span8")
		(:h5 (str (title item)))
		(:p (str (short-description item)))
		(:p (:em "Item price: ") (str (print-price (get-price item)))))
	       ((:div :class "span2")
		(:input :type "text" :class "input-mini" :name (sku item)
			:value quantity))))))
     
     (:hr)
     ((:div :class "row")
      ((:button :type "submit" :class "btn btn-large pull-left") "Update")
      ((:a :href "/checkout" :class "pull-right btn btn-large btn-primary")
       "Check out")
      ((:a :href "/" :class "pull-right btn btn-large btn-success")
       "Continue shopping")))
    
    

    ;; (dolist (i (items cart)))
    ;; ((:div :class "row")
    ;;  ((:div :class "span5")
      
      
    ;;   ((:a :href "/checkout" :class "pull-right btn btn-large btn-primary")
    ;; 	"Continue >>"))
    ;;  ((:div :class "span5")
    ;;   ((:div :class "well")
    ;;    (str (print-shopping-cart cart)))))
    ))

