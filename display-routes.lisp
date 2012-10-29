(in-package #:shopper)

(restas:define-route r/index-page
    ("/")
  (make-page (format nil "Welcome to ~A" (store-name *web-store*))
	     (thumbnails (all-items)
			 #'render-thumb)
	     (main-site-bar "")))

(restas:define-route r/view-tag
    ("/view/tag/:(tag)")
  (if-let (tag-object (get-tag tag))
    (make-page (tag-name tag-object)
	       (tag-display-page tag-object)
	       (main-site-bar (tag-name tag-object)))
    hunchentoot:+http-not-found+))

(restas:define-route r/shopping-cart/view ("/shopping-cart")
  (make-page "View shopping cart"
	     (shopping-cart-form (get-or-initialize-cart))
	     (main-site-bar "")))

(restas:define-route r/shopping-cart/view/post
    ("/shopping-cart" :method :post)
  (maybe-update-cart (get-or-initialize-cart) (hunchentoot:post-parameters*))
  (basic-page "View shopping cart"
	      (with-html-output-to-string (s)
		((:div :class "container")
		 (str (shopping-cart-form (get-or-initialize-cart)))))))


(defun maybe-update-cart (cart parameters)
  (dolist (item-q (get-valid-objects-from-post parameters))
    (destructuring-bind (item . quantity) item-q
      (set-item-quantity item cart quantity))))

(restas:define-route r/shopping-cart/checkout
    ("/checkout")
  (make-page "Enter address details"
	     (customer-address-content)
	     (main-site-bar "")))

(restas:define-route r/shopping-cart/checkout/post
    ("/checkout" :method :post)
  (make-page "Enter address details"
	     (customer-address-content)
	     (main-site-bar "")))

(restas:define-route r/shopping-cart/place-order
    ("/place-order" :method :post)
  (multiple-value-bind (customer errors)
      (maybe-create/update-customer)
    (multiple-value-bind (valid verrors)
	(is-valid-customer? customer)
      (if valid
	  (make-page "Finalize order"
		     (validate-or-cancel-order)
		     (main-site-bar ""))
	  (make-page "Re-enter address details"
		     (customer-address-content (append errors (list verrors)))
		     (main-site-bar ""))))))

(restas:define-route r/shopping-cart/place-order/get
    ("/place-order")
  (multiple-value-bind (customer errors)
      (maybe-create/update-customer)
    (multiple-value-bind (valid verrors)
	(is-valid-customer? customer)
      (if valid
	  (make-page "Finalize order"
		     (validate-or-cancel-order)
		     (main-site-bar ""))
	  (make-page "Re-enter address details"
		     (customer-address-content (append errors (list verrors)))
		     (main-site-bar ""))))))

(defun validate-or-cancel-order ()
  (let ((cart (get-or-initialize-cart))
	(customer (get-or-initialize-customer)))
    (with-html-output-to-string (s)
      ((:div :class "row")
       ((:div :class "span5")
	((:div :class "well")
	 (str (display-customer customer))))
       ((:div :class "span5")
	((:div :class "well")
	 (str (print-shopping-cart cart)))))
      ((:div :class "row")
       ((:div :class "span5")
	((:a :href "/checkout" :class "btn btn-primary pull-left")
	 "Change address"))
       ((:div :class "span5")
	((:a :href "/shopping-cart" :class "btn btn-primary pull-right")
	 "Change shopping cart")))
      ((:div :class "row")
       ((:div :class "span3")
	((:a :href "/gateway" :class "btn btn-large btn-warning")
	 "PLACE ORDER"))))))

(defun tag-buddies (item)
  (remove item (mappend (lambda (tag)
			  (ele:pset-list (tag-members tag)))
			(get-tags item))))

(defun display-related-items (item)
  (when-let (tbs (tag-buddies item))
    (thumbnails (if (< (length tbs) 4)
			   tbs
			   (subseq tbs 0 4))
		       #'render-very-short
		       3)))


(defun view-item-page (item)
  (make-page (format nil "Viewing ~A" (title item))
	     (with-html-output-to-string (s)
	       (str (display-item-content item))
	       (str (display-related-items item)))
	     
	     (main-site-bar "")
	     (with-html-output-to-string (s)
	       (:script "$('.carousel').carousel()"))))

(restas:define-route r/view-item
    ("/view/item/:(sku)")
  (if-let (item (get-item sku))
    (view-item-page item)
    hunchentoot:+http-not-found+))


