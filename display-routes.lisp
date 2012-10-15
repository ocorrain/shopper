(in-package #:shopper)


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
  (make-page "View shopping cart"
	     (shopping-cart-form (get-or-initialize-cart))
	     (main-site-bar "")))

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