(in-package #:shopper)

(restas:define-route r/index-page
    ("/")
  (index-page))

(restas:define-route r/featured
    ("/featured")
  (featured-items-page))

(restas:define-route r/view-tag
    ("/view/tag/:(tag)")
  (if-let (tag-object (get-tag tag))
    (make-page (tag-name tag-object)
	       (tag-display-page tag-object)
	       (main-site-bar (tag-name tag-object)))
    hunchentoot:+http-not-found+))

(restas:define-route r/shopping-cart/view ("/shopping-cart")
  (store-open-dependent-page #'shopping-cart-page))

(restas:define-route r/enter-details ("/enter-details")
  (store-open-dependent-page #'enter-details-page))

(restas:define-route r/shopping-cart/view/post
    ("/shopping-cart" :method :post)
  (store-open-dependent-page
   (lambda ()
     (maybe-update-cart (get-or-initialize-cart) (hunchentoot:post-parameters*))
     (shopping-cart-page))))

(defun maybe-update-cart (cart parameters)
  (dolist (item-q (get-valid-objects-from-post parameters))
    (destructuring-bind (item . quantity) item-q
      (set-item-quantity item cart quantity))))


(restas:define-route r/shopping-cart/checkout/post
    ("/checkout" :method :post)
  (store-open-dependent-page
   (lambda ()
     (if-let ((cart (get-cart))
	      (postage (hunchentoot:post-parameter "postage"))
	      (customer (get-customer)))
       (if-let (geo (get-geo-from-country-code (country customer)))
	 (let ((rates (get-applicable-postage-rates (get-weight cart) geo)))
	   (if-let (this-rate (assoc postage rates :test #'string-equal))
	     (let ((order (cart->order cart)))
	       (setf (postage-price order) (cdr this-rate))
	       (handler-case
		   (when (paypal-api-call-setexpresscheckout order)
		     (hunchentoot:redirect (paypal-redirect-url order)))
		 (paypal-api-error (condition)
		   (hunchentoot:log-message* :debug "in handler-case")
		   (paypal-error-page (sent condition) (received condition))))))))))))



(restas:define-route r/shopping-cart/success
    ("/success")
  (store-open-dependent-page
   (lambda ()
     (if-let (order (get-order-by-gateway-ref (hunchentoot:get-parameter "token")))
       (when (handler-case
		 (paypal-api-call-getexpresscheckoutdetails order)
	       (paypal-api-error (condition)
		 (paypal-error-page (sent condition) (received condition))))
	 (basic-page "Confirm your purchase"
		     (with-html-output-to-string (s)
		       ((:div :class "container")
			(str (shopping-cart-display
			      (cart order)
			      (with-html-output-to-string (s)
				(:h3 "Confirm order")
				(:p "If you wish to make this
				 purchase, please click Confirm &
				 Buy on this page."))))
			((:a :class "btn btn-large btn-warning pull-left"
			     :href (cancellation-url order))
			 "Cancel order")
			((:a :class "btn btn-large btn-primary pull-right"
			     :href (confirmation-url order))
			 "Confirm & buy")))))))))




;; (restas:define-route r/shopping-cart/checkout/post
;;     ("/checkout" :method :post)
;;   (make-page "Enter address details"
;; 	     (customer-address-content)
;; 	     (main-site-bar "")))

(restas:define-route r/shopping-cart/place-order
    ("/place-order" :method :post)
  (store-open-dependent-page #'order-edit-details-page))

(defun order-edit-details-page ()
  (multiple-value-bind (customer errors)
      (maybe-create/update-customer)
    (multiple-value-bind (valid verrors)
	(is-valid-customer? customer)
      (if valid
	  (basic-finalize-page)
	  (basic-edit-address-page (append errors (list verrors)))))))

(defun basic-finalize-page ()
  (let ((cart (get-cart))
	(customer (get-customer)))
    (multiple-value-bind (valid invalid)
	(check-order cart customer)
      (setf (items cart) valid)
      (basic-page "Finalize order"
		  (with-html-output-to-string (s)
		    ((:div :class "container")
		     
		      
		     ((:div :class "row")
		      
		      ((:div :class "span4")
		       (:h4 "Shipping address")
		       (:address (str (email customer))) (:br)
		       ((:a :href "/enter-details" :class "btn btn-primary btn-small pull-left")
			"Change address"))
		     
		      ((:div :class "span4")
		       (str (display-customer-address customer))))

		     (:hr)
		     ((:div :class "row")
		      (:h4 "Order contents")
		      (when invalid
			(htm ((:div :class "alert alert-error")
			      (:p "The following items are not
			      available in your geographical area.
			      They have been removed from your
			      shopping cart")
			      (:ul
			       (dolist (inv invalid)
				 (htm (:li (str (title (qlist-entry-item inv)))))))))))
		     (dolist (i (items cart))
		       (destructuring-bind (item quantity) i
			 (htm ((:div :class "row")
			       ((:div :class "span2")
				(str (display-an-image item #'get-small-url)))
			       ((:div :class "span8")
				(:h5 (str (title item)))
				(:p (str (short-description item)))
				(:p (:em "Item price: ") (str (print-price (get-price item)))))
			       ((:div :class "span2")
				(:p (str quantity)))))))
		     ((:div :class "row")
		      ((:div :class "span2 offset8")
		       (:p (:strong "Subtotal: ")
			   (str (print-price (get-price cart))))))
		     ((:div :class "row")
		      ((:a :href "/shopping-cart" :class "btn btn-primary pull-left")
		       "Change shopping cart")
		      (:hr)
		      (when valid
			(htm
			 ((:div :class "row")
			  ((:div :class "span8")
			   (:h4 "Shipping method")))
			 ((:form :action "/checkout" :method :post)
			  (let* ((rates (sort (get-applicable-postage-rates
						(get-weight cart)
						(get-geo-from-country-code (country customer)))
					       #'< :key #'cdr))
				  (cheapest (car rates)))
			     (dolist (rate rates)
			       (htm
				((:div :class "row")
				 ((:div :class "span6 offset2")
				  (:h5 (str (car rate)))
				  (:p (:strong "Shipping cost: ")
				      (str (print-price (cdr rate)))))
				 ((:div :class "span2")
				  (:p (:strong "TOTAL: ")
				      (str (print-price (+ (get-price cart)
							   (cdr rate))))))
				 ((:div :class "span2")
				  (if (equalp rate cheapest)
				      (htm (:input :type "radio" :name "postage" :id "true"
						   :value (car rate) :checked "checked"))
				      (htm (:input :type "radio" :name "postage" :id "true"
						   :value (car rate))))))
				 )))
			   ((:div :class "row")
			    ((:button :type "submit"
				      :class "btn btn-large btn-success pull-right")
			    "PLACE ORDER"))))))))))))

(defun basic-edit-address-page (&optional errors)
  (basic-page "Re-enter address details"
	      (with-html-output-to-string (s)
		((:div :class "container")
		 (str (customer-address-form errors))))))

(restas:define-route r/shopping-cart/place-order/get
    ("/place-order")
  (store-open-dependent-page #'order-edit-details-page))

;; (defun validate-or-cancel-order ()
;;   (let ((cart (get-or-initialize-cart))
;; 	(customer (get-or-initialize-customer)))
;;     (with-html-output-to-string (s)
;;       ((:div :class "row")
;;        ((:div :class "span5")
;; 	((:div :class "well")
;; 	 (str (display-customer customer))))
;;        ((:div :class "span5")
;; 	((:div :class "well")
;; 	 (str (print-shopping-cart cart)))))
;;       ((:div :class "row")
;;        ((:div :class "span5")
;; 	((:a :href "/checkout" :class "btn btn-primary pull-left")
;; 	 "Change address"))
;;        ((:div :class "span5")
;; 	((:a :href "/shopping-cart" :class "btn btn-primary pull-right")
;; 	 "Change shopping cart")))
;;       ((:div :class "row")
;;        ((:div :class "span3")
;; 	((:a :href "/gateway" :class "btn btn-large btn-warning")
;; 	 "PLACE ORDER"))))))

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
		       4)))


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


