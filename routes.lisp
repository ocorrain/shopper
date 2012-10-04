(restas:define-module #:shopper
  (:use #:cl #:cl-who #:alexandria))

(in-package #:shopper)

;; set the HTML5 doctype
(setf *prologue* "<!DOCTYPE html>")

;; static content routes
;; (restas:start '#:restas.directory-publisher
;;               :port 8080
;;               :context (restas:make-context (restas.directory-publisher:*baseurl* '("s"))
;;                                             (restas.directory-publisher:*directory* #P"/home/ocorrain/lisp/dev/bootstrap/")
;;                                             (restas.directory-publisher:*autoindex* nil)))



(defun mount-webstore-content ()
  (restas:mount-submodule webstore-images (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("images"))
    (restas.directory-publisher:*directory* (image-path *web-store*))
    (restas.directory-publisher:*autoindex* nil))
  (restas:mount-submodule twitter-bootstrap-files (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("s"))
    (restas.directory-publisher:*directory* (get-twitter-bootstrap-path))
    (restas.directory-publisher:*autoindex* nil)))


;; ITEMs, bundles or single items

; new items
(restas:define-route r/new-item
    ("/new/item")
  (new-item-page))


(restas:define-route r/new-item/post
    ("/new/item" :method :post)
  (maybe-create 'item (fix-alist (hunchentoot:post-parameters*))))

(restas:define-route r/delete-item
    ("/delete/item/:(sku)")
  (if-let (obj (get-item sku))
    (progn
      (dolist (tag (ele:pset-list (tags obj)))
	(untag-item obj tag))
      (ele:remove-kv sku (items *web-store*))
      (ele:drop-instance obj)
      (hunchentoot:redirect (hunchentoot:referer))) 
    hunchentoot:+http-not-found+))

; existing items
(defun edit-item-page (test title)
  (make-page title
	     (thumbnails (collect-items-with test)
			 #'render-thumb)
	     (edit-bar title)))

(defun edit-tag-page (test title)
  (make-page title
	     (thumbnails (collect-tags-with test)
			 #'render-thumb)
	     (edit-bar title)))

(restas:define-route r/edit-items
    ("/edit/items")
  (edit-item-page #'identity "All items"))

(restas:define-route r/edit-published
    ("/edit/items/published")
  (edit-item-page #'published "Published items"))

(restas:define-route r/edit-unpublished
    ("/edit/items/unpublished")
  (edit-item-page (lambda (item) (not (published item))) "Unpublished items"))

(restas:define-route r/edit-featured
    ("/edit/items/featured")
  (edit-item-page #'featured "Featured items"))

(restas:define-route r/edit-item/view 
    ("/edit/item/:(sku)/view")
  (if-let (item (get-item sku))
    (display-item-page item)
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-item/images
    ("/edit/item/:(sku)/images")
  (if-let (item (get-item sku))
    (make-page (format nil "Editing images for ~A" (sku item))
	       (concatenate 'string (edit-tabs item "Images") (image-form item)
			    (item-gallery item)) 
	       (edit-bar "All items"))))

(restas:define-route r/edit-item/images/post
    ("/edit/item/:(sku)/images" :method :post)
  (if-let (item (get-item sku))
    (progn
      (when-let (picture (hunchentoot:post-parameter "picture"))
      (maybe-add-image picture item))
      (make-page (format nil "Editing images for ~A" (sku item))
		 (concatenate 'string (edit-tabs item "Images") (image-form item)
			      (item-gallery item))
		 (edit-bar "All items")))
    hunchentoot:+http-not-found+))


;; (make-page (format nil "Editing images for ~A" (sku item))
;; 	       (concatenate 'string (edit-tabs item) (image-form item)) 
;; 	       (edit-bar))))

(restas:define-route r/delete-tag
    ("/delete/tag/:(tag)")
  (if-let (obj (get-tag tag))
    (progn
      (dolist (item (ele:pset-list (tag-members obj)))
	(untag-item item obj))
      (ele:drop-instance obj)
      (hunchentoot:redirect (hunchentoot:referer))) 
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-item/tags
    ("/edit/item/:(sku)/tags")
  (if-let (item (get-item sku))
    (progn
      (when-let (toggle-tag (hunchentoot:get-parameter "tag"))
	(when-let (toggle-tag-obj (get-tag toggle-tag))
	  (if (tagged? item toggle-tag-obj)
	      (untag-item item toggle-tag-obj)
	      (tag-item item toggle-tag-obj))))
      (make-tags-page item)) 
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-item/contents
    ("/edit/item/:(sku)/contents")
  (cl-who:with-html-output-to-string (s)
    (format s "Editing ~A contents" sku)))

(restas:define-route r/edit-item/edit
    ("/edit/item/:(sku)/edit")
  (if-let (item (get-item sku))
    (edit-item-edit-page item)
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-item/edit/post
    ("/edit/item/:(sku)/edit" :method :post)
  (if-let (item (get-item sku))
    (progn
       (maybe-update item (fix-alist (hunchentoot:post-parameters*)))
       (edit-item-edit-page item))
    hunchentoot:+http-not-found+))


;; TAGS, collections of items
(restas:define-route r/new-tag
    ("/new/tag")
  (make-page "Create new tag" (tag-form) (edit-bar "New tag")))

(restas:define-route r/new-tag/post
    ("/new/tag" :method :post)
  (maybe-create 'tag (fix-alist (hunchentoot:post-parameters*))))

(restas:define-route r/edit-tags
    ("/edit/tags")
  (edit-tag-page #'identity "All tags"))

(restas:define-route r/edit-menu-tags
    ("/edit/tags/menu")
  (edit-tag-page #'appears-in-menu "Menu tags"))

(restas:define-route r/edit-featured-tags
    ("/edit/tags/featured")
  (edit-tag-page #'featured "Featured tags"))

(defun tag-display-page (tag)
  (with-html-output-to-string (s)
    (when (and (description tag) (not (zerop (length (description tag)))))
      (htm ((:div :class "well") (str (description tag)))))
    (when-let (thumbs (remove-if-not #'published
				   (ele:pset-list (tag-members tag))))
      (str (thumbnails thumbs #'render-thumb-display)))))

(restas:define-route r/view-tag
    ("/view/tag/:(tag)")
  (if-let (tag-object (get-tag tag))
    (make-page (tag-name tag-object)
	       (tag-display-page tag-object)
	       (main-site-bar (tag-name tag-object)))
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-tag/view 
    ("/edit/tag/:(tag)/view")
  (if-let (tag-object (get-tag tag))
    (make-page (tag-name tag-object)
	       (concatenate 'string
			    (edit-tabs tag-object "View")
			    (tag-display-page tag-object))
	       
	       
	       (edit-bar (tag-name tag-object)))
    hunchentoot:+http-not-found+))

(defun tag-edit-page (tag)
  (make-page (format nil "Editing ~A" (tag-name tag))
	     (concatenate 'string
			  (edit-tabs tag "Edit")
			  (tag-form tag))
	     (edit-bar "New tag")))

(restas:define-route r/edit-tag/edit
    ("/edit/tag/:(tag)/edit")
  (if-let (tag-obj (get-tag tag))
    (tag-edit-page tag-obj)
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-tag/edit/post
    ("/edit/tag/:(tag)/edit" :method :post)
  (if-let (tag-obj (get-tag tag))
    (progn
      (maybe-update tag-obj (fix-alist (hunchentoot:post-parameters*)))
      (tag-edit-page tag-obj))
    hunchentoot:+http-not-found+))


;; shopping cart
(restas:define-route r/add-to-cart
    ("/add-to-cart" :method :post)
  (maybe-add-items-to-cart)
  (hunchentoot:redirect (hunchentoot:referer)))


(defun maybe-add-items-to-cart ()
  (let* ((sku (hunchentoot:post-parameter "sku"))
	 (quantity (hunchentoot:post-parameter "number"))
	 (sku-item (get-item sku))
	 (valid-quantity (validate-number quantity)))
    (when (and sku-item valid-quantity)
      (add-item sku-item (get-or-initialize-cart) valid-quantity))))

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
  (dolist (p parameters)
    (if-let (item (get-item (car p)))
      (if-let (quantity (validate-number (cdr p)))
	(set-item-quantity item cart quantity)))))

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





	