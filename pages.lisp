;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(hunchentoot:define-easy-handler (index-page :uri "/index.html")
    ()
  (standard-page "Welcome to sample store"
		 (lambda (stream)
	       (display-table 5 (get-random-featured-items 20) #'display-short stream))))

(hunchentoot:define-easy-handler (display-tag :uri "/display-tag")
    (name)
  (when-let ((tag (get-tag name)))
    (let ((items (get-tagged-items tag)))
      (make-page (tag-name tag)
		 (lambda (stream)
		   (funcall (header (store-name *web-store*)
				    (tag-name tag)) stream)
		   (with-html-output (s stream)
		     (when-let (bundles (remove 'single-item items :key #'type-of))
		       (htm (:h2 "Bundles")
			    (display-table 5 bundles
					   #'display-short s)))

		     (when-let (single-items (remove 'bundle items :key #'type-of))
		       (htm (:h2 "Single-items")
			    (display-table 5 single-items
					   #'display-short s)))))
		 
		 (sample-sidebar tag)))))

(hunchentoot:define-easy-handler (add-to-bundle-page :uri "/add-to-bundle")
    ((bundleadd :parameter-type 'hash-table)
     sku)
  (case (hunchentoot:request-method*)
    (:post (when-let ((bundle (get-item sku))
		      (items-to-add
		       (let ((items '())) 
				   (maphash (lambda (k v)
					      (when-let (number (parse-integer
								 v :junk-allowed t))
						(push (cons (get-item k) number) items)))
					    bundleadd)
				   items)))
	     (dolist (item items-to-add)
	       (add-item (car item) bundle (cdr item)))
	     (hunchentoot:redirect (get-url bundle))))))


(hunchentoot:define-easy-handler (new-single-item :uri "/single-item/new")
    ()
  (case (hunchentoot:request-method*)
    (:get (standard-page "Create new single item" (lambda (stream) (single-item-form stream))))
    (:post (maybe-create 'single-item (fix-alist (hunchentoot:post-parameters*))))))




(hunchentoot:define-easy-handler (new-bundle :uri "/bundle/new")
    ()
  (case (hunchentoot:request-method*)
    (:get (standard-page "Create new bundle" (lambda (stream) (bundle-form stream))))
    (:post (maybe-create 'bundle (fix-alist (hunchentoot:post-parameters*))))))

(hunchentoot:define-easy-handler (display-item :uri "/item") (sku)
  (when-let (item (get-item sku))
    (case (hunchentoot:request-method*)
      (:get (make-page (title item)
		       (lambda (stream)
			 (with-html-output (s stream)
			   (funcall (header (store-name *web-store*)
					    (title item)) s) 
			   (lightbox-js s)
			   (funcall (get-tabs item) stream)
			   (tab-js stream)))
		       (sample-sidebar item)))
      (:post (maybe-update item (fix-alist (hunchentoot:post-parameters*)))))))

(hunchentoot:define-easy-handler (tag-page :uri "/tags")
    (sku newtag (tags :parameter-type 'hash-table))
  (when-let (item (get-item sku))
    ;; update the tags for this item
    (maphash (lambda (k v)
	       (when-let (tag (get-tag k))
		 (if (string-equal v "on")
		     (tag-item item tag)
		     (untag-item item tag))))
	     tags)

    ;; create and set a new tag, if one exists
    (when (and newtag (not (zerop (length newtag))))
      (if-let (tag (get-tag (get-webform newtag)))
	(tag-item item tag)
	(let ((tag (make-instance 'tag :name newtag)))
	  (tag-item item tag))))
    (hunchentoot:redirect (get-url item))))

(hunchentoot:define-easy-handler (add-to-cart :uri "/shopping-cart")
    (sku number)
  (hunchentoot:log-message* :info "~A" (hunchentoot:post-parameters*))
  (case (hunchentoot:request-method*)
    (:get (display-shopping-cart))
    (:post (let ((item (get-item sku))
		 (quantity (validate-number number)))
	     (when (and item quantity)
	       (hunchentoot:log-message* :debug "Got item ~A~%" item)
	       (let ((cart (get-or-initialize-cart)))
		 (add-item item cart quantity))))
	   (display-shopping-cart))))

(hunchentoot:define-easy-handler (single-items-list :uri "/single-items")
    ()
  (standard-page "List of all single items"
		 (lambda (stream)
		   (with-html-output (s stream)
		     ((:div :class "span-24")
		      (:pre (fmt "~{~A~^~%~}" (hunchentoot:post-parameters*)))
		      ((:form :method "post" :action "/single-items")
		       (:input :type "submit" :value "Delete")
		       (:table
			(:tr (:th "Delete")
			     (:th "SKU")
			     (:th "Item"))
			(dolist (i (ele:get-instances-by-class 'single-item))
			  (htm 
			   (:tr (:td (:input :type "checkbox" :name (sku i)))
				(:td ((:a :href (get-url i))
				      (str (sku i))))
				(:td (str (title i)))))))
		       (:input :type "submit" :value "Delete")))))))

(hunchentoot:define-easy-handler (bundles-list :uri "/bundles")
    ()
  (standard-page "List of all bundles"
		 (lambda (stream)
		   (with-html-output (s stream)
		     ((:div :class "span-24")
		      (:pre (fmt "~{~A~^~%~}" (hunchentoot:post-parameters*)))
		      ((:form :method "post" :action "/bundles")
		       (:input :type "submit" :value "Delete")
		       (:table
			(:tr (:th "Delete")
			     (:th "SKU")
			     (:th "Item"))
			(dolist (i (ele:get-instances-by-class 'bundle))
			  (htm 
			   (:tr (:td (:input :type "checkbox" :name (sku i)))
				(:td ((:a :href (get-url i))
				      (str (sku i))))
				(:td (str (title i)))))))
		       (:input :type "submit" :value "Delete")))))))


