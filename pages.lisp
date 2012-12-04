;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defun make-tags-page (item)
  (make-page
   (format nil "Tags for ~A" (title item))
   (concatenate 'string
		(edit-tabs item "Tags")
		(with-html-output-to-string (s)
		  (:h5 "Current tags")
		  (:p (str (render-tags (ele:pset-list (tags item)))))
		  (:h5 "Available tags")
		  (:p (str (render-tags (remove-if (lambda (tag)
						     (tagged? item tag))
						   (all-tags)))))))
   (edit-bar "All items")))
 


;; (hunchentoot:define-easy-handler (index-page :uri "/index.html")
;;     ()
;;   (standard-page "Welcome to sample store"
;; 		 (thumbnails (get-random-featured-items 20) #'display-short)))

(defun thumbnails (list render-func &optional (items-across 4))
  (let ((rows (partition-list list items-across)))
    (with-html-output-to-string (s)
      (dolist (row rows)
	(htm ((:div :class "row-fluid")
	      (dolist (item row)
		(let ((span (format nil "span~A" (truncate 12 items-across))))
		  (htm ((:div :class span)
			(str (funcall render-func item))))))))))))

;; (defun thumbnails (list render-func)
;;   (with-html-output-to-string (s)
;;     ((:ul :class "thumbnails")
;;      (dolist (obj list)
;;        (htm ((:li :class "span3")
;; 	     ((:div :class "thumbnail")
;; 	      (str (funcall render-func obj)))))))))

(defmethod render-very-short ((obj line-item))
  (with-html-output-to-string (s)
    ((:a :href (get-view-url obj))
     (when (images obj)
       (htm (str (display-a-small-image obj))))
     (:h5 (str (title obj))))))


(defmethod render-thumb ((obj line-item) &optional edit)
  (with-html-output-to-string (s)
    ((:a :href (get-view-url obj))
     (when (images obj)
       (htm (str (display-an-image obj)))))

    (when (not edit)
      (htm (str (cart-widget obj))))

    ((:a :href (get-view-url obj)) (:h5 (str (title obj))))
    
    (when edit
      (htm (:p (when (published obj)
		 (htm ((:span :class "label") "Published")
		      (when (featured obj)
			(htm ((:span :class "label label-success") "Featured"))))))))
    
    
    (:p (str (short-description obj))
	(:br)
	(:em "Price: ")
	(str (print-price (get-price obj))))

    (when edit
      (htm ((:a :class "btn btn-mini" :href (get-edit-edit-url obj))
	    "Edit")
	   ((:a :class "btn btn-mini btn-danger pull-right" :href (get-delete-url obj))
	    "Delete")))

))

(defmethod render-thumb ((obj tag) &optional edit)
  (with-html-output-to-string (s)
    ((:a :href (get-view-url obj))
     (htm (str (display-an-image obj)))
     (:h5 (str (tag-name obj))))
 
    (when edit
      (htm (:p (when (appears-in-menu obj)
	    (htm ((:span :class "label") "Menu")
		 (when (featured obj)
		   (htm ((:span :class "label label-success") "Featured"))))))))
    
    (:p (str (description obj)))
    
    (when edit
      (htm ((:a :class "btn btn-mini" :href (get-edit-edit-url obj))
	    "Edit")
	   ((:a :class "btn btn-mini btn-danger pull-right" :href (get-delete-url obj))
	    "Delete")))))

(defmethod get-delete-url ((tag tag))
  (format nil "/delete/tag/~A" (webform tag)))


;; (defmethod render-thumb-display ((obj line-item))
;;   (with-html-output-to-string (s)
;;     (when (images obj)
;;       (htm (str (display-an-image obj))))
;;     (:h5 (str (title obj)))
;;     (:p (str (short-description obj)))
;;     ))




(defun display-tag (tag)
  (when-let ((valid-tag (get-tag tag)))
    (let ((items (get-tagged-items valid-tag)))
      (make-page (tag-name valid-tag)
		 (thumbnails items #'display-short)
		 (sample-sidebar valid-tag)))))

;; (hunchentoot:define-easy-handler (display-tag :uri "/display-tag")
;;     (name)
;;   (when-let ((valid-tag (get-tag tag)))
;;     (let ((items (get-tagged-items valid-tag)))
;;       (make-page (tag-name valid-tag)
;; 		 (thumbnails items #'display-short)
;; 		 (sample-sidebar valid-tag)))))

;; (hunchentoot:define-easy-handler (add-to-bundle-page :uri "/add-to-bundle")
;;     ((bundleadd :parameter-type 'hash-table)
;;      sku)
;;   (case (hunchentoot:request-method*)
;;     (:post (when-let ((bundle (get-item sku))
;; 		      (items-to-add
;; 		       (let ((items '())) 
;; 				   (maphash (lambda (k v)
;; 					      (when-let (number (parse-integer
;; 								 v :junk-allowed t))
;; 						(push (cons (get-item k) number) items)))
;; 					    bundleadd)
;; 				   items)))
;; 	     (dolist (item items-to-add)
;; 	       (add-item (car item) bundle (cdr item)))
;; 	     (hunchentoot:redirect (get-url bundle))))))


;; (hunchentoot:define-easy-handler (new-single-item :uri "/single-item/new")
;;     ()
;;   (case (hunchentoot:request-method*)
;;     (:get (standard-page "Create new single item" (single-item-form)))
;;     (:post (maybe-create 'single-item (fix-alist (hunchentoot:post-parameters*))))))

(defun new-item-page ()
  (make-page "Create new single item" (item-form) (edit-bar "New item")))

(defun edit-item-edit-page (item &optional debug)
  (make-page (format nil "Editing ~A" (sku item))
	     (with-html-output-to-string (s)
	       (when debug
		 (htm (:pre (esc (format nil "~S" debug)))))
	       ;; (:pre (esc (format nil "~S" (hunchentoot:post-parameters*))))
	       ;; (:pre (esc (with-output-to-string (string)
	       ;; 		    (describe hunchentoot:*request* string))))
	       (str (edit-tabs item "Edit"))
	       (str (item-form item)))
	     (edit-bar "All items")))

(defun edit-bar (active)
  (nav-tabs `("Items"
	      ("/new/item" . "New item")
	      ("/edit/items" . "All items")
	      ("/edit/items/published" . "Published items")
	      ("/edit/items/unpublished" . "Unpublished items")
	      ("/edit/items/featured" . "Featured items")
	      "Tags"
	      ("/new/tag" . "New tag")
	      ("/edit/tags" . "All tags")
	      ("/edit/tags/menu" . "Menu tags")
	      ("/edit/tags/featured" . "Featured tags")
	      "Geo"
	      ("/new/geo" . "New geography")
	      ("/edit/geos" . "All geographies"))
	    active
	    :class "nav nav-list"))

(defun main-site-bar (active)
  (nav-tabs (append
	     (list "Featured")
	     (tag->nav (featured-tags))
	     (list "Categories")
	     (tag->nav (menu-tags)))
	    active
	    :class "nav nav-list"))

(defun nav-tabs (alist active &key (class "nav nav-tabs"))
  "ALIST cells of the form (URL . LABEL) or plain strings for headers"
  (with-html-output-to-string (s)
    ((:ul :class class)
     (dolist (item alist)
       (if (stringp item)
	   (htm ((:li :class "nav-header") (str item)))
	   (destructuring-bind (url . label)
	       item
	     (if (string-equal label active)
		 (htm ((:li :class "active")
		       ((:a :href url)
			(str label))))
		 (htm (:li
		       ((:a :href url)
			(str label)))))))))))


(defmethod edit-tabs ((item line-item) active)
  (nav-tabs `((,(get-edit-view-url item) . "View")
	      (,(get-edit-edit-url item) . "Edit")
	      (,(get-edit-image-url item) . "Images")
	      (,(get-edit-tags-url item) . "Tags")
	      (,(get-edit-contents-url item) . "Contents"))
	    active))

(defmethod edit-tabs ((tag tag) active)
  (nav-tabs `((,(get-edit-view-url tag) . "View")
	      (,(get-edit-edit-url tag) . "Edit"))
	    active))

(defun display-item-page (item)
  (make-page (format nil "Viewing ~A" (title item))
	     (concatenate 'string
			  (edit-tabs item "View")
			  (display-item-content item))
	     (edit-bar "All items")
	     (with-html-output-to-string (s)
	       (:script "$('.carousel').carousel()"))))


(defun display-item-content (item)
  (with-html-output-to-string (s)
    ((:div :class "row")
     ((:div :class "span5")
      (:h2 (str (title item)))
      

      ((:p :class "lead") (str (short-description item)))
      (:p (str (long-description item)))

      (str (cart-widget item))

      (when (not (empty? (get-children-qlist item)))
	(htm (:h5 "Contains")
	     (:ul (dolist (i (items (get-children-qlist item)))
		    (destructuring-bind (ii iq) i
		      (htm (:li (fmt "~A x ~A" iq (title ii))))))))))
     ((:div :class "span5")
      ((:div :class "well")
       (str (carousel "imageCarousel" (images item) #'full-image-element))
       ((:dl :class "dl-horizontal")
	(:dt "Price")
	(:dd (str (print-price (get-price item))))
	(:dt "Weight")
	(:dd (fmt "~A g" (get-weight item))))
       )))))

(defun full-image-element (image)
  (with-html-output-to-string (s)
    ((:p :align "center")
     (:img :src (get-full-url image)))))

(defun carousel (carousel-id elements render-function)
  (when elements
    (with-html-output-to-string (s)
      ((:div :id carousel-id :class "carousel slide")
       ((:div :class "carousel-inner")
	(let ((first (car elements)))
	  (htm ((:div :class "active item")
		(str (funcall render-function first)))))
	(dolist (element (cdr elements))
	  (htm ((:div :class "item")
		(str (funcall render-function element))))))
       ((:a :class "carousel-control left" :href (format nil "#~A" carousel-id)
	    :data-slide "prev") (str "&lsaquo;"))
       ((:a :class "carousel-control right" :href (format nil "#~A" carousel-id)
	    :data-slide "next") (str "&rsaquo;"))))))


  ;; (flet ((active? ())))

  ;; (with-html-output-to-string (s)
  ;;   ((:ul :class "nav nav-tabs")
  ;;    (:li ((:a href (get-edit-view-url item))
  ;; 	   "View"))
  ;;    (:li ((:a :href (get-edit-edit-url item))
  ;; 	   "Edit"))
  ;;    (:li ((:a :href (get-edit-image-url item))
  ;; 	   "Images"))
  ;;    (:li ((:a :href "#")
  ;; 	   "Tags"))
  ;;    (:li ((:a :href "#")
  ;; 	   "Contents")))))


;; (hunchentoot:define-easy-handler (new-single-item :uri "/single-item/new")
;;     ()
;;   (case (hunchentoot:request-method*)
;;     (:get (standard-page "Create new single item" (lambda (stream) (single-item-form stream))))
;;     (:post (maybe-create 'single-item (fix-alist (hunchentoot:post-parameters*))))))




;; (hunchentoot:define-easy-handler (new-bundle :uri "/bundle/new")
;;     ()
;;   (case (hunchentoot:request-method*)
;;     (:get (standard-page "Create new bundle" (lambda (stream) (bundle-form stream))))
;;     (:post (maybe-create 'bundle (fix-alist (hunchentoot:post-parameters*))))))

(defun edit-display-item (sku)
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


;; (hunchentoot:define-easy-handler (display-item :uri "/item") (sku)
;;   (when-let (item (get-item sku))
;;     (case (hunchentoot:request-method*)
;;       (:get (make-page (title item)
;; 		       (lambda (stream)
;; 			 (with-html-output (s stream)
;; 			   (funcall (header (store-name *web-store*)
;; 					    (title item)) s) 
;; 			   (lightbox-js s)
;; 			   (funcall (get-tabs item) stream)
;; 			   (tab-js stream)))
;; 		       (sample-sidebar item)))
;;       (:post (maybe-update item (fix-alist (hunchentoot:post-parameters*)))))))

;; (hunchentoot:define-easy-handler (tag-page :uri "/tags")
;;     (sku newtag (tags :parameter-type 'hash-table))
;;   (when-let (item (get-item sku))
;;     ;; update the tags for this item
;;     (maphash (lambda (k v)
;; 	       (when-let (tag (get-tag k))
;; 		 (if (string-equal v "on")
;; 		     (tag-item item tag)
;; 		     (untag-item item tag))))
;; 	     tags)

;;     ;; create and set a new tag, if one exists
;;     (when (and newtag (not (zerop (length newtag))))
;;       (if-let (tag (get-tag (get-webform newtag)))
;; 	(tag-item item tag)
;; 	(let ((tag (make-instance 'tag :name newtag)))
;; 	  (tag-item item tag))))
;;     (hunchentoot:redirect (get-url item))))

;; (hunchentoot:define-easy-handler (add-to-cart :uri "/shopping-cart")
;;     (sku number)
;;   (hunchentoot:log-message* :info "~A" (hunchentoot:post-parameters*))
;;   (case (hunchentoot:request-method*)
;;     (:get (display-shopping-cart))
;;     (:post (let ((item (get-item sku))
;; 		 (quantity (validate-number number)))
;; 	     (when (and item quantity)
;; 	       (hunchentoot:log-message* :debug "Got item ~A~%" item)
;; 	       (let ((cart (get-or-initialize-cart)))
;; 		 (add-item item cart quantity))))
;; 	   (display-shopping-cart))))

;; (hunchentoot:define-easy-handler (single-items-list :uri "/single-items")
;;     ()
;;   (standard-page "List of all single items"
;; 		 (lambda (stream)
;; 		   (with-html-output (s stream)
;; 		     ((:div :class "span-24")
;; 		      (:pre (fmt "~{~A~^~%~}" (hunchentoot:post-parameters*)))
;; 		      ((:form :method "post" :action "/single-items")
;; 		       (:input :type "submit" :value "Delete")
;; 		       (:table
;; 			(:tr (:th "Delete")
;; 			     (:th "SKU")
;; 			     (:th "Item"))
;; 			(dolist (i (ele:get-instances-by-class 'single-item))
;; 			  (htm 
;; 			   (:tr (:td (:input :type "checkbox" :name (sku i)))
;; 				(:td ((:a :href (get-url i))
;; 				      (str (sku i))))
;; 				(:td (str (title i)))))))
;; 		       (:input :type "submit" :value "Delete")))))))

;; (hunchentoot:define-easy-handler (bundles-list :uri "/bundles")
;;     ()
;;   (standard-page "List of all bundles"
;; 		 (lambda (stream)
;; 		   (with-html-output (s stream)
;; 		     ((:div :class "span-24")
;; 		      (:pre (fmt "~{~A~^~%~}" (hunchentoot:post-parameters*)))
;; 		      ((:form :method "post" :action "/bundles")
;; 		       (:input :type "submit" :value "Delete")
;; 		       (:table
;; 			(:tr (:th "Delete")
;; 			     (:th "SKU")
;; 			     (:th "Item"))
;; 			(dolist (i (ele:get-instances-by-class 'bundle))
;; 			  (htm 
;; 			   (:tr (:td (:input :type "checkbox" :name (sku i)))
;; 				(:td ((:a :href (get-url i))
;; 				      (str (sku i))))
;; 				(:td (str (title i)))))))
;; 		       (:input :type "submit" :value "Delete")))))))



;; (hunchentoot:define-easy-handler (display-geo :uri "/geo") (n)
;;   (when-let (geo (get-geo n))
;;     (case (hunchentoot:request-method*)
;;       (:get (make-page (title item)
;; 		       (lambda (stream)
;; 			 (with-html-output (s stream)
;; 			   (funcall (header (store-name *web-store*)
;; 					    (title item)) s) 
;; 			   (lightbox-js s)
;; 			   ;; (funcall (get-tabs item) stream)
;; 			   (:h1 (str (geo-name geo)))
;; 			   (tab-js stream)))
;; 		       (sample-sidebar item)))
;;       (:post (maybe-update item (fix-alist (hunchentoot:post-parameters*)))))))

