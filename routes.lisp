(restas:define-module #:shopper
  (:use #:cl #:cl-who #:alexandria))

(in-package #:shopper)

;; set the HTML5 doctype
(setf *prologue* "<!DOCTYPE html>")

(defun mount-webstore-content ()
  (restas:mount-submodule webstore-images (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("images"))
    (restas.directory-publisher:*directory* (image-path *web-store*))
    (restas.directory-publisher:*autoindex* nil))
  (restas:mount-submodule twitter-bootstrap-files (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("s"))
    (restas.directory-publisher:*directory* (get-twitter-bootstrap-path))
    (restas.directory-publisher:*autoindex* nil))
  (restas:reconnect-all-routes))


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

(defun get-image-number-as-string (image)
  (second (split-sequence:split-sequence #\_
					 (pathname-name image))))

(defun image-edit-page (item)
  (let ((this-url (format nil "/edit/item/~A/images" (sku item))))
    (make-page (format nil "Editing images for ~A" (sku item))
	       (concatenate 'string (edit-tabs item "Images") (image-form item)
			    (image-thumbnails (images item)
					      (lambda (image)
						(with-html-output-to-string (s)
						  (:img :src (get-thumb-url image))
						  (:br)
						  ((:a :class "btn btn-danger"
						       :href (url-rewrite:add-get-param-to-url this-url
											       "delete"
											       (get-image-number-as-string image)))
						   "Delete"))))) 
	       (edit-bar "All items"))))

(restas:define-route r/edit-item/images
    ("/edit/item/:(sku)/images")
  (if-let (item (get-item sku))
    (progn
      (when-let (image-to-delete (hunchentoot:get-parameter "delete"))
	(setf (images item) (remove-if (lambda (i)
					 (string-equal image-to-delete
						       (get-image-number-as-string i)))
				       (images item))))
      (image-edit-page item))
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-item/images/post
    ("/edit/item/:(sku)/images" :method :post)
  (if-let (item (get-item sku))
    (progn
      (when-let (picture (hunchentoot:post-parameter "picture"))
	(maybe-add-image picture item))
      (image-edit-page item))
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-item/contents
    ("/edit/item/:(sku)/contents")
  (if-let (item (get-item sku))
    (bundle-edit-page item)
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-item/contents/post
    ("/edit/item/:(sku)/contents" :method :post)
  (if-let (item (get-item sku))
    (progn (maybe-update-bundle item)
	   (bundle-edit-page item))
    hunchentoot:+http-not-found+))

(defun maybe-update-bundle (bundle)
  (dolist (item-q (get-valid-objects-from-post (hunchentoot:post-parameters*)))
    (destructuring-bind (item . quantity) item-q
      (when (not (equal item bundle))
	(set-bundle-quantity item bundle quantity)))))






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

;; (restas:define-route r/edit-item/contents
;;     ("/edit/item/:(sku)/contents")
;;   (cl-who:with-html-output-to-string (s)
;;     (format s "Editing ~A contents" sku)))

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



(defun get-valid-objects-from-post (parameters)
  (let ((valid '()))
    (dolist (p parameters)
      (when-let (item (get-item (car p)))
	(when-let (quantity (validate-number (cdr p)))
	  (push (cons item quantity) valid))))
    valid))








	