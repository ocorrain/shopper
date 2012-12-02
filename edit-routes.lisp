(in-package #:shopper)

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
			 (lambda (item)
			   (render-thumb item t)))
	     (edit-bar title)))

(restas:define-route /r/edit
    ("/edit")
  (make-page
   "Edit"
   (with-html-output-to-string (s)
     ((:div :class "hero-unit")
      (:h1 "Edit and create items")
      (:p "Here you can add new items, and edit existing items")
      ((:a :class "btn btn-primary btn-large" :href "/new/item")
       "Create a new item")
      ((:a :class "btn btn-primary btn-large pull-right" :href "/edit/items")
       "Edit an existing item"))

     ((:div :class "hero-unit")
      (:h1 "Edit and create tags")
      (:p "Here you can add new items, and edit existing items")
      ((:a :class "btn btn-primary btn-large" :href "/new/tag")
       "Create a new tag")
      ((:a :class "btn btn-primary btn-large pull-right" :href "/edit/tags")
       "Edit an existing tag")))
   (edit-bar "Edit")))


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
      (update-geos item (hunchentoot:post-parameters*))
      (maybe-update item (fix-alist (hunchentoot:post-parameters*)))
      (edit-item-edit-page item))
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