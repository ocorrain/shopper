(in-package #:shopper)

(restas:define-module #:shopper-edit
    (:use #:cl #:shopper #:restas #:alexandria)
  (:decorators '@http-auth-require))

(in-package #:shopper-edit)

;; ITEMs, bundles or single items

; new items

(restas:define-route r/new-item
    ("new/item")
  (new-item-page))

(restas:define-route r/new-item/post
    ("new/item" :method :post)
  (maybe-create 'item (fix-alist (hunchentoot:post-parameters*))))

(restas:define-route r/delete-item
    ("delete/item/:(sku)")
  (if-let (obj (get-item sku))
    (progn
      (dolist (tag (ele:pset-list (tags obj)))
	(untag-item obj tag))
      (ele:remove-kv sku (items *web-store*))
      (ele:drop-instance obj)
      (hunchentoot:redirect (hunchentoot:referer))) 
    hunchentoot:+http-not-found+))

(restas:define-route /r/edit
    ("/edit")
  (edit-front-page))

(restas:define-route r/edit-items
    ("edit/items")
  (edit-item-page #'identity "All items"))

(restas:define-route r/edit-published
    ("edit/items/published")
  (edit-item-page #'published "Published items"))

(restas:define-route r/edit-unpublished
    ("edit/items/unpublished")
  (edit-item-page (lambda (item) (not (published item))) "Unpublished items"))

(restas:define-route r/edit-featured
    ("edit/items/featured")
  (edit-item-page #'featured "Featured items"))

(restas:define-route r/edit-item/view 
    ("edit/item/:(sku)/view")
  (if-let (item (get-item sku))
    (display-item-page item)
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-item/images
    ("edit/item/:(sku)/images")
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
    ("edit/item/:(sku)/images" :method :post)
  (if-let (item (get-item sku))
    (progn
      (when-let (picture (hunchentoot:post-parameter "picture"))
	(maybe-add-image picture item))
      (image-edit-page item))
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-item/contents
    ("edit/item/:(sku)/contents")
  (if-let (item (get-item sku))
    (bundle-edit-page item)
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-item/contents/post
    ("edit/item/:(sku)/contents" :method :post)
  (if-let (item (get-item sku))
    (progn (maybe-update-bundle item)
	   (bundle-edit-page item))
    hunchentoot:+http-not-found+))

(restas:define-route r/delete-tag
    ("delete/tag/:(tag)")
  (if-let (obj (get-tag tag))
    (progn
      (dolist (item (ele:pset-list (tag-members obj)))
	(untag-item item obj))
      (ele:drop-instance obj)
      (hunchentoot:redirect (hunchentoot:referer))) 
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-item/tags
    ("edit/item/:(sku)/tags")
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
    ("edit/item/:(sku)/edit")
  (if-let (item (get-item sku))
    (edit-item-edit-page item)
    hunchentoot:+http-not-found+))

(restas:define-route r/edit-item/edit/post
    ("edit/item/:(sku)/edit" :method :post)
  (if-let (item (get-item sku))
    (progn
      (update-geos item (hunchentoot:post-parameters*))
      (maybe-update item (fix-alist (hunchentoot:post-parameters*)))
      (edit-item-edit-page item))
    hunchentoot:+http-not-found+))

;; TAGS

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
  (edit-tag-page (lambda (tag)
		   (and (featured tag) (appears-in-menu tag))) "Featured tags"))

(restas:define-route r/edit-tag/view 
    ("/edit/tag/:(tag)/view")
  (if-let (tag-object (get-tag tag))
    (make-page (tag-name tag-object)
	       (concatenate 'string
			    (edit-tabs tag-object "View")
			    (tag-display-page tag-object))
	       
	       
	       (edit-bar (tag-name tag-object)))
    hunchentoot:+http-not-found+))


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

;; geography

(restas:define-route geo/new
    ("/new/geo")
  (make-page "Create a new geography" (geo-form) (edit-bar "New geography")))

(restas:define-route geo/new/post
    ("/new/geo" :method :post)
  (when-let (name (hunchentoot:post-parameter "title"))
    (if-let (already-existing (ele:get-instance-by-value 'geography
							  'geography-name
							  name))
	     (hunchentoot:redirect (get-geo-edit-url already-existing))
	     (let ((new-geo (make-instance 'geography :name name)))
	       (setf (geo-members new-geo)
		     (remove-if-not (lambda (p)
				      (get-country-info-from-iso-code p))
				    (mapcar #'car (hunchentoot:post-parameters*))))
	       (hunchentoot:redirect (get-geo-edit-url new-geo))))))

(restas:define-route geo/delete
    ("geo/delete/:(geoid)")
  (when-let (geo (ele:get-instance-by-value 'geography
					    'geography-name
					    (hunchentoot:url-decode geoid)))
    (ele:drop-instance geo)
    (hunchentoot:redirect "/edit/geos")))

(restas:define-route geos/edit
    ("edit/geos")
  (edit-geographies-page))

(restas:define-route geo/edit
    ("edit/geo/:(geoid)")
  (geo-form-page geoid))

(restas:define-route geo/edit/post
    ("edit/geo/:(geoid)" :method :post)
  (geo-form-page geoid (hunchentoot:post-parameters*)))

(restas:define-route geo/edit/postage
    ("edit/geo/:(geoid)/postage")
  (shopper::geo-postage-page geoid (hunchentoot:get-parameters*)))

(restas:define-route store/edit/parameters
    ("edit/store")
  (edit-store-page))

(restas:define-route store/edit/parameters/post
    ("edit/store" :method :post)
  (edit-store-page (hunchentoot:post-parameters*)))
