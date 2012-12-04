(restas:define-module #:shopper
  (:use #:cl #:cl-who #:alexandria)
  (:export #:get-item
	   #:new-item-page
	   #:maybe-create
	   #:maybe-update
	   #:items
	   #:images
	   #:get-image-number-as-string
	   #:get-tag
	   #:tag-members
	   #:fix-alist
	   #:tags
	   #:item
	   #:items
	   #:untag-item
	   #:*web-store*
	   #:make-page
	   #:thumbnails
	   #:collect-items-with
	   #:render-thumb
	   #:edit-bar
	   #:published
	   #:featured
	   #:display-item-page
	   #:edit-item-page
	   #:edit-front-page
	   #:image-edit-page
	   #:maybe-add-image
	   #:bundle-edit-page
	   #:maybe-update-bundle
	   #:untag-item
	   #:toggle-tag
	   #:tagged?
	   #:tag-item
	   #:make-tags-page
	   #:tag-form
	   #:edit-tag-page
	   #:appears-in-menu
	   #:tag-name
	   #:edit-tabs
	   #:tag-display-page
	   #:tag-edit-page
	   #:geo-form
	   #:geo-form-page
	   #:get-geo-edit-url
	   #:geo-members
	   #:get-country-info-from-iso-code
	   #:edit-geographies-page
	   #:edit-item-edit-page
	   #:update-geos
	   #:geo-postage-page
	   #:image-edit-page)
  )

(in-package #:shopper)

;; set the HTML5 doctype
(setf *prologue* "<!DOCTYPE html>")

(defclass http-auth-route (routes:proxy-route) ())

(defmethod routes:route-check-conditions ((route http-auth-route) bindings)
  (and (call-next-method)
       (multiple-value-bind (user password) (hunchentoot:authorization)
         (or (and (string= user "hello")
                  (string= password "world"))
             (hunchentoot:require-authorization)))))

(defun @http-auth-require (route)
  (make-instance 'http-auth-route :target route))

(defun mount-webstore-content ()
  (restas:mount-submodule webstore-images (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("images"))
    (restas.directory-publisher:*directory* (image-path *web-store*))
    (restas.directory-publisher:*autoindex* nil))
  (restas:mount-submodule twitter-bootstrap-files (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("s"))
    (restas.directory-publisher:*directory* (get-twitter-bootstrap-path))
    (restas.directory-publisher:*autoindex* nil))
  (restas:mount-submodule admin-interface (#:shopper-edit @http-auth-require))
  (restas:reconnect-all-routes))


(defun maybe-update-bundle (bundle)
  (dolist (item-q (get-valid-objects-from-post (hunchentoot:post-parameters*)))
    (destructuring-bind (item . quantity) item-q
      (when (not (equal item bundle))
	(set-bundle-quantity item bundle quantity)))))






;; (make-page (format nil "Editing images for ~A" (sku item))
;; 	       (concatenate 'string (edit-tabs item) (image-form item)) 
;; 	       (edit-bar))))




(defun get-valid-objects-from-post (parameters)
  (let ((valid '()))
    (dolist (p parameters)
      (when-let (item (get-item (car p)))
	(when-let (quantity (validate-number (cdr p)))
	  (push (cons item quantity) valid))))
    valid))

(defun add-get-parameters-to-url (url alist)
  (dolist (entry alist)
    (setf url (url-rewrite:add-get-param-to-url
	       url
	       (format nil "~A" (car entry))
	       (format nil "~A" (cdr entry)))))
  url)







	