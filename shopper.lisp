(restas:define-module #:shopper
  (:use #:cl #:cl-who #:alexandria)
  (:export #:get-item
	   #:new-item-page
	   #:maybe-create
	   #:maybe-update
	   #:items
	   #:images
	   #:basic-page
	   #:get-image-number-as-string
	   #:get-tag
	   #:tag-members
	   #:tag
	   #:fix-alist
	   #:tags
	   #:item
	   #:items
	   #:untag-item
	   #:*web-store*
	   #:edit-store-page
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
	   #:image-edit-page
	   #:static-content
	   #:get-content-from-webform
	   #:static-content-edit-page
	   #:static-content-new-page
	   #:edit-static-content-page))


(in-package #:shopper)

;; set the HTML5 doctype
(setf *prologue* "<!DOCTYPE html>")

(defclass http-auth-route (routes:proxy-route) ())

(defclass https-require (routes:proxy-route) ())

(defclass shopper-acceptor (restas:restas-acceptor)
  ())

(defclass shopper-ssl-acceptor (restas:restas-acceptor)
  ())

(defun start-shopper (&optional store-path)
  (unless *web-store*
    (open-web-store store-path))
  (mount-webstore-content)
  (restas:start '#:shopper :port 9292
		:acceptor-class 'shopper-acceptor)
  (restas:start '#:shopper-edit :port 9292
		:acceptor-class 'shopper-ssl-acceptor)
  (restas:start '#:shopper-login :port 9292
		:acceptor-class 'shopper-ssl-acceptor))

(defun log-describe (object)
  (hunchentoot:log-message* :debug "~A"
			    (with-output-to-string (s)
			      (describe object s))))

(defun generate-secure-url (route &rest args)
  (let ((uri (restas::genurl/impl (concatenate 'list
					       (restas::submodule-full-baseurl restas:*submodule*)
					       (restas::route-symbol-template (get-current-route-symbol)))
				  args)))
    (setf (puri:uri-scheme uri)
	  :https)
    (setf (puri:uri-host uri)
          (if (boundp 'hunchentoot:*request*)
                      (hunchentoot:host)
                      "localhost"))
    (puri:render-uri uri nil)))

(defun get-current-route-symbol ()
  (restas:route-symbol (routes:proxy-route-target restas:*route*)))

(defmethod routes:route-check-conditions ((route http-auth-route) bindings)
					;  (log-describe hunchentoot:*session*)
  (hunchentoot:log-message* :debug "SSLP: ~A" (hunchentoot:ssl-p))
  (hunchentoot:log-message* :debug "Host: ~A" (hunchentoot:host))
;  (hunchentoot:log-message* :debug "Secure URI: ~A" (generate-secure-url route bindings))
  (hunchentoot:log-message* :debug "Script: ~A" (hunchentoot:script-name*))
  (hunchentoot:log-message* :debug "URI: ~A" (hunchentoot:request-uri*))

  (cond ((null (hunchentoot:session-value :user))
  	 (hunchentoot:log-message*
  	  :debug "No user session found, redirecting to /login")
  	 (hunchentoot:redirect "/login"))
	
  	((not (equal (hunchentoot:header-in* "X-Forwarded-Proto")
  		     "https"))
	 (hunchentoot:log-message* :debug "HTTP found, switching to https")
	 (let ((request-uri (puri:uri (hunchentoot:request-uri*))))
	   (setf (puri:uri-host request-uri) (hunchentoot:host))
	   (setf (puri:uri-scheme request-uri) :https)
	   (hunchentoot:log-message* :debug "Redirect to: ~A" (puri:render-uri request-uri nil))
	   (hunchentoot:redirect (puri:render-uri request-uri nil))))
	
  	(t (call-next-method))))


(defmethod routes:route-check-conditions ((route https-require) bindings)
  (hunchentoot:log-message* :debug "SSLP: ~A" (hunchentoot:ssl-p))
  (if (not (equal (hunchentoot:header-in* "X-Forwarded-Proto")
  		  "https"))
      (let* ((route-symbol (get-current-route-symbol))
	     (route-uri (puri:uri (restas:gen-full-url route-symbol))))
	(setf (puri:uri-scheme route-uri) :https)
	;; (log-describe (with-output-to-string (s)
	;; 		   (puri:render-uri route-uri s)))
	(hunchentoot:redirect (with-output-to-string (s)
				(puri:render-uri route-uri s))))
      (call-next-method)))




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
  ;; (restas:mount-submodule admin-interface (#:shopper-edit @http-auth-require))
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







	