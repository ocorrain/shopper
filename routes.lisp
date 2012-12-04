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
  (restas:mount-submodule admin-interface (#:shopper-edit)
    (shopper-edit:*baseurl* '("edit")))
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








	