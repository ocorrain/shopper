;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

;; tags will be implemented as persistent objects with a persistent
;; set of members.  Circularly, line items contain a list of tags that
;; are associated with them

(ele:defpclass tag ()
  ((name :initarg :name :initform "" :accessor tag-name :index t
	 :documentation "Tag name" :type string)
   (webform :accessor webform :index t :documentation "Web safe form
   of the tag name for transmission" :type string)
   (members :initarg :members :initform (ele:make-pset) :accessor tag-members
	    :documentation "Pset of items tagged with this tag")))

(defmethod initialize-instance :after ((instance tag) &rest stuff)
  (declare (ignore stuff))
  (setf (webform instance) (get-webform (tag-name instance))))

(defmethod tag-item ((item line-item) (tag tag))
  (ele:with-transaction ()
    (ele:insert-item item (tag-members tag))
    (ele:insert-item tag (tags item))))


(defmethod untag-item ((item line-item) (tag tag))
  (ele:remove-item item (tag-members tag))
  (ele:remove-item item (tags item)))

(defun tagged? (item tag)
  (ele:find-item tag (tags item)))

(defun all-tags ()
  (ele:get-instances-by-class 'tag))

(defun get-tag (webform)
  (ele:get-instance-by-value 'tag 'webform webform))

(defun get-webform (tag-title)
  (string-downcase (remove-if-not #'alpha-char-p tag-title)))

(defun tag-widget-printer (item stream)
  ;; FIXME, this is broken somehow
  (with-html-output (s stream)
    ((:div :id "tags")
     ((:form :action "/tags" :method "post")
      (when (all-tags)
	(htm (fmt "Select from the following tags:")
	     (:br))
	(dolist (tag (all-tags))
	  (htm ((:label :for (webform tag)) (esc (tag-name tag)))
	       (:input :id (webform tag)
		       :name (format nil "tags{~A}" (webform tag))
		       :type "checkbox" :checked (tagged? item tag))
	       (:br))))
      (:br)
      ((:label :for "newtag") "Create a new tag and tag this item with it")
      (:input :type "text" :id "newtag" :name "newtag")
      (:input :type "hidden" :name "sku" :value (sku item))
      (:br)
      (:input :type "submit" :value "tag")))))

(defun get-tag-linked-list (item stream)
  (with-slots (tags) item
    (list-of-tags (ele:pset-list tags) stream)))

(defun list-of-tags (tags stream)
  (with-html-output (s stream)
    (:ul
     (dolist (tag tags)
       (htm (:li ((:a :href (get-tag-url tag)) (str (tag-name tag)))))))))

(defun get-tag-url (tag)
  (url-rewrite:add-get-param-to-url "/display-tag" "name" (webform tag)))

(defun get-tagged-items (tag)
  (ele:pset-list (tag-members tag)))

(defmethod get-tags ((item line-item))
  (ele:pset-list (tags item)))

