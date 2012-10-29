(in-package #:shopper)


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
  (edit-tag-page (lambda (tag)
		   (and (featured tag) (appears-in-menu tag))) "Featured tags"))

(defun edit-tag-page (test title)
  (make-page title
	     (thumbnails (collect-tags-with test)
			 (lambda (item)
			   (render-thumb item t)))
	     (edit-bar title)))


(defun tag-display-page (tag)
  (with-html-output-to-string (s)
    (when (and (description tag) (not (zerop (length (description tag)))))
      (htm ((:div :class "well") (str (description tag)))))
    (when-let (thumbs (remove-if-not #'published
				   (ele:pset-list (tag-members tag))))
      (str (thumbnails thumbs #'render-thumb)))))

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

