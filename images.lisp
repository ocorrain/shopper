;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defun create-thumbnail (filename thumbname width height)
  "Create a thumbnail the image in FILENAME with a max size of WIDTH x HEIGHT
pixel (but with the original aspect ratio) and save it in THUMBNAME."
  (if (or (pathnamep filename)
	  (pathnamep thumbname))
      (create-thumbnail (namestring filename) (namestring thumbname) width height)
      (lisp-magick:with-magick-wand (wand :load filename)
	(let ((a (/ (lisp-magick:magick-get-image-width wand)
		    (lisp-magick:magick-get-image-height wand))))
	  (if (> a (/ width height))
	      (lisp-magick:magick-scale-image wand width (truncate (/ width a)))
	      (lisp-magick:magick-scale-image wand (truncate (* a height)) height)))
	(lisp-magick:magick-write-image wand thumbname))))

(defun get-thumb-url (path)
  (concatenate 'string "/pics/"
	       (namestring (get-thumb-path path))))

(defun get-full-url (path)
  (concatenate 'string "/pics/"
	       (namestring (get-full-size-path path))))

(defun resize-all-images ()
  (ele:map-btree (lambda (key item)
		   (declare (ignore key))
		   (when-let (images (images item))
		     (dolist (i images)
		       (let ((dest-path (make-pathname
					 :name (pathname-name i) :type (pathname-type i)
					 :defaults (image-path *web-store*))))

			 (create-thumbnail dest-path (get-thumb-path dest-path)
					   (get-config-option :thumbnail-width)
					   (get-config-option :thumbnail-height))
			 (create-thumbnail dest-path (get-full-size-path dest-path)
					   (get-config-option :display-width)
					   (get-config-option :display-height))))))
		 (items *web-store*)))

(defun display-gallery (images id)
  (lambda (stream)
    (let ((thumb-width (get-config-option :thumbnail-width))
	  (thumb-height (get-config-option :thumbnail-height)))
      (with-html-output (s stream)
	(lightbox-gallery s id)
	((:div :id id)
	 (:ul
	  (dolist (i images)
	    (htm (:li ((:a :href (get-full-url i))
		       (:img :src (get-thumb-url i))))))))))))


(defmethod edit-display-images ((item line-item) stream)
  (when (images item)
    (let ((thumb-width (get-config-option :thumbnail-width))
	  (thumb-height (get-config-option :thumbnail-height)))
      (with-html-output (s stream)
	(lightbox-gallery s "gallery")
	((:div :id "gallery")
	 ((:form :action (get-url item) :method :post)
	  (:ul
	   (dolist (i (images item))
	     (htm (:li ((:a :href (get-full-url i))
			(:img :src (get-thumb-url i)))
		       (:input :type "checkbox" :name "imgdel" :value i)))))
	  (:input :type "submit" :value "Delete")))))))

(defmethod display-images ((item line-item) stream)
  (when (images item)
    (let ((thumb-width (get-config-option :thumbnail-width))
	  (thumb-height (get-config-option :thumbnail-height)))
      (with-html-output (s stream)
	((:div :id "gallery")
	 (:ul
	  (dolist (i (images item))
	    (htm (:li ((:a :href (get-full-url i))
		       (:img :src (get-thumb-url i))))))))))))

(defun add-image (path line-item)
  (let ((type (string-downcase (pathname-type path)))
	(stub (get-next-image-stub line-item)))
    (let ((dest-path (make-pathname
		      :name stub :type type
		      :defaults (image-path *web-store*))))
      (cl-fad:copy-file path dest-path)
      (create-thumbnail dest-path (get-thumb-path dest-path)
			(get-config-option :thumbnail-width)
			(get-config-option :thumbnail-height))
      (create-thumbnail dest-path (get-full-size-path dest-path)
			(get-config-option :display-width)
			(get-config-option :display-height))
      (push (make-pathname :name stub :type type) (images line-item)))))
