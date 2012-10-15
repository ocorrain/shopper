;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

;; (defun create-thumbnail (filename thumbname width height)
;;   "Create a thumbnail the image in FILENAME with a max size of WIDTH x HEIGHT
;; pixel (but with the original aspect ratio) and save it in THUMBNAME."
;;   (if (or (pathnamep filename)
;; 	  (pathnamep thumbname))
;;       (create-thumbnail (namestring filename) (namestring thumbname) width height)
;;       (lisp-magick:with-magick-wand (wand :load filename)
;; 	(let ((a (/ (lisp-magick:magick-get-image-width wand)
;; 		    (lisp-magick:magick-get-image-height wand))))
;; 	  (if (> a (/ width height))
;; 	      (lisp-magick:magick-scale-image wand width (truncate (/ width a)))
;; 	      (lisp-magick:magick-scale-image wand (truncate (* a height)) height)))
;; 	(lisp-magick:magick-write-image wand thumbname))))


(defun create-thumbnail (filename thumbname width height)
  "Create a thumbnail the image in FILENAME with a max size of WIDTH x HEIGHT
pixel (but with the original aspect ratio) and save it in THUMBNAME."
  (if (or (pathnamep filename)
	  (pathnamep thumbname))
      (create-thumbnail (namestring filename) (namestring thumbname) width height)
      (lisp-magick:with-magick-wand (wand :load filename)
	(lisp-magick:with-pixel-wand (pwand)
	  (multiple-value-bind (new-width new-height padding-width padding-height)
	      (get-thumbnail-dimensions (lisp-magick:magick-get-image-width wand)
					(lisp-magick:magick-get-image-height wand)
					width height)
	    (lisp-magick:magick-scale-image wand new-width new-height)
	      (lisp-magick:magick-frame-image wand pwand 
					      padding-width
					      padding-height
					      0 0))
	(lisp-magick:magick-write-image wand thumbname)))))

(defun get-thumbnail-dimensions (width height box-width box-height)
  (let ((image-aspect (/ width height))
	(box-aspect (/ box-width box-height)))
    (format t "Image aspect: ~A ; box aspect: ~A" image-aspect box-aspect)
    (let* ((new-width (if (> image-aspect box-aspect)
			  box-width (truncate (* box-height image-aspect))))
	   (new-height (if (> image-aspect box-aspect)
			   (truncate (/ box-width image-aspect)) box-height))
	   
	   (padding-width (truncate (/ (- box-width new-width) 2)))
	   (padding-height (truncate (/ (- box-height new-height) 2))))
      (values new-width new-height padding-width padding-height))))

(defun get-thumb-url (path)
  (concatenate 'string "/images/"
	       (namestring (get-thumb-path path))))

(defun get-full-url (path)
  (concatenate 'string "/images/"
	       (namestring (get-full-size-path path))))

(defun resize-all-images ()
  (ele:map-btree (lambda (key item)
		   (declare (ignore key))
		   (when-let (images (images item))
		     (dolist (i images)
		       (let* ((dest-path (make-pathname
					 :name (pathname-name i) :type (pathname-type i)
					 :defaults (image-path *web-store*)))
			      (thumb-path (get-thumb-path dest-path))
			      (full-size-path (get-full-size-path dest-path)))
			 (format t "Making ~A~%" thumb-path)
			 (create-thumbnail dest-path thumb-path
					   (get-config-option :thumbnail-width)
					   (get-config-option :thumbnail-height))
			 (format t "Making ~A~%" full-size-path)
			 (create-thumbnail dest-path full-size-path
					   (get-config-option :display-width)
					   (get-config-option :display-height))))))
		 (items *web-store*)))

(defun image-thumbnails (list render-func)
  (with-html-output-to-string (s)
    ((:ul :class "thumbnails")
     (dolist (obj list)
       (htm ((:li :class "span2")
	     (str (funcall render-func obj))))))))

(defun item-gallery (item)
  (image-thumbnails (images item)
	      (lambda (image)
		(with-html-output-to-string (s)
		  (:img :src (get-thumb-url image))))))

(defun display-an-image (item)
  (with-html-output-to-string (s)
    (:img :class "img-polaroid" :src (get-thumb-url (random-elt (images item))))))

(defun display-gallery (images id)
  (let ((thumb-width (get-config-option :thumbnail-width))
	  (thumb-height (get-config-option :thumbnail-height)))
      (with-html-output-to-string (s)
	(lightbox-gallery s id)
	((:div :id id)
	 (:ul
	  (dolist (i images)
	    (htm (:li ((:a :href (get-full-url i))
		       (:img :src (get-thumb-url i)))))))))))


(defmethod edit-display-images ((item line-item))
  (when (images item)
    (let ((thumb-width (get-config-option :thumbnail-width))
	  (thumb-height (get-config-option :thumbnail-height)))
      (with-html-output-to-string (s)
	(lightbox-gallery s "gallery")
	((:div :id "gallery")
	 ((:form :action (get-url item) :method :post)
	  (:ul
	   (dolist (i (images item))
	     (htm (:li ((:a :href (get-full-url i))
			(:img :src (get-thumb-url i)))
		       (:input :type "checkbox" :name "imgdel" :value i)))))
	  (:input :type "submit" :value "Delete")))))))

(defmethod display-images ((item line-item))
  (when (images item)
    (let ((thumb-width (get-config-option :thumbnail-width))
	  (thumb-height (get-config-option :thumbnail-height)))
      (with-html-output-to-string (s)
	((:div :id "gallery")
	 (:ul
	  (dolist (i (images item))
	    (htm (:li ((:a :href (get-full-url i))
		       (:img :src (get-thumb-url i))))))))))))

(defun add-image (path original-filename line-item)
  (let ((type (string-downcase (pathname-type original-filename)))
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
