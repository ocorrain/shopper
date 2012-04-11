;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(hunchentoot:define-easy-handler (index-page :uri "/index.html")
    ()
  (standard-page "Welcome to sample store"
		 (lambda (stream)
	       (display-table 5 (get-random-featured-items 20) #'display-short stream))))

(hunchentoot:define-easy-handler (display-tag :uri "/display-tag")
    (name)
  (when-let ((tag (get-tag name)))
    (let ((items (get-tagged-items tag)))
      (make-page (tag-name tag)
		 (lambda (stream)
		   (funcall (header (store-name *web-store*)
				    (tag-name tag)) stream)
		   (with-html-output (s stream)
		     (:h2 "Bundles")
		     (display-table 5 (remove 'single-item items :key #'type-of)
				    #'display-short s)
		     (:h2 "Single items")
		     (display-table 5 (remove 'bundle items :key #'type-of)
				    #'display-short s)))
		 (sample-sidebar tag)))))

(defun get-all-featured-items ()
  (append (ele:get-instances-by-value 'single-item 'featured t)
	  (ele:get-instances-by-value 'bundle 'featured t)))

(defun get-random-featured-items (number)
  (let ((featured (shuffle (get-all-featured-items))))
    (subseq featured 0 (min number (length featured)))))

(defun display-table (columns items display-func stream)
  (with-html-output (s stream)
    (:table
     (dolist (row (partition-list items columns))
       (htm (:tr (dolist (col row)
		   (htm (:td (funcall display-func col s)))))))))
  "")

(defun partition-list (list partition-length)
  (if (< (length list) partition-length)
      (list list)
      (cons (subseq list 0 partition-length)
	    (partition-list (subseq list partition-length) partition-length))))

(defun display-q (item stream)
  (with-html-output (s stream)
    (when (images item)
      (htm (:img :src (get-thumb-url (random-elt (images item))))
	   (:br)
	   ((:a :href (get-url item)) (str (title item)))
	   (:input :type "text" :size 3 :name (format nil "bundleadd{~A}" (sku item)))))))

(defmethod display-short ((item single-item) stream)
  "Simple image and title link for display"
  (with-html-output (s stream)
    (when (images item)
      (htm (:img :src (get-thumb-url (random-elt (images item))))
	   (:br)))
    ((:a :href (get-url item)) (str (title item)))))

(defmethod display-short ((bundle bundle) stream)
  (with-html-output (s stream)
    (when-let (images (or (images bundle)
			  (get-images bundle)))
      (htm (:img :src (get-thumb-url (random-elt images)))
	   (:br)))
    ((:a :href (get-url bundle)) (str (title bundle)))))


(defmethod display ((item line-item))
  "Returns a function that can be applied to a stream to produce output"
  (with-slots (title short-description long-description packing-weight sku featured published)
      item
    (lambda (stream)
      (with-html-output (s stream :indent t)
	((:div :class "span-10 border")
	 (funcall (item-widget item) s))
	((:div :class "prepend-1 span-4 last")
	 (funcall (get-packing-details item) s)
	 (funcall (cart-widget item) s))
	((:div :class "span-16 last")
	 (:hr)
	 (:center (funcall (display-gallery (images item) "igallery") s))
	 (:hr)
	 (if (not (zerop (length long-description)))
	     (htm (:p (str long-description)))
	     (htm (:p "No long description")))))
      "")))

(defgeneric item-widget (item))

(defmethod item-widget ((item line-item))
  (lambda (stream)
    (with-slots (published featured short-description title)
	item
      (with-html-output (s stream)
	(:h1 (str title))
	(:p (:small (fmt "(published: ~A; featured: ~A)"
			 (if published "yes" "no")
			 (if featured "yes" "no"))))
	 
      (:p (str "Tagged with:" )
	  (get-tag-linked-list item s))
      (if (not (zerop (length short-description)))
	  (htm (:p (:i (str short-description))))
	  (htm (:p (:i "No short description"))))))))



(defgeneric get-packing-details (item))

(defmethod get-packing-details ((item single-item))
  (lambda (stream)
    (with-slots (weight price packing-weight)
	item
      (with-html-output (s stream :indent t)
	(:h2 (str (print-price price)))
	(definition-list
	    `(("Item weight" . ,weight)
	      ("Packing weight" . ,(format nil "~Ag" packing-weight))
	      ("Total weight" . ,(format nil "~Ag" (+ packing-weight weight))))
	    stream)))))

(defmethod get-packing-details ((bundle bundle))
  (lambda (stream)
    (with-slots (packing-weight)
	bundle
      (let ((weight (get-weight bundle)))
	(with-html-output (s stream :indent t)
	  (:h2 (str (print-price (get-price bundle))))
	  (funcall (simple-bundle-list bundle) s)
	  (definition-list
	      `(("Item weight" . ,weight)
		("Packing weight" . ,(format nil "~Ag" packing-weight))
		("Total weight" . ,(format nil "~Ag" (+ packing-weight weight))))
	      stream))))))

(defun simple-bundle-list (bundle)
  (lambda (stream)
    (with-html-output (s stream)
      (:ul (dolist (ql (items bundle))
	     (htm
	      (:li (fmt "~A x ~A" (qlist-entry-quantity ql)
			(title (qlist-entry-item ql))))))))))

(defun bundle-add-form (bundle &optional tag)
  (let ((items (if tag
		   (ele:pset-list (get-members tag))
		   (ele:get-instances-by-class 'single-item))))
    (lambda (stream)
      (with-html-output (s stream)
	((:form :action "/add-to-bundle" :method "post")
	 (:input :type "hidden" :name "sku" :value (sku bundle))
	 (:input :type "submit" :value "Save")
	 (display-table 5 items #'display-q stream)
	 (:input :type "submit" :value "Save")))
      "")))

(hunchentoot:define-easy-handler (add-to-bundle-page :uri "/add-to-bundle")
    ((bundleadd :parameter-type 'hash-table)
     sku)
  (standard-page "Debug"
		 (lambda (stream)
		   (with-html-output (s stream)
		     (:p (fmt "~S" (hunchentoot:post-parameters*)))
		     (:p (esc (format nil "~A" bundleadd)))
		     (:p (str sku))
		     (when-let ((bundle (get-item sku))
				(items-to-add
				 (let ((items '())) 
				   (maphash (lambda (k v)
					      (when-let (number (parse-integer
								 v :junk-allowed t))
						(push (cons (get-item k) number) items)))
					    bundleadd)
				   items)))
		       (htm (esc (format nil "~S : ~S" bundle items-to-add)))
		       (dolist (item items-to-add)
			 (add-item (car item) bundle (cdr item)))
		       (hunchentoot:redirect (get-url bundle)))))))



(defun print-price (price-in-cents)
  (multiple-value-bind (euro cent)
      (floor price-in-cents 100)
    (format nil "€~:D.~2,'0D" euro cent)))

(defmethod edit-widget ((item single-item))
  (lambda (stream)
    (single-item-form stream item) ""))

(defmethod edit-widget ((item bundle))
  (lambda (stream)
    (bundle-form stream item) ""))

(defmethod images-widget ((item line-item))
  (lambda (stream)
    (image-form stream item)
    (edit-display-images item stream)))

(defmethod tag-widget ((item line-item))
  (lambda (stream)
    (tag-widget-printer item stream)
    ""))

(defmethod bundle-widget ((bundle bundle))
  (lambda (stream)
    (funcall (simple-bundle-list bundle) stream)
    (funcall (bundle-add-form bundle) stream)
    ""))
