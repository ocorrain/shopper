;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defun css-links ()
  (with-html-output-to-string (s nil :indent t)
    (:link :rel "stylesheet" :href "/css/blueprint/screen.css"
			 :type "text/css" :media "screen,projection")
    (:link :rel "stylesheet" :href "/css/blueprint/print.css"
	   :type "text/css" :media "print")
    (:link :rel "stylesheet" :href "/css/blueprint/plugins/tabs/screen.css"
	   :type "text/css" :media "screen,projection")
    (:link :rel "stylesheet" :href "/styles/jquery.lightbox-0.5.css")
    (:link :rel "stylesheet" :href "/styles/gallery.css")
    (:link :rel "stylesheet" :href "/styles/shopper.css")
		  (str "<!--[if lt IE 8]>

    <link rel=\"stylesheet\" href=\"css/blueprint/ie.css\" type=\"text/css\" media=\"screen, projection\">

<![endif]-->")))


(defun make-page (title body-function &optional sidebar)
  "BODY-FUNCTION takes a single argument, the stream S"
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html (:head (:title (str title))
		  (str (css-links))
		  (:script :type "text/javascript"
			   :src "https://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.min.js"))
	   
	   (:body ((:div :class "container")
		   ;; ((:a :href "#" :class "login_btn") "Login")
		   ;; ((:div :id "login_box")
		   ;;  ((:form :action "/login")
		   ;;   (:input :name "username" :type "text" :placeholder "Username")
		   ;;   (:input :name "password" :type "password" :placeholder "Password")
		   ;;   (:br)
		   ;;   (:input :type "submit" :value "login")))
		   (if sidebar
		       (htm ((:div :id "sidebar" :class "span-4")
			     (funcall sidebar s))
			    ((:div :id "main" :class "span-20 last")
			     ;; (funcall (basic-menu) s)
			     (funcall body-function s)))
		       (htm ((:div :id "main")
			     (funcall body-function s)))))
		  (:script :type "text/javascript" :src "/js/login.js")))))

(defun standard-page (title func)
  (make-page title (lambda (stream)
		     (funcall (header (store-name *web-store*)
				      title) stream)
		     (funcall func stream))
	     (sample-sidebar nil)))

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
    (edit-display-images item stream)
    ""))

(defmethod tag-widget ((item line-item))
  (lambda (stream)
    (tag-widget-printer item stream)
    ""))

(defmethod bundle-widget ((bundle bundle))
  (lambda (stream)
    (funcall (simple-bundle-list bundle) stream)
    (funcall (bundle-add-form bundle) stream)
    ""))

(defun sample-sidebar (item)
  (lambda (stream)
    (with-html-output (s stream)
      (:ul (:li ((:a :href "/index.html") "Home"))
	   (:li ((:a :href "/single-item/new") "New single item"))
	   (:li ((:a :href "/single-items") "List of single items"))
	   (:li ((:a :href "/bundle/new") "New bundle"))
	   (:li ((:a :href "/bundles") "List of bundles"))
	   (:li ((:a :href "/shopping-cart") "View cart"))))
    (list-of-tags (all-tags) stream)))


(defun header (store-name title)
  (lambda (stream)
    (with-html-output (s stream)
      ((:div :id "header")
       ((:div :class "span-24 last")
	(:h1 (str store-name))
	(:h2 (str title)))))))

(defmethod get-tabs ((item single-item))
  (tabs (mapcar #'cons
		(list "Display item" "Edit item" "Manage images" "Manage tags")
		(list (display item) (edit-widget item) (images-widget item) (tag-widget item)))))

(defmethod get-tabs ((item bundle))
  (tabs (mapcar #'cons
		(list "Display item" "Edit item" "Manage contents" "Manage images" "Manage tags")
		(list (display item) (edit-widget item) (bundle-widget item)
		      (images-widget item) (tag-widget item)))))



(defmethod item-q-form ((item line-item) stream)
  "Spits out a table with the following notation:
     Quantity (form element with name of the sku) | SKU | Title - short description
   There will be another method to make the table headings"
  (with-html-output (s stream :indent t)
    (:tr (:td (:input :name (sku item) :value 0 :type "text" :length 3))
	 (:td (str (sku item)))
	 (:td (str (title item))
	      " - "
	      (:i (str (short-description item)))))))

(defmethod item-q-headers ((item line-item) stream)
  "Spits out table headers as follows:
       Quantity | SKU | Item name and description"
  (with-html-output (s stream :indent t)
    (:tr (:th (str "Quantity"))
	 (:th (str "SKU#"))
	 (:th (str "Item name and description")))))


(defgeneric display (item))

(defun basic-menu ()
  (lambda (stream)
    (with-html-output (s stream)
      ((:ul :id "jsddm")
       (:li ((:a :href "#") "New")
	    (:ul (:li ((:a :href "/single-item/new") "Single item"))
		 (:li ((:a :href "/bundle/new") "Bundle"))))
       (:li ((:a :href "#") "View")
	    (:ul (:li ((:a :href "/single-items") "Single items"))
		 (:li ((:a :href "/bundles") "Bundles"))))))
    ""))


;; <ul id="jsddm">
;;     <li><a href="#">JavaScript</a>
;;         <ul>
;;             <li><a href="#">Drop Down Menu</a></li>
;;             <li><a href="#">jQuery Plugin</a></li>
;;             <li><a href="#">Ajax Navigation</a></li>
;;         </ul>
;;     </li>
;;     <li><a href="#">Effect</a>
;;         <ul>
;;             <li><a href="#">Slide Effect</a></li>
;;             <li><a href="#">Fade Effect</a></li>
;;             <li><a href="#">Opacity Mode</a></li>
;;             <li><a href="#">Drop Shadow</a></li>
;;             <li><a href="#">Semitransparent</a></li>
;;         </ul>
;;     </li>
;;     <li><a href="#">Navigation</a></li>
;;     <li><a href="#">HTML/CSS</a></li>
;;     <li><a href="#">Help</a></li>
;; </ul>