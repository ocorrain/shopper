;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defun css-links ()
  (with-html-output-to-string (s)
    (:link :rel "stylesheet" :href "/css/blueprint/screen.css"
			 :type "text/css" :media "screen,projection")
    (:link :rel "stylesheet" :href "/css/blueprint/print.css"
	   :type "text/css" :media "print")
    (:link :rel "stylesheet" :href "/css/blueprint/plugins/tabs/screen.css"
	   :type "text/css" :media "screen,projection")
    (:link :rel "stylesheet" :href "/styles/jquery.lightbox-0.5.css")
    (:link :rel "stylesheet" :href "/styles/gallery.css")
		  (str "<!--[if lt IE 8]>

    <link rel=\"stylesheet\" href=\"css/blueprint/ie.css\" type=\"text/css\" media=\"screen, projection\">

<![endif]-->")))


(defun make-page (title body)
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html (:head (:title (str title))
		  (str (css-links))
		  (:script :type "text/javascript"
			   :src "https://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.min.js"))
	   
	   (:body ((:div :class "container showgrid")
		   ((:div :id "header")
		    ((:div :class "span-24 last")
		     (:h1 (str (store-name *web-store*)))
		     (:h2 (str title))))
		   ((:div :id "main")
		    (str body))
		   )
		  ))))

(hunchentoot:define-easy-handler (new-single-item :uri "/single-item/new")
    ()
  (case (hunchentoot:request-method*)
    (:get (make-page "Enter new single item"
		     (with-html-output-to-string (s)
		       (single-item-form s))))
    (:post (maybe-create 'single-item (fix-alist (hunchentoot:post-parameters*)));; (make-page "iu"
	   ;; 	      ;; (with-html-output-to-string (s)
	   ;; 	      ;; 	(:pre (fmt "~{~S~^~%~}" (hunchentoot:post-parameters*))
	   ;; 	      ;; 	      (fmt "~{~S~^~%~}"
	   ;; 	      ;; 		   (mapcar (lambda (p)
	   ;; 	      ;; 			     (cons (read-from-string (car p))
	   ;; 	      ;; 				   (cdr p)))
	   ;; 	      ;; 			   (hunchentoot:post-parameters*))))))
		      
	   ;; )
	   )))

(defun fix-alist (alist)
  "Fixes the alist so that string keys are turned into symbols.  No
  casing is done, so the symbols will, in most lisp implementations,
  end up uppercased"
  (mapcar (lambda (p)
	    (cons (read-from-string (car p))
		  (cdr p)))
	  alist))

(hunchentoot:define-easy-handler (new-bundle :uri "/bundle/new")
    ()
  (case (hunchentoot:request-method*)
    (:get (make-page "Enter new bundle"
		     (with-html-output-to-string (s)
		       (bundle-form s))))
    (:post (maybe-create 'bundle (fix-alist (hunchentoot:post-parameters*))))))

(hunchentoot:define-easy-handler (display-item :uri "/item") (sku)
  (when-let (item (get-item sku))
    (case (hunchentoot:request-method*)
      (:get (make-page (title item)
		       (with-html-output-to-string (s)
			 (lightbox-js s)
;			 ((:div :class "column span-24 last" :id "tab-set")
			 ((:div :class "column prepend-4 span-16 append-4 last"
				:id "tab-set")
			 ((:ul :class "tabs")
			   (:li ((:a :href "#display" :class "selected") "Display item"))
			   (:li ((:a :href "#edit") "Edit item"))
			   (:li ((:a :href "#images") "Manage images")))

			  ((:div :id "display")
			   (display item s))
			  ((:div :id "edit")
			   (case (type-of item)
			     (single-item (single-item-form s item))
			     (bundle (bundle-form s item))))
			  ((:div :id "images")
			   (image-form s item)
			   (edit-display-images item s))
			  (tab-js s)))))
      (:post (maybe-update item (fix-alist (hunchentoot:post-parameters*)))))))


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




(defmethod display ((item line-item) stream)
  (with-slots (title short-description long-description packing-weight sku featured published)
      item
    (with-html-output (s stream)
      ((:div :class "span-10 border")
       (:h1 (str title))
       (:p (:small (fmt "(published: ~A; featured: ~A)"
			(if published "yes" "no")
			(if featured "yes" "no"))))
       (if (not (zerop (length short-description)))
	   (htm (:p (:i (str short-description))))
	   (htm (:p (:i "No short description")))))
      ((:div :class "prepend-1 span-4 last")
       (get-packing-details item s))

      ((:div :class "span-16 last")

       (:hr)
       (:center (display-gallery (images item) "igallery" s) )
       (:hr)
       
       (if (not (zerop (length long-description)))
	   (htm (:p (str long-description)))
	   (htm (:p "No long description")))))))


;; (defmethod display :after ((item single-item) stream)
;;   (with-slots (weight price packing-weight)
;;       item
;;     (with-html-output (s stream)
;;       (:p (:b "Item weight: ")
;; 	  (fmt "~Ag" weight))
;;       (:p (:b "Total weight: ")
;;       	  (fmt "~Ag" (+ packing-weight weight)))
;;       (multiple-value-bind (euro cent)
;; 	  (floor price 100)
;; 	(htm (:p (:b "Price: ")
;; 		 (fmt "€~:D.~2,'0D" euro cent)))))))

(defmethod get-packing-details ((item single-item) stream)
  (with-slots (weight price packing-weight)
      item
    (with-html-output (s stream)
      (multiple-value-bind (euro cent)
	  (floor price 100)
	(htm (:h2 (fmt "€~:D.~2,'0D" euro cent))))
      (:dl (:dt "Item weight")
	   (:dd (fmt "~Ag" weight))
	   (:dt "Packing weight")
	   (:dd (fmt "~Ag" packing-weight))
	   (:dt "Total weight")
	   (:dd (fmt "~Ag" (+ packing-weight weight)))))))



;; (defmethod display :after ((item bundle) stream)
;;   )

