;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defun make-page (title body &key sidebar end-matter (sidebar-span 2) (content-span 10)
		  navigation)
  "each of TITLE BODY and SIDEBAR should return strings.  This
function just makes sure the right components are included."
  (basic-page title
	      (with-html-output-to-string (s)
		((:div :class "container")
		 ((:div :class "row")
		  ((:div :class (format nil "span~A" sidebar-span))
		   (when sidebar (htm (str sidebar))))
		  ((:div :class (format nil "span~A" content-span))
		   (str body))))
		(when end-matter (str end-matter)))
	      navigation))


(defun basic-page (title body &optional navigation)
  "each of TITLE BODY and SIDEBAR should return strings.  This
function just makes sure the right components are included and sets up
some basic CSS."
  (with-html-output-to-string (s nil :prologue t :indent t)
    ((:html :lang "en") (:head (:title (str title))
			       (:link :href "/s/css/bootstrap.min.css" :rel "stylesheet")
			       ;; (:style "body {
			       ;;   padding-top: 186px; /* 60px to make the container go all the way to the bottom of the topbar */
			       ;; }")
			       )
     (:body

      ((:div :class "container")
       (str (main-navigation navigation)))
      (str body)
      (:script :src "http://code.jquery.com/jquery-latest.js")
      (:script :src "/s/js/bootstrap.min.js")))))


(defun get-all-featured-items ()
  (append (ele:get-instances-by-value 'single-item 'featured t)
	  (ele:get-instances-by-value 'bundle 'featured t)))

(defun get-random-featured-items (number)
  (let ((featured (shuffle (get-all-featured-items))))
    (subseq featured 0 (min number (length featured)))))

(defun display-table (columns items display-func stream)
  (with-html-output (s stream)
    ((:div :class "displaytable")
     (:table
      (dolist (row (partition-list items columns))
	(htm (:tr (dolist (col row)
		    (htm (:td (funcall display-func col s))))))))))
  "")



(defun print-price (price-in-cents)
  (multiple-value-bind (euro cent)
      (floor price-in-cents 100)
    (format nil "€~:D.~2,'0D" euro cent)))

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



