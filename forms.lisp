;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defun item-form (&optional line-item)
  (with-html-output-to-string (s nil :indent t)
    ((:form :action (if line-item (format nil (get-edit-edit-url line-item) (sku line-item))
			"/new/item")
	    :method :post)
     (textfield "title" s "Name" "Name or title of this item" (when line-item (title line-item)))
     (textfield "short-description" s "Short description" "A one-line description of the item"
		(when line-item (short-description line-item)))
     (textarea "long-description" s "Full description"
	       "A paragraph length description of the item"
	       (when line-item (long-description line-item)))

     (checkbox "published" s "Published?"
	       (when line-item (published line-item)))
     (checkbox "featured" s "Featured?"
	       (when line-item (featured line-item)))
 
     (textfield "packing-weight" s "Packing weight"
		"The extra weight of packaging for this item: packing weight + item weight (the next field equals the total shipping weight)"
		(when line-item (packing-weight line-item)))
     (textfield "weight" s "Weight"
		(if (and line-item (not (empty? (get-children-qlist line-item))))
		    (format nil "The weight (net) of the item in
		    grams. Computed weight for this item's contents is
		    ~A"
			    (get-weight (get-children-qlist line-item)))
		    "The weight (net) of the item in grams")
		
		(when line-item (weight line-item)))
     (textfield "price" s "Price"
		(if (and line-item (not (empty? (get-children-qlist line-item))))
		    (format nil "The price of the item in euro cents.
		    Computed price for this item's contents is ~A"
			    (get-price (get-children-qlist line-item)))
		    "The price of the item in euro cents")

		(when line-item (price line-item)))

     (:p "Enter the postal regions in which this item is available")
     (dolist (geo (all-geographies))
       (checkbox (format nil "G_~A" (hunchentoot:url-encode (geo-name geo)))
		 s
		 (geo-name geo)
		 (when line-item
		   (member geo (geographies line-item))))
       (htm (:br)))
     
     (submit-button "Submit" s))))

(defun tag-form (&optional tag)
  (with-html-output-to-string (s nil :indent t)
    ((:form :action (if tag (get-edit-edit-url tag)
			"/new/tag")
	    :method :post)
     (textfield "name" s "Name" "The name of this tag" (when tag (tag-name tag)))
     (checkbox "appearsinmenu" s "Appears in menu?"
	       (when tag (appears-in-menu tag)))
     (checkbox "featured" s "Featured?"
	       (when tag (featured tag)))
     (textarea "description" s "Description"
	       "An (optional) description of the tag.  If present,
this will be used as a blurb when viewing this tag"
	       (when tag (description tag)))
     (submit-button "Submit" s))))


;; (defun single-item-form (&optional line-item)
;;   (with-html-output-to-string (s nil :indent t)
;;     ((:form :action (if line-item (format nil "/edit/item/~A/edit" (sku line-item))
;; 			"/new/item")
;; 	    :method :post)
;;      (textfield "title" s "Name or title of this item" (when line-item (title line-item)))
;;      (textfield "short-description" s "A one-line description of the item"
;; 		(when line-item (short-description line-item)))
;;      (textarea "long-description" s "A paragraph length description of the item"
;; 	       (when line-item (long-description line-item)))
;;      (checkbox "published" s "Published?"
;; 	       (when line-item (published line-item)))
;;      (checkbox "featured" s "Featured?"
;; 	       (when line-item (featured line-item)))
;;      (textfield "packing-weight" s "The extra weight of packaging for this item: packing weight + item weight (the next field equals the total shipping weight"
;; 		(when line-item (packing-weight line-item)))
;;      (textfield "weight" s "The weight (net) of the item in grams"
;; 		(when line-item (weight line-item)))
;;      (textfield "price" s "The price of the item in euro cents"
;; 		(when line-item (price line-item)))
;;      (submit-button "Submit" s))))

;; (defun bundle-form (stream &optional line-item)
;;   (with-html-output (s stream :indent t)
;;     ((:form :action (if line-item (get-url line-item) "/bundle/new")
;; 	    :method :post)
;;      (generic-line-item-fields stream line-item)
;;      (submit-button "Next >>" s))))

(defun image-form (line-item)
  (with-html-output-to-string (s nil)
    ((:form :action (get-edit-image-url line-item) :method :post :enctype "multipart/form-data")
     ((:label :for "picture") "Upload an image")
     (:input :type "file" :name "picture" :class "text")
     (:input :type "submit" :value "Upload" :class "text"))))


;; (defun generic-line-item-fields (stream &optional line-item)
;;   (with-html-output (s stream :indent t)
;;     ))



(defun textfield (name stream label text default-value)
  (with-html-output (s stream :indent t)
    ((:label :for name) (str label)) 
    (:input :type "text" :name name :class "text"
	    :value (if default-value (if (stringp default-value)
					 (escape-string-all default-value)
					 default-value)  ""))
    ((:span :class "help-block") (str text))))

(defun textarea (name stream label text default-value)
  (with-html-output (s stream :indent t)
    ((:label :for name) (str label)) 
    ((:textarea :name name :class "text") (if default-value (str default-value) (str "")))
    ((:span :class "help-block") (str text))
    ))

(defun radio-button (name stream label default-value)
  (with-html-output (s stream :indent t)
    ((:label :for name) (str label)) 
    ((:label :for "true") "Yes")
    (if default-value
	(htm (:input :type "radio" :name name :id "true" :value "true" :checked "checked"))
	(htm (:input :type "radio" :name name :id "true" :value "true")))
    ((:label :for "false") "No")
    (if default-value
	(htm (:input :type "radio" :name name :id "false" :value "false"))
	(htm (:input :type "radio" :name name :id "false" :value "false" :checked "checked")))
    ))

(defun checkbox (name stream label default-value)
  (with-html-output (s stream :indent t)
    ((:label :class "checkbox inline" :for name) 
     (if default-value
	 (htm (:input :type "checkbox" :name name :value "true" :checked "checked"))
	 (htm (:input :type "checkbox" :name name :value "true")))
     (str label))))


(defun submit-button (label stream)
  (with-html-output (s stream :indent t)
    ((:button :type "submit" :name "submit" :class "btn btn-primary")
     (str label))))


(defun expand-form (spec)
  (typecase spec
    (cons (expand-form-cons spec))
    (otherwise spec)))

(defun expand-form-cons (spec)
  (if-let ((template (get-form-template (car spec))))
    (append (sublis (second spec) template)
	    (third spec))
    (mapcar #'expand-form spec)))

(defun form (spec)
  (lambda (stream)
    (eval (cl-who::tree-to-commands (expand-form spec) stream))))

  ;;   (with-html-output (s stream)
  ;;     (expand-form spec))))
  ;; )

;; (defmacro html-eval-to-string (h)
;;   `(with-html-output-to-string (s)
;;      ,@h))

;;   (cond () (find (car)) case (car spec)
;;     (text '(((:label :for (add-prefix (second spec))) (str (second spec)))
;; 	    (:input :type "text" :name (add-prefix (second spec) prefix)
;; 	     :value (fourth spec) :id (add-prefix (second spec)))))
;;     (textarea '())))

;; (text "title" "Item title" "dummy title")
;; (text "description" "Item description" "dummy description")
;; (textarea "longdesc" "Long description" "dummy long description")

(defparameter *form-templates*
  '((text :input :type "text" :name name :value value :id id)
    (textarea (:textarea :id id :name name) (str value))
    (label (:label :for target) label)
    (file :input :type "file" :name name :id id)
    (checkbox :input :type "checkbox" :name name :value value :checked checked)
    (radio :input :type "radio" :name name :id id :value value :checked checked)))


(defun get-form-template (symbol)
  (cdr (assoc symbol *form-templates*)))

(defun get-symbols (spec)
  (remove-if-not (lambda (thing) (and (symbolp thing)
				      (not (keywordp thing))
				      (not (member thing '(str esc htm fmt)))))
		 (flatten spec)))

(defun form-filler (spec)
  (lambda (alist)
    (let ((new-spec (sublis alist spec)))
      (values new-spec (get-symbols new-spec) (form-filler new-spec)))))

(defun country-selector (name &optional selected)
  (with-html-output-to-string (s)
    ((:select :class "input" :name name)
     (dolist (cinfo (all-countries-info))
       (destructuring-bind (code . country) cinfo
	 (if (equal code selected)
	     (htm ((:option :value code :selected "selected")
		   (str country)))
	     (htm ((:option :value code)
		   (str country)))))))))


