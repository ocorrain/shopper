(in-package #:shopper)

;; (ele:defpclass bundle (line-item quantity-list)
;;   ((discount :initarg :discount :initform 0 :accessor discount
;; 	     :documentation "Percentage discount for a bundle")))

;; (defmethod get-images ((bundle bundle))
;;   (remove-duplicates
;;    (flatten (mapcar (compose #'images #'car) (items bundle)))))

;; (defmethod get-price :around ((bundle bundle))
;;   "Applies the bundle discount"
;;   (let ((initial-price (call-next-method)))
;;     (round (* initial-price (/ (- 100 (discount bundle)) 100)))))

(defmethod set-bundle-quantity ((item line-item) (bundle line-item) quantity)
  ;; fixme test that bundle doesn't exist in item
  (set-item-quantity item (get-children-qlist bundle) quantity))

(defun bundle-edit-page (item)
  (let ((available-items (remove-if (lambda (i)
				      (or (contains? item i)
					  (equal item i)))
				    (all-items))))
    (make-page (format nil "Editing contents for ~A" (sku item))
	     (concatenate 'string
			  (edit-tabs item "Contents")
			  (when (empty? (get-children-qlist item))
			    (with-html-output-to-string (s)
			      (:p "This item is currently defined as
				a single item.  This means that it has
				no contents.  To make it into a
				bundle, click 'Add items'")))
			  (with-html-output-to-string (s)
			    (when available-items
			      (htm ((:a :href (url-rewrite:add-get-param-to-url
					       (format nil "/edit/item/~A/contents" (sku item))
					       "action" "add")
					:class "btn btn-primary") "Add new items")))
			    (str (qlist->table-form (get-children-qlist item)
						    #'sku #'title
						    (format nil "/edit/item/~A/contents"
							    (sku item)))))
			  
			  (when (and available-items
				     (string-equal (hunchentoot:get-parameter "action") "add")) 
			    (with-html-output-to-string (s)
			      (:hr)
			      (str (item-list->table-form available-items
				    #'sku #'title (format nil "/edit/item/~A/contents"
							  (sku item))))))) 
	     (edit-bar "All items")))
  )