;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(ele:defpclass quantity-list ()
  ((items :initarg :items :initform '() :accessor items
	  :documentation "An alist of the form (ITEM or BUNDLE . QUANTITY)")))

(defstruct (qlist-entry (:type list)) item quantity)

(defmethod add-item (item (qlist quantity-list) quantity)
  (if-let (found (find item (items qlist) :key #'qlist-entry-item))
    (let ((current-quantity (qlist-entry-quantity found)))
      (set-item-quantity item qlist (+ quantity current-quantity)))
    (set-item-quantity item qlist quantity))
  (setf (items qlist)
	(remove-if-not #'positive-integer-p
		       (items qlist) :key #'qlist-entry-quantity))
  (items qlist))

(defmethod set-item-quantity (item (qlist quantity-list) quantity)
  (setf (items qlist)
	(cons (make-qlist-entry :item item :quantity quantity)
	      (remove-item item qlist)))
  (setf (items qlist)
	(remove-if-not #'positive-integer-p
		       (items qlist) :key #'qlist-entry-quantity))
  qlist)

(defmethod remove-item (item (qlist quantity-list))
  (setf (items qlist) (remove item (items qlist) :key #'qlist-entry-item)))

(defmethod empty-qlist ((qlist quantity-list))
  (setf (items qlist) nil))

(defmethod empty? ((qlist quantity-list))
  (null (items qlist)))

(defun qlist-reduce (qlist &key (reduce #'+) (item-function #'identity) (combine #'*))
  (reduce reduce (mapcar (lambda (qentry)
			   (funcall combine
				    (funcall item-function (qlist-entry-item qentry))
				    (qlist-entry-quantity qentry)))
			 qlist)))

(defun qlist->table-form (qlist item-input-func item-display-func action-url)
  (with-html-output-to-string (s)
    ((:table :class "table table-striped")
     ((:form :method "post" :action action-url)
      (dolist (entry (items qlist))
	(htm (:tr (:td (str (funcall item-display-func (qlist-entry-item entry))))
		  (:td (:input :type "text" :class "input-mini"
			       :name (funcall item-input-func (qlist-entry-item entry))
			       :value (qlist-entry-quantity entry))))))
      ((:button :type "submit" :class "btn btn-success") "Update")))))

(defmethod contains? ((qlist quantity-list) (item line-item))
  (member item (mapcar #'qlist-entry-item (items qlist))))

(defmethod contains? ((item1 line-item) (item2 line-item))
  (contains? (get-children-qlist item1) item2))

(defmethod get-price ((qlist quantity-list))
  (qlist-reduce (items qlist) :item-function #'get-price))

(defmethod get-weight ((qlist quantity-list))
  (qlist-reduce (items qlist) :item-function #'get-weight))
