;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(ele:defpclass quantity-list ()
  ((items :initarg :items :initform '() :accessor items
	  :documentation "An alist of the form (ITEM or BUNDLE . QUANTITY)")))

(defstruct (qlist-entry (:type list)) item quantity)

(defmethod add-item (item (qlist quantity-list) quantity)
  (if-let (found (find item (items qlist) :key #'qlist-entry-item))
          (setf (items qlist)
		(cons (make-qlist-entry :item item
				       :quantity quantity)
		      (remove found (items qlist) :test #'equalp)))
	  (push (make-qlist-entry :item item :quantity quantity) (items qlist)))
  (setf (items qlist)
	(remove-if-not #'positive-integer-p
		       (items qlist) :key #'qlist-entry-quantity))
  (items qlist))

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
