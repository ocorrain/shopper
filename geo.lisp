(in-package #:shopper)

(defparameter *geos*
  '(ireland europe us rest-of-world))

(ele:defpclass geography ()
  ((geography-name :initarg :name :accessor geo-name :index t)
   (providers :initform nil :accessor geo-providers)))

(ele:defpclass provider (qlist)
  ((provider-name :initarg :name :accessor provider-name :index t)
   (provider-rates :initform '() :accessor provider-rates)))

(defmethod add-postage (weight-limit price (provider provider))
  (add-item price provider weight-limit))

(defmethod get-postage-rates ((provider provider))
  (sort (copy-list (items provider)) #'< :key #'qlist-entry-quantity))

(defmethod get-url ((geo geography))
  (url-rewrite:add-get-param-to-url "/geo" "n" (get-webform (geo-name geo))))

(defun get-geo (webform)
  (find webform (ele:get-instances-by-class 'geography)
	:key (lambda (i) (get-webform (geo-name i)))
	:test #'string-equal))

(defun get-postage (weight geo)
  "Takes a weight (in grams) and a geo.  Returns postage for the item
in that geo"
  (labels ((qweight (e) (qlist-entry-quantity (car e)))
	   (get-rate (weight list-of-weights)
	     (if (null list-of-weights)
		 nil
		 (if (> weight (qweight list-of-weights))
		       (get-rate weight (cdr list-of-weights))
		       (car list-of-weights)))))
    (get-rate weight (get-postage-rates geo))))




