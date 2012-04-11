(in-package #:shopper)

(ele:defpclass single-item (line-item)
  ((weight :initarg :weight :initform 0 :accessor weight
	   :documentation "The weight of the item in grams" :type integer)
   (price :initarg :price :initform 0 :accessor price
	  :documentation "The price of the item in euro cents" :type integer)))

(defmethod get-price ((item single-item))
  (price item))

(defmethod get-weight ((item single-item))
  (weight item))

(defmethod get-images ((item single-item))
  (images item))

