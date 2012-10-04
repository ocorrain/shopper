(in-package #:shopper)

(ele:defpclass single-item (line-item)
  ())

(defmethod get-price ((item single-item))
  (price item))

(defmethod get-weight ((item single-item))
  (weight item))

(defmethod get-images ((item single-item))
  (images item))

