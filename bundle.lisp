(in-package #:shopper)

(ele:defpclass bundle (line-item quantity-list)
  ((discount :initarg :discount :initform 0 :accessor discount
	     :documentation "Percentage discount for a bundle")))

(defmethod get-images ((bundle bundle))
  (remove-duplicates
   (flatten (mapcar (compose #'images #'car) (items bundle)))))

(defmethod get-price :around ((bundle bundle))
  "Applies the bundle discount"
  (let ((initial-price (call-next-method)))
    (round (* initial-price (/ (- 100 (discount bundle)) 100)))))