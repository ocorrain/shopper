(in-package #:shopper)


;; shopping cart
(restas:define-route r/add-to-cart
    ("/add-to-cart" :method :post)
  (maybe-add-items-to-cart)
  (hunchentoot:redirect (hunchentoot:referer)))


(defun maybe-add-items-to-cart ()
  (let* ((sku (hunchentoot:post-parameter "sku"))
	 (quantity (hunchentoot:post-parameter "number"))
	 (sku-item (get-item sku))
	 (valid-quantity (validate-number quantity)))
    (when (and sku-item valid-quantity)
      (add-item sku-item (get-or-initialize-cart) valid-quantity))))

