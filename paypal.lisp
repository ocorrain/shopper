(in-package #:shopper)

(defparameter *paypal-api-version* "94.0")
(defparameter *paypal-api-endpoint* "https://api-3t.sandbox.paypal.com/nvp")
(defparameter *paypal-redirect-url* "https://www.sandbox.paypal.com/webscr")

(defun get-paypal-api-username ()
  "uks_1354475765_biz_api1.gmail.com")

(defun get-paypal-api-password ()
  "1354475788")

(defun get-paypal-api-signature ()
  "An5ns1Kso7MWUdW4ErQKJJJ4qi4-A5tv818mwyzg6zbIv0gKNBCpssNq")


(defun get-return-url ()
  "https://ceist.net/success")

(defun get-cancel-url ()
  "https://ceist.net/cancel")

(defun paypal-amt (amount-in-cents)
  (multiple-value-bind (euro cent)
      (floor amount-in-cents 100)
    (format nil "~:D.~2,'0D" euro cent)))

(defmethod paypal-item-manifest ((order order) &optional (order-number 0))
  "return an alist of the paypal fields to describe the items"
  (declare (ignore order-number))
  (with-slots (reified-cart) order
    (let ((alist '()))
      (dotimes (i (length reified-cart))
	(flet ((add-numbered-nvp (key value)
		 (setf alist (acons (format nil "~A~D" key i)
				    (format nil "~A" value)
				    alist))))
	  (destructuring-bind (sku title quantity price weight)
	      (elt reified-cart i)
	    (add-numbered-nvp "L_PAYMENTREQUEST_0_NUMBER" sku)
	    (add-numbered-nvp "L_PAYMENTREQUEST_0_NAME" title)
	    (add-numbered-nvp "L_PAYMENTREQUEST_0_AMT" (paypal-amt price))
	    (add-numbered-nvp "L_PAYMENTREQUEST_0_QTY" quantity)
	    (add-numbered-nvp "L_PAYMENTREQUEST_0_ITEMWEIGHTVALUE" weight))))
      (reverse alist))))

(defmethod standard-paypal-fields ((order order) method &optional (order-number 0))
  (declare (ignore order-number))
  (list (cons "METHOD" method)
	(cons "VERSION" *paypal-api-version*)
	(cons "USER" (get-paypal-api-username))
	(cons "PWD" (get-paypal-api-password))
	(cons "SIGNATURE" (get-paypal-api-signature))
	(cons "RETURNURL" (get-return-url))
	(cons "CANCELURL" (get-cancel-url))
	(cons "NOSHIPPING" "0")
	(cons "ALLOWNOTE" "1")))

(defmethod paypal-payment-fields ((order order) &optional (order-number 0))
  (declare (ignore order-number))
 (list (cons "PAYMENTREQUEST_0_AMT"
	      (paypal-amt (+ (postage-price order) (get-price (cart order)))))
	(cons "PAYMENTREQUEST_0_CURRENCYCODE" "EUR")
	(cons "PAYMENTREQUEST_0_PAYMENTACTION" "Sale")
	(cons "PAYMENTREQUEST_0_INVNUM" (order-number order))
	(cons "PAYMENTREQUEST_0_ITEMAMT" (paypal-amt (get-price (cart order))))
	(cons "PAYMENTREQUEST_0_SHIPPINGAMT" (paypal-amt (postage-price order)))))

(defmethod customer-paypal-fields ((order order) &optional (order-number 0))
  (declare (ignore order-number))
  (with-slots (customer) order
    (let ((customer-alist
	   (list (cons "PAYMENTREQUEST_0_SHIPTONAME" (name customer))
		 (cons "PAYMENTREQUEST_0_SHIPTOSTREET" (address1 customer))
		 (cons "PAYMENTREQUEST_0_SHIPTOCITY" (address2 customer))
		 (cons "PAYMENTREQUEST_0_SHIPTOSTATE" (region customer))
		 (cons "PAYMENTREQUEST_0_SHIPTOCOUNTRYCODE" (country customer))
		 (cons "SHIPTOCOUNTRY" (country customer))
		 (cons "ADDROVERRIDE" "1"))))
      (when (address2 customer)
	(push (cons "PAYMENTREQUEST_0_SHIPTOSTREET2" (address2 customer)) customer-alist))
      (when (postcode customer)
	(push (cons "PAYMENTREQUEST_0_SHIPTOZIP" (address2 customer)) customer-alist))
      customer-alist)))


(defmethod paypal-setexpresscheckout-parameters ((order order) &optional (order-number 0))
  (append (standard-paypal-fields order "SetExpressCheckout" order-number)
	  (paypal-payment-fields order order-number)
	  (paypal-item-manifest order order-number)
	  (customer-paypal-fields order order-number)))

(defmethod paypal-getexpresscheckoutdetail-parameters ((order order) &optional (order-number 0))
  (acons "TOKEN" (gateway-ref order)
	 (standard-paypal-fields order "GetExpressCheckoutDetails" order-number)))

(defmethod paypal-doexpresscheckoutpayment-parameters ((order order) &optional (order-number 0))
  (append (acons "TOKEN" (gateway-ref order)
		 (acons "PAYERID" (payer-id order)
			(standard-paypal-fields order "DoExpressCheckoutPayment" order-number)))
	  (paypal-payment-fields order order-number)))

(defmacro with-paypal-context ((function order order-number) &body body)
  `(let ((api-call-parameters (,function ,order ,order-number)))
     (multiple-value-bind (status response)
	 (paypal-api-call api-call-parameters)
       (flet ((get-pp-response (key)
		(get-pp-value key response)))
	 (if status
	     (progn ,@body)
	     (if (numberp response)
		 (error 'paypal-http-error :code response)
		 (error 'paypal-api-error
			:sent api-call-parameters
			:received response)))))))



(define-condition paypal-api-error (error)
  ((sent-parameters :initarg :sent :reader sent)
   (response-parameters :initarg :received :reader received)))

(define-condition paypal-http-error (error)
  ((response-code :initarg :code :reader response-code)))

(defmethod paypal-api-call-setexpresscheckout ((order order) &optional (order-number 0))
  (with-paypal-context (paypal-setexpresscheckout-parameters order order-number)
    (setf (gateway-ref order) (get-pp-response "TOKEN")
	  (correlation-id order) (get-pp-response "CORRELATIONID"))
    (add-order-timestamp order :got-paypal-token)
    response))

(defmethod paypal-api-call-getexpresscheckoutdetails ((order order) &optional (order-number 0))
  (with-paypal-context (paypal-getexpresscheckoutdetail-parameters order order-number)
    (unless (payer-id order)
      (add-order-timestamp order :got-payer-id)
      (setf (payer-id order) (get-pp-response "PAYERID")))
    response))

(defmethod paypal-api-call-doexpresscheckoutpayment ((order order) &optional (order-number 0))
  (with-paypal-context (paypal-doexpresscheckoutpayment-parameters order order-number)
    (add-order-timestamp order :payment-request-sent)
    response))

(defun paypal-redirect-url (order)
  (format nil "~A?cmd=_express-checkout&token=~A"
	  *paypal-redirect-url* (hunchentoot:url-encode (gateway-ref order))))

(defun paypal-api-call (parameters)
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request *paypal-api-endpoint*
			   :method :post
			   :parameters parameters)
    (if (= status-code 200)
	(let ((decoded (decode-pp-response body)))
	  (if (equal (get-pp-value "ACK" decoded) "Success")
	      (values t decoded)
	      (values nil decoded)))
	(values nil status-code))))


(defun decode-pp-response (response-string)
  (mapcar (lambda (kv)
	    (cl-ppcre:register-groups-bind (key value)
		("(\\w+)=(.*)" kv)
	      (cons key (hunchentoot:url-decode value))))
	  (cl-ppcre:split "&" response-string)))

(defun get-pp-value (key decoded-response)
  (cdr (assoc key decoded-response :test #'equal)))


;; (defparameter *pp-response* "TOKEN=EC%2d39R81963KT576233T&TIMESTAMP=2012%2d12%2d01T14%3a04%3a32Z&CORRELATIONID=324a7664170e9&ACK=Success&VERSION=94%2e0&BUILD=4181146")

;; (defparameter *pp-payer-id* "E7CPQ7VYE4QW4")