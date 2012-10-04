(in-package #:shopper)

(ele:defpclass customer ()
  ((name :initarg :name :initform nil :accessor name)
   (email :initarg :email :initform nil :accessor email :index t)
   (address1 :initarg :address1 :initform nil :accessor address1)
   (address2 :initarg :address2 :initform nil :accessor address2)
   (city :initarg :city :initform nil :accessor city)
   (postcode :initarg :postcode :initform nil :accessor postcode)
   (region :initarg :region :initform nil :accessor region)
   (country :initarg :country :initform nil :accessor country)
   (orders :initform (ele:make-pset) :accessor orders)
   (created :initform (get-universal-time) :accessor created)))

(defun get-customer ()
  (hunchentoot:session-value :customer))

(defun cart-in-well ()
  (with-html-output-to-string (s)
    ((:div :class "well")
      (str (print-shopping-cart (get-or-initialize-cart))))))

(defun customer-address-content (&optional errors)
  (with-html-output-to-string (s)
    ((:div :class "row")
     ((:div :class "span5")
      (str (customer-address-form errors)))
     ((:div :class "span5")
      (str (cart-in-well))))))


(defun customer-address-form (&optional errors)
  (let ((customer (get-or-initialize-customer)))
    (with-html-output-to-string (s)
      ;; (esc (with-output-to-string (s)
      ;; 	     (describe customer s)))
      (when-let (general-error (cdr (assoc 'general errors)))
	(htm ((:div :class "well")
	      ((:p :class "text-error")
	       (str general-error)))))
      
      ((:form :class "form-horizontal" :action "/place-order" :method :post)
       ((:div :class "control-group")
	((:label :class "control-label" :for "name")
	 "Name")
	((:div :class "controls")
	 (:input :type "text" :class "input" :name "name"
		 :value (if customer (name customer) ""))))
       
       ((:div :class "control-group")
	((:label :class "control-label" :for "email")
	 "Email")
	((:div :class "controls")
	 (:input :type "text" :class "input" :name "email" :value
		 (if customer (email customer) ""))))
       
       ((:div :class "control-group")
	((:label :class "control-label" :for "email2")
	 "Email (again)")
	((:div :class "controls")
	 (:input :type "text" :class "input" :name "email2")))

       ((:div :class "control-group")
	((:label :class "control-label" :for "address")
	 "Address Line 1")
	((:div :class "controls")
	 (:input :type "text" :class "input" :name "address" :value
		 (if customer (address1 customer) ""))))

       ((:div :class "control-group")
	((:label :class "control-label" :for "address2")
	 "Address Line 2")
	((:div :class "controls")
	 (:input :type "text" :class "input" :name "address2"
		 :value (if customer (address2 customer) ""))))

       ((:div :class "control-group")
	((:label :class "control-label" :for "city")
	 "City")
	((:div :class "controls")
	 (:input :type "text" :class "input" :name "city"
		 :value (if customer (city customer) ""))))

       ((:div :class "control-group")
	((:label :class "control-label" :for "postcode")
	 "Postcode")
	((:div :class "controls")
	 (:input :type "text" :class "input" :name "postcode"
		 :value (if customer (postcode customer) ""))))

       ((:div :class "control-group")
	((:label :class "control-label" :for "region")
	 "Region/Province/State")
	((:div :class "controls")
	 (:input :type "text" :class "input" :name "region"
		 :value (if customer (region customer) ""))))

       ((:div :class "control-group")
	((:label :class "control-label" :for "country")
	 "Country")
	((:div :class "controls")
	 (str (country-selector "country" (if customer (country customer) nil)))))
       

       ;; ((:div :class "control-group")
       ;;  ((:label :class "control-label" :for "remember")
       ;;   "Remember my details on this computer")
       ;;  ((:div :class "controls")
       ;;   (:input :type "checkbox" :class "input" :name "remember")))

       ((:a :href "/shopping-cart" :class "pull-left btn btn-large btn-primary")
	"<< Change order")
       ((:button :type "submit" :class "pull-right btn btn-large btn-success")
	"Place order >>")))))

(defun get-or-initialize-customer ()
  (if-let (customer (hunchentoot:session-value :customer))
    customer
    (let ((new-customer (make-instance 'customer)))
      (setf (hunchentoot:session-value :customer) new-customer)
      customer)))

(defun maybe-create/update-customer ()
  (let ((customer (get-or-initialize-customer))
	(errors '()))
    (flet ((pp (string) (hunchentoot:post-parameter string))) 
      (when-let (name (validate-as-string (pp "name")))
	(setf (name customer) name))
      (when-let (email (validate-as-string (pp "email")))
	(when-let (email2 (validate-as-string (pp "email")))
	  (if (string-equal email email2)
	      (setf (email customer) email))
	  (push (cons 'email "Email addresses do not match") errors)))
      (when-let (address (validate-as-string (pp "address")))
	(setf (address1 customer) address))
      (when-let (address2 (validate-as-string (pp "address2")))
	(setf (address2 customer) address2))
      (when-let (city (validate-as-string (pp "city")))
	(setf (city customer) city))
      (when-let (region (validate-as-string (pp "region")))
	(setf (region customer) region))
      (when-let (country (validate-as-string (pp "country")))
	(if (find country *w3-countries* :key #'car :test #'equal)
	    (setf (country customer) country)
	    (push (cons 'country "Invalid country") errors))))
    (values customer errors)))

(defmethod is-valid-customer? ((customer customer))
  "Require at least address1, email, one of city or region, and country"
  (with-slots (address1 email city region country)
      customer
    (if (and (stringp address1)
	     (stringp email)
	     (or (stringp city)
		 (stringp region))
	     (stringp country))
	(values t nil)
	(values nil '(general . "This address is not valid.  The minimum we
    require is the first lineof the address, a confirmed email adress,
    one of city or region/province/state, and a valid contry")))))

(defmethod display-customer ((customer customer))
  (with-html-output-to-string (s)
    (:address (:strong (str (name customer))) (:br)
	      (str (email customer)))

    (:address (str (address1 customer)) (:br)
	      (when-let (address2 (address2 customer))
		(htm (str address2) (:br)))
	      
	      (when-let (city (city customer))
		(htm (str city) (:br)))
	      
	      (when-let (region (region customer))
		(htm (str region) (:br)))
	      
	      (when-let (postcode (postcode customer))
		(htm (str postcode) (:br)))
	      
	      (:strong (str (country customer))))))