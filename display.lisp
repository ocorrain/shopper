;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)


(defun make-page (title body &optional sidebar end-matter)
  "each of TITLE BODY and SIDEBAR should return strings.  This
function just makes sure the right components are included."
  (basic-page title
	      (with-html-output-to-string (s)
		((:div :class "container")
		 ((:div :class "row")
		  ((:div :class "span2")
		   (when sidebar (htm (str sidebar))))
		  ((:div :class "span10")
		   (str body))))
		(when end-matter (str end-matter)))))


(defun basic-page (title body)
  "each of TITLE BODY and SIDEBAR should return strings.  This
function just makes sure the right components are included."
  (with-html-output-to-string (s nil :prologue t :indent t)
    ((:html :lang "en") (:head (:title (str title))
			       (:link :href "/s/css/bootstrap.min.css" :rel "stylesheet")
			       (:style "body {
        padding-top: 60px; /* 60px to make the container go all the way to the bottom of the topbar */
      }"))
     (:body
      ((:div :class "navbar navbar-inverse navbar-fixed-top")
       ((:div :class "navbar-inner")
	((:div :class "container")
	 ((:a :class "btn btn-navbar" :data-toggle "collapse" :data-target ".nav-collapse")
	  (:span :class "icon-bar")
	  (:span :class "icon-bar")
	  (:span :class "icon-bar"))
	 

	 ((:a :class "brand" href="#") (str (store-name *web-store*)))
	 ((:div :class "nav-collapse collapse")
	  ((:ul :class "nav")
	   ((:li :class "active")
	    ((:a :href "/") "Home"))
	   (:li ((:a :href "#about") "About"))
	   (:li ((:a :href "#contact") "Contact"))))
	 
	 (when-let (cart (get-cart))
	   (htm ((:a :href "/checkout" :class "pull-right btn btn btn-primary")
		 "CHECKOUT")
		((:a :href "/shopping-cart" :class "pull-right btn btn-warning")
		 (:i :class "icon-shopping-cart icon-white")
		 (str (count-items-in cart))
		 ;; (fmt "~A items in cart" (count-items-in cart))
		 ))))))
      
      (str body)
	    
      (:script :src "http://code.jquery.com/jquery-latest.js")
      (:script :src "/s/js/bootstrap.min.js")))))



(defun standard-page (title func)
  (make-page title (lambda (stream)
		     (funcall (header (store-name *web-store*)
				      title) stream)
		     (funcall func stream))
	     (sample-sidebar nil)))

(defun get-all-featured-items ()
  (append (ele:get-instances-by-value 'single-item 'featured t)
	  (ele:get-instances-by-value 'bundle 'featured t)))

(defun get-random-featured-items (number)
  (let ((featured (shuffle (get-all-featured-items))))
    (subseq featured 0 (min number (length featured)))))

(defun display-table (columns items display-func stream)
  (with-html-output (s stream)
    ((:div :class "displaytable")
     (:table
      (dolist (row (partition-list items columns))
	(htm (:tr (dolist (col row)
		    (htm (:td (funcall display-func col s))))))))))
  "")


(defun partition-list (list partition-length)
  (if (< (length list) partition-length)
      (list list)
      (cons (subseq list 0 partition-length)
	    (partition-list (subseq list partition-length) partition-length))))

(defun display-q (item stream)
  (with-html-output (s stream)
    (when (images item)
      (htm (:img :src (get-thumb-url (random-elt (images item))))
	   (:br)
	   ((:a :href (get-url item)) (str (title item)))
	   (:input :type "text" :size 3 :name (format nil "bundleadd{~A}" (sku item)))))))

(defgeneric display-short (item))

(defmethod display-short ((item line-item))
  (with-html-output-to-string (s)
    (when-let ((images (get-images item)))
      (htm (:img :src (get-thumb-url (random-elt images)))
	   (:br)))
    ((:a :href (get-url item)) (str (title item)))
    (:br)
    (str (print-price (get-price item)))))


;; (defmethod display-short ((item single-item) stream)
;;   "Simple image and title link for display"
;;   (with-html-output (s stream)
;;     (when (images item)
;;       (htm (:img :src (get-thumb-url (random-elt (images item))))
;; 	   (:br)))
;;     ))

;; (defmethod display-short ((bundle bundle) stream)
;;   (with-html-output (s stream)
;;     (when-let (images (or (images bundle)
;; 			  (get-images bundle)))
;;       (htm (:img :src (get-thumb-url (random-elt images)))
;; 	   (:br)))
;;     ((:a :href (get-url bundle)) (str (title bundle)))
;;     (:br)
;;     (str (print-price (get-price bundle)))))


(defmethod display ((item line-item))
  "Returns a function that can be applied to a stream to produce output"
  (with-slots (title short-description long-description packing-weight sku featured published)
      item
    (lambda (stream)
      (with-html-output (s stream :indent t)
	((:div :class "span-10 border")
	 (funcall (item-widget item) s))
	((:div :class "prepend-1 span-4 last")
	 (funcall (get-packing-details item) s)
	 (funcall (cart-widget item) s))
	((:div :class "span-16 last")
	 (:hr)
	 (:center (funcall (display-gallery (images item) "igallery") s))
	 (:hr)
	 (if (not (zerop (length long-description)))
	     (htm (:p (str long-description)))
	     (htm (:p "No long description")))))
      "")))

(defgeneric item-widget (item))

(defmethod item-widget ((item line-item))
  (lambda (stream)
    (with-slots (published featured short-description title)
	item
      (with-html-output (s stream)
	(:h1 (str title))
	(:p (:small (fmt "(published: ~A; featured: ~A)"
			 (if published "yes" "no")
			 (if featured "yes" "no"))))
	 
      (:p (str "Tagged with:" )
	  (get-tag-linked-list item s))
      (if (not (zerop (length short-description)))
	  (htm (:p (:i (str short-description))))
	  (htm (:p (:i "No short description"))))))))



(defgeneric get-packing-details (item))

(defmethod get-packing-details ((item single-item))
  (lambda (stream)
    (with-slots (weight price packing-weight)
	item
      (with-html-output (s stream :indent t)
	(:h2 (str (print-price price)))
	(definition-list
	    `(("Item weight" . ,weight)
	      ("Packing weight" . ,(format nil "~Ag" packing-weight))
	      ("Total weight" . ,(format nil "~Ag" (+ packing-weight weight))))
	    stream)))))

;; (defmethod get-packing-details ((bundle bundle))
;;   (lambda (stream)
;;     (with-slots (packing-weight)
;; 	bundle
;;       (let ((weight (get-weight bundle)))
;; 	(with-html-output (s stream :indent t)
;; 	  (:h2 (str (print-price (get-price bundle))))
;; 	  (funcall (simple-bundle-list bundle) s)
;; 	  (definition-list
;; 	      `(("Item weight" . ,weight)
;; 		("Packing weight" . ,(format nil "~Ag" packing-weight))
;; 		("Total weight" . ,(format nil "~Ag" (+ packing-weight weight))))
;; 	      stream))))))

(defun simple-bundle-list (bundle)
  (lambda (stream)
    (with-html-output (s stream)
      (:ul (dolist (ql (items bundle))
	     (htm
	      (:li (fmt "~A x ~A" (qlist-entry-quantity ql)
			(title (qlist-entry-item ql))))))))))

(defun bundle-add-form (bundle &optional tag)
  (let ((items (if tag
		   (ele:pset-list (get-members tag))
		   (ele:get-instances-by-class 'single-item))))
    (lambda (stream)
      (with-html-output (s stream)
	((:form :action "/add-to-bundle" :method "post")
	 (:input :type "hidden" :name "sku" :value (sku bundle))
	 (:input :type "submit" :value "Save")
	 (display-table 4 items #'display-q stream)
	 (:input :type "submit" :value "Save")))
      "")))



(defun print-price (price-in-cents)
  (multiple-value-bind (euro cent)
      (floor price-in-cents 100)
    (format nil "€~:D.~2,'0D" euro cent)))

(defmethod edit-widget ((item single-item))
  (lambda (stream)
    (single-item-form stream item) ""))

;; (defmethod edit-widget ((item bundle))
;;   (lambda (stream)
;;     (bundle-form stream item) ""))

(defmethod images-widget ((item line-item))
  (lambda (stream)
    (image-form stream item)
    (edit-display-images item stream)
    ""))

(defmethod tag-widget ((item line-item))
  (lambda (stream)
    (tag-widget-printer item stream)
    ""))

;; (defmethod bundle-widget ((bundle bundle))
;;   (lambda (stream)
;;     (funcall (simple-bundle-list bundle) stream)
;;     (funcall (bundle-add-form bundle) stream)
;;     ""))

(defun sample-sidebar (item)
  (lambda (stream)
    (with-html-output (s stream)
      (:ul (:li ((:a :href "/index.html") "Home"))
	   (:li ((:a :href "/single-item/new") "New single item"))
	   (:li ((:a :href "/single-items") "List of single items"))
	   (:li ((:a :href "/bundle/new") "New bundle"))
	   (:li ((:a :href "/bundles") "List of bundles"))
	   (:li ((:a :href "/shopping-cart") "View cart"))))
    (list-of-tags (all-tags) stream)))


(defun header (store-name title)
  (lambda (stream)
    (with-html-output (s stream)
      ((:div :id "header")
       ((:div :class "span-24 last")
	(:h1 (str store-name))
	(:h2 (str title)))))))

(defmethod get-tabs ((item single-item))
  (tabs (mapcar #'cons
		(list "Display item" "Edit item" "Manage images" "Manage tags")
		(list (display item) (edit-widget item) (images-widget item) (tag-widget item)))))

;; (defmethod get-tabs ((item bundle))
;;   (tabs (mapcar #'cons
;; 		(list "Display item" "Edit item" "Manage contents" "Manage images" "Manage tags")
;; 		(list (display item) (edit-widget item) (bundle-widget item)
;; 		      (images-widget item) (tag-widget item)))))



(defmethod item-q-form ((item line-item) stream)
  "Spits out a table with the following notation:
     Quantity (form element with name of the sku) | SKU | Title - short description
   There will be another method to make the table headings"
  (with-html-output (s stream :indent t)
    (:tr (:td (:input :name (sku item) :value 0 :type "text" :length 3))
	 (:td (str (sku item)))
	 (:td (str (title item))
	      " - "
	      (:i (str (short-description item)))))))

(defmethod item-q-headers ((item line-item) stream)
  "Spits out table headers as follows:
       Quantity | SKU | Item name and description"
  (with-html-output (s stream :indent t)
    (:tr (:th (str "Quantity"))
	 (:th (str "SKU#"))
	 (:th (str "Item name and description")))))


(defgeneric display (item))

(defun basic-menu ()
  (lambda (stream)
    (with-html-output (s stream)
      ((:ul :id "jsddm")
       (:li ((:a :href "#") "New")
	    (:ul (:li ((:a :href "/single-item/new") "Single item"))
		 (:li ((:a :href "/bundle/new") "Bundle"))))
       (:li ((:a :href "#") "View")
	    (:ul (:li ((:a :href "/single-items") "Single items"))
		 (:li ((:a :href "/bundles") "Bundles"))))))
    ""))


;; <ul id="jsddm">
;;     <li><a href="#">JavaScript</a>
;;         <ul>
;;             <li><a href="#">Drop Down Menu</a></li>
;;             <li><a href="#">jQuery Plugin</a></li>
;;             <li><a href="#">Ajax Navigation</a></li>
;;         </ul>
;;     </li>
;;     <li><a href="#">Effect</a>
;;         <ul>
;;             <li><a href="#">Slide Effect</a></li>
;;             <li><a href="#">Fade Effect</a></li>
;;             <li><a href="#">Opacity Mode</a></li>
;;             <li><a href="#">Drop Shadow</a></li>
;;             <li><a href="#">Semitransparent</a></li>
;;         </ul>
;;     </li>
;;     <li><a href="#">Navigation</a></li>
;;     <li><a href="#">HTML/CSS</a></li>
;;     <li><a href="#">Help</a></li>
;; </ul>


(defparameter *w3-countries*
  '(("Ireland" . "Ireland")
    ("United States" . "United States")
    ("United Kingdom" . "United Kingdom")
    ("Afghanistan" . "Afghanistan")
    ("Albania" . "Albania")
    ("Algeria" . "Algeria")
    ("American Samoa" . "American Samoa")
    ("Andorra" . "Andorra")
    ("Angola" . "Angola")
    ("Anguilla" . "Anguilla")
    ("Antarctica" . "Antarctica")
    ("Antigua and Barbuda" . "Antigua and Barbuda")
    ("Argentina" . "Argentina")
    ("Armenia" . "Armenia")
    ("Aruba" . "Aruba")
    ("Australia" . "Australia")
    ("Austria" . "Austria")
    ("Azerbaijan" . "Azerbaijan")
    ("Bahamas" . "Bahamas")
    ("Bahrain" . "Bahrain")
    ("Bangladesh" . "Bangladesh")
    ("Barbados" . "Barbados")
    ("Belarus" . "Belarus")
    ("Belgium" . "Belgium")
    ("Belize" . "Belize")
    ("Benin" . "Benin")
    ("Bermuda" . "Bermuda")
    ("Bhutan" . "Bhutan")
    ("Bolivia" . "Bolivia")
    ("Bosnia and Herzegovina" . "Bosnia and Herzegovina")
    ("Botswana" . "Botswana")
    ("Bouvet Island" . "Bouvet Island")
    ("Brazil" . "Brazil")
    ("British Indian Ocean Territory" . "British Indian Ocean Territory")
    ("Brunei Darussalam" . "Brunei Darussalam")
    ("Bulgaria" . "Bulgaria")
    ("Burkina Faso" . "Burkina Faso")
    ("Burundi" . "Burundi")
    ("Cambodia" . "Cambodia")
    ("Cameroon" . "Cameroon")
    ("Canada" . "Canada")
    ("Cape Verde" . "Cape Verde")
    ("Cayman Islands" . "Cayman Islands")
    ("Central African Republic" . "Central African Republic")
    ("Chad" . "Chad")
    ("Chile" . "Chile")
    ("China" . "China")
    ("Christmas Island" . "Christmas Island")
    ("Cocos (Keeling) Islands" . "Cocos (Keeling) Islands")
    ("Colombia" . "Colombia")
    ("Comoros" . "Comoros")
    ("Congo" . "Congo")
    ("Congo, The Democratic Republic of The" . "Congo, The Democratic Republic of The")
    ("Cook Islands" . "Cook Islands")
    ("Costa Rica" . "Costa Rica")
    ("Cote D'ivoire" . "Cote D'ivoire")
    ("Croatia" . "Croatia")
    ("Cuba" . "Cuba")
    ("Cyprus" . "Cyprus")
    ("Czech Republic" . "Czech Republic")
    ("Denmark" . "Denmark")
    ("Djibouti" . "Djibouti")
    ("Dominica" . "Dominica")
    ("Dominican Republic" . "Dominican Republic")
    ("Ecuador" . "Ecuador")
    ("Egypt" . "Egypt")
    ("El Salvador" . "El Salvador")
    ("Equatorial Guinea" . "Equatorial Guinea")
    ("Eritrea" . "Eritrea")
    ("Estonia" . "Estonia")
    ("Ethiopia" . "Ethiopia")
    ("Falkland Islands (Malvinas)" . "Falkland Islands (Malvinas)")
    ("Faroe Islands" . "Faroe Islands")
    ("Fiji" . "Fiji")
    ("Finland" . "Finland")
    ("France" . "France")
    ("French Guiana" . "French Guiana")
    ("French Polynesia" . "French Polynesia")
    ("French Southern Territories" . "French Southern Territories")
    ("Gabon" . "Gabon")
    ("Gambia" . "Gambia")
    ("Georgia" . "Georgia")
    ("Germany" . "Germany")
    ("Ghana" . "Ghana")
    ("Gibraltar" . "Gibraltar")
    ("Greece" . "Greece")
    ("Greenland" . "Greenland")
    ("Grenada" . "Grenada")
    ("Guadeloupe" . "Guadeloupe")
    ("Guam" . "Guam")
    ("Guatemala" . "Guatemala")
    ("Guinea" . "Guinea")
    ("Guinea-bissau" . "Guinea-bissau")
    ("Guyana" . "Guyana")
    ("Haiti" . "Haiti")
    ("Heard Island and Mcdonald Islands" . "Heard Island and Mcdonald Islands")
    ("Holy See (Vatican City State)" . "Holy See (Vatican City State)")
    ("Honduras" . "Honduras")
    ("Hong Kong" . "Hong Kong")
    ("Hungary" . "Hungary")
    ("Iceland" . "Iceland")
    ("India" . "India")
    ("Indonesia" . "Indonesia")
    ("Iran, Islamic Republic of" . "Iran, Islamic Republic of")
    ("Iraq" . "Iraq")
    ("Ireland" . "Ireland")
    ("Israel" . "Israel")
    ("Italy" . "Italy")
    ("Jamaica" . "Jamaica")
    ("Japan" . "Japan")
    ("Jordan" . "Jordan")
    ("Kazakhstan" . "Kazakhstan")
    ("Kenya" . "Kenya")
    ("Kiribati" . "Kiribati")
    ("Korea, Democratic People's Republic of" . "Korea, Democratic People's Republic of")
    ("Korea, Republic of" . "Korea, Republic of")
    ("Kuwait" . "Kuwait")
    ("Kyrgyzstan" . "Kyrgyzstan")
    ("Lao People's Democratic Republic" . "Lao People's Democratic Republic")
    ("Latvia" . "Latvia")
    ("Lebanon" . "Lebanon")
    ("Lesotho" . "Lesotho")
    ("Liberia" . "Liberia")
    ("Libyan Arab Jamahiriya" . "Libyan Arab Jamahiriya")
    ("Liechtenstein" . "Liechtenstein")
    ("Lithuania" . "Lithuania")
    ("Luxembourg" . "Luxembourg")
    ("Macao" . "Macao")
    ("Macedonia, The Former Yugoslav Republic of" . "Macedonia, The Former Yugoslav Republic of")
    ("Madagascar" . "Madagascar")
    ("Malawi" . "Malawi")
    ("Malaysia" . "Malaysia")
    ("Maldives" . "Maldives")
    ("Mali" . "Mali")
    ("Malta" . "Malta")
    ("Marshall Islands" . "Marshall Islands")
    ("Martinique" . "Martinique")
    ("Mauritania" . "Mauritania")
    ("Mauritius" . "Mauritius")
    ("Mayotte" . "Mayotte")
    ("Mexico" . "Mexico")
    ("Micronesia, Federated States of" . "Micronesia, Federated States of")
    ("Moldova, Republic of" . "Moldova, Republic of")
    ("Monaco" . "Monaco")
    ("Mongolia" . "Mongolia")
    ("Montserrat" . "Montserrat")
    ("Morocco" . "Morocco")
    ("Mozambique" . "Mozambique")
    ("Myanmar" . "Myanmar")
    ("Namibia" . "Namibia")
    ("Nauru" . "Nauru")
    ("Nepal" . "Nepal")
    ("Netherlands" . "Netherlands")
    ("Netherlands Antilles" . "Netherlands Antilles")
    ("New Caledonia" . "New Caledonia")
    ("New Zealand" . "New Zealand")
    ("Nicaragua" . "Nicaragua")
    ("Niger" . "Niger")
    ("Nigeria" . "Nigeria")
    ("Niue" . "Niue")
    ("Norfolk Island" . "Norfolk Island")
    ("Northern Mariana Islands" . "Northern Mariana Islands")
    ("Norway" . "Norway")
    ("Oman" . "Oman")
    ("Pakistan" . "Pakistan")
    ("Palau" . "Palau")
    ("Palestinian Territory, Occupied" . "Palestinian Territory, Occupied")
    ("Panama" . "Panama")
    ("Papua New Guinea" . "Papua New Guinea")
    ("Paraguay" . "Paraguay")
    ("Peru" . "Peru")
    ("Philippines" . "Philippines")
    ("Pitcairn" . "Pitcairn")
    ("Poland" . "Poland")
    ("Portugal" . "Portugal")
    ("Puerto Rico" . "Puerto Rico")
    ("Qatar" . "Qatar")
    ("Reunion" . "Reunion")
    ("Romania" . "Romania")
    ("Russian Federation" . "Russian Federation")
    ("Rwanda" . "Rwanda")
    ("Saint Helena" . "Saint Helena")
    ("Saint Kitts and Nevis" . "Saint Kitts and Nevis")
    ("Saint Lucia" . "Saint Lucia")
    ("Saint Pierre and Miquelon" . "Saint Pierre and Miquelon")
    ("Saint Vincent and The Grenadines" . "Saint Vincent and The Grenadines")
    ("Samoa" . "Samoa")
    ("San Marino" . "San Marino")
    ("Sao Tome and Principe" . "Sao Tome and Principe")
    ("Saudi Arabia" . "Saudi Arabia")
    ("Senegal" . "Senegal")
    ("Serbia and Montenegro" . "Serbia and Montenegro")
    ("Seychelles" . "Seychelles")
    ("Sierra Leone" . "Sierra Leone")
    ("Singapore" . "Singapore")
    ("Slovakia" . "Slovakia")
    ("Slovenia" . "Slovenia")
    ("Solomon Islands" . "Solomon Islands")
    ("Somalia" . "Somalia")
    ("South Africa" . "South Africa")
    ("South Georgia and The South Sandwich Islands" . "South Georgia and The South Sandwich Islands")
    ("Spain" . "Spain")
    ("Sri Lanka" . "Sri Lanka")
    ("Sudan" . "Sudan")
    ("Suriname" . "Suriname")
    ("Svalbard and Jan Mayen" . "Svalbard and Jan Mayen")
    ("Swaziland" . "Swaziland")
    ("Sweden" . "Sweden")
    ("Switzerland" . "Switzerland")
    ("Syrian Arab Republic" . "Syrian Arab Republic")
    ("Taiwan, Province of China" . "Taiwan, Province of China")
    ("Tajikistan" . "Tajikistan")
    ("Tanzania, United Republic of" . "Tanzania, United Republic of")
    ("Thailand" . "Thailand")
    ("Timor-leste" . "Timor-leste")
    ("Togo" . "Togo")
    ("Tokelau" . "Tokelau")
    ("Tonga" . "Tonga")
    ("Trinidad and Tobago" . "Trinidad and Tobago")
    ("Tunisia" . "Tunisia")
    ("Turkey" . "Turkey")
    ("Turkmenistan" . "Turkmenistan")
    ("Turks and Caicos Islands" . "Turks and Caicos Islands")
    ("Tuvalu" . "Tuvalu")
    ("Uganda" . "Uganda")
    ("Ukraine" . "Ukraine")
    ("United Arab Emirates" . "United Arab Emirates")
    ("United Kingdom" . "United Kingdom")
    ("United States" . "United States")
    ("United States Minor Outlying Islands" . "United States Minor Outlying Islands")
    ("Uruguay" . "Uruguay")
    ("Uzbekistan" . "Uzbekistan")
    ("Vanuatu" . "Vanuatu")
    ("Venezuela" . "Venezuela")
    ("Viet Nam" . "Viet Nam")
    ("Virgin Islands, British" . "Virgin Islands, British")
    ("Virgin Islands, U.S." . "Virgin Islands, U.S.")
    ("Wallis and Futuna" . "Wallis and Futuna")
    ("Western Sahara" . "Western Sahara")
    ("Yemen" . "Yemen")
    ("Zambia" . "Zambia")
    ("Zimbabwe" . "Zimbabwe")))






  ;; (with-html-output-to-string (s nil :prologue t :indent t)
  ;;   ((:html :lang "en") (:head (:title (str title))
  ;; 			       (:link :href "/s/css/bootstrap.min.css" :rel "stylesheet")
  ;; 			       (:style "body {
  ;;       padding-top: 60px; /* 60px to make the container go all the way to the bottom of the topbar */
  ;;     }"))
  ;;    (:body
  ;;     ((:div :class "navbar navbar-inverse navbar-fixed-top")
  ;;      ((:div :class "navbar-inner")
  ;; 	((:div :class "container")
  ;; 	 ((:a :class "btn btn-navbar" :data-toggle "collapse" :data-target ".nav-collapse")
  ;; 	  (:span :class "icon-bar")
  ;; 	  (:span :class "icon-bar")
  ;; 	  (:span :class "icon-bar"))
	 

  ;; 	 ((:a :class "brand" href="#") (str (store-name *web-store*)))
  ;; 	 ((:div :class "nav-collapse collapse")
  ;; 	  ((:ul :class "nav")
  ;; 	   ((:li :class "active")
  ;; 	    ((:a :href "/") "Home"))
  ;; 	   (:li ((:a :href "#about") "About"))
  ;; 	   (:li ((:a :href "#contact") "Contact"))))
	 
  ;; 	 (when-let (cart (get-cart))
  ;; 	   (htm ((:a :href "/checkout" :class "pull-right btn btn btn-primary")
  ;; 		 "CHECKOUT")
  ;; 		((:a :href "/shopping-cart" :class "pull-right btn btn-warning")
  ;; 		 (:i :class "icon-shopping-cart icon-white")
  ;; 		 (str (count-items-in cart))
  ;; 		 ;; (fmt "~A items in cart" (count-items-in cart))
  ;; 		 ))))))
      

	    
  ;;     (:script :src "http://code.jquery.com/jquery-latest.js")
  ;;     (:script :src "/s/js/bootstrap.min.js")
  ;;     ))))
