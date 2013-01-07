(in-package #:shopper)

(defun main-nav-tabs ()
  `(("/" . "Home")
    ("#about" . "About")
    ("#contact" . "Contact")))

(defun main-navigation (&optional navigation)
  (with-html-output-to-string (s)
    ((:div :class "navbar")
     (:img :src "/images/banner.jpg")
     ((:div :class "nav-collapse collapse")
      (str (nav-tabs (append (main-nav-tabs)
			     (tag->nav (featured-tags))
			     (nav-dropdown "More Chocolate!"
					   (tag->nav (menu-tags)))
			     (login-tabs))
		     navigation :class "nav nav-tabs")))
     (str (navigation-cart)))))

(defun login-tabs ()
  (when (hunchentoot:session-value :user)
    '(("/edit" . "Edit")
      ("/logout" . "Log out"))))

(defun nav-dropdown (label nav)
  (list (cons nav label)))

(defun navigation-cart ()
  (if-let ((store-open-p (store-open *web-store*))
	   (cart (get-cart)))
    (with-html-output-to-string (s)
      ((:a :href "/enter-details" :class "pull-right btn btn btn-primary")
       "CHECKOUT")
      ((:a :href "/shopping-cart" :class "pull-right btn btn-warning")
       (:i :class "icon-shopping-cart icon-white")
       (str (count-items-in cart))))
    ""))

(defun nav-tabs (alist active &key (class "nav nav-tabs"))
  "ALIST cells of the form (CONTENT . LABEL), NIL for dividers, or
plain strings for headers.  CONTENT can be an alist, in which case a
submenu is created"
  (with-html-output-to-string (s)
    ((:ul :class class)
     (dolist (item alist)
       (cond ((stringp item) (htm ((:li :class "nav-header") (str item))))
	     ((null item) (htm (:li :class "divider")))
	     (t (destructuring-bind (url . label)
		    item
		  (cond ((listp url)
			 (htm ((:li :class "dropdown")
			       ((:a :class "dropdown-toggle"
				    :data-toggle "dropdown"
				    :href "#")
				(str label)
				(:b :class "caret"))
			       (str (nav-tabs url nil :class "dropdown-menu")))))
			(t (if (string-equal label active)
			       (htm ((:li :class "active")
				     ((:a :href url)
				      (str label))))
			       (htm (:li
				     ((:a :href url)
				      (str label))))))))))))))