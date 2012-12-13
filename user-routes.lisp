(in-package #:shopper)

(restas:define-module #:shopper-login
    (:use #:cl #:shopper #:restas #:alexandria)
  (:decorators '@https-require))

(in-package #:shopper-login)

(restas:define-route login
    ("login")
  (login-form))

(restas:define-route login/post
    ("login" :method :post)
  (let ((username (hunchentoot:post-parameter "username"))
	(password (hunchentoot:post-parameter "password")))
    (when (and username password)
;      (hunchentoot:log-message* :debug "Username: ~A, password: ~A" username password)
      (when-let (userobj (shopper::get-user username password))
	(hunchentoot:start-session)
	(setf (hunchentoot:session-value :user) userobj)
	(restas:redirect 'shopper-edit:/r/edit)))
    (login-form :error)))

(restas:define-route logout
    ("logout")
  (hunchentoot:delete-session-value :user)
  (restas:redirect 'r/index-page))

(defun login-form (&optional errors)
  (basic-page "Log in"
	      (who:with-html-output-to-string (s)
;		(:pre (who:fmt "~S" (hunchentoot:session-value :user)))
		(:h2 "Sign in")
		(when errors
		  (who:htm ((:p :class "text-error")
			"The login details you supplied were incorrect")))
		((:form :class "form-horizontal" :method "post" :action (restas:genurl 'login))
		 ((:div :class "control-group")
		  ((:label :class "control-label" :for "username")
		   "Username")
		  ((:div :class "controls")
		   (:input :name "username" :type "text"
			   :id "username" :placeholder "Username")))
		 ((:div :class "control-group")
		  ((:label :class "control-label" :for "password")
		   "Password")
		  ((:div :class "controls")
		   (:input :name "password" :type "password"
			   :id "password" :placeholder "Password")))
		 ((:div :class "control-group")
		  ((:div :class "controls")
		   ((:button :type "submit" :class "btn")
		    "Sign in")))))))




