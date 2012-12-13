(in-package #:shopper)

(defun @https-require (route)
  (make-instance 'https-require :target route))

(defun all-users ()
  (ele:get-instances-by-class 'user))

(defun add-user (username password)
  (multiple-value-bind (hash salt)
      (get-password-digest password)
    (make-instance 'user
		   :username username
		   :pwhash hash
		   :salt salt)))

(defun get-user (username password)
  (when-let (user-obj (ele:get-instance-by-value 'user 'username username))
    (with-slots (pwhash salt) user-obj
      (when (equal (get-password-digest password salt) pwhash)
	user-obj))))


(defun get-password-digest (password &optional salt-in)
  (let* ((salt (or salt-in
		   (hunchentoot::create-random-string 
		    (+ 10 (random 10)) 36)) )
	 (salted-password (concatenate 'string salt password)))
    (values (format nil "~{~X~}"
		    (map 'list #'identity
			 (md5:md5sum-sequence salted-password))) 
	    salt)))

(ele:defpclass user ()
  ((username :initarg :username :initform nil :accessor username :index t)
   (pwhash :initarg :pwhash :initform nil :accessor pwhash)
   (salt :initarg :salt :initform nil :accessor salt)
   (capabilities :initarg :capabilities :initform nil :accessor capabilities)))

(defun has-capability (capability user)
  (member capability (capabilities user)))
