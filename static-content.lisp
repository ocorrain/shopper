(in-package #:shopper)

(ele:defpclass static-content ()
  ((title :initarg :title :initform nil :accessor title :index t)
   (content :initarg :content :initform nil :accessor content)
   (appears-in-menu :initarg :appears-in-menu :initform nil
		    :accessor appears-in-menu :index t)))

(defun static-content-form (&optional static-content)
  (with-html-output-to-string (s nil :indent t)
    ((:form :action (if static-content
			(restas:genurl 'shopper-edit:static-content-edit
				       :contentform (get-webform (title static-content)))
			(restas:genurl 'shopper-edit:new-static-content))
	    :method "post")
     (textfield "title" s "Title" "Page title"
		(when static-content (title static-content)))
     (checkbox "menu" s "Appears in menu?"
	       (when static-content (appears-in-menu static-content)))
     (textarea "content" s "Content" "Page contents"
	       (when static-content (content static-content)))
     (:br)
     (submit-button "Submit" s))))

(defun static-content-edit-page (content)
  (make-page (format nil "Editing ~A" (title content))
	       (with-html-output-to-string (s)
		 (str (static-content-form content)))
	       :sidebar (edit-bar "")))

(defun static-content-new-page ()
  (make-page "Create new static content"
	     (static-content-form)
	     :sidebar (edit-bar "")))

(defun static-content-nav ()
  (mapcar (lambda (content)
	    (cons (restas:genurl 'r/view-static-content
				 :contentform (get-webform (title content)))
		  (title content)))
	  (ele:get-instances-by-value 'static-content 'appears-in-menu t)))


(defun all-static-content ()
  (ele:get-instances-by-class 'static-content))

(defun get-content-from-webform (webform)
  (find-if (lambda (content)
		    (equal webform
			   (get-webform (title content))))
	   (all-static-content)))

(defmethod maybe-create ((type (eql 'static-content)) parameters)
  (flet ((assoc-val (val) (cdr (assoc val parameters))))
    (if-let (title (validate-as-string (assoc-val 'title)))
      (let ((webform (get-webform title)))
	(if-let (content (get-content-from-webform webform))
	  (restas:redirect 'shopper-edit:static-content-edit :contentform webform)
	  (let ((static-content-obj (make-instance 'static-content :title title)))
	    (maybe-update static-content-obj parameters)
	    (restas:redirect 'shopper-edit:static-content-edit :contentform webform)))))))

(defmethod maybe-update ((obj static-content) parameters)
  (flet ((assoc-val (val) (cdr (assoc val parameters))))
    (when-let (title (validate-as-string (assoc-val 'title)))
      (setf (title obj) title))
    (when-let (content (validate-as-string (assoc-val 'content)))
      (setf (content obj) content))
    (if (assoc-val 'menu)
	(setf (appears-in-menu obj) t)
	(setf (appears-in-menu obj) nil))))

(defun edit-static-content-page ()
  (make-page "All static content"
	     (with-html-output-to-string (s)
	       (:h1 "All static content")
	       (dolist (g (all-static-content))
		 (let ((webform (get-webform (title g))))
		   (htm
		  (:h4 (str (title g)))
		  (when (appears-in-menu g)
		    (htm (:span :class "label label-success"
				"Menu")))
		  (:p ((:a :class "btn btn-small btn-primary pull-left"
			   :href (restas:genurl 'shopper-edit:static-content-edit
						:contentform webform))
		       "Edit")
		      ((:a :class "btn btn-small btn-danger pull-right"
			   :href (restas:genurl 'shopper-edit:static-content-delete
						:contentform webform))
		       "Delete"))
		  (:hr)))))
	     
	     :sidebar (edit-bar "All static content")))

(defun view-static-content (content)
  (with-html-output-to-string (s)
    (:h1 (str (title content)))
    (str (content content))))

(defun view-static-content-page (content)
  (make-page (title content)
	     (view-static-content content)
	     :navigation (title content)))
