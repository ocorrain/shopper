(in-package #:shopper)

(defvar *words* (make-array 98569))

(defun get-pictures (directory)
  (let ((pics '()))
    (cl-fad:walk-directory directory 
			   (lambda (pic) (push pic pics))
			   :test (lambda (f)
				   (equal (string-downcase (pathname-type f))
					  "jpg")))
    pics))

(defvar *pictures*
  (get-pictures "/home/ocorrain/Pictures"))

(defun get-words ()
  (with-open-file (f "/etc/dictionaries-common/words")
    (do ((l (read-line f nil 'eof) (read-line f nil 'eof))
	 (l-index 0 (+ l-index 1)))
	((eq l 'eof) 'done)
      (setf (svref *words* l-index) (string-trim '(#\Newline #\Tab #\Space) l)))))

(defun random-word (arg)
  (declare (ignore arg))
  (svref *words* (random (length *words*))))

(defun random-word-list (number)
  (mapcar #'random-word (make-list number)))

(defun random-words (number)
  (format nil "~{~A~^ ~}" (random-word-list number)))

(defun flip ()
  (if (zerop (random 2))
      nil t))

(defun test-provision-store (number-of-items number-of-tags images-per-item)
  (get-words)
  (new-web-store (random-words 4)
		 (random-letters 3)
		 (random-letters 3)
		 (dirconcat (pathname "/home/ocorrain/teststores/")
			    (get-webform (random-words 1))))
  (provision-items-test number-of-items)
  (provision-tags-test number-of-tags)
  (let ((items (ele:get-instances-by-class 'single-item))
	(tags (all-tags)))
    (provision-images-test items images-per-item)
    (tag-items-test items tags)))



(defun random-letters (number)
  (map 'string
       (lambda (c)
	 (declare (ignore c))
	 (code-char (+ (random 26) 65)))
       (make-string number)))

(defun provision-tags-test (number)
  (dotimes (i number)
    (let ((tag (make-instance 'tag :name (random-words 3))))
      (format t "~&Provisioned ~A, webform ~A~%" (tag-name tag) (webform tag)))))

(defun provision-images-test (items number-per-item)
  (dolist (i items)
    (format t "~&Provisioning ~A~%" (title i))
    (dotimes (n number-per-item)
      (add-image (random-elt *pictures*) i))))

(defun tag-items-test (items tags)
  (dolist (i items)
    (let ((tag (random-elt tags)))
      (tag-item i tag)
      (format t "~&Tagged ~A with ~A~&" (title i) (tag-name tag)))))


(defun provision-items-test (number)
  (dotimes (i number)
    (format t "Provisioning item ~A~%" (+ i 1))
    (make-instance 'single-item
		 :title (random-words 3)
		 :short-description (random-words 30)
		 :long-description (random-words 150)
		 :weight (random 2000)
		 :price (random 10000)
		 :meta (random-word-list 10)
		 :featured (flip)
		 :published (flip))))

;; (defun export-items-test (filename)
;;   (with-open-file (f filename :direction :output :if-exists :supersede)
;;     (dolist (i (ele:get-instances-by-class 'line-item))
;;       (format f "TITLE: ~A~%SHORT-DESCRIPTION: ~A~%LONG-DESCRIPTION: ~A~%"
;; 	      (title i) (short-description i) (long-description i))
;;       (format f "WEIGHT: ~Ag~%PRICE: ~Ac~%CATEGORIES: ~S~%" (weight i) (price i) (categories i))
;;       (format f "SKU: ~A~%META: ~S~%FEATURED: ~A~%PUBLISHED: ~A~%~%~%"
;; 	      (sku i) (meta i) (featured i) (published i)))))

(defun delete-items-test ()
  (ele:drop-instances (ele:get-instances-by-class 'single-item)))

(defmethod delete-item ((item line-item))
  (ele:remove-kv (sku item) (items *web-store*))
  (dolist (tag (tags item))
    (untag-item item tag))
  (ele:drop-instance item))