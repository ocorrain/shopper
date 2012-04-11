(in-package #:shopper)

(defun tabs (alist &key component-namespace label)
  "ALIST is a list of keys and closures.  TABS outputs a closure that
takes a stream as an argument"
  (lambda (stream)
    (labels ((prefix (text)
	       (add-prefix (get-webform text) component-namespace))
	     
	     (tab-anchor (text)
	       (concatenate 'string "#" (prefix text))))
      
      (with-html-output (s stream :indent t)
	((:div :class "column span-24 last" :id "tab-set")
	 ((:ul :class "tabs")
	  (when label
	    (htm ((:li :class "label") (str label))))
	  (:li ((:a :href (tab-anchor (caar alist)) :class "selected") (str (caar alist))))
	  (dolist (alist-entry (cdr alist))
	    (htm (:li ((:a :href (tab-anchor (car alist-entry)))
		       (str (car alist-entry)))))))

	 (dolist (alist-entry alist)
	   (htm ((:div :id (prefix (car alist-entry)))
		 (str (funcall (cdr alist-entry) s))))))))))


(defun add-prefix (symbol-or-string prefix)
  (typecase symbol-or-string
    (symbol (add-prefix (string-downcase (symbol-name symbol-or-string)) prefix))
    (t (if prefix
	   (format nil "~A:~A" prefix symbol-or-string)
	   symbol-or-string))))

(defun definition-list (alist stream)
  (with-html-output (s stream)
    (:dl (dolist (item alist)
	   (htm (:dt (str (car item)))
		(:dd (str (cdr item))))))))

(defun fix-alist (alist)
  "Fixes the alist so that string keys are turned into symbols.  No
  casing is done, so the symbols will, in most lisp implementations,
  end up uppercased"
  (mapcar (lambda (p)
	    (cons (read-from-string (car p))
		  (cdr p)))
	  alist))

