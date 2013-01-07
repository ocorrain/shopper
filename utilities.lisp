(in-package #:shopper)

(defun partition-list (list partition-length)
  (if (< (length list) partition-length)
      (list list)
      (cons (subseq list 0 partition-length)
	    (partition-list (subseq list partition-length) partition-length))))
