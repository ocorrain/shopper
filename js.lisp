;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defun toggle-js (stream)
  (with-html-output (s stream)
    (:script (str "
function toggle(showHideDiv, switchTextDiv, hiddenText, visibleText) {
	var ele = document.getElementById(showHideDiv);
	var text = document.getElementById(switchTextDiv);
	if(ele.style.display == \"block\") {
    		ele.style.display = \"none\";
		text.innerHTML = hiddenText;
  	}
	else {
		ele.style.display = \"block\";
		text.innerHTML = visibleText;
	}
}
")
)))


(defun tab-js (stream)
  (with-html-output (s stream)
    (:script (str "
	$(\"ul.tabs li.label\").hide(); 
	$(\"#tab-set > div\").hide(); 
	$(\"#tab-set > div\").eq(0).show(); 
  $(\"ul.tabs a\").click( 
  	function() { 
  		$(\"ul.tabs a.selected\").removeClass('selected'); 
  		$(\"#tab-set > div\").hide();
  		$(\"\"+$(this).attr(\"href\")).fadeIn('slow'); 
  		$(this).addClass('selected'); 
  		return false; 
  	}
  );
  $(\"#toggle-label\").click( function() {
  	$(\".tabs li.label\").toggle(); 
  	return false; 
  }); 
"))))

(defun lightbox-js (stream)
  (with-html-output (s stream :indent t)
    (:script :type "text/javascript" :src "/js/jquery.lightbox-0.5.pack.js")))

(defun lightbox-gallery (stream id)
  (with-html-output (s stream)
    ((:script :type "text/javascript")
     	(fmt "	$(function() {
		$('#~A a').lightBox({fixedNavigation:true});
	});
" id))))



