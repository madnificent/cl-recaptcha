(defpackage :cl-recaptcha
  (:use :common-lisp
	:drakma
	:split-sequence)
  (:export :*private-captcha-key*
	   :*public-captcha-key*
	   :challenge-js
	   :challenge-ns
	   :challenge-ajax-src
	   :verify-captcha))

(in-package :cl-recaptcha)

(defvar *private-captcha-key* nil "Private key for the recaptcha service, register at http://recaptcha.net/api/getkey")
(defvar *public-captcha-key* nil "Private key for the recaptcha service, register at http://recaptcha.net/api/getkey")
(defvar *js-source* "http://api.recaptcha.net/js/recaptcha_ajax.js" "The source of the javascript file, the standard value comes from recaptcha itself and should be available in any case")
(defvar *captcha-verify-url* "http://api-verify.recaptcha.net/verify" "This is the URL that will be used to verify the result of the captcha.  This is the one recaptcha provides")

(defun challenge-js (&optional (public-key *public-captcha-key*))
  "This is the javascript-enabled version of the challenge, recaptcha advises you to use this one *and* (challenge-ns) for the normal handling of captchas"
  (format nil 
	  "<script type=\"text/javascript\" src=\"http://api.recaptcha.net/challenge?k=~A\"></script>" public-key))
(defun challenge-ns (&optional (public-key *public-captcha-key*))
  "This is the javascript-disabled version of the challenge, recaptcha advises you to use this one *and* (challenge-js) for the normal handling of captchas"
  (format nil
	  "<noscript><iframe src=\"http://api.recaptcha.net/noscript?k=~A\" height=\"300\" width=\"500\" frameborder=\"0\"></iframe><br><textarea name=\"recaptcha_challenge_field\" rows=\"3\" cols=\"40\"></textarea><input type=\"hidden\" name=\"recaptcha_response_field\" value=\"manual_challenge\"></noscript>" public-key))
(defun challenge-ajax-src ()
  "This is for the ajaxified version of recaptcha.  Please look at the api for more information on how to use this.  http://recaptcha.net/apidocs/captcha/client.html"
  (format nil
	  "<script type=\"text/javascript\" src=\"~A\"></script>" *js-source*))

(defun verify-captcha ( recaptcha-challenge-field recaptcha-response-field remote-ip &key (private-key *private-captcha-key*) )
  "Verifies the result the user gave.  There are two values returned.  The first indicates the success or failure, the second indicates the error code the captcha-server gave."
  (let ((response 
	 (split-sequence #\Newline
			 (http-request *captcha-verify-url* 
				       :method :post 
				       :parameters `(("privatekey" . ,private-key)
						     ("remoteip" . ,remote-ip)
						     ("challenge" . ,recaptcha-challenge-field)
						     ("response" . ,recaptcha-response-field))))))
    (values (string= (first response) "true")
	    (second response))))
