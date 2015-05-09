(defpackage :cl-recaptcha
  (:use :common-lisp
        :jsown
        :drakma
        :flexi-streams
        :split-sequence)
  (:export :*secret-key*
           :*site-key*
           :*recaptcha-header-script-tag*
           :challenge-js
           :challenge-ns
           :verify-captcha))

(in-package :cl-recaptcha)

(defvar *secret-key* nil "Secret key for the recaptcha service. This is mainly used server side to validate the challenge resopnse. Register at https://www.google.com/recaptcha/intro/index.html")
(defvar *site-key* nil "Site key for the recaptcha service. This is public facing and will appear in the div element that holds the recaptcha form. Register at https://www.google.com/recaptcha/intro/index.html")
(defvar *recaptcha-header-script-tag* "<script src='https://www.google.com/recaptcha/api.js'></script>" "The source of the javascript file, the standard value comes from recaptcha itself and should be available in any case")
(defvar *captcha-verify-url* "https://www.google.com/recaptcha/api/siteverify" "This is the URL that will be used to verify the result of the captcha.  This is the one recaptcha provides")

(defun challenge-js (&key (site-key *site-key*) (theme "light") (callback "") (expired-callback ""))
  "This is the javascript-enabled version of the challenge, recaptcha advises you to use this."
  (format nil 
          "<div class='g-recaptcha' data-sitekey='~a' data-theme='~a' data-callback='~a' data-expired-callback='~a'></div>"
          site-key
          theme
          callback
          expired-callback))

(defun challenge-ns (&key (site-key *site-key*) (theme "light") (callback "") (expired-callback ""))
  "This is the javascript-disabled version of the challenge. Google does not recommend it's use if Javascript is a requirement for your site"
  (format nil
          "<div class='g-recaptcha' data-sitekey='~a' data-theme='~a' data-callback='~a' data-expired-callback='~a'></div><noscript><div style='width: 302px; height: 352px;'><div style='width: 302px; height: 352px; position: relative;'><div style='width: 302px; height: 352px; position: absolute;'><iframe src='https://www.google.com/recaptcha/api/fallback?k=~a' frameborder='0' scrolling='no' style='width: 302px; height:352px; border-style: none;'></iframe></div><div style='width: 250px; height: 80px; position: absolute; border-style: none; bottom: 21px; left: 25px; margin: 0px; padding: 0px; right: 25px;'> <textarea id='g-recaptcha-response' name='g-recaptcha-response' class='g-recaptcha-response' style='width: 250px; height: 80px; border: 1px solid #c1c1c1; margin: 0px; padding: 0px; resize: none;' value=''></textarea></div></div></div></noscript>"
          site-key
          theme
          callback
          expired-callback
          site-key))

(defun verify-captcha (g-recaptcha-response &optional (remote-ip) &key (secret-key *secret-key*))
  "Verifies the result the user gave.  There are two values returned.  The first indicates the success or failure, the second indicates the error code the captcha-server gave. Errors are returned in a list (just as the original JSON response returns errors in an array)."
  (let* ((http-request-parameters (append `(("secret" . ,secret-key)
                                            ("response" . ,g-recaptcha-response))
                                          (if remote-ip
                                              `(("remoteip" . ,remote-ip))
                                              nil)))
         (response (parse (octets-to-string (http-request *captcha-verify-url* 
                                                          :method :post 
                                                          :parameters http-request-parameters)))))
    (values (val response "success")
            (if (not (val response "success"))
                (val response "error-codes")
                'nil))))
