# cl-recaptcha

cl-recaptcha is a simple interconnection between recaptcha.com and common-lisp. It can be used for the trivial usage of the recaptcha service.

## dependencies

cl-recaptcha requires cl-ppcre, flexi-streams, jsown and drakma. This library is available via quicklisp.


    (ql:quickload :cl-recaptcha)


## usage

The simplest way to use cl-recaptcha is to get a public and private key from "https://www.google.com/recaptcha/intro/index.html".  Set the key in cl-recaptcha, insert the needed code in your site and ask cl-recaptcha for verification.  All this should look somewhat like this:


    (setf cl-recaptcha:*site-key* "your-site-key")
    (setf cl-recaptcha:*secret-key* "your-secret-key")


In the source of the page, insert a call to challenge where you want to have your recaptcha inserted.


    ;; Javascript enabled version
    (challenge-js)

or

    ;; Javascript with a noscript tag
    (challenge-ns)

Assuming you get your post-vars by (get-post-var "varname") and the ip of the user by (get-ip-address), you can check the validity of the entry of the user by running:


    (verify-captcha (get-post-var "g-recaptcha-response") (get-ip-address))


Please note that the ip address is an optional parameter, as it is not required by the recaptcha API call itself.

The `verify-captcha` function will return ```T``` on success and ```nil``` on failure.  The second value it returns specifies the reason why the challenge was rejected. The result is returned as a list, however the error is usually a single string.
