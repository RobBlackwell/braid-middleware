braid-middleware
================

Braid middleware are higher order functions that add additional
functionality to Braid request handlers. See
[braid](https://github.com/RobBlackwell/braid).

A common pattern is take an existing request-handler, wrap it, and
return a new request handler.

	(defun wrap-content-type (request-handler content-type)
	  "Middleware to set the content-type header of the response."
	  (lambda (request)
  	    (let ((response (funcall request-handler request)))
	      (setf (braid:http-message-header response :content-type) content-type)
	      response)))

THIS IS EXPERIMENTAL CODE THAT IS SUBJECT TO CHANGE. I welcome
feedback, but it's probably too early to consider including in
Quicklisp yet. That doesnt stop you trying it with quicklisp by using
[local-projects](http://www.quicklisp.org/beta/faq.html).

Rob Blackwell    
December 2014

