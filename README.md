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
	      (setf (braid:header response :content-type) content-type)
	      response)))

By convention, Braid functions tend to be named as follows

* make-x-request if it creates a request object;
* make-x-response if it creates a response object;
* wrap-x if it wraps a handler and returns a handler;
* make-x-request-handler if it creates a request handler;
* x-request-handler if it's defining a named request handler.

THIS IS EXPERIMENTAL CODE THAT IS SUBJECT TO CHANGE. I welcome
feedback, but it's probably too early to consider including in
Quicklisp yet. That doesnt stop you trying it with quicklisp by using
[local-projects](http://www.quicklisp.org/beta/faq.html).

Rob Blackwell    
November 2014

