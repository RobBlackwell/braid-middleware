;;;; braid-middleware.lisp

(in-package #:braid-middleware)

;;; Content Type

(defun wrap-set-content-type (request-handler content-type)
	"Middleware to set the content-type header of the response."
	(lambda (request)
		(set-content-type (funcall request-handler request) content-type)))

(defun wrap-set-content-length (request-handler)
	"Middleware to set the content-length header of the response to the
length of the response body."
	(lambda (request)
		(set-content-length-response (funcall request-handler request) content-type)))

(defun wrap-diagnostic (request-handler)
	"Middleware that always returns diagnostic information for the
request and correponding response."
	(lambda (request)
		(diagnostic-response request (funcall request-handler request))))

(defun wrap-set-headers (request-handler headers)
	"Middleware that replaces headers with HEADERS which is assumed to be a plist."
	(lambda (request)
		(let ((response (funcall request-handler request)))
			(setf (braid:headers response) headers)
			response)))

(defun wrap-set-header (request-handler header-name value)
	"Middleware that sets a header using HEADER-NAME and VALUE."
	(lambda (request)
		(let ((response (funcall request-handler request)))
			(setf (braid:header response header-name) value)
			response)))

(defun wrap-merge-headers (request-handler headers)
	"Middleware that merges HEADERS with the existing headers in the response."
	(lambda (request)
		(merge-headers (funcall request-handler request) headers)))

(defun wrap-load-pathname-body (request-handler)
	"Middleware that replaces a pathname body with a byte vector being
the contents of the file designated by the pathname. "
	(lambda (request)
			(braid-util:load-pathname-body (funcall request-handler request))))

(defun wrap-ensure-response (request-handler)
	"Middleware that turns a shorthand response such as a string or
pathname into a full Braid response."
	(lambda (request)
		(braid-util:ensure-response (funcall request-handler request))))

(defun wrap-conditions (request-handler &key (condition-handler 'default-condition-handler))
	"Middleware that masks any conditions raised by HANDLER and returns
an internal server error response produced by the supplied
CONDITION-HANDLER."
	(lambda (request)
		(handler-case
				(funcall request-handler request)
			(condition (condition) (condition-handler condition request)))))

;;; Head

(defun head-request (request)
	"Turns a HEAD request into a GET."
	(when (eq (braid:request-method request) :head)
			(setf (braid:request-method request) :get))
	request)

(defun head-response (response request)
	"Return a NIL body if original request was a HEAD"
	(when (and response (eq (braid:request-method request) :head))
		(setf (braid:body response) nil))
	response)

(defun wrap-head (request-handler)
	"Braid middleware that turns any HEAD request into a GET, and then
  sets the response body to nil."
	(lambda (request)
		(head-response
		 (funcall request-handler (head-request request)) request)))

;;; Log

(defun default-log-handler (message)
	""
	(format t "~a~%" message))

(defun wrap-log (request-handler &key (log-handler default-log-handler))
	""
	(lambda (request)
		(funcall log-handler request)
		(let ((response (funcall request-handler request)))
			(funcall log-handler response)
			response)))

(defun wrap-last-modified (handler)
	"Middleware to set the Last-Modified header for response bodies of type pathname."
	(lambda (request)
		(set-last-modified (funcall handler request))))

(defun wrap-basic-authorization (realm username-password-handler request-handler)
	""
	(lambda (request)
			(multiple-value-bind (username password)
					(get-basic-authorization request)
				(if (funcall username-password-handler username password)
						(funcall request-handler request)
						(make-not-authorized-response realm)))))
						
;;;
(defun wrap-set-query-params (handler)
	"Middleware that parses the query string from the REQUEST URI and
stashes the resulting paraeters as a plist accessible via
the :query-params key."
	(lambda (request)
		(funcall handler (set-query-params request))))

(defun wrap-sanitise-response (request-handler)
	""
	(lambda (request)
		(braid-util:sanitise-response (funcall request-handler request))))

(defun wrap-request-handlers (&rest request-handlers)
	""
	(lambda (request)
		(let ((result request))
			(dolist (request-handler request-handlers)
				(setf result (funcall request-handler result)))
			result)))

;;; End





