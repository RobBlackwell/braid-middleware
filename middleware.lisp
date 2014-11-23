;;;; braid-middleware.lisp

(in-package #:braid-middleware)

;;; Content Type

(defun set-content-type-response (response content-type)
	"Sets the content-type header of the response."
	(setf (braid:header response :content-type) content-type)
	response)
	
(defun wrap-set-content-type (request-handler content-type)
	"Middleware to set the content-type header of the response."
	(lambda (request)
		(set-content-type (funcall request-handler request) content-type)))

;;; Content Length

(defun set-content-length-response (response)
	"Sets the content-length header of the response to the length of the
response body."
	(setf (braid:header response :content-length) (length (braid:body response)))
	response)
	
(defun wrap-set-content-length (request-handler)
	"Middleware to set the content-length header of the response to the
length of the response body."
	(lambda (request)
		(set-content-length-response (funcall request-handler request) content-type)))

;;; Diagnostics

(defun diagnostic-response (request response)
	"A handler that returns diagnostic information."
	(braid:make-response :body (plist-html-table
															(list :request request
																		:response response
																		:room (with-output-to-string (*standard-output*)(room))
																		:machine-instance (machine-instance)
																		:machine-type (machine-type)
																		:machine-version (machine-version)
																		:software-type (software-type)
																		:software-version (software-version)
																		:features *features*))))

(defun wrap-diagnostic (request-handler)
	"Middleware that always returns diagnostic information for the
request and correponding response."
	(lambda (request)
		(diagnostic-response request (funcall request-handler request))))

;;; Headers

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

(defun merge-headers-response (response headers)
	"Merges HEADERS with the existing headers in RESPONSE."
	(setf (braid:headers response) (merge-plists (braid:headers response) headers))
	response)

(defun wrap-merge-headers (request-handler headers)
	"Middleware that merges HEADERS with the existing headers in the response."
	(lambda (request)
		(merge-headers (funcall request-handler request) headers)))

;;;

(defun realise-body-response (response)
	""
	(typecase (braid:body response)
		(pathname (setf (braid:body response) (alexandria:read-file-into-byte-vector (braid:body response)))))
	response)

(defun wrap-realise-body (request-handler)
	"Middleware that "
	(lambda (request)
			(realise-body (funcall request-handler response))))

;;;
				
(defun realise-response (response)
	""
	(typecase response
			(string (braid:make-response :body response))
			((simple-array (unsigned-byte 8)) (braid:make-response :body response))
			(pathname (braid:make-response :body (alexandria:read-file-into-byte-vector response)))
			(null (braid:make-response :status 404 :body "Not found"))
			(cons response)
			(t (braid:make-response :body (format nil "~a" response)))))

(defun wrap-realise (request-handler)
	"Middleware that "
	(lambda (request)
		(realise-response (funcall request-handler request))))

;;;

(defun default-condition-handler (condition request)
	""
	(braid:make-response :status 500 :body "Internal Server Error"))

(defun condition-handler (condition request)
	""
	(braid:make-response :status 500
											 :body (format nil "Internal Server Error~%Condition : ~s~%Request : ~s" condition request)))

(defun wrap-conditions (request-handler &key (condition-handler default-condition-handler))
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

;;;

(defun wrap-request-handlers (&rest request-handlers)
	""
	(lambda (request)
		(let ((result request))
			(dolist (request-handler request-handlers)
				(setf result (funcall request-handler result)))
			result)))

(defun combine-request-handlers (&rest request-handlers)
	""
	(lambda (request)
		(dolist (request-handler request-handlers)
			(when-let (result (funcall request-handler request))
				(return result)))))

;;; End





