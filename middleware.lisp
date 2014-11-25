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

(defun diagnostic-handler (request)
	""
	(diagnostic-response request nil))

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

(defun realise-body (response)
	""
	(typecase (braid:body response)
		(pathname (setf (braid:body response) (alexandria:read-file-into-byte-vector (braid:body response))))
		(function (setf (braid:body response) (funcall (braid:body response)))))
	response)

(defun wrap-realise-body (request-handler)
	"Middleware that "
	(lambda (request)
			(realise-body-response (funcall request-handler request))))

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

(defun wrap-realise-response (request-handler)
	"Middleware that "
	(lambda (request)
		(realise-response (funcall request-handler request))))

;;;
(defun realise-request (request)
	""
	(typecase request
		(string (braid:make-request :uri request))
		(cons request)
		(t (braid:make-response :body (format nil "~a" response)))))


;;;

(defun default-condition-handler (condition request)
	""
	(braid:make-response :status 500 :body "Internal Server Error"))

(defun condition-handler (condition request)
	""
	(braid:make-response :status 500
											 :body (format nil "Internal Server Error~%Condition : ~a ~%Request : ~s" condition request)))

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

;;;

(defun rfc-1123-date (&optional (time (get-universal-time)))
  "Generates a time string according to RFC 1123. Default is current
time."
  (multiple-value-bind
        (second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            (svref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day-of-week)
            date
            (svref #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (1- month))
            year
            hour
            minute
            second)))

(defun set-last-modified (response)
	"If RESPONSE body is a pathname to a file, use its file-write-date to set the Last-Modified header."
	(when (typep (braid:body response) 'pathname)
		(setf (braid:header response :last-modified) (rfc-1123-date (file-write-date (braid:body response)))))
	response)

(defun wrap-last-modified (handler)
	"Middleware to set the Last-Modified header for response bodies of type pathname."
	(lambda (request)
		(set-last-modified (funcall handler request))))

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





