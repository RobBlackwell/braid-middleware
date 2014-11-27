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



(defun wrap-load-pathname-body (request-handler)
	"Middleware that replaces a pathname body with a byte vector being
the contents of the file designated by the pathname. "
	(lambda (request)
			(braid:load-pathname-body (funcall request-handler request))))

;;;

(defun wrap-ensure-response (request-handler)
	"Middleware that turns a shorthand response such as a string or
pathname into a full Braid response."
	(lambda (request)
		(braid:ensure-response (funcall request-handler request))))

;;;

(defun default-condition-handler (condition request)
	"Returns a simple, production safe HTTP 500 Internal Server Error."
	(braid:make-response :status 500 :body "Internal Server Error"))

(defun condition-handler (condition request)
	"Returns an HTTP 500 Internal Server Error with some information
about the condition."
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

;;; Basic Authorization

(defun set-basic-authorization (request username password)
	"Sets the authorization header for REQUEST using basic auth and the
supplied USERNAME and PASSWORD."
	(setf (braid:header request :authorization)
				(format nil "Basic ~A"
								(base64:string-to-base64-string
								 (format nil "~A:~A"
												 username
												 password))))
	request)

;; Borrowed from Hunchentoot ..

(defun get-basic-authorization (request)
  "Returns as two values the user and password \(if any) as encoded in
the 'AUTHORIZATION' header.  Returns NIL if there is no such header."
  (let* ((authorization (braid:header request :authorization))
         (start (and authorization
                     (> (length authorization) 5)
                     (string-equal "Basic" authorization :end2 5)
                     (cl-ppcre:scan "\\S" authorization :start 5))))
    (when start
      (destructuring-bind (&optional user password)
          (cl-ppcre:split ":" (base64:base64-string-to-string (subseq authorization start)) :limit 2)
        (values user password)))))

(defun make-not-authorized-response (realm)
	"Creates an HTTP 401 not-authorized response for the specified REALM."
	(braid:make-response :status 401
											 :headers (list :www-authenticate
																			(format nil "Basic realm=\"~a\"" realm))))

(defun wrap-basic-authorization (realm username-password-handler request-handler)
	""
	(lambda (request)
			(multiple-value-bind (username password)
					(get-basic-authorization request)
				(if (funcall username-password-handler username password)
						(funcall request-handler request)
						(make-not-authorized-response realm)))))
						


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





