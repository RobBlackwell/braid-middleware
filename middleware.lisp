;;;; braid-middleware.lisp

(in-package #:braid-middleware)

;;; Content Type

(defun wrap-set-content-type (http-request-handler content-type)
  "Middleware that sets the content-type header of the response."
  (lambda (http-request)
    (let ((http-response (funcall http-request-handler http-request)))
      (set-content-type http-response content-type)
      http-response)))

(defun wrap-set-content-length (http-request-handler)
  "Middleware to set the content-length header of the response to the
length of the response body."
  (lambda (http-request)
    (let ((http-response (funcall http-request-handler http-request)))
      (set-content-length http-response)
      http-response)))

(defun wrap-diagnostic (http-request-handler)
  "Middleware that always returns diagnostic information for the
request and corresponding response."
  (lambda (http-request)
    (diagnostic-response http-request (funcall http-request-handler http-request))))

(defun wrap-set-response-headers (http-request-handler headers)
  "Middleware that replaces the HTTP-RESPONSE headers with HEADERS
which is assumed to be a plist."
  (lambda (http-request)
    (let ((http-response (funcall http-request-handler http-request)))
      (setf (braid:http-message-headers http-response) headers)
      http-response)))

(defun wrap-set-response-header (http-request-handler header-name value)
  "Middleware that sets an HTTP-RESPONSE-HEADER using HEADER-NAME and
VALUE."
  (lambda (http-request)
    (let ((http-response (funcall http-request-handler http-request)))
      (setf (braid:http-message-header http-response header-name) value)
      http-response)))

(defun wrap-merge-response-headers (http-request-handler headers)
  "Middleware that merges HTTP-RESPONSE-HEADERS with the existing headers in the response."
  (lambda (http-request)
    (let ((http-response (funcall http-request-handler http-request)))
      (merge-headers http-response headers)
      http-response)))

(defun wrap-set-body-pathname-to-bytes (http-request-handler)
  "Middleware that replaces a pathname body with a byte vector being
the contents of the file designated by the pathname. "
  (lambda (http-request)
    (let ((http-response (funcall http-request-handler http-request)))
      (set-body-pathname-to-bytes http-response)
      http-response)))

(defun wrap-ensure-http-response (http-request-handler)
  "Middleware that turns a shorthand response such as a string or
pathname into a full Braid response."
  (lambda (http-request)
    (braid-util:ensure-http-response (funcall http-request-handler http-request))))

(defun wrap-conditions (http-request-handler &key (condition-handler 'make-default-condition-response))
  "Middleware that masks any conditions raised by HANDLER and returns
an internal server error response produced by the supplied
CONDITION-HANDLER."
  (lambda (http-request)
    (handler-case
	(funcall http-request-handler http-request)
      (condition (condition) (condition-handler condition http-request)))))

;;; Head

(defun head-request (http-request)
  "Turns a HEAD request into a GET."
  (when (eq (braid:http-request-method http-request) :head)
    (setf (braid:http-request-method http-request) :get))
  http-request)

(defun head-response (http-response http-request)
  "Return a NIL body if original request was a HEAD"
  (when (and http-response (eq (braid:http-request-method http-request) :head))
    (setf (braid:http-message-body http-response) nil))
  http-response)

(defun wrap-head (http-request-handler)
  "Braid middleware that turns any HEAD request into a GET, and then
  sets the response body to nil."
  (lambda (http-request)
    (head-response
     (funcall http-request-handler (head-request http-request)) http-request)))

;;; Log

(defun default-log-handler (message)
  ""
  (format t "~a~%" message))

(defun wrap-log (http-request-handler &key (log-handler default-log-handler))
  ""
  (lambda (http-request)
    (funcall log-handler http-request)
    (let ((http-response (funcall http-request-handler http-request)))
      (funcall log-handler http-response)
      http-response)))

(defun wrap-last-modified (http-request-handler)
  "Middleware to set the Last-Modified header for response bodies of type pathname."
  (lambda (http-request)
    (let ((http-response (funcall http-request-handler http-request)))
      (set-last-modified http-response)
      http-response)))

(defun wrap-basic-authorization (realm username-password-handler http-request-handler)
  "Middleware that tests basic authorization credentials by calling
USERNAME-PASSWORD-HANDLER with a USERNAME and PASSWORD. If
USERNAME-PASSWORD-HANDLER returns true proceed with the
HTTP-REQUEST-HANDLER otherwise returns a not authorized response."
  (lambda (http-request)
    (multiple-value-bind (username password)
	(get-basic-authorization http-request)
      (if (funcall username-password-handler username password)
	  (funcall http-request-handler http-request)
	  (make-not-authorized-response realm)))))
						

(defun wrap-set-body-utf-8-bytes-to-string (http-request-handler)
  "Middleware that converts a UTF-8 response body into a string."
  (lambda (http-request)
    (let ((http-response (funcall http-request-handler http-request)))
      (set-body-utf-8-bytes-to-string http-response)
      http-response)))
;;;

(defvar *args* nil)
(defvar *arg* nil)

(defun wrap-route (method regex http-request-handler)
  "Middleware that only calls HTTP-REQUEST-HANDLER if the HTTP-REQUEST
method matches METHOD and uri matches REGEX. Binds *args* to the any
matching REGEX parameters, and *ARG* to the first such parameter."
  (lambda (http-request)
    (when (eq (braid:http-request-method http-request) method)
      (let ((uri (braid:http-request-uri http-request)))
	(multiple-value-bind (match *args*) (cl-ppcre:scan-to-strings regex (uri-to-string uri))
	  (when match
	    (let ((*arg* (and (> (length *args*) 0) (svref *args* 0))))
	      (funcall http-request-handler http-request))))))))


(defun combine-http-request-handlers (&rest http-request-handlers)
  "Executes a list of HTTP-REQUEST-HANDLERS until one returns non NIL."
  (lambda (http-request)
    (dolist (http-request-handler http-request-handlers)
      (alexandria:when-let (result (funcall http-request-handler http-request))
	(return result)))))


;;; End





