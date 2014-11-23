;;;; braid-middleware.asd

(asdf:defsystem #:braid-middleware
		:version "0.0.1"
		:author "Rob Blackwell"
		:description ""
		:serial t
		:depends-on (#:alexandria
								 #:puri
								 #:babel
								 #:cl-fad
								 #:trivial-backtrace
								 #:cl-ppcre
								 #:braid)
		:components ((:file "package")
								 (:file "util")
								 (:file "middleware")))
