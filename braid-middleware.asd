;;;; braid-middleware.asd

(asdf:defsystem #:braid-middleware
		:version "0.0.1"
		:author "Rob Blackwell"
		:description "Higher order functions that add additional
		functionality to Braid request handlers."
		:serial t
		:depends-on (#:alexandria
								 #:puri
								 #:babel
								 #:cl-fad
								 #:trivial-backtrace
								 #:cl-ppcre
								 #:cl-base64
								 #:braid)
		:components ((:file "package")
								 (:file "util")
								 (:file "middleware")))
