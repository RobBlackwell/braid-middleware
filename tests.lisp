(ql:quickload :braid-middleware)

(defun my-response ()
	(braid:make-response :body (merge-pathnames (asdf:system-source-directory :braid-static) "README.md")))

(defun test1 ()
	""
	(let ((response	(braid-middleware:set-last-modified (my-response))))
		(stringp (braid:header response :last-modified))))


;; (test1)
		
