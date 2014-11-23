;; util.lisp

(in-package :braid-middleware)

;;; Courtesy Rainer Joswig
;;; http://stackoverflow.com/questions/3398602/easy-way-to-merge-plists

(defun merge-plists (p1 p2)
	"Merges two property lists with values from p2 overriding those in p1."
  (loop with notfound = '#:notfound
        for (indicator value) on p1 by #'cddr
        when (eq (getf p2 indicator notfound) notfound) 
        do (progn
             (push value p2)
             (push indicator p2))))

;;;

(defun plist-html-table (plist)
	"Converts a plist to a simple two-column HTML table."
	(with-output-to-string (s)
		(format s "<table>")
		(loop for (key value) on plist by #'cddr do
				 (format s "<tr><td>~a</td><td>~s</td></tr>" key value))
		(format s "</table>")))

;;; End
