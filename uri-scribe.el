;;; uri-scribe.el --- helper uri functions

;; Copyright (C) 2014 William Clifford

;; Author: William Clifford <wobh@yahoo.com>
;; Keywords: local, comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(ert-deftest uri-scribe-test-make-query ()
  "Make query"
  (should
   (equal (uri-scribe-make-query '(("file" . "filename.file")("action" . "upload")))
	  "file=filename.file&action=upload")))


(defun uri-scribe-make-query (args)
  (mapconcat (lambda (arg)
	       (concat (url-hexify-string (car arg))
		       "="
		       (url-hexify-string (cdr arg))))
	     args
	     "&"))

(provide 'uri-scribe)
;;; uri-scribe.el ends here
