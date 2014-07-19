;;; uri-scribe.el --- helper uri functions

;; Copyright (C) 2014 William Clifford

;; Author: William Clifford <wobh@yahoo.com>
;; Keywords: local, comm
;; Version: "0.1.0"

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

;; Some helper functions for composing URIs. Mainly reading and
;; writing query strings but also for some other helpful stuff not in
;; the url package.

;; Example use:

;; (let ((uri (url-generic-parse-url "https://duckduckgo.com"))
;;       (query (uri-scribe-make-query '((q . "emacs uri-scribe")))))
;;   (setf (url-filename uri) (uri-scribe-make-path (list query)))
;;   (url-recreate-url uri))

;; "https://duckduckgo.com/?q=emacs%20uri-scribe"

;;; Code:

(ert-deftest uri-scribe-test-join-query-fields ()
  "Test joining fields in a query"
  (should
   (equal (uri-scribe-join-query-fields "key1=val1" "key2=val2")
	  "key1=val1&key2=val2")))

(defun uri-scribe-join-query-fields (&rest fields)
  (mapconcat 'identity fields "&"))

(ert-deftest uri-scribe-test-make-query-field ()
  "Test making a query field"
  (should
   (equal (uri-scribe-make-query-field "key" "value")
	  "key=value"))
  (should
   (equal (uri-scribe-make-query-field 'key 'value)
	  "key=value"))
  (should
   (equal (uri-scribe-make-query-field 1 1)
	  "1=1"))
  (should
   (equal (uri-scribe-make-query-field "key 1" "value 1")
	  "key%201=value%201"))
  (should
   (equal (uri-scribe-make-query-field "key=" "value?")
	  "key%3D=value%3F"))
  (should
   (equal (uri-scribe-make-query-field "key" "value🙆")
	  "key=value%F0%9F%99%86")))

(defun uri-scribe-make-query-field (key value)
  "Make a query field from key and value"
  (assert (and (atom key) (atom value)))
  (mapconcat (lambda (arg)
	       (url-hexify-string (format "%s" arg)))
	     (list key value)
	     "="))

(ert-deftest uri-scribe-test-make-query-field-values ()
  "Test making fields of one key with many values"
  (should
   (equal (uri-scribe-make-query-field-values "key" "value")
	  "key=value"))
  (should
   (equal (uri-scribe-make-query-field-values "key" "val1" "val2" "val3")
	  "key=val1&key=val2&key=val3")))

(defun uri-scribe-make-query-field-values (key value &rest values)
  "Make fields of one key with many values"
  (let ((field (uri-scribe-make-query-field key value)))
    (if values
	(apply 'uri-scribe-join-query-fields
	       field
	       (mapcar (lambda (val)
			 (uri-scribe-make-query-field key val))
		       values))
      field)))

(ert-deftest uri-scribe-test-make-query ()
  "Test making queries from association list"
  (should
   (equal (uri-scribe-make-query '(("key1" . ("value1" "value2"))
				   ("key2" . "value3")))
	  "?key1=value1&key1=value2&key2=value3")))

(defun uri-scribe-make-query (alist)
  "Make uri query string from alist"
  (concat "?"
	  (apply 'uri-scribe-join-query-fields
		 (mapcar (lambda (arg)
			   (if (atom (cdr arg))
			       (uri-scribe-make-query-field (car arg) (cdr arg))
			     (apply 'uri-scribe-make-query-field-values
				    (car arg) (cdr arg))))
			 alist))))

(ert-deftest uri-scribe-test-read-query ()
  "Test making queries"
  (should
   (equal (uri-scribe-read-query "?key1=value1&key1=value2&key2=value3")
	  '(("key1" . ("value1" "value2"))
	    ("key2" . "value3")))))

(defun uri-scribe-read-query (query)
  "Make an alist from query string"
  (let ((alist ()))
    (mapc (lambda (field)
	    (let* ((flist (split-string field "="))
		   (key (car flist))
		   (value (cadr flist))
		   (acons (assoc-string key alist)))
	      (if (null alist)
		  (push (cons key value) alist)
		(if acons
		    (if (atom (cdr acons))
			(setcdr acons
				(list (cdr acons) value))
		      (nconc (cdr acons) (list value)))
		  (nconc alist (list (cons key value))))
		)))
	  (split-string (substring query 1 nil) "&"))
    alist))

(ert-deftest uri-scribe-test-make-domain ()
  "Test making a domain"
  (should
   (equal (uri-scribe-make-domain '())
	  ""))
  (should
   (equal (uri-scribe-make-domain '(""))
	  ""))
  (should
   (equal (uri-scribe-make-domain '("foo"))
	  "foo"))
  (should
   (equal (uri-scribe-make-domain '("foo" "com"))
	  "foo.com")))

(defun uri-scribe-make-domain (domains)
  "Make domain from list"
  (mapconcat 'identity domains "."))

(ert-deftest uri-scribe-test-make-path ()
  "Test making a domain"
  (should
   (equal (uri-scribe-make-path '())
	  ""))
  (should
   (equal (uri-scribe-make-path '(""))
	  "/"))
  (should
   (equal (uri-scribe-make-path '("bar"))
	  "/bar"))
  (should
   (equal (uri-scribe-make-path '("bar" "baz" "qux"))
	  "/bar/baz/qux")))

(defun uri-scribe-make-path (nodes)
  "Make path from list"
  (mapconcat 'identity (cons "" nodes) "/"))

(provide 'uri-scribe)
;;; uri-scribe.el ends here
