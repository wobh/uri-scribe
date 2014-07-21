;; -*- mode: emacs-lisp; -*-
;;; uri-scribe.el --- helper uri functions

;; Copyright (C) 2014 William Clifford

;; Author: William Clifford <wobh@yahoo.com>
;; Keywords: local, comm
;; Version: "0.2.2"

;; This file is NOT part of GNU Emacs.

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
;; writing query strings from and to association lists. Intended for
;; use with the url package's url structs

;; Example use:

;; (let ((uri (url-generic-parse-url "https://duckduckgo.com"))
;;       (query (uri-scribe-make-query '((q . "emacs uri-scribe")))))
;;   (setf (url-filename uri) (uri-scribe-make-path (list query)))
;;   (url-recreate-url uri))

;; "https://duckduckgo.com/?q=emacs%20uri-scribe"

;;; Code:


;;; URI Utilities

(defun uri-scribe-join-fields (infix &rest fields)
  "Join fields with INFIX string"
  (mapconcat 'identity fields infix))

(defun uri-scribe-set-prefix (prefix string)
  (if (string-prefix-p prefix string)
      string
    (concat prefix string)))

;;; URI Queries

(defun uri-scribe-make-query-field (key value)
  "Make a query field from key and value"
  (unless (stringp key)
    (signal 'wrong-type-argument (list 'stringp key)))
  (unless (stringp value)
    (signal 'wrong-type-argument (list 'stringp value)))
  (mapconcat (lambda (arg) (url-hexify-string arg))
	     (list key value)
	     "="))

(defun uri-scribe-read-query-field (field)
  "Read a query field into cons cell"
  (unless (stringp field)
    (signal 'wrong-type-argument (list 'stringp field)))
  (let* ((flist (split-string field "="))
	 (key (url-unhex-string (car flist)))
	 (value (url-unhex-string (cadr flist))))
    (cons key value)))

(defun uri-scribe-make-query-field-values (key value &rest values)
  "Make fields of one key with many values"
  (let ((field (uri-scribe-make-query-field key value)))
    (if values
	(apply 'uri-scribe-join-fields "&"
	       field
	       (mapcar (lambda (val)
			 (uri-scribe-make-query-field key val))
		       values))
      field)))

(defun uri-scribe-make-query (alist)
  "Make uri query string from alist"
  (format "?%s"
	  (apply 'uri-scribe-join-fields "&"
		 (mapcar (lambda (arg)
			   (cond ((stringp (cdr arg))
				  (uri-scribe-make-query-field (car arg) (cdr arg)))
				 (t
				  (apply 'uri-scribe-make-query-field-values
					 (car arg) (cdr arg)))))
			 alist))))

(defun uri-scribe-read-query (query)
  "Make an alist from query string"
  (let ((start (if (string-prefix-p "?" query) 1 0))
	(stop (string-match "#" query))
	(alist ()))
    (mapc (lambda (field)
	    (let* ((fcons (uri-scribe-read-query-field field))
		   (acons (assoc-string (car fcons) alist)))
	      (if (null alist)
		  (push fcons alist)
		(if acons
		    (if (stringp (cdr acons))
			(setcdr acons
				(list (cdr acons) (cdr fcons)))
		      (nconc (cdr acons) (list (cdr fcons))))
		  (nconc alist (list fcons))))))
	  (split-string (substring query start stop) "&"))
    alist))


;;; URI Domains

(defun uri-scribe-make-domain (names)
  "Make domain from list of names"
  (unless (consp names)
    (signal 'wrong-type-argument (list 'consp names)))
  (cond ((equal '("") names)
	 ".")
	(t
	 (apply 'uri-scribe-join-fields "."
		(append (remove "" (butlast names)) (last names))))))

(defun uri-scribe-read-domain (domain)
  "Read domain into list of names"
  (split-string domain "[\.]"))


;;; URI Paths

(defun uri-scribe-make-path (nodes)
  "Make path from list of nodes"
  (unless (consp nodes)
    (signal 'wrong-type-argument (list 'consp nodes)))
  (cond ((equal '("") nodes)
	 "/")
	(t
	 (apply 'uri-scribe-join-fields "/"
		(cons (car nodes) (remove "" (cdr nodes)))))))

(defun uri-scribe-read-path (path-str)
  "Read path into list of nodes"
  (split-string path-str "/"))

(defun uri-scribe-set-path-root (root path)
  "Set a root for path if it isn't already set"
  (setf root (uri-scribe-set-prefix "/" root))
  (setf path (uri-scribe-set-prefix "/" path))
  (uri-scribe-set-prefix root path))



;;; URI Fragments

(defun uri-scribe-make-fragment (fragment &optional path)
  "Make a fragment, append to optional path"
  (unless (stringp fragment)
    (signal 'wrong-type-argument (list 'stringp fragment)))
  (setf fragment (uri-scribe-set-prefix "#" fragment))
  (cond (path
	 (unless (stringp path)
	   (signal 'wrong-type-argument (list 'stringp path)))
	 (setf path (substring path 0 (string-match "#" path)))
	 (concat path fragment))
	(t fragment)))

(defun uri-scribe-read-fragment (path)
  "Read a fragment from a URI path"
  (unless (stringp path)
    (signal 'wrong-type-argument (list 'stringp path)))
  (let ((start (string-match "#" path)))
    (when start
      (setf start (1+ start))
      (substring path start (string-match "?" path)))))

(provide 'uri-scribe)
;;; uri-scribe.el ends here
