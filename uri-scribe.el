;; -*- mode: emacs-lisp; -*-
;;; uri-scribe.el --- helper uri functions

;; Copyright (C) 2014 William Clifford

;; Author: William Clifford <wobh@yahoo.com>
;; Keywords: local, comm
;; Version: "0.2.0"

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

;;; URI Queries

(defun uri-scribe-join-query-fields (&rest fields)
  (mapconcat 'identity fields "&"))

(defun uri-scribe-make-query-field (key value)
  "Make a query field from key and value"
  (unless (stringp key)
    (signal 'wrong-type-argument '(stringp key)))
  (unless (stringp value)
    (signal 'wrong-type-argument '(stringp value)))
  (mapconcat (lambda (arg) (url-hexify-string arg))
	     (list key value)
	     "="))

(defun uri-scribe-read-query-field (field)
  "Read a query field into cons cell"
  (unless (stringp field)
    (signal 'wrong-type-argument '(stringp field)))
  (let* ((flist (split-string field "="))
	 (key (url-unhex-string (car flist)))
	 (value (url-unhex-string (cadr flist))))
    (cons key value)))

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

(defun uri-scribe-make-query (alist)
  "Make uri query string from alist"
  (format "?%s#"
	  (apply 'uri-scribe-join-query-fields
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
	(stop (when (equal "#" (substring query -1)) -1))
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
    (signal 'wrong-type-argument '(consp names)))
  (cond ((equal '("") names)
	 ".")
	(t
	 (mapconcat 'identity 
		    (append (remove "" (butlast names)) (last names))
		    "."))))

(defun uri-scribe-read-domain (domain)
  "Read domain into list of names"
  (split-string domain "[\.]"))

;;; URI Paths

(defun uri-scribe-make-path (nodes)
  "Make path from list of nodes"
  (unless (consp nodes)
    (signal 'wrong-type-argument '(consp nodes)))
  (cond ((equal '("") nodes)
	 "/")
	(t
	 (mapconcat 'identity 
		    (cons (car nodes) (remove "" (cdr nodes)))
		    "/"))))

(defun uri-scribe-read-path (path-str)
  "Read path into list of nodes"
  (split-string path-str "/"))

(defun uri-scribe-set-path-root (root path)
  "Set a root for path if it isn't already set"
  (unless (stringp root)
    (signal 'wrong-type-argument '(stringp root)))
  (unless (stringp path)
    (signal 'wrong-type-argument '(stringp path)))
  (unless (string-prefix-p "/" root)
    (setf root (concat "/" root)))
  (unless (string-prefix-p "/" path)
    (setf path (concat "/" path)))
  (if (string-prefix-p root path)
      path 
    (concat root path)))
	
(provide 'uri-scribe)
;;; uri-scribe.el ends here
