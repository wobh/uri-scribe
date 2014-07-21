;; -*- mode: emacs-lisp; -*-
;;; uri-scribe-tests.el --- tests for uri-scribe

;; Copyright (C) 2014 William Clifford

;; Author: Clifford <wclifford@wclifford901.local>
;; Keywords: local
;; Version: 0.2.2
;; Package-Requires: ((uri-scribe))

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

;; Tests for uri-scribe.

;;; Code:

(require 'uri-scribe)

;;; URI Utilities

(ert-deftest uri-scribe-test-join-fields ()
  "Test joining fields in a query"
  (should
   (equal (uri-scribe-join-fields "&" "key1=val1" "key2=val2")
	  "key1=val1&key2=val2"))
  (should 
   (equal (uri-scribe-join-fields "/" "foo" "bar" "baz")
	  "foo/bar/baz"))
  (should
   (equal (uri-scribe-join-fields "." "bar" "baz" "qux")
	  "bar.baz.qux")))


;;; URI Queries

(ert-deftest uri-scribe-test-make-query-field ()
  "Test making a query field"
  (should-error
   (uri-scribe-make-query-field t "value")
   :type '(wrong-type-argument stringp key))
  (should-error
   (uri-scribe-make-query-field "key" t)
   :type '(wrong-type-argument stringp value))
  (should
   (equal (uri-scribe-make-query-field "key" "value")
	  "key=value"))
  (should
   (equal (uri-scribe-make-query-field "?key=" "=value#")
	  "%3Fkey%3D=%3Dvalue%23"))
  (should
   (equal (uri-scribe-make-query-field "&key%" " value&")
	  "%26key%25=%20value%26"))
  (should
   (equal (uri-scribe-make-query-field "baz" "[3 4 5]")
	  "baz=%5B3%204%205%5D")))

(ert-deftest uri-scribe-test-read-query-field ()
  "Test reading a query field"
  (should-error
   (uri-scribe-read-query-field t)
   :type '(wrong-type-argument stringp field))
  (should
   (equal (uri-scribe-read-query-field "=value")
	  '("" . "value")))
  (should
   (equal (uri-scribe-read-query-field "key=")
	  '("key" . "")))
  (should
   (equal (uri-scribe-read-query-field "key=value")
	  '("key" . "value")))
  (should
   (equal (uri-scribe-read-query-field "%3Fkey%3D=%3Dvalue%23")
	  '("?key=" . "=value#")))
  (should
   (equal (uri-scribe-read-query-field "%26key%25=%20value%26")
	  '("&key%" . " value&")))
  (should
   (equal (uri-scribe-read-query-field "baz=%5B3%204%205%5D")
	   '("baz" . "[3 4 5]"))))

(ert-deftest uri-scribe-test-make-query-field-values ()
  "Test making fields of one key with many values"
  (should
   (equal (uri-scribe-make-query-field-values "key" "value")
	  "key=value"))
  (should
   (equal (uri-scribe-make-query-field-values "key" "val1" "val2" "val3")
	  "key=val1&key=val2&key=val3")))

(ert-deftest uri-scribe-test-make-query ()
  "Test making queries from association list"
  (should
   (equal (uri-scribe-make-query '(("key1" . ("value1" "value2"))
				   ("key2" . "value3")))
	  "?key1=value1&key1=value2&key2=value3#"))
   (should
    (equal (uri-scribe-make-query '(("1" . "2")
				    ("a" . "b")
				    ("foo" . "bar")
				    ("baz" . "[3 4 5]")
				    ("qux" . ("6" "7" "8"))))
	   "?1=2&a=b&foo=bar&baz=%5B3%204%205%5D&qux=6&qux=7&qux=8#")))

(ert-deftest uri-scribe-test-read-query ()
  "Test reading queries into association list"
  (should
   (equal (uri-scribe-read-query "?key1=value1&key1=value2&key2=value3#")
	  '(("key1" . ("value1" "value2"))
	    ("key2" . "value3"))))
  (should
   (equal (uri-scribe-read-query
	   "?1=2&a=b&foo=bar&baz=%5B3%204%205%5D&qux=6&qux=7&qux=8#")
	  '(("1" . "2")
	    ("a" . "b")
	    ("foo" . "bar")
	    ("baz" . "[3 4 5]")
	    ("qux" . ("6" "7" "8"))))))


;;; URI domains

(ert-deftest uri-scribe-test-make-domain ()
  "Test making a domain from a list of names"
  (should-error (uri-scribe-make-domain '())
		:type '(wrong-type-argument consp names))
  (should
   (equal (uri-scribe-make-domain '(""))
	  "."))
  (should
   (equal (uri-scribe-make-domain '("foobar"))
	  "foobar"))
  (should
   (equal (uri-scribe-make-domain '("foobar" "com"))
	  "foobar.com"))
  (let* ((control '("foobar" "" "com"))
	 (subject (copy-tree control)))
    (should
     (equal (uri-scribe-make-domain subject) "foobar.com"))
    (should
     (equal subject control)))
  (should
   (equal (uri-scribe-make-domain '("foobar" "com" ""))
	  "foobar.com."))
  )

(ert-deftest uri-scribe-test-read-domain ()
  "Test reading a domain into a list of names"
  (should-error (uri-scribe-read-domain '())
		:type '(wrong-type-argument stringp domain))
  (should
   (equal (uri-scribe-read-domain "foobar.com")
	  '("foobar" "com")))
  (should
   (equal (uri-scribe-read-domain "foobar.com.")
	  '("foobar" "com" "")))
  )



;;; URI Paths

(ert-deftest uri-scribe-test-make-path ()
  "Test making a path from list of nodes"
  (should-error (uri-scribe-make-path '())
		:type '(wrong-type-argument consp nodes))
  (should
   (equal (uri-scribe-make-path '(""))
	  "/"))
  (should
   (equal (uri-scribe-make-path '("bar"))
	  "bar"))
  (should
   (equal (uri-scribe-make-path '("foo" "bar"))
	  "foo/bar"))
  (let* ((control '("foo" "" "bar"))
	 (subject (copy-tree control)))
    (should
     (equal (uri-scribe-make-path subject) "foo/bar"))
    (should
     (equal subject control)))
  (should
   (equal (uri-scribe-make-path '("" "foo" "bar"))
	  "/foo/bar"))
  )

(ert-deftest uri-scribe-test-read-path ()
  "Test reading a path into list of nodes"
  (should-error (uri-scribe-read-path '())
		:type '(wrong-type-argument stringp path))
  (should
   (equal (uri-scribe-read-path "foo/bar")
	  '("foo" "bar")))
  (should
   (equal (uri-scribe-read-path "/foo/bar")
	  '("" "foo" "bar")))
  )

(ert-deftest uri-scribe-test-set-path-root ()
  "Test setting path root if unspecified."
  (should-error
   (uri-scribe-set-path-root 1 "baz/foo")
   :type '(wrong-type-argument stringp root))
  (should-error
   (uri-scribe-set-path-root "foo" 1)
   :type '(wrong-type-argument stringp path))
  (should
   (equal (uri-scribe-set-path-root "/bar" "baz/foo")
	  "/bar/baz/foo"))
  (should
   (equal (uri-scribe-set-path-root "/bar" "/baz/foo")
	  "/bar/baz/foo"))
  (should
   (equal (uri-scribe-set-path-root "/bar" "/bar/baz/foo")
	  "/bar/baz/foo"))
  (should
   (equal (uri-scribe-set-path-root "bar" "baz/foo")
	  "/bar/baz/foo")))


(provide 'uri-scribe-tests)
;;; uri-scribe-tests.el ends here
