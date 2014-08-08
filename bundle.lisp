;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (C) by Marcus Pemer <mpemer@gmail.com>
;;;
;;; This file is part of DELTA-BASE.
;;;
;;; DELTA-BASE is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; DELTA-BASE is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with DELTA-BASE.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl)

(ql:quickload "delta-base")
(when (> (lisp-unit::fail (lisp-unit:run-tests :all :delta-base)) 0)
  (format t "Unit tests were not successful - exiting doing nothing.~%")
  (sb-ext:exit :code 1))

(ql:quickload "com.dvlsoft.clon")

(defpackage :delta-base-bundle
  (:use cl delta-base com.dvlsoft.clon)
  (:export #:main))

(in-package :delta-base-bundle)

(defparameter *version* "0.0.1")

(defsynopsis (:postfix "[FILE-A FILE-B]")
  (text :contents "Database schema comparison using S-SQL")
  (group (:header "Options:")

    (enum :short-name "o" :long-name "op" :enum '(:compare :extract)
      :description "Operation")

    (enum :short-name "f" :long-name "format" :enum '(:sql :s-sql)
      :description "Output Format")

    (flag :short-name "?" :long-name "help"
      :description "Print this help and exit.")

    (stropt :short-name "d" :long-name "database"
      :description "The database name")

    (stropt :short-name "h" :long-name "host"
      :description "Database host")

    (stropt :short-name "u" :long-name "user"
      :description "Database user name")

    (stropt :short-name "p" :long-name "pass"
      :description "Database user password")

    (flag :short-name "v" :long-name "version"
	  :description "Print version number and exit.")))

(defun print-version ()
  (format t "DELTA-BASE, VERSION ~a

Copyright (C) by Marcus Pemer <mpemer@gmail.com>

DELTA-BASE is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

DELTA-BASE is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with DELTA-BASE.  If not, see <http://www.gnu.org/licenses/>.
" *version*))

(defun main () 
  (make-context)

  (when (getopt :long-name "help")
    (help)
    (exit :code 0))
  
  (when (getopt :long-name "version")
    (print-version)
    (exit :code 0))

  (let* ((database (getopt :long-name "database"))

	 (user (getopt :long-name "user"))
	 (pass (getopt :long-name "pass"))
	 (host (getopt :long-name "host"))
	 (db-params (list database user pass host :pooled-p t)) 
	 (file-a (first (remainder)))
	 (file-b (second (remainder)))

	 (schema-a 
	  (if file-a
	      (with-open-file (file file-a) (read file))
	      (if (listen)
		  (read)
		  (read-schema db-params))))

	 (schema-b 
	  (if file-b
	      (with-open-file (file file-b) (read file))
	      (if (first db-params)
		  (read-schema db-params)
		  (read))))

	 (output 
	  (if (string-equal (getopt :long-name "op") "extract")
	      schema-a      
	      (schema-diff schema-a schema-b))))

    (when output 
      (if (string-equal (getopt :long-name "format") "sql")
	  (format t "~a" (make-sql output))
	  (progn (print output) (terpri)))))
  
  (exit))

(in-package :cl)
(sb-ext:save-lisp-and-die "delta-base" :toplevel #'delta-base-bundle:main :executable t)
