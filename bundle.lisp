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
  (format t "delta-base, version ~a

Copyright © 2010 Marcus Pemer, All Rights Reserved

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
" *version*))

(defun main () 
  (make-context)

  (when (getopt :long-name "help")
    (help))
  
  (when (getopt :long-name "version")
    (print-version))

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
