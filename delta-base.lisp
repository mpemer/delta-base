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

(defpackage :delta-base
  (:use cl asdf postmodern iterate lisp-unit)
  (:export
   #:read-schema
   #:schema-diff
   #:delta-base
   #:delta-base-sql
   #:make-sql
   :*database*))

(in-package :delta-base)

(defparameter *database* :postgres
  "Valid values include :POSTGRES :ORACLE")

(defprepared tables-stmt
"SELECT table_name
            FROM information_schema.tables
            WHERE table_schema='public'"
  :column)

(defun table-name (table) (second table))
(defun table-columns (table) (third table))
(defun column-name (column) (sql-symbol (first column)))
(defun column-type (column) (third column))

(defun read-tables ()
  (tables-stmt))



(defprepared column-stmt 
    "SELECT column_name, data_type, is_nullable
            FROM information_schema.columns
            WHERE table_name = $1")

(defun column-row-name (column) (sql-symbol (first column)))
(defun column-row-type (column) (sql-symbol (second column)))
(defun column-row-is-nullable (column) (third column))

(defun column-row-is-nullable-p (column)
  "Return non-nil if a column is nullable"
  (string-equal (column-row-is-nullable column) "YES"))




(defun sql-symbol (symbol-string)
  "Helper function to make an INTERNed symbol out of input string"
  (intern (string-upcase symbol-string)))

(defun read-columns (table-name)
  "Read columns from table in database"
  (column-stmt table-name))




(defun find-table (table schema)
  "Find a table in a schema"
  (find-if
   #'(lambda (tbl)
       (string-equal (table-name tbl) (table-name table)))
   schema))

(defun find-column (schema table column)
  "Find column in a table, in a schema"
  (let ((table-b (find-table table schema)))
    (find-if
     #'(lambda (clmn)
	 (string-equal (column-name clmn) (column-name column)))
     (table-columns table-b))))



(defun read-schema (db-params)
  "Query database and return schema definition as an S-SQL expression"
  (with-connection db-params
    (iter (for table-name in (read-tables))
	  (collect
	      (list :create-table (sql-symbol table-name)
		    (iter (for column in (read-columns table-name))
			  (collect 
			      (list
			       (column-name column)
			       :type
			       (if (column-row-is-nullable-p column)
				   (append '(or db-null) (list (column-row-type column)))
				   (column-row-type column))))))))))

(defun concat-sql-strings (list)
  "A non-recursive function that concatenates a list of sql statement strings with semi-colons and line breaks"
  (if (listp list)
      (with-output-to-string (s)
	(dolist (item list)
	  (if (stringp item)
	      (format s "~a;~%" item))))))

(defun make-sql (schema)
  "Convert an S-SQL expression to an executable SQL string"
  (concat-sql-strings
   (mapcar #'s-sql::sql-compile schema)))


(defun make-table-statement (table &key (op :create-table))
  "Make CREATE TABLE or DROP TABLE s-sql statement"
  (cond
    ((eq op :create-table) (list table))
    ((eq op :drop-table) (list (list :drop-table (table-name table))))
    (t (error (format nil "Operation ~a not supported" op)))))

(defun create-table (table)
  "Make CREATE-TABLE s-sql statement"
  (make-table-statement table :op :create-table))

(defun drop-table (table)
  "Make DROP TABLE s-sql statement"
  (make-table-statement table :op :drop-table))


(defun make-column-statement (column table &key (op :add-column))
  "Make ALTER TABLE statement to manipulate columns"
  (cond
    ((or (eq op :add-column) (eq op :alter-column))
     (list (append
	    (list :alter-table (table-name table) op (column-name column))
	    (rest column))))
    ((eq op :drop-column) (list (list :alter-table (table-name table) op (column-name column))))
    (t (error (format nil "Operation ~a not supported" op)))))

(defun alter-column (column table)
  "Make ALTER COLUMN s-sql statement"
  (make-column-statement column table :op :alter-column))

(defun add-column (column table)
  "Make ADD COLUMN s-sql statement"
  (make-column-statement column table :op :add-column))

(defun drop-column (column table)
  "Make DROP COLUMN s-sql statement"
  (make-column-statement column table :op :drop-column))

(defun columns-differ-p (a b)
  "Return nil if the definitions of column a and b are equal, non-nil otherwise"
  (not (equal (rest a) (rest b))))


(defun schema-diff (schema-a schema-b)
  "Create S-SQL statements that represent going from schema-a to schema-b.
Both schema-a and schema-b are given as S-SQL forms."
  (let ((statements ()))
    (labels ((add-statement (statement) (setf statements (append statements statement))))

      ;; find and add statements for any added or altered objects
      (iter
	(for table-a in schema-a)
	(if (find-table table-a schema-b)
	    (iter (for column-a in (table-columns table-a))
		  (let ((column-b (find-column schema-b table-a column-a)))
		    (if column-b
			(when (columns-differ-p column-a column-b)
			  (add-statement (alter-column column-a table-a)))
			(add-statement (add-column column-a table-a)))))
	    (add-statement (create-table table-a))))
      
      ;; find and add statements for any removed objects
      (iter
	(for table-b in schema-b)
	(if (find-table table-b schema-a)
	    (iter (for column-b in (table-columns table-b))
		  (unless (find-column schema-a table-b column-b)	       
		    (add-statement (drop-column column-b table-b))))
	    (add-statement (drop-table table-b))))
      
      ;; return results
      statements)))


(defun delta-base (schema db)
  "Calculate delta between schema and db, returns results as s-sql statements"
  (with-connection db
    (schema-diff schema (read-schema db))))

(defun delta-base-sql (schema db)
  "Calculate delta between schema and db, returns results as a string of sql statements"
  (make-sql (delta-base (schema sb))))


(schema-diff '((:create-table bs
		((id :type integer) 
		 (instr_id :type integer) 
		 (date :type date)
		 (period :type smallint))))
	     '((:create-table bs
		((id :type integer) 
		 (instr_id :type integer) 
		 (period :type smallint)))))
