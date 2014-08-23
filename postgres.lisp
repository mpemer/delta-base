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

(ql:quickload "plain-odbc")
(use-package :plain-odbc)

(setf *con* (connect-oracle plain-odbc:*default-oracle-dsn* "glasir_core" "password"))

(defpackage :delta-base.postgres
  (:use cl asdf postmodern iterate lisp-unit)
  (:export
   #:read-tables
   #:read-columns))

(in-package :delta-base.postgres)

(defprepared tables-stmt
"SELECT table_name
            FROM information_schema.tables
            WHERE table_schema='public'"
  :column)

(defun read-tables ()
  (tables-stmt))

(defprepared column-stmt 
    "SELECT column_name, data_type, is_nullable
            FROM information_schema.columns
            WHERE table_name = $1")

(defun read-columns (table-name)
  "Read columns from table in database"
  (column-stmt table-name))


