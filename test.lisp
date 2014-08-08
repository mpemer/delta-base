(in-package :delta-base)


(define-test schema-diff

  (assert-equal  ;; equal
   nil
   (schema-diff
    '((:CREATE-TABLE MKT
       ((ID :TYPE INTEGER) (SYMBOL :TYPE TEXT) (DESC :TYPE (OR DB-NULL TEXT)))))
    '((:CREATE-TABLE MKT
       ((ID :TYPE INTEGER) (SYMBOL :TYPE TEXT) (DESC :TYPE (OR DB-NULL TEXT)))))))

  (assert-equal  ;; change in column type
   '((:ALTER-TABLE MKT :ALTER-COLUMN SYMBOL :TYPE TEXT))
   (schema-diff
    '((:CREATE-TABLE MKT
       ((ID :TYPE INTEGER) (SYMBOL :TYPE TEXT) (DESC :TYPE (OR DB-NULL TEXT)))))
    '((:CREATE-TABLE MKT
       ((ID :TYPE INTEGER) (SYMBOL :TYPE INTEGER) (DESC :TYPE (OR DB-NULL TEXT)))))))

  (assert-equal  ;; added column
   '((:ALTER-TABLE MKT :ADD-COLUMN SYMBOL :TYPE TEXT))
   (schema-diff
    '((:CREATE-TABLE MKT
       ((ID :TYPE INTEGER) (SYMBOL :TYPE TEXT) (DESC :TYPE (OR DB-NULL TEXT)))))
    '((:CREATE-TABLE MKT
       ((ID :TYPE INTEGER) (DESC :TYPE (OR DB-NULL TEXT)))))))

  (assert-equal  ;; dropped column
   '((:ALTER-TABLE MKT :DROP-COLUMN SYMBOL))
   (schema-diff
    '((:CREATE-TABLE MKT
       ((ID :TYPE INTEGER) (DESC :TYPE (OR DB-NULL TEXT)))))
    '((:CREATE-TABLE MKT
       ((ID :TYPE INTEGER) (SYMBOL :TYPE TEXT) (DESC :TYPE (OR DB-NULL TEXT))))))))

(define-test make-sql
  (assert-equal
   "CREATE TABLE mkt (id INTEGER NOT NULL, symbol TEXT NOT NULL, \"desc\" TEXT);
"
   (make-sql '((:CREATE-TABLE MKT
		((ID :TYPE INTEGER) (SYMBOL :TYPE TEXT) (DESC :TYPE (OR DB-NULL TEXT))))))))
   
(in-package :lisp-unit)
(setq *print-errors* t
      *print-summary* t
      *print-failures* t)
