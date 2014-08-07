((:create-table mkt
		((id :type integer)
		 (symbol :type text)
		 (desc :type (or s-sql:db-null text))))

 (:create-table bs_ass_val
		((id :type integer) 
		 (bs_id :type integer) 
		 (name :type text)
		 (value :type numeric)))

 (:create-table bs
		((id :type integer) 
		 (instr_id :type integer) 
		 (date :type date)
		 (period :type smallint)))

 (:create-table bs_lia_val
		((id :type integer) 
		 (bs_id :type integer) 
		 (name :type text)
		 (value :type numeric)))

 (:create-table instr
		((id :type integer) 
		 (mkt_id :type integer) 
		 (ticker :type text)
		 (name :type (or s-sql:db-null text)) 
		 (ipo_year :type (or s-sql:db-null smallint))
		 (sector :type (or s-sql:db-null text)) 
		 (industry :type (or s-sql:db-null text))
		 (fy_month :type smallint)))

 (:create-table istmt
		((id :type integer) 
		 (instr_id :type integer) 
		 (date :type date)
		 (period :type smallint)))

 (:create-table is_val
		((id :type integer) 
		 (istmt_id :type integer) 
		 (name :type text)
		 (value :type numeric)))

 (:create-table cf
		((id :type integer) 
		 (instr_id :type integer) 
		 (date :type date)
		 (period :type smallint)))
 
 (:create-table cf_val
		((id :type integer) 
		 (cf_id :type integer)
		 (name :type text)
		 (value :type numeric)))

 (:create-table price
		((id :type integer) 
		 (instr_id :type integer) 
		 (date :type date)
		 (open :type numeric) 
		 (high :type numeric) 
		 (low :type numeric)
		 (close :type numeric) 
		 (volume :type integer) 
		 (adj_close :type numeric))))
