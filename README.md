DELTA-BASE
==========

Delta-base is a Common Lisp library for interacting with the schema definition of databases. Features are:

* Extract the schema definition from a database in S-SQL or textual SQL format
* Compare two S-SQL schemas and generate the delta in form of alter statements
* Compare an S-SQL manifest to the schema in a database 

Delta-base uses Marijn Haverbeke's excellent [Postmodern](http://marijnhaverbeke.nl/postmodern/) library and as such comes with support for Postgres. The code for Postmodern can be found on [Github](https://github.com/marijnh/Postmodern). All schema definitions are done in S-SQL. At the time of writing this README, the delta-base library depends on an extended version of S-SQL as described in [this pull request](https://github.com/marijnh/Postmodern/pull/66). Until further notice, please refer to [the forked repo of Postmodern](https://github.com/mpemer/Postmodern) if you are going to use delta-base.

Supported Lisp
--------------
I use SBCL and have developed/tested using this platform. I also ran the library in ABCL once. It seemed to work. This is about the extent of my testing at this time.

Bundling / Packaging
--------------------
If you wish to create an executable bundle out of this library, you may do so by running the bundle.sh shell script. Currently, only SBCL is supported for bundling the application.

Exported Symbols
----------------

 **READ-SCHEMA**

    Lambda-list: (&OPTIONAL DB-PARAMS)
      Derived type: (FUNCTION (&OPTIONAL T) (VALUES LIST &OPTIONAL))
      Documentation:
        Query database and return schema definition as an S-SQL expression


**SCHEMA-DIFF**

    Lambda-list: (SCHEMA-A SCHEMA-B)
    Derived type: (FUNCTION (T T) (VALUES T &OPTIONAL))
    Documentation:
      Create S-SQL statements that represent going from schema-a to schema-b.
      Both schema-a and schema-b are given as S-SQL forms.


**DELTA-BASE**

    Lambda-list: (SCHEMA DB)
    Derived type: (FUNCTION (T T) (VALUES T &OPTIONAL))
    Documentation:
      Calculate delta between schema and db, returns results as s-sql statements


**DELTA-BASE-SQL**

    Lambda-list: (SCHEMA DB)
    Derived type: (FUNCTION (T T) *)
    Documentation:
      Calculate delta between schema and db, returns results as a string of sql statements

**MAKE-SQL**

    Lambda-list: (SCHEMA)
    Derived type: (FUNCTION (T) *)
    Documentation:
      Convert an S-SQL expression to an executable SQL string


Example Usage
-------------

Running:

    (schema-diff
       '((:create-table bs
		  ((id :type integer) 
		   (instr_id :type integer) 
		   (date :type date)
		   (period :type smallint))))
	   '((:create-table bs
		  ((id :type integer) 
		   (instr_id :type integer) 
		   (period :type smallint)))))

Should yield:

    ((:ALTER-TABLE BS :ADD-COLUMN DATE :TYPE DATE))


Known Issues and Caveats
------------------------

At the time of writing, the schema support is limited. The following is supported:

* Table definitions with columns and types
* null / not null flags for columns

The following is not yet supported:

* Table references (foreign key constraints)
* Indices
* Sequences
* Anything else

As you can see this functionality is quite limited in this initial version of the library. I plan on implementing indices, constraints, intelligent ordering of statements based on references, sequences and whatever else I need as I actually need it. If you feel something is missing you are welcome to contribute to this library by forking the repository, committing your changes and submitting a pull request.

