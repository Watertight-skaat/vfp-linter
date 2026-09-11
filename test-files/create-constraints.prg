* Table-level constraints, which open with words a column definition will also swallow: `UNIQUE stnum TAG stnum` reads as a column named UNIQUE of type stnum, and the column then stops mid-clause. That used to cost the whole CREATE TABLE its parse rather than one clause. A top-level fixture, so an `.expected` file beside it means the statement fell back to the catch-all again.
CREATE TABLE custinfo ;
	(stnum C(6), ;
	acctnum C(10), ;
	invbal N(12, 2) NOT NULL DEFAULT 0, ;
	PRIMARY KEY stnum TAG stnum, ;
	UNIQUE acctnum TAG acctnum COLLATE "MACHINE", ;
	CHECK invbal >= 0 ERROR "balance cannot be negative")

CREATE TABLE invinfo ;
	(invnum C(8) PRIMARY KEY, ;
	stnum C(6) CHECK NOT EMPTY(stnum), ;
	FOREIGN KEY stnum TAG stnum NODUP REFERENCES custinfo TAG stnum)

* A column genuinely named for one of those words is still a column.
CREATE CURSOR flags (check_flag L, unique_id C(6), primary_ref C(6))

* The column-level forms, which were already read and have to stay that way.
CREATE TABLE settings (skey C(20) UNIQUE, sval C(40) NULL DEFAULT "", sref C(6) REFERENCES custinfo TAG stnum)
