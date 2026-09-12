* ALTER TABLE's tail, one clause at a time. It used to be kept as source, which cost it two ways: a second clause on a continuation line was left behind as a statement of its own and reported as a gap, and the columns it adds were invisible to the rules that ask what the file's field names are. A top-level fixture, so an `.expected` file beside it means a clause stopped reading again.
ALTER TABLE items ADD COLUMN billcode C(6) ;
                  ADD COLUMN ledacct C(8) ;
                  ADD COLUMN postdate D NULL

* Every column option CREATE TABLE takes, ALTER takes too, and NOVALIDATE is its own.
ALTER TABLE items ADD COLUMN qty N(12, 2) NOT NULL CHECK qty >= 0 ERROR "quantity cannot be negative" DEFAULT 0
ALTER TABLE items ADD COLUMN itemno C(10) UNIQUE COLLATE "MACHINE" REFERENCES catalog TAG itemno
ALTER TABLE items ADD COLUMN notes M NOCPTRANS NOVALIDATE
ALTER TABLE items ADD COLUMN seq I AUTOINC NEXTVALUE 1 STEP 1

* The form that changes a column without restating its type, and the one that restates it.
ALTER TABLE items ALTER COLUMN billcode SET DEFAULT m.cDefaultCode SET CHECK NOT EMPTY(billcode) ERROR "code required"
ALTER TABLE items ALTER COLUMN ledacct NULL DROP DEFAULT
ALTER TABLE items ALTER COLUMN ledacct C(12) NOT NULL

* The table-level constraints, added and dropped, and the FOR clause that is ALTER's alone.
ALTER TABLE items ADD PRIMARY KEY itemno TAG itemno
ALTER TABLE items ADD UNIQUE billcode TAG billcode FOR NOT EMPTY(billcode) COLLATE "MACHINE"
ALTER TABLE items ADD FOREIGN KEY ledacct TAG ledacct REFERENCES ledger TAG acct
ALTER TABLE items DROP FOREIGN KEY TAG ledacct SAVE
ALTER TABLE items DROP UNIQUE TAG billcode
ALTER TABLE items DROP PRIMARY KEY
ALTER TABLE items SET CHECK qty >= 0 ERROR "quantity cannot be negative"
ALTER TABLE items DROP CHECK

* The clauses stand side by side without commas as well, which is how the constraint forms are written.
ALTER TABLE items DROP COLUMN postdate ADD PRIMARY KEY itemno TAG itemno
ALTER TABLE items RENAME COLUMN billcode TO bill_code NOVALIDATE

* A column named for one of the constraint keywords is still a column.
ALTER TABLE flags ADD COLUMN unique_id C(6)
ALTER TABLE flags DROP COLUMN check_flag
