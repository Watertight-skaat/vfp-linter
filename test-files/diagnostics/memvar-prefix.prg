* A variable sharing a name with a field of an open table. A bare reference resolves to the field, so
* the assignment below can update the record instead of the variable.
PROCEDURE UpdateNote
	LOCAL cust_id
	USE customer
	cust_id = 7
	REPLACE cust_id WITH m.cust_id
ENDPROC

* The qualified reference is the other way the linter learns a name is a field.
PROCEDURE ReadTotal
	LOCAL total
	USE orders
	total = orders.total
	? m.total
ENDPROC

* No evidence that either name is a field, so neither is reported.
PROCEDURE NoCollision
	LOCAL lcName, lnCount
	USE customer
	lcName = "Abernathy"
	lnCount = 1
	? m.lcName + TRANSFORM(m.lnCount)
ENDPROC
