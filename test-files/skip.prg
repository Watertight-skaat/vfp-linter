USE customer  && Opens Customer table
CLEAR
SKIP 4 IN 'customer'
SKIP 4 IN dirname/tablename
skip -1

go bottom in customer
IF .t.
	skip in customer
ENDIF