USE customer  && Opens Customer table
CLEAR
SKIP 4 IN 'customer'

go bottom in customer
IF .t.
	skip in customer
ENDIF