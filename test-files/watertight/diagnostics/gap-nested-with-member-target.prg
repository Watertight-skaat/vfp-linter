* A nested WITH whose target is a member of the enclosing one: `WITH .Fields(n)` inside `WITH this.Chart`. The target is parsed as an expression, and an expression may not begin with a dot, so the inner WITH is rejected -- which rejects the SCAN around it, then the ELSE branch, then the outer WITH, and the file fails on a terminator twenty lines further down.
* Chart, grid and toolbar code reaches for this whenever it configures a collection of child objects in a loop.
LPARAMETERS m.oChart, m.cColumns

WITH m.oChart
	.IsSeries		= .t.
	.ColumnsTable	= m.cColumns

	SELECT (m.cColumns)
	SCAN
		WITH .Fields(RECNO(m.cColumns))
			.FieldValue	= ALLTRIM(EVALUATE(m.cColumns + ".Column"))
			.Legend		= ALLTRIM(EVALUATE(m.cColumns + ".Legend"))
		ENDWITH
	ENDSCAN
ENDWITH

RETURN .t.
