* A WITH member assignment is read only as a direct child of the WITH block. Put an IF, a SCAN or a DO CASE between the WITH and the assignment and the leading dot is no longer accepted, so every property write inside a conditional inside a WITH reports as unsupported.
* This one does not fail the parse, which is what makes it expensive: form and report code conditions most of its property writes, so the rules that read the symbol table see a fraction of what a WITH block actually writes, and `implicit-private` in particular cannot tell a missed property from a variable it never saw.
LPARAMETERS m.oLabel, m.lWide

WITH m.oLabel
	.Anchor = 0

	IF m.lWide
		.Width		= 400
		.AutoSize	= .f.
	ELSE
		.Visible	= .f.
	ENDIF

	DO CASE
		CASE m.lWide
			.FontSize = 10
		OTHERWISE
			.FontSize = 8
	ENDCASE

	.Anchor = 8
ENDWITH

RETURN .t.
