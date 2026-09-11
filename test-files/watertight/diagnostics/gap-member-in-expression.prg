* A leading-dot member reference is read where a statement *starts* with it, but not where one appears inside an expression. Form and report code is full of `IF .Height > 0` and `CASE .Mode = 1` reading the WITH target, and the cost is not one statement: the condition fails, so the whole IF or DO CASE is rejected, the opener is swallowed by the unsupported fallback, and the parse dies on the orphaned ELSE far below. This is the single most expensive gap the corpus has -- it accounts for more whole-file parse failures than everything else put together.
* The same shape occurs outside WITH as well, where the dot reads the containing form's own member.
LPARAMETERS m.oChart

WITH m.oChart
	IF .ChartsCount > 1 .and. !EMPTY(.SourceAlias)
		.AlphaChannel = 230
	ELSE
		.AlphaChannel = 255
	ENDIF

	DO CASE
		CASE .ScaleLegend.Format = "@$"
			.TITLE.Caption = "Amount"
		OTHERWISE
			.TITLE.Caption = "Count"
	ENDCASE
ENDWITH

RETURN .t.
