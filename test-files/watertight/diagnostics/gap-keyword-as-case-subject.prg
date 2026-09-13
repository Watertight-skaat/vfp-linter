* A command word used as a variable where a CASE expects an expression. The 2.75 menus store the operator's choice in a variable called SELECT, declared on a PRIVATE line with the rest, and then branch on it.
* `gap-select-as-function.prg` covers SELECT() the function and `gap-keyword-as-variable.prg` covers it in an IF; this is the third position, and it is the one that costs a whole DO CASE. The first CASE is rejected, so the DO CASE has no branches, and the OTHERWISE and ENDCASE are left orphaned.
* `STORE SELECT(0) TO m.nArea` parses, but `store select (0) to p_prarea` -- the same call with a space before the parenthesis, which is how RECEIPTC.prg writes it -- does not.
PRIVATE SELECT, GOON, p_prarea

SELECT = "X"

DO CASE
	CASE SELECT = "X"
		? "post"
	OTHERWISE
		? "cancel"
ENDCASE

store select (0) to p_prarea

RETURN .t.
