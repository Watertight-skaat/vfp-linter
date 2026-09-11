* PRIVATE ALL hides every variable of the caller from this routine and everything it calls, which in
* a call chain this deep means a routine several levels down can no longer see what it was passed.
PROCEDURE RebuildTotals
	PRIVATE ALL

	m_total	= 0
	m_count	= 0
	DO SumBranch WITH "00001"
	RETURN m_total
ENDPROC


* Naming what is hidden, or declaring what this routine owns, is the way out.
PROCEDURE RebuildTotalsScoped
	PRIVATE ALL LIKE m_*
	LOCAL m.nTotal

	m.nTotal = 0
	DO SumBranch WITH "00001"
	RETURN m.nTotal
ENDPROC
