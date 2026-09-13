* A table named by an expression rather than written out: `UPDATE (m.cTable) SET`, `ALTER TABLE (m.cTable)`, `CREATE CURSOR &cTable.`. The integration code loops over a list of tables and runs the same statement against each, so the name is always a variable.
* `SELECT ... FROM (m.cTable)` already parses; it is the write statements that do not.
LPARAMETERS m.cTable

UPDATE (m.cTable) SET stnum = "00001" WHERE EMPTY(stnum)

ALTER TABLE (m.cTable) ADD COLUMN qbid C(20)

CREATE CURSOR &cTable. (mosnum C(8), stnum C(5))

RETURN .t.
