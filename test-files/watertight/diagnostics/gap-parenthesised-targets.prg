* A work area named by an expression rather than a literal. The grammar accepts the parenthesised
* form where a table name is expected but not where an alias is expected, so these lines are read
* only as far as the parenthesis and the rest of the line is left over.
LPARAMETERS m.cPath, m.cAlias, m.cTag

* Closing a work area whose alias is held in a variable.
USE IN (D_MTPC)

* Opening a second handle under a computed alias.
USE (m.cPath) AGAIN ALIAS (m.cAlias) IN 0

* Aiming a relation, an order or a record move at a computed alias.
SET RELATION TO stnum + acctnum INTO (m.cAlias)
SET ORDER TO (m.cTag) DESCENDING IN (m.cAlias)
GO TOP IN (m.cAlias)

* Clearing the filter. The bare TO is the documented way to do it.
SET FILTER TO
