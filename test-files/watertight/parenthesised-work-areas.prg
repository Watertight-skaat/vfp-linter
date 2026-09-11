* A work area named by an expression rather than a literal. The parenthesised form is accepted wherever an alias is expected, not only where a table name is: accepting it in one place and not the other left the rest of each of these lines to the catch-all.
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
