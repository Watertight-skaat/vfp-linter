* `COPY TO <file> NEXT <n>` -- a record-count scope on COPY TO, used to write a table out in fixed-size chunks. The scope clause is not read, and because NEXT is also a loop terminator the leftover cannot be absorbed as an unsupported statement either: the enclosing DO WHILE is rejected and the file fails on the ENDDO.
* Both operands are parenthesised expressions here, which is how the chunking loops write it.
LPARAMETERS m.cTempDir, m.nChunk

LOCAL m.cAlias

m.cAlias = "curChunk"

DO WHILE !EOF("curWebPortalCustomers")
	SELECT curWebPortalCustomers
	COPY TO (m.cTempDir + m.cAlias) NEXT (m.nChunk)

	SELECT info
	APPEND FROM (m.cTempDir + m.cAlias)
	ERASE (m.cTempDir + m.cAlias + ".dbf")
ENDDO

RETURN .t.
