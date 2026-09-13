* DELETE naming the work area it acts on with IN, which is what lets a scan over one cursor delete from another without selecting it. The form appears with and without a scope and a FOR condition.
* Plain `DELETE IN <alias>` already parses. It is the FOR that costs a block: the statement is rejected, the FOR is read as the start of a counted loop, and the rest of the routine is swallowed waiting for an ENDFOR.
LPARAMETERS m.cSingCode

SELECT mosb_temp
SCAN FOR filename = "OBJECTS "
	DELETE ALL IN mosr_temp FOR mosnum = mosb_temp.mosnum .or. pmnum = mosb_temp.mosnum
	DELETE IN mosb_temp
ENDSCAN

RETURN .t.
