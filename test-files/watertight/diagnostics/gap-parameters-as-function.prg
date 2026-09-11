* PARAMETERS() is both a declaration keyword and the function returning how many arguments the caller passed. The declaration wins, so `IF PARAMETERS() < 4` reads as a PARAMETERS statement declaring nothing, the IF is rejected, and its ENDIF is left orphaned. Legacy routines default their optional arguments this way and nothing else, so a file that uses it loses its whole parse.
* The same boundary problem shows up in `CASE PARAMETERS() = 6` and `FOR m.n = 1 TO PARAMETERS()`.
PARAMETERS cPath, nUserKey, lEnumKeys, cOption

IF PARAMETERS() < 3 .or. TYPE("m.lEnumKeys") # "L"
	lEnumKeys = .f.
ENDIF

DO CASE
	CASE PARAMETERS() = 2
		cOption = ""
	CASE PARAMETERS() = 1
		cOption = "?"
ENDCASE

RETURN lEnumKeys
