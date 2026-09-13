* Ordinary calls to the built-ins. Every one of these reaches VFP's own function, not the same-named routine in moseslib.prg.
* The expectation below records too-many-arguments firing on all of them: 1291 findings for VARTYPE and 457 for DATETIME in the Watertight tree, together 73% of everything that rule reports. The rule has to know that a built-in name cannot be shadowed before any of its other findings can be trusted.
LPARAMETERS m.uValue

IF VARTYPE(m.uValue) = "C"
	? DATETIME()
	? DATETIME(2026, 9, 13, 10, 0, 0)
ENDIF

* A user routine really is over-supplied, which must keep reporting.
DO Announce WITH "one", "two"

RETURN .t.

PROCEDURE Announce
LPARAMETERS tcMessage
	? m.tcMessage
ENDPROC
