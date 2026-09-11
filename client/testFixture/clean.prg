* Fixture for the end-to-end diagnostics test: this file must lint clean.
LOCAL lcName, lnTotal, i

lcName = "ok"
lnTotal = 0

FOR i = 1 TO 10
	lnTotal = lnTotal + i
NEXT i
