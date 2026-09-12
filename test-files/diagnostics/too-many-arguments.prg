* A call passing more arguments than the routine declares. FoxPro raises "Too many arguments" at run time, so this is broken code rather than a style choice. Everything here is in one file, which is the half of the rule that needs no workspace index.
LOCAL lnResult
LOCAL ARRAY Totals[4, 4]

lnResult = Add(1, 2)
lnResult = Add(1, 2, 3)
DO Report WITH "Totals"
DO Report WITH "Totals", "extra"

* Passing fewer is legal: a parameter that was not supplied arrives as .F.
lnResult = Add(1)
DO Report

* An omitted argument still occupies its place, so this passes three.
lnResult = Add(1, , 3)

* A subscript reads as a call, so an array must not be measured against a routine that shares its name.
? Totals(2, 3)

* A call on an object is a method, which is not a routine of this file at all.
? _screen.Resize(1, 2, 3)

PROCEDURE Report
LPARAMETERS tcTitle
? m.tcTitle
ENDPROC

FUNCTION Add(tnA, tnB)
RETURN m.tnA + m.tnB
ENDFUNC

PROCEDURE Totals
LPARAMETERS tnRow
? m.tnRow
ENDPROC
