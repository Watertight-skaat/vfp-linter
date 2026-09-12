* A definition in the calling file wins over the library, so two arguments are right here and the rule must read this one.
PROCEDURE Announce
LPARAMETERS tcWhat, tnLevel
? m.tcWhat, m.tnLevel
ENDPROC

PROCEDURE Run
DO Announce WITH "starting", 1
ENDPROC
