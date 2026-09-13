* VFP's own system memory variables -- the underscore-prefixed set: _CUROBJ, _TALLY, _PAGENO, _PRETEXT, _TEXT, _ALIGNMENT and the rest. They exist before any code runs, so writing to one is not creating a private, and there is no declaration a user could add to satisfy the rule.
* implicit-private used to fire on every one of them, 47 times in the Watertight tree, each finding asking for a LOCAL that would be a syntax error. The silence is what this fixture pins now: no .expected file means no diagnostic at all.
* _SCREEN is already quiet, because the code only ever reads members off it rather than assigning the variable itself.
LPARAMETERS m.nObject

_curobj = m.nObject

_pageno = 1

_screen.Caption = "Watertight"

IF _tally = 0
	RETURN .f.
ENDIF

RETURN .t.
