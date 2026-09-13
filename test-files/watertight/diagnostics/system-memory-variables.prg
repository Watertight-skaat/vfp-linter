* VFP's own system memory variables -- the underscore-prefixed set: _CUROBJ, _TALLY, _PAGENO, _PRETEXT, _TEXT, _ALIGNMENT and the rest. They exist before any code runs, so writing to one is not creating a private, and there is no declaration a user could add to satisfy the rule.
* The expectation below records the rule getting this wrong: implicit-private fires on every one of them, 47 times in the Watertight tree, and each finding asks for a LOCAL that would be a syntax error. Fixing the rule empties this file's expectation rather than changing the fixture.
* _SCREEN is already quiet, because the code only ever reads members off it rather than assigning the variable itself.
LPARAMETERS m.nObject

_curobj = m.nObject

_pageno = 1

_screen.Caption = "Watertight"

IF _tally = 0
	RETURN .f.
ENDIF

RETURN .t.
