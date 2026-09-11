* Command words used as ordinary names. `LOOP` is a loop-control keyword and `CLASS` opens a definition, but both are also column names in the metadata tables and both are used as flag variables in the 1990s code. Wherever one appears where an expression is expected the expression fails, and the block around it goes with it.
* A keyword after a dot is already accepted as a member name; this is the same problem one level out, where the keyword stands alone.
LOCAL loop, classloc

STORE .t. TO loop

DO WHILE loop
	IF !(class == "frame" .and. "controls.vcx" $ LOWER(classloc))
		REPLACE class WITH "frame", classloc WITH "controls.vcx"
	ENDIF
	STORE .f. TO loop
ENDDO

RETURN .t.
