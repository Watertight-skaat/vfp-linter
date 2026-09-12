SET ORDER TO fieldA DESCENDING IN tableA
set order to fieldA desc in tableA

PROCEDURE pName
	SET ORDER TO stpart IN prodmast
endproc

* The argument is a list, and the tail can carry clauses of its own.
SET PROCEDURE TO lib1, lib2 ADDITIVE
SET CLASSLIB TO mylib IN app ALIAS al
SET SKIP TO orders, items
SET RELATION OFF INTO orders

* TOPIC begins with the word TO, which without a boundary read as SET TO with a setting called PIC.
SET TOPIC TO "customers"

* The settings whose name is two words, and the one whose field sits between its keywords.
SET TOPIC ID TO 5
SET NOTIFY CURSOR OFF
SET NOTIFY OFF
SET WINDOW OF MEMO notes TO myform
SET WINDOW OF MEMO notes TO
SET STATUS BAR OFF
SET CENTURY ON
