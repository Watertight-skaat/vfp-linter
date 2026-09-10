FOR m.cnt = 1 TO ALEN(ArrayName, 1)
	? "test"
ENDFOR

FOR EACH oButton IN THIS.MyArray
	? "test"
NEXT

* NEXT is a valid FOR terminator, not just ENDFOR
FOR i = 1 TO 10
	? i
NEXT

* NEXT may name the loop variable
FOR i = 1 TO 10
	? i
NEXT i

* STEP, including a negative step
FOR i = 1 TO 10 STEP 2
	? i
NEXT

FOR i = 10 TO 1 STEP -1
	? i
ENDFOR

* nested loops mixing terminators
FOR i = 1 TO 3
	FOR j = 1 TO 3
		? i * j
	NEXT j
ENDFOR
