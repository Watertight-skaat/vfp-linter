* Legacy screen I/O. The option tails after the verb are kept as raw source rather than modelled.
@ 2,5 SAY "Name:"
@ 2,20 GET lcName SIZE 1,30 VALID !EMPTY(lcName)
@ 4,5 SAY "Total:" + TRANSFORM(lnTotal, "999,999.99")
@ 1,1 TO 10,40 PANEL
@ 6,5 CLEAR TO 8,40
