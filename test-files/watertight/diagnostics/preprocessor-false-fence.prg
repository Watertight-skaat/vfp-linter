* `#IF .F.` fencing off a block that is not code at all. FoxPro's preprocessor evaluates the condition and never compiles the body, which is how mosesrules.prg keeps a page of house rules at the top of a .prg and how unfinished work is parked without commenting every line.
* The linter read the body anyway, so the prose was parsed as statements. Any `if` in an English sentence opened a block, and the file ended unterminated -- 72 findings on mosesrules.prg alone, none of them about code that runs.
* A constant condition is evaluated now and the body behind a false one is taken as text. `#IF .T.` and a condition on a #DEFINE are untouched: only `.F.` and `0` are settled here, because anything else is a name only the preprocessor can resolve.
#IF .F.

	*Miscellaneous rules and tools
		-for grids and lists in the resize event use moses.lst_colwidths(thisform.lst1)
			if it is a list, make the header label objects have object names of c_label1, c_label2

#ENDIF

LOCAL m.lReady
m.lReady = .t.

RETURN m.lReady
