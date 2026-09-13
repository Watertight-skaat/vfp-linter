* `#IF .F.` fencing off a block that is not code at all. FoxPro's preprocessor evaluates the condition and never compiles the body, which is how mosesrules.prg keeps a page of house rules at the top of a .prg and how unfinished work is parked without commenting every line.
* The linter reads the body anyway, so the prose is parsed as statements. Any `if` in an English sentence opens a block, and the file ends unterminated -- 72 findings on mosesrules.prg alone, none of them about code that runs.
* The fix is to evaluate a constant condition and skip the body, not to parse it more cleverly. `#IF .T.` and a condition on a #DEFINE both have to keep working.
#IF .F.

	*Miscellaneous rules and tools
		-for grids and lists in the resize event use moses.lst_colwidths(thisform.lst1)
			if it is a list, make the header label objects have object names of c_label1, c_label2

#ENDIF

LOCAL m.lReady
m.lReady = .t.

RETURN m.lReady
