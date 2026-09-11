* A ledger of constructs the grammar still does not read, recorded so the gap is stated out loud rather than passing silently. When one of these is implemented, `bun run test:update` removes its line and the diff shows coverage improving.
* Everything here announces itself as unsupported-syntax, which is the acceptable failure mode: a gap that reports itself costs one statement, while a statement that misparses into a valid tree costs every rule downstream and reports nothing at all.
* A line that leaves a *partial* node behind is marked as such: the statement before the remainder parsed into something, so half of it is already readable and only the tail announces itself.

* DEFINE CLASS member declarations, the largest group left. On a property each costs only its own line, and the class around it still reads.
DEFINE CLASS Poster AS Custom
	PROTECTED cName, nAge
	HIDDEN lDirty
	IMPLEMENTS IPoster IN "poster.dll"
	ADD OBJECT cmdPost AS CommandButton WITH Caption = "Post", Top = 1
ENDDEFINE

* The output commands. ?? prints without the leading newline, and a backslash line is TEXTMERGE output written one line at a time.
?? lcMessage
\ Dear <<m.cName>>,
\\ and the rest of it.

* Memo fields read from and written to a text file. These sit beside APPEND FROM and COPY TO, which are read.
APPEND MEMO notes FROM notes.txt OVERWRITE
COPY MEMO notes TO notes.txt

* Menu handlers. ON SELECTION runs a command and is read; ON PAD and ON BAR open a submenu instead, and are not.
ON PAD pFile OF mMain ACTIVATE POPUP pFileMenu
ON BAR 1 OF pFileMenu ACTIVATE POPUP pSubMenu

* SET commands whose argument is a file path, or that carry a second clause of their own. The bare form reads them, so each leaves a SetCommand behind and only the tail is lost. (partial)
SET DEFAULT TO c:\temp
SET PRINTER TO FILE output.txt
SET TEXTMERGE ON DELIMITERS TO "<<", ">>"

* A quoted class library in an AS ... OF clause. The bare name reads, so the declaration is already in the symbol table and only the library is lost. (partial)
LOCAL loPoster AS Poster OF "poster.vcx"
? m.loPoster

* The rest, each costing only its own statement.
CANCEL
READ EVENTS
COMPILE program.prg
BUILD APP myapp FROM myproject

* The one gap that costs more than its own line, and so the one to do first: the same access words on a method rather than a property leave the whole class unreadable, and every method in it leaves the outline and the symbol table with it. Kept second to last, because the wreckage runs past the end of the block.
DEFINE CLASS Later AS Custom
	PROTECTED PROCEDURE Post
	ENDPROC
ENDDEFINE

* Kept last: RETURN parses on its own, so the tail reads as a statement after it and reports as unreachable as well. (partial)
RETURN TO MASTER
