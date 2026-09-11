* A ledger of constructs the grammar still does not read, recorded so the gap is stated out loud
* rather than passing silently. When one of these is implemented, `bun run test:update` removes its
* line and the diff shows coverage improving.
SCATTER MEMVAR
GATHER MEMVAR
MODIFY STRUCTURE
COUNT TO lnN
AVERAGE nAmount TO lnAvg
FLUSH
PUSH KEY
POP KEY
EXTERNAL ARRAY laX
RUN /N notepad.exe

* Found in the Watertight corpus (see test-files/watertight/). The constructs that need surrounding
* code to show what they cost are fixtures of their own under test-files/watertight/diagnostics/.
PARAM MNM, ARR, FLD
PARAMETER cName
SCATTER NAME m.RecObj MEMO
SCATTER MEMO NAME m.OldValues
GATHER NAME m.RecObj MEMO
FLUSH FORCE
MD datalog
REINDEX
ALTER TABLE custinfo ADD COLUMN websync l(1)
COUNT FOR invbal > 0 TO lnOverdue
CONTINUE
NODEFAULT
WAIT WINDOW "Resizing graphs..." NOWAIT NOCLEAR
SET FILTER TO
