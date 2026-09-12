LOCAL lcProc
lcProc = "LogEntry"

DO LogEntry WITH "fine"
DO LogEntry WITH "one", "two"

DO helper.prg WITH 1
DO helper.prg WITH 1, 2

* Named at run time, so nothing can be concluded about what it takes.
DO &lcProc WITH 1, 2, 3

* A method is not a routine the index knows about.
? _screen.Resize(1, 2, 3)
