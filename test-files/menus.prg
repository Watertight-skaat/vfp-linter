* The screen and menu commands. A 30-year-old application carries a lot of them, and none of them touches a table or a variable -- so what matters here is only that they are recognised, which is what stops the catch-all reporting a whole menu definition as unsupported. This is a top-level fixture, so an `.expected` file appearing beside it means one of them regressed.
LOCAL llLocked
llLocked = .F.

DEFINE WINDOW wOutput FROM 1, 1 TO 20, 60 TITLE "Output" CLOSE FLOAT GROW
DEFINE MENU mMain BAR AT LINE 1
DEFINE PAD pFile OF mMain PROMPT "\<File" KEY ALT+F, ""
DEFINE POPUP pFileMenu MARGIN RELATIVE SHADOW
DEFINE BAR 1 OF pFileMenu PROMPT "\<Open"
DEFINE BAR 2 OF pFileMenu PROMPT "\<Close"

ON SELECTION PAD pFile OF mMain ACTIVATE POPUP pFileMenu
ON SELECTION BAR 1 OF pFileMenu DO OpenFile
ON SELECTION BAR 2 OF pFileMenu DO CloseFile
ON SELECTION MENU mMain DO Dispatch
ON SELECTION POPUP pFileMenu DO Handler

SET SKIP OF BAR 2 OF pFileMenu m.llLocked
SET SKIP OF PAD pFile OF mMain m.llLocked
SET SKIP OF POPUP pFileMenu .F.

SET MARK OF BAR 1 OF pFileMenu TO .T.
SET MARK OF PAD pFile OF mMain TO m.llLocked
SET MARK OF MENU mMain TO .F.
SET MARK OF POPUP pFileMenu TO .T.
* The other SET MARK, which sets the date delimiter and has nothing to do with menus.
SET MARK TO "/"

ACTIVATE SCREEN
ACTIVATE WINDOW wOutput
ACTIVATE MENU mMain
ACTIVATE POPUP pFileMenu
MOVE WINDOW wOutput TO 2, 2
SIZE WINDOW wOutput TO 24, 70
ZOOM WINDOW wOutput MAX
HIDE WINDOW wOutput
SHOW WINDOW wOutput
DEACTIVATE WINDOW wOutput
DEACTIVATE MENU mMain

PROCEDURE OpenFile
	? "open"
ENDPROC

PROCEDURE CloseFile
	? "close"
ENDPROC

PROCEDURE Dispatch
	? "dispatch"
ENDPROC

PROCEDURE Handler
	? "handler"
ENDPROC
