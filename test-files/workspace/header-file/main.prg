* The constants of a header file are what #INCLUDE is for, so the .h stays in the index and the names it defines resolve from here.
#INCLUDE winuser.h

LOCAL lnAnswer
lnAnswer = MESSAGEBOX("Close?", MB_OK + MB_ICONSTOP, "Watertight")

RETURN lnAnswer
