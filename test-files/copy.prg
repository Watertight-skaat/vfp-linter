COPY FILE c:\dirname\filename.app to dirname\filename.app
COPY FILE (m.pathname + "dirname\" + "filename.dll") TO (FuncName(sys2004()) + "filename.dll")

COPY TO ("test\" + m.dirname + "\test.dbf")

RENAME old.dbf TO new.dbf
RENAME TABLE oldname TO newname
RENAME VIEW oldview TO newview
RENAME CONNECTION oldconn TO newconn
RENAME CLASS poster OF posters.vcx TO banner
RENAME CLASS (lcOld) OF (lcLib) TO (lcNew)
