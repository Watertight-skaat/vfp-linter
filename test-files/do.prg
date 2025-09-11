DO testdir\testprg.app
DO testprg
Do m.testvar + "dirname\" + "filename.prg"
Do "dirname\" + m.testvar + "filename.prg"
DO (STRTRAN(m.tmpfile,"","")) WITH m.obj1, m.obj2, m.obj3
DO prgname WITH DATETIME() - (60 * 60 * 2) IN prgplace