* Macro substitution. Table names, alias prefixes, index keys and whole field lists are held in
* variables and expanded at run time, which is how one routine drives every table in the schema.
LPARAMETERS m.cTable, m.cKeyExpr, m.cTagName

PRIVATE m.d_files, m.d_dirs
LOCAL m.cFieldName, m.cWhere, m.cRecordObj

m.d_files	= "files"
m.d_dirs	= "dirs"
m.cWhere	= "!inactive"

* A macro on the alias side of the arrow, and on both sides at once.
IF &d_files->datalog
	m.cFieldName = ALLTRIM(&d_files->mosidtag)
ENDIF
m.cRecordObj = &d_dirs->&cFieldName

* Macros in the places a name is expected.
USE (m.cTable) AGAIN IN 0
SELECT (m.cTable)
INDEX ON &cKeyExpr TAG &cTagName
SET RELATION TO &cKeyExpr INTO files
SET FILTER TO &cWhere

* A macro standing in for a whole select list, and one inside a WHERE.
SELECT &cKeyExpr AS recordkey, mosnum ;
	FROM (m.cTable) ;
	WHERE &cWhere ;
	INTO CURSOR curKeys NOFILTER

* Assigning through a macro-named property, which is how a scattered record object is patched up.
LOCAL m.RecObj
m.RecObj = CREATEOBJECT("Empty")
ADDPROPERTY(m.RecObj, m.cFieldName, "")
m.RecObj.&cFieldName = ALLTRIM(curKeys.recordkey)

SET FILTER TO .t.
SET RELATION TO

RETURN m.RecObj
