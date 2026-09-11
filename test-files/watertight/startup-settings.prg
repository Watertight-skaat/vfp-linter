* Startup. The SET state the whole application depends on is established once, the class libraries
* are loaded, and the handful of globals every screen reads are created as PUBLICs.
PUBLIC m_tpath, m_cpath, m_apath, sysstnum, sysunum, sysuser, proc_inst
PUBLIC app_mobile, app_websync, app_quickbooks
PUBLIC ARRAY laBranches(1)

SET TALK OFF
SET ECHO OFF
SET SAFETY OFF
SET STATUS BAR OFF
SET EXCLUSIVE OFF
SET DELETED ON
SET CENTURY ON
SET MULTILOCKS ON
SET REPROCESS TO 0
SET TABLEVALIDATE TO 7
SET CPDIALOG OFF
SET EXACT OFF
SET NEAR OFF
SET NOTIFY OFF
SET DATE TO AMERICAN
SET SYSMENU OFF

m_apath		= ADDBS(SYS(5) + SYS(2003))
m_tpath		= ADDBS(m.m_apath + "aadbf")
m_cpath		= ADDBS(m.m_apath + "temp")
proc_inst	= RIGHT(SYS(2015), 6)
sysstnum	= "00001"
sysunum	 	= "0000"
sysuser		= "SYSTEM"

SET PATH TO (ADDBS(m.m_apath))
SET PATH TO (SET("path") + IIF(RIGHT(SET("path"), 1) = ";", "", ";") + ADDBS(m.m_cpath))

SET CLASSLIB TO controls ADDITIVE
SET CLASSLIB TO moses ADDITIVE
SET PROCEDURE TO wwDotNetBridge ADDITIVE
SET PROCEDURE TO transfer_procedures ADDITIVE

ON ERROR DO MosesErrorHandler WITH ERROR(), MESSAGE(), LINENO(), PROGRAM(), SYS(16), 0
ON SHUTDOWN DO endprog WITH .t.
ON ESCAPE

app_mobile		= .f.
app_websync		= .f.
app_quickbooks	= .f.

IF !USED("mastinfo")
	USE mastinfo IN 0
ENDIF
app_mobile		= mastinfo.usemobile
app_websync		= mastinfo.usewebsync
app_quickbooks	= mastinfo.useqb
USE IN mastinfo

_screen.AddObject("MosDA", "Session")
_screen.AddProperty("QuittingDueToError", .f.)
PUBLIC moses
moses = CREATEOBJECT("moseslib")

RETURN .t.
