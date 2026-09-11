* Screens are launched from metadata, so a .prg holds the logic and the form is a thin shell. Modal
* pickers return through TO; persistent instances are held by NAME.
LPARAMETERS m.ParentForm, m.cStnum, m.cAcctnum, m.nAddrnum, m.lNoDate

LOCAL m.cTempTable, m.SelectedItem, m.oCustomer

m.cTempTable = m_cpath + "\temp" + m.proc_inst + "\" + udalias("dbf")

SELECT prodmast.* ;
	FROM prodmast, partswap ;
	WHERE partswap.stnum + partswap.part_no = prodmast.stnum + prodmast.part_no ;
		.and. prodmast.acti = 1 ;
	INTO TABLE (m.cTempTable)
USE

DO FORM datasrch WITH m.cTempTable + " @PRODMAST", "", .f., .f., .t., .t., '.t.', "Select an item:" TO m.SelectedItem
ERASE (m.cTempTable + ".dbf")
ERASE (m.cTempTable + ".cdx")

IF EMPTY(m.SelectedItem)
	RETURN .f.
ENDIF

DO FORM custedit NAME oCustomerForm LINKED
DO FORM branchpick WITH m.cStnum TO m.cStnum NOSHOW

WITH m.ParentForm
	.mos_item_locktype	= "E"
	.mos_item_cursor	= "trancur"
	.Caption			= "Customer " + ALLTRIM(m.cAcctnum)
	.grdLines.RecordSource = "trancur"
	.SetAll("Enabled", .t., "txt")
	.Refresh()
ENDWITH

DO TrancurUpdateDesc WITH m.ParentForm, m.lNoDate
DO TrancurApplyDefaultPricing WITH m.cStnum + m.cAcctnum, m.nAddrnum, trancur.taxarea, trancur.taxexempt

RETURN .t.


DEFINE CLASS CustomerEditor AS Custom

	cCaption	= "Customer"
	nAddrnum	= 0
	oParent		= .null.

	PROCEDURE Init
		LPARAMETERS m.cStnum, m.cAcctnum
		DODEFAULT(m.cStnum)
		THIS.cCaption = "Customer " + ALLTRIM(m.cAcctnum)
		RETURN .t.
	ENDPROC

	PROCEDURE Save
		LOCAL m.lOk
		m.lOk = CustomerEditor::Validate()
		IF !m.lOk
			RETURN .f.
		ENDIF
		RETURN .t.
	ENDPROC

	FUNCTION Validate()
		RETURN !EMPTY(THIS.cCaption)
	ENDFUNC

ENDDEFINE
