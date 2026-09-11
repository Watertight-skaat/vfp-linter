* Everything that leaves the desktop goes through the wwDotNetBridge wrapper. Long-lived clients are
* cached as _screen properties; data crosses as property bags built one field at a time.
LPARAMETERS m.cStnum, m.oRecord

LOCAL m.oClient, m.oDto, m.oResponse, m.lOk, m.cProperty

IF VARTYPE(_screen.WtMobileHelper) # "O"
	_screen.AddProperty("WtMobileHelper", CreateDotNetObject("Watertight.WebClients.MobileHelper"))
ENDIF

IF VARTYPE(EVALUATE("_screen.dbClient" + m.cStnum)) # "O"
	_screen.AddProperty("dbClient" + m.cStnum, CreateDotNetObject("Watertight.WebClients.HttpClientCom"))
ENDIF

m.oClient	= EVALUATE("_screen.dbClient" + m.cStnum)
m.oDto		= _screen.WtMobileHelper.CreateJSObject()

_screen.WtMobileHelper.SetProperty(m.oDto, "BranchId", m.cStnum)
_screen.WtMobileHelper.SetProperty(m.oDto, "CustomerId", ALLTRIM(m.oRecord.acctnum))
_screen.WtMobileHelper.SetProperty(m.oDto, "Balance", m.oRecord.invbal)
_screen.WtMobileHelper.SetProperty(m.oDto, "Posted", .t.)

FOR EACH m.cProperty IN m.oRecord.ChangedFields FOXOBJECT
	_screen.WtMobileHelper.SetProperty(m.oDto, m.cProperty, EVALUATE("m.oRecord." + m.cProperty))
ENDFOR

m.lOk = .f.
TRY
	m.oResponse	= m.oClient.PostJson("api/customers", m.oDto)
	m.lOk		= m.oResponse.Success
CATCH TO oPostException
	DO LogWebError WITH m.cStnum, oPostException.Message, oPostException.StackTrace
ENDTRY

RETURN m.lOk


DEFINE CLASS WebHeartbeat AS Session

	DataSession		= 2
	jsObj			= .null.
	LastPollTicks	= 0

	PROCEDURE Poll
		LOCAL m.nNow
		m.nNow = timeGetTime()
		IF m.nNow - THIS.LastPollTicks < 2000
			RETURN .f.
		ENDIF
		THIS.LastPollTicks	= m.nNow
		THIS.jsObj			= _screen.WtMobileHelper.CreateJSObject()
		RETURN .t.
	ENDPROC

ENDDEFINE
