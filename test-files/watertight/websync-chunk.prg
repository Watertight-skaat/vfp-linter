* Pushing changed rows to the web service. The table name, the id expression and the field list all
* arrive as text from the schema metadata, so the query that gathers a chunk is assembled with macros
* and every row is sent inside its own TRY so one bad row cannot stop the batch.
LPARAMETERS m.cTableName, m.cMosid, m.nChunkSize, m.lNotLogged

LOCAL m.cIdExpression, m.cFieldsExpression, m.cDatalogAlias, m.cWebSyncAlias ;
	, m.nSkipped, m.nAdded, m.oWebObj, m.oResponse, m.lRowFailed
LOCAL loRowChanges AS Collection
LOCAL oLastWebSyncError AS Exception, loPropertySetException AS Exception

m.cIdExpression		= moses.mosidexpr(m.cTableName)
m.cFieldsExpression	= m.cTableName + ".*"
m.cDatalogAlias		= "Datalog_" + ALLTRIM(m.cTableName)
m.cWebSyncAlias		= "Websync_" + ALLTRIM(m.cTableName)
m.nSkipped			= 0
m.nAdded			= 0

IF !USED("WebSyncRecordChunk")
	CREATE CURSOR WebSyncRecordChunk (mosid c(LEN(m.cMosid)), NewRow l(1))
	INSERT INTO WebSyncRecordChunk (mosid, NewRow) VALUES (m.cMosid, .t.)
ENDIF

SELECT &cFieldsExpression, NewRow ;
	FROM (ADDBS(m.m_tpath) + ADDBS(ALLTRIM(files.filepath)) + m.cTableName), WebSyncRecordChunk ;
	WHERE &cIdExpression = WebSyncRecordChunk.mosid ;
	INTO CURSOR datalogWebP READWRITE NOFILTER

SELECT TOP m.nChunkSize log_id, log_mosnum AS logmos, log_stat ;
	FROM (m.cDatalogAlias) ;
	WHERE log_stat = 1 ;
	GROUP BY 1, 2, 3 ;
	ORDER BY 1 ;
	INTO CURSOR curChunk NOFILTER

SET RELATION TO log_mosnum INTO datalogWebP

loRowChanges = CREATEOBJECT("Collection")

SCAN
	m.lRowFailed = .f.
	TRY
		m.oWebObj = _screen.WtMobileHelper.CreateJSObject()
		_screen.WtMobileHelper.SetProperty(m.oWebObj, "RowId", ALLTRIM(curChunk.logmos))
		loRowChanges.Add(m.oWebObj, ALLTRIM(curChunk.logmos))
		m.nAdded = m.nAdded + 1
	CATCH TO loCollAddException
		m.lRowFailed = .t.
		m.nSkipped = m.nSkipped + 1
		DO LogWebsyncRowError WITH m.cTableName, curChunk.logmos, loCollAddException.Message
	ENDTRY

	IF m.lRowFailed
		LOOP
	ENDIF
ENDSCAN

SET RELATION TO
USE IN curChunk

RETURN m.nAdded
