* The outbound message queue. Items are objects in a Collection rather than rows, so this is the one
* corner of the app written with FOR EACH and typed locals.
LPARAMETERS m.cStnum, m.nMaxBatch

LOCAL m.oQueue, m.oMessage, m.oAttachment, m.nSent, m.nFailed, m.cStatus
LOCAL loAttachments AS Collection
LOCAL loSendException AS Exception

m.oQueue	= _screen.MosDA.MailQueue.Pending(m.cStnum, m.nMaxBatch)
m.nSent		= 0
m.nFailed	= 0

IF m.oQueue.Count = 0
	RETURN 0
ENDIF

FOR EACH m.oMessage AS QueuedMessage IN m.oQueue
	IF m.oMessage.Status # MSG_SEND_NOW
		LOOP
	ENDIF

	loAttachments = m.oMessage.Attachments
	FOR EACH m.oAttachment IN loAttachments FOXOBJECT
		IF !FILE(m.oAttachment.FullPath)
			m.oMessage.AddWarning("Missing attachment: " + m.oAttachment.FileName)
		ENDIF
	ENDFOR

	TRY
		m.cStatus	= m.oMessage.Send()
		m.nSent		= m.nSent + 1
		INSERT INTO maillog (mosnum, stnum, sentwhen, recipient, subject, status) ;
			VALUES (m_mnum("maillog"), PADR(m.cStnum, 5), DATETIME(), ;
					PADR(m.oMessage.Recipient, 80), PADR(m.oMessage.Subject, 120), PADR(m.cStatus, 20))
	CATCH TO loSendException
		m.nFailed = m.nFailed + 1
		m.oMessage.LastError = loSendException.Message
		DO ReportException WITH loSendException, 2, .f., "send", .null.
	FINALLY
		m.oMessage.Release()
	ENDTRY
ENDFOR

=FlushWrapper(.f., "maillog")
WAIT WINDOW NOWAIT ALLTRIM(STR(m.nSent)) + " sent, " + ALLTRIM(STR(m.nFailed)) + " failed"

RETURN m.nSent
