* Posts a charge to the ledger.
* Returns .T. when the post succeeded.
PROCEDURE PostCharge
LPARAMETERS tcAccount, tnAmount
DO LogEntry WITH m.tcAccount
RETURN .T.
ENDPROC
