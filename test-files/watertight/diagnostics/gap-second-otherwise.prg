* Two OTHERWISE branches in one DO CASE. VFP takes the first and the second is dead, so the construct is harmless where it occurs -- but the grammar allows only one, so the DO CASE is rejected and the file fails on the ENDCASE. Reading it costs nothing; refusing it costs the file.
* Once it parses, the second branch is worth reporting: it is unreachable, and a reader who adds code under it is writing code that never runs.
LPARAMETERS m.oMessage

DO CASE
	CASE !ISNULL(m.oMessage.GetProperty("ConnectHandshake"))
		_screen.AddProperty("ChildPipeConnected", .t.)
	CASE !ISNULL(m.oMessage.GetProperty("Message"))
		=MESSAGEBOX(m.oMessage.GetProperty("Message"))
	OTHERWISE
		_screen.PipeMessages.Add(m.oMessage)
	OTHERWISE
ENDCASE

RETURN .t.
