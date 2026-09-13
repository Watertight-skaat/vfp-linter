* A routine named after a command word. `PROCEDURE Error` is the standard VFP error method and appears on every class in the tree; `PROCEDURE declare` and `PROCEDURE use` are how the Windows-API wrappers name their setup and teardown. The grammar reads the name position as a command word and rejects the header.
* Rejecting a header inside DEFINE CLASS costs the whole class: its ENDPROC is left orphaned, and the ENDDEFINE with it, so the class stops being indexed and every method in it disappears from Go to Definition.
* Some keywords already work in this position -- `PROCEDURE error` parses -- which is what makes the gap hard to spot by reading.
DEFINE CLASS Crypto AS Session

	PROTECTED PROCEDURE declare
		DECLARE INTEGER CryptAcquireContext IN advapi32
	ENDPROC

	PROCEDURE use
		RETURN .t.
	ENDPROC

ENDDEFINE
