* A routine named after a command word. `PROCEDURE Error` is the standard VFP error method and appears on every class in the tree; `PROCEDURE declare` and `PROCEDURE use` are how the Windows-API wrappers name their setup and teardown. The grammar used to read the name position as a command word and reject the header.
* Rejecting a header inside DEFINE CLASS cost the whole class: its ENDPROC was left orphaned, and the ENDDEFINE with it, so the class stopped being indexed and every method in it disappeared from Go to Definition -- and promoting the file to tier 2 therefore *removed* symbols the header scan had found.
* Some keywords already worked in this position -- `PROCEDURE error` parses -- which is what made the gap hard to spot by reading. The name position now takes any name, and this file is one the two tiers are held to each other over.
DEFINE CLASS Crypto AS Session

	PROTECTED PROCEDURE declare
		DECLARE INTEGER CryptAcquireContext IN advapi32
	ENDPROC

	PROCEDURE use
		RETURN .t.
	ENDPROC

ENDDEFINE
