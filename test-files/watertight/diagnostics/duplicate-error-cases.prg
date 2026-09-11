* A long DO CASE over vendor return codes where a second block of codes was pasted in below the
* first. The repeated numbers can never be reached, so the messages in the second block never show.
LPARAMETERS m.nReturnCode

DO CASE
	CASE m.nReturnCode = 20281
		RETURN "Invalid part index."
	CASE m.nReturnCode = 20282
		RETURN "Unknown MIME type."
	CASE m.nReturnCode = 20283
		RETURN "No MIME boundary found."

	* TLS errors, pasted in from the other component's documentation.
	CASE m.nReturnCode = 20281
		RETURN "Error verifying certificate."
	CASE m.nReturnCode = 20282
		RETURN "Could not find client certificate."
	CASE m.nReturnCode = 20283
		RETURN "Could not find server certificate."

	OTHERWISE
		RETURN "Unknown error " + TRANSFORM(m.nReturnCode)
ENDCASE
