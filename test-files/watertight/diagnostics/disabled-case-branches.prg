* CASE .f. is how a branch is switched off here without deleting it, so a DO CASE can hold several
* of them at once. They are duplicates by structure, which is what the duplicate-case rule reports.
LPARAMETERS m.oHttpClient, m.oBranchRecord

DO CASE
	CASE .f.	&& bad API key
		m.oHttpClient.SetBearer("INVALID")
	CASE .f.	&& timeout
		m.oBranchRecord.webDomain = "https://example.invalid:9999/"
	CASE .f.	&& actively refused
		m.oBranchRecord.webDomain = "https://localhost:5000/"
	OTHERWISE
		m.oHttpClient.SetBearer(ALLTRIM(m.oBranchRecord.apiKey))
ENDCASE

RETURN .t.
