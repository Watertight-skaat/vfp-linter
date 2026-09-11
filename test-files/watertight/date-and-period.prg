* Billing periods and date arithmetic. Empty dates are {} rather than null, and the century setting
* is saved and restored around anything that formats a date into a key.
LPARAMETERS m.dStart, m.dThru, m.cPeriod

LOCAL m.cCentury, m.dPeriodEnd, m.nMonths, m.cKey, m.tStamp

m.cCentury = SET("CENTURY")
SET CENTURY ON

m.tStamp	= DATETIME()
m.dPeriodEnd = {}
m.nMonths	= 0

IF EMPTY(m.dStart)
	m.dStart = GOMONTH(DATE(), -1)
ENDIF

DO CASE
	CASE m.cPeriod = "M"
		m.nMonths = 1
	CASE m.cPeriod = "Q"
		m.nMonths = 3
	CASE m.cPeriod = "S"
		m.nMonths = 6
	CASE m.cPeriod = "A"
		m.nMonths = 12
	OTHERWISE
		m.nMonths = 1
ENDCASE

m.dPeriodEnd	= GOMONTH(m.dStart, m.nMonths) - 1
m.cKey			= DTOS(m.dStart) + DTOS(m.dPeriodEnd)

SELECT stnum, acctnum, SUM(amount) AS charged ;
	FROM charcred ;
	WHERE datechgd >= m.dStart .and. datechgd <= m.dPeriodEnd .and. datechgd # {} ;
	GROUP BY 1, 2 ;
	INTO CURSOR curPeriod NOFILTER

SELECT * FROM charcred WHERE datechgd = {^2024-01-15} INTO CURSOR curOneDay
SELECT * FROM charcred WHERE spec1dt >= {^2024-01-15 08:30:00} INTO CURSOR curAfter

IF m.cCentury = "OFF"
	SET CENTURY OFF
ENDIF

? TTOC(m.tStamp), DTOC(m.dPeriodEnd), m.cKey

RETURN m.dPeriodEnd
