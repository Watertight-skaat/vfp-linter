* String work in the old style: single-letter variables, recursion, and character maps built by
* concatenating quoted literals of both kinds so the quote characters can be embedded.
FUNCTION PictChop
	PARAMETERS f_string
	PRIVATE l_count, g_len, f_len, q_string, s_count

	STORE '' TO q_string
	STORE 1 TO s_count, l_count
	g_len = LEN(f_string)

	DO WHILE s_count <= g_len
		IF SUBSTR(f_string, s_count, 1) $ "!AXN9#"
			q_string = q_string + SUBSTR(f_string, s_count, 1)
			l_count = l_count + 1
		ENDIF
		s_count = s_count + 1
	ENDDO

	RETURN q_string
ENDFUNC


FUNCTION Scramble
	PARAMETERS C, R, F, B
	PRIVATE S1, S2, L, T, I, N, S

	S1 = '"' + "'!@#$%^&*()_+abcdefghijklmnopqrs:{}|~wxyz"
	S2 = "ZXCVBNM,./';LKJHGFDSAQWERT YUIOP[]\=-098765" + '"'
	L = LEN(C)
	T = L + IIF(INT(L / 2) = L / 2, 2, 1)
	I = 0
	S = ""

	DO WHILE I < L
		I = I + 1
		N = IIF(INT(I / 2) = I / 2, T - I, I)
		F = SUBSTR(F, 2) + SUBSTR(F, 1, 1)
		S = S + IIF(F = "1", CHRTRAN(SUBSTR(C, N, 1), S1, S2) ;
					   , IIF(F = "2", CHRTRAN(SUBSTR(C, N, 1), S2, S1) ;
									, STUFF(SUBSTR(C, N, 1), 1, 1, " ")))
	ENDDO

	R = R - 1
	RETURN IIF(R < 1, S, Scramble(S, R, F, B))
ENDFUNC


FUNCTION NormalizePhone(m.pcPhoneNumberString AS Character) AS Character
	LOCAL m.lcSeparatorChars, m.lcDigits, m.lcOut

	m.lcSeparatorChars	= " -.()/"
	m.lcDigits			= CHRTRAN(ALLTRIM(m.pcPhoneNumberString), m.lcSeparatorChars, "")
	m.lcOut				= ""

	DO CASE
		CASE LEN(m.lcDigits) = 10
			m.lcOut = "(" + LEFT(m.lcDigits, 3) + ") " + SUBSTR(m.lcDigits, 4, 3) + "-" + RIGHT(m.lcDigits, 4)
		CASE LEN(m.lcDigits) = 7
			m.lcOut = LEFT(m.lcDigits, 3) + "-" + RIGHT(m.lcDigits, 4)
		CASE LEN(m.lcDigits) = 11 .and. LEFT(m.lcDigits, 1) == "1"
			m.lcOut = NormalizePhone(SUBSTR(m.lcDigits, 2))
		OTHERWISE
			m.lcOut = ALLTRIM(m.pcPhoneNumberString)
	ENDCASE

	RETURN m.lcOut
ENDFUNC
