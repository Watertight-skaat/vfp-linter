* `DO CASE <expression>`. VFP ignores whatever follows DO CASE -- the branches are still chosen by their own conditions -- and the old code writes the variable there as documentation of what is being switched on. The grammar requires DO CASE to stand alone, so the whole DO CASE is rejected and the file fails on the ENDCASE.
LPARAMETERS m_emu_type

LOCAL m_emulation

do case m_emu_type
	case m_emu_type = 1
		store "E" to m_emulation
	case m_emu_type = 2
		store "O" to m_emulation
	otherwise
		store "N" to m_emulation
endcase

RETURN m_emulation
