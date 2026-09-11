* A #IF inside the branch of another #IF. The outer fence is read and a nested one is not, so the outer directive is rejected and the file fails on the first terminator past it. The dual-era code fences the DOS build off from the Visual build at the top of a routine and then fences individual statements again inside it, so the nesting is not exotic here.
#IF "VISUAL" $ UPPER(VERSION())
	do txt_launch with "00000127"
#ELSE
	if mfound
		private goon
		#IF "VISUAL" $ UPPER(VERSION())
		#ELSE
			define window _hidden from 11,39 to 12,40 none
			activate window _hidden
		#ENDIF
		store 13 to goon
		release goon
	endif
#ENDIF
