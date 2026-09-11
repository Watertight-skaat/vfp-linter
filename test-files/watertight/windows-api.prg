* The Windows API surface, seeded from community code and left in its original Hungarian style. The
* declarations wrap across continuation lines and pass buffers by reference.
#DEFINE CRLF				CHR(13) + CHR(10)
#DEFINE MAX_PATH			260
#DEFINE PROCESS_QUERY_INFO	1040
#INCLUDE MosWindowsAPI.h

DECLARE LONG timeGetTime IN winmm.dll
DECLARE INTEGER GetLastError IN kernel32
DECLARE INTEGER IsUserAnAdmin IN shell32
DECLARE INTEGER GetAsyncKeyState IN user32 INTEGER vKey

DECLARE SHORT PostMessage IN user32 ;
	INTEGER hWnd, INTEGER Msg, INTEGER wParam, INTEGER lParam

DECLARE LONG MoveFile IN WIN32API STRING lpExistingFileName, STRING lpNewFileName

DECLARE INTEGER GetModuleFileName ;
	IN kernel32.DLL ;
		INTEGER hModule, STRING @ lpFileName, INTEGER nSize

DECLARE INTEGER EnumProcesses IN psapi ;
	STRING @lpidProcess, INTEGER cb, INTEGER @cbNeeded

DECLARE INTEGER OpenProcess IN kernel32 ;
	INTEGER dwDesiredAccess, INTEGER bInheritHandle, ;
	INTEGER dwProcId


FUNCTION ModulePath(tnProcessId AS Integer) AS Character
	LOCAL lcBuffer, lnHandle, lnLength, lcResult

	lcBuffer	= SPACE(MAX_PATH)
	lnHandle	= OpenProcess(PROCESS_QUERY_INFO, 0, tnProcessId)
	IF lnHandle = 0
		RETURN ""
	ENDIF

	lnLength	= GetModuleFileName(lnHandle, @lcBuffer, MAX_PATH)
	lcResult	= IIF(lnLength > 0, LEFT(lcBuffer, lnLength), "")
	RETURN lcResult
ENDFUNC


#IF .f.
	* Left behind from the port; kept so the original call sequence can still be read.
	DECLARE INTEGER AttachThreadInput IN user32 INTEGER idAttach, INTEGER idAttachTo, INTEGER fAttach
#ENDIF
