/*
 * An excerpt of the Windows header the framework #INCLUDEs for its constants. It is C, it is never compiled as FoxPro, and the linter has no business reading its lines as statements.
 */
#ifndef _WINUSER_
#define _WINUSER_

#define WM_PAINT            0x000F
#define WM_CLOSE            0x0010
#define MB_OK               0x00000000
#define MB_ICONSTOP         0x00000010

typedef struct tagPOINT {
    LONG  x;
    LONG  y;
} POINT, *PPOINT, NEAR *NPPOINT;

WINUSERAPI int WINAPI MessageBoxA(
    HWND hWnd,
    LPCSTR lpText,
    LPCSTR lpCaption,
    UINT uType);

#if !defined(NOWINSTYLES)
#define WS_OVERLAPPED       0x00000000L
#endif

#endif /* _WINUSER_ */
