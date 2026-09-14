' Opens a file in the designer of the Visual FoxPro the developer already has open, or asks a running one what classes a library holds.
'
' Windows Script Host is here only because COM needs a host and this one is always installed. Every command sent to VFP is built and tested in client/src/vfp.ts and server/src/vfp.ts; this script decides nothing except which VFP to talk to.
'
' Driven by a request file rather than by arguments: the fields carry whole paths and whole VFP commands, and one field per line leaves the caller no quoting rules to get wrong.

Option Explicit

' How long a Visual FoxPro started here is given to register itself before we give up on it.
Const StartSeconds = 30

Dim fso, shell
Set fso = CreateObject("Scripting.FileSystemObject")
Set shell = CreateObject("WScript.Shell")

If WScript.Arguments.Count < 1 Then Fail "no request file was given"

Dim mode, payload, exePath, cwd, startupText
mode = "run"
payload = ""
exePath = ""
cwd = ""
startupText = ""

Dim stream, line, at, key, value
Set stream = fso.OpenTextFile(WScript.Arguments(0), 1, False, -1)
Do Until stream.AtEndOfStream
	line = stream.ReadLine
	at = InStr(line, "=")
	If at > 1 Then
		key = Left(line, at - 1)
		value = Mid(line, at + 1)
		Select Case key
			Case "mode" : mode = value
			Case "payload" : payload = value
			Case "exe" : exePath = value
			Case "cwd" : cwd = value
			Case "startup" : startupText = startupText & value & vbLf
		End Select
	End If
Loop
stream.Close

If Len(payload) = 0 Then Fail "the request carried no command"

' The developer's own session first: it is the one with their default directory and their SET PATH, and a designer opened anywhere else stops on a modal Locate dialog it cannot answer.
Dim vfp, created
Set vfp = Attach()
created = False

If vfp Is Nothing Then
	If Len(exePath) = 0 Then Fail "Visual FoxPro is not running and none could be found to start. Set foxpro.vfp.path to the VFPA.EXE or vfp9.exe to use."
	If Len(cwd) > 0 Then shell.CurrentDirectory = cwd
	shell.Run """" & exePath & """ -t", 1, False
	created = True

	Dim waited
	waited = 0
	Do While waited < StartSeconds
		WScript.Sleep 500
		waited = waited + 0.5
		Set vfp = Attach()
		If Not (vfp Is Nothing) Then Exit Do
	Loop
	If vfp Is Nothing Then Fail "Visual FoxPro was started but never answered."

	' Only an instance started here is told anything about the workspace. One the developer already had open is theirs, and reaching into it to change the path would be rude and would outlast this call.
	Dim commands, i
	commands = Split(startupText, vbLf)
	For i = 0 To UBound(commands)
		If Len(commands(i)) > 0 Then Send commands(i)
	Next
End If

vfp.Visible = True
If created Then WScript.Echo "created" Else WScript.Echo "attached"

If mode = "classes" Then
	Dim count, k
	Send "PUBLIC ARRAY __vfplint_classes[1]"
	On Error Resume Next
	count = vfp.Eval(payload)
	If Err.Number <> 0 Then Fail "Visual FoxPro could not read the class library: " & Err.Description
	On Error GoTo 0
	For k = 1 To count
		WScript.Echo Trim(vfp.Eval("__vfplint_classes[" & k & ",1]"))
	Next
	Send "RELEASE __vfplint_classes"
Else
	Send payload
	' Forward, or the designer opens behind the editor and nothing looks like it happened.
	shell.AppActivate vfp.Caption
End If

WScript.Quit 0

' The version-independent name first, which is whichever install registered itself last; then each version by name, so a VFP 9 running beside an installed VFP Advanced is still found.
Function Attach()
	Dim ids, i, candidate
	ids = Array("VisualFoxpro.Application", "VisualFoxpro.Application.a", "VisualFoxpro.Application.9")
	Set Attach = Nothing
	For i = 0 To UBound(ids)
		On Error Resume Next
		Set candidate = GetObject(, ids(i))
		If Err.Number = 0 Then
			On Error GoTo 0
			Set Attach = candidate
			Exit Function
		End If
		Err.Clear
		On Error GoTo 0
	Next
End Function

Sub Send(command)
	On Error Resume Next
	vfp.DoCmd command
	If Err.Number <> 0 Then Fail "Visual FoxPro refused " & command & ": " & Err.Description
	On Error GoTo 0
End Sub

Sub Fail(message)
	WScript.StdErr.WriteLine "error: " & message
	WScript.Quit 1
End Sub
