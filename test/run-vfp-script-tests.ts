// What resources/vfp-open.vbs does with a request, run rather than read.
//
// The script decides two things -- which Visual FoxPro to talk to, and whether that one is told about the workspace -- and the second is the one worth pinning down: a session the developer started is theirs and is sent nothing, while one started here, or one they have just agreed to set up, is given the path first. Getting that backwards either reaches into somebody's IDE uninvited or opens a designer that cannot find a class, and neither is visible from the editor.
//
// So the two seams are replaced with stubs -- the VFP it attaches to, and the shell it would start one with -- and everything between them is the real script.

import { execFileSync } from 'child_process';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { check, report } from './check.js';

// cscript is Windows', as is Visual FoxPro. On the Linux runner there is nothing here to run.
if (process.platform !== 'win32') {
	console.log('VFP launcher checks: skipped, Windows only');
	process.exit(0);
}

const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'vfplint-'));
// Repo-relative, as the other suites read their fixtures: they are all run from the root.
const source = fs.readFileSync('./resources/vfp-open.vbs', 'utf8');

// The stub answers Eval with whatever the expression asks for: a class count, a class name, or the environment probe.
const stubs = `
Dim attempts
attempts = 0

Function Attach()
	attempts = attempts + 1
	If attempts <= CInt(shell.ExpandEnvironmentStrings("%VFPLINT_MISSES%")) Then
		Set Attach = Nothing
	Else
		Set Attach = New FakeVfp
	End If
End Function

Class FakeShell
	Public CurrentDirectory
	Public Sub Run(command, style, waitFor)
		WScript.Echo "start> " & command
	End Sub
	Public Sub AppActivate(title)
	End Sub
	Public Function ExpandEnvironmentStrings(name)
		ExpandEnvironmentStrings = shellReal.ExpandEnvironmentStrings(name)
	End Function
End Class

Class FakeVfp
	Public Visible
	Public Property Get Caption()
		Caption = "Microsoft Visual FoxPro Advanced"
	End Property
	Public Sub DoCmd(command)
		WScript.Echo "cmd> " & command
	End Sub
	Public Function Eval(expression)
		If InStr(expression, "AVCXCLASSES") = 1 Then
			Eval = 2
		ElseIf InStr(expression, "__vfplint_classes[") = 1 Then
			Eval = "someclass"
		Else
			Eval = "C:\\PF\\VFPA\\|"
		End If
	End Function
End Class
`;

const attach = source.indexOf('Function Attach()');
const script = path.join(directory, 'stubbed.vbs');
fs.writeFileSync(script, [
	source.slice(0, attach).replace('Set shell = CreateObject("WScript.Shell")', 'Dim shellReal\nSet shellReal = CreateObject("WScript.Shell")\nSet shell = New FakeShell'),
	stubs,
	source.slice(source.indexOf('End Function', attach) + 'End Function'.length)
].join(''));

/** The lines the script writes for one request, or what it failed with. `misses` is how many times Attach answers that no Visual FoxPro is running. */
function run(fields: string[], misses = 0): string[] {
	const file = path.join(directory, `request-${Math.random().toString(36).slice(2)}.txt`);
	fs.writeFileSync(file, '\ufeff' + fields.join('\r\n') + '\r\n', 'utf16le');
	const environment = { ...process.env, VFPLINT_MISSES: String(misses) };
	try {
		// stderr is piped rather than inherited: a refused request is one of the things under test, not noise for the console.
		return execFileSync('cscript', ['//nologo', script, file], { encoding: 'latin1', env: environment, stdio: ['ignore', 'pipe', 'pipe'] }).trim().split(/\r?\n/);
	} catch (error) {
		return [(error as { stderr?: string }).stderr?.trim() ?? 'the script wrote nothing'];
	}
}

const startup = ['startup=SET DEFAULT TO ("W:\\Devstaging2")', 'startup=SET PATH TO "W:\\Devstaging2\\classes\\framework" ADDITIVE'];
const open = ['mode=run', 'payload=MODIFY FORM ("W:\\x.scx") NOWAIT', 'exe=C:\\PF\\VFPA\\VFPA.EXE', 'cwd=W:\\Devstaging2'];
const probe = ['mode=value', 'payload=SYS(5) + CURDIR() + "|" + SET("PATH")', 'exe=C:\\PF\\VFPA\\VFPA.EXE', 'cwd=W:\\Devstaging2'];

// The developer's own session: it already has their default directory and their SET PATH, and this is the one case where the script must say nothing but the command.
check('a session already running is sent the command and nothing else', run([...open, 'setup=0', ...startup]), [
	'attached',
	'cmd> MODIFY FORM ("W:\\x.scx") NOWAIT'
]);
// The same session after the developer has been asked whether to set it up and said yes.
check('and the path first when they have agreed to it', run([...open, 'setup=1', ...startup]), [
	'cmd> SET DEFAULT TO ("W:\\Devstaging2")',
	'cmd> SET PATH TO "W:\\Devstaging2\\classes\\framework" ADDITIVE',
	'attached',
	'cmd> MODIFY FORM ("W:\\x.scx") NOWAIT'
]);
// One started here knows nothing at all, so it is told whatever the request carries without anybody being asked.
check('one started here is always told where the tree is', run([...open, 'setup=0', ...startup], 1), [
	'start> "C:\\PF\\VFPA\\VFPA.EXE" -t',
	'cmd> SET DEFAULT TO ("W:\\Devstaging2")',
	'cmd> SET PATH TO "W:\\Devstaging2\\classes\\framework" ADDITIVE',
	'created',
	'cmd> MODIFY FORM ("W:\\x.scx") NOWAIT'
]);
// Which is what tells the editor it need not ask about the path at all: the probe answers `created`, and the editor takes that for an answer without putting a dialog in front of anybody.
check('and says so, so the editor knows not to ask', run([...probe, 'setup=0', ...startup], 1), [
	'start> "C:\\PF\\VFPA\\VFPA.EXE" -t',
	'cmd> SET DEFAULT TO ("W:\\Devstaging2")',
	'cmd> SET PATH TO "W:\\Devstaging2\\classes\\framework" ADDITIVE',
	'created',
	'C:\\PF\\VFPA\\|'
]);

// The probe itself: one expression in, one line back, and not a command sent to anything.
check('the probe answers with what the expression came to', run([...probe, 'setup=0', ...startup]), ['attached', 'C:\\PF\\VFPA\\|']);

check('a request carrying no command is refused', run(['mode=run', 'payload=', 'exe=', 'cwd=']), ['error: the request carried no command']);

fs.rmSync(directory, { recursive: true, force: true });
report('VFP launcher checks');
