// Everything the extension says to a running Visual FoxPro: which files have a designer, the command that opens one, and the request file the launcher script reads.
// It is all text, and it is worth pinning down here because the failure mode is invisible from the editor -- VFP answers a malformed command with a modal dialog inside the developer's own IDE, and the extension sees only a call that never returns.

import { classListExpression, designerFile, designerFor, vfpCommandFor } from '../server/src/vfp.js';
import { environmentExpression, isSetUpFor, parseEnvironment, requestText, searchedDirectories, startupCommands, vfpExeCandidates } from '../client/src/vfp.js';
import { check, report } from './check.js';

// --- which files VFP has to open ---------------------------------------------

check('a form has a designer', designerFor('C:\\forms\\CUSTEDIT.SCX'), 'form');
check('so does a class library', designerFor('lib/QBINT.vcx'), 'class');
check('and a report, a label, a menu, a database and a project', ['x.frx', 'x.lbx', 'x.mnx', 'x.dbc', 'x.pjx'].map(designerFor), ['report', 'label', 'menu', 'database', 'project']);
// The one that matters: a .prg is text, and sending it to VFP would be worse than the editor it is already open in.
check('a program has none', designerFor('mainset.prg'), null);
check('nor has a table or a file with no extension', [designerFor('stmast.dbf'), designerFor('README')], [null, null]);

// The memo half of a form is beside it in the explorer and is the same document to a developer, so right-clicking it opens the form rather than reporting that a .sct has no designer.
check('a companion file names its designer', [designerFor('CUSTEDIT.sct'), designerFor('QBINT.vct'), designerFor('inv.frt')], ['form', 'class', 'report']);
check('and resolves to the file VFP opens', designerFile('W:\\forms\\CUSTEDIT.sct'), 'W:\\forms\\CUSTEDIT.scx');
check('a file that is already the one VFP opens is left alone', designerFile('W:\\forms\\CUSTEDIT.scx'), 'W:\\forms\\CUSTEDIT.scx');
check('and the case it was written in is kept', designerFile('W:\\forms\\CUSTEDIT.SCT'), 'W:\\forms\\CUSTEDIT.SCX');

// --- the command that opens it -----------------------------------------------

// The path is parenthesised because VFP reads a bare name as a name: MODIFY FORM custedit.scx stops at the dot. NOWAIT is what keeps DoCmd from blocking until the developer closes the designer.
check('a form opens in the form designer', vfpCommandFor('W:\\forms\\CUSTEDIT.scx'), 'MODIFY FORM ("W:\\forms\\CUSTEDIT.scx") NOWAIT');
check('a class needs the class as well as the library', vfpCommandFor('W:\\classes\\QBINT.vcx', 'qbint'), 'MODIFY CLASS qbint OF ("W:\\classes\\QBINT.vcx") NOWAIT');
// A library holds many classes and MODIFY CLASS opens one of them, so without a name there is no command to send.
check('and without one there is no command', vfpCommandFor('W:\\classes\\QBINT.vcx'), null);
check('the other designers each have their own verb', ['x.frx', 'x.lbx', 'x.mnx', 'x.dbc', 'x.pjx'].map(f => vfpCommandFor(f)?.split(' ')[1]), ['REPORT', 'LABEL', 'MENU', 'DATABASE', 'PROJECT']);
check('a file with no designer has no command', vfpCommandFor('mainset.prg'), null);
check('a companion file is opened as the file VFP knows', vfpCommandFor('CUSTEDIT.sct'), 'MODIFY FORM ("CUSTEDIT.scx") NOWAIT');

// Windows forbids a double quote in a path, which is what makes one delimiter enough -- a path holding ' or [ ] is legal and still quotes cleanly. A quote could only arrive from somewhere other than the filesystem, and a command built around it would reach VFP as two commands.
check('a path holding the other delimiters still quotes', vfpCommandFor("W:\\it's [here]\\f.scx"), 'MODIFY FORM ("W:\\it\'s [here]\\f.scx") NOWAIT');
check('a path holding a quote has no command', vfpCommandFor('W:\\od"d\\f.scx'), null);
// The class name is spliced in bare, so it has to be a name and nothing else.
check('a class name that is not an identifier is refused', vfpCommandFor('lib.vcx', 'qbint NOWAIT & QUIT'), null);

check('the class list is asked for by expression', classListExpression('W:\\classes\\QBINT.vcx'), 'AVCXCLASSES(__vfplint_classes, "W:\\classes\\QBINT.vcx")');
check('and only of a class library', [classListExpression('x.scx'), classListExpression('x.prg')], [null, null]);

// --- the request the script reads --------------------------------------------

const request = { mode: 'run' as const, payload: 'MODIFY FORM ("x.scx") NOWAIT', exe: 'C:\\vfp\\VFPA.EXE', cwd: 'W:\\Devstaging2\\wt', startup: ['SET PATH TO "app" ADDITIVE', 'SET PATH TO "framework" ADDITIVE'] };
check('the request is one field per line, startup commands last', requestText(request).split('\r\n'), [
	'mode=run',
	'payload=MODIFY FORM ("x.scx") NOWAIT',
	'exe=C:\\vfp\\VFPA.EXE',
	'cwd=W:\\Devstaging2\\wt',
	'setup=0',
	'startup=SET PATH TO "app" ADDITIVE',
	'startup=SET PATH TO "framework" ADDITIVE',
	''
]);
// A session the extension did not start is left as the developer set it up, unless they have just been asked about it and said to set it up.
check('and whether a session already running is to be set up too', requestText({ ...request, setup: true }).split('\r\n')[4], 'setup=1');
// A line break in a value would be read back as the start of another field, and the field it would look like is a command to run.
check('a line break in a value cannot split the field', requestText({ ...request, payload: 'A\r\nstartup=QUIT' }).split('\r\n')[1], 'payload=A startup=QUIT');

// What a VFP started by the extension is missing: a default directory and the path the code is on. Without them the form designer opens a form whose classes it cannot find and stops on a modal Locate dialog, which from the outside looks exactly like a hang.
check('a new instance is given the workspace and its search path', startupCommands('W:\\Devstaging2\\wt', ['W:\\Devstaging2\\programs\\app'], []), [
	'SET DEFAULT TO ("W:\\Devstaging2\\wt")',
	'SET PATH TO "W:\\Devstaging2\\programs\\app" ADDITIVE'
]);
check('the configured commands come after it', startupCommands('W:\\wt', [], ['DO setpath']), ['SET DEFAULT TO ("W:\\wt")', 'DO setpath']);
check('and nothing is sent when there is nothing to say', startupCommands('', [], []), []);

// --- is the Visual FoxPro already running set up for this tree? ---------------

// The one question worth asking before handing a form to a session the extension did not start. A VFP that looks nowhere inside the tree opens the designer, cannot find the first class the form is built from, and stops on a modal Locate dialog -- inside VFP, where the editor can neither see it nor cancel it, and the automation call simply never returns.
check('the probe asks where VFP is and where it looks', environmentExpression, 'SYS(5) + CURDIR() + "|" + SET("PATH")');
// A bar separates them because no Windows path can hold one.
check('the answer splits in two on the bar', parseEnvironment('W:\\Devstaging2\\wt\\ACCESS\\|c:\\a;c:\\b'), { directory: 'W:\\Devstaging2\\wt\\ACCESS\\', path: 'c:\\a;c:\\b' });
check('an empty path is still an answer', parseEnvironment('C:\\PF\\VFPA\\|'), { directory: 'C:\\PF\\VFPA\\', path: '' });
check('and so is one the bar never reached', parseEnvironment('C:\\PF\\VFPA\\'), { directory: 'C:\\PF\\VFPA\\', path: '' });

// SET PATH takes either separator, quotes an entry holding a space, and keeps a relative entry relative -- which is the whole reason this resolves rather than matching strings.
check('the directories searched are the default one and each entry resolved from it', searchedDirectories({ directory: 'W:\\Devstaging2\\wt\\ACCESS', path: '..\\..\\classes\\framework; "C:\\shared libs" ,D:\\x' }), [
	'W:\\Devstaging2\\wt\\ACCESS',
	'W:\\Devstaging2\\classes\\framework',
	'C:\\shared libs',
	'D:\\x'
]);
check('an empty path searches only where VFP is', searchedDirectories({ directory: 'C:\\PF\\VFPA', path: '' }), ['C:\\PF\\VFPA']);

const tree = 'W:\\Devstaging2';
// A VFP sitting in its own install directory with nothing set on it: every class the form is built from is in the tree, and it will ask where each of them is.
check('a VFP that looks nowhere inside the tree is not set up for it', isSetUpFor(tree, { directory: 'C:\\Program Files (x86)\\VFPA', path: '' }), false);
// The session dev.cmd starts: its default directory is inside the tree, whatever else is on its path.
check('one standing in the tree is', isSetUpFor(tree, { directory: 'W:\\Devstaging2\\wt\\ACCESS', path: '' }), true);
check('and so is one that only reaches into it', isSetUpFor(tree, { directory: 'C:\\PF\\VFPA', path: 'W:\\Devstaging2\\classes\\framework' }), true);
check('by a relative entry as much as an absolute one', isSetUpFor(tree, { directory: 'W:\\Devstaging2\\wt\\ACCESS', path: '..\\..\\classes\\framework' }), true);
// The tree is on a Windows drive: W:\DEVSTAGING2 and W:\Devstaging2 are the same directory.
check('case is not what tells two directories apart', isSetUpFor(tree, { directory: 'w:\\devstaging2\\WT', path: '' }), true);
// Neither a sibling of the tree nor the drive above it finds classes\framework\CONTROLS.vcx by name.
check('a sibling of the tree is outside it', isSetUpFor(tree, { directory: 'W:\\Devstaging3\\wt', path: '' }), false);
check('as is the drive above it', isSetUpFor(tree, { directory: 'W:\\', path: '' }), false);
// With no folder open there is no tree to be set up for, and nothing worth saying about a session that is the developer's own.
check('a window with no workspace asks nothing', isSetUpFor('', { directory: 'C:\\PF\\VFPA', path: '' }), true);

// --- finding the executable ---------------------------------------------------

const installs = ['Microsoft Visual FoxPro 9', 'Microsoft Visual Foxpro Advanced 10.2.20260326', 'Microsoft Visual Foxpro Advanced 10.10.1', 'Common Files'];
const candidates = vfpExeCandidates({ programFiles: 'C:\\PF', entries: installs });
// VFP Advanced is what the tree this linter is aimed at is built with, so it is preferred over a VFP 9 beside it; 10.10 is a later build than 10.2 and a plain sort would disagree.
check('the newest VFP Advanced comes first, then VFP 9', candidates, ['C:\\PF\\Microsoft Visual Foxpro Advanced 10.10.1\\VFPA.EXE', 'C:\\PF\\Microsoft Visual Foxpro Advanced 10.2.20260326\\VFPA.EXE', 'C:\\PF\\Microsoft Visual FoxPro 9\\vfp9.exe']);
check('the setting wins over anything installed', vfpExeCandidates({ configured: 'D:\\my\\vfp.exe', programFiles: 'C:\\PF', entries: installs })[0], 'D:\\my\\vfp.exe');
// The team's own VFP tooling is already pointed at an install by this variable, so the extension reads the same one rather than asking to be configured twice.
check('and VFPA_EXE is honoured after it', vfpExeCandidates({ configured: 'D:\\my\\vfp.exe', env: 'E:\\vfpa\\VFPA.EXE', programFiles: 'C:\\PF', entries: installs }).slice(0, 2), ['D:\\my\\vfp.exe', 'E:\\vfpa\\VFPA.EXE']);
check('an empty Program Files still offers what is configured', vfpExeCandidates({ configured: 'D:\\my\\vfp.exe', programFiles: 'C:\\PF', entries: [] }), ['D:\\my\\vfp.exe']);

report('VFP designer checks');
