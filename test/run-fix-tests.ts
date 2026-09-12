// Quick fixes, the Outline and folding: what the server hands the editor beyond diagnostics.
// A fix is asserted by applying it and reading the text the user ends up with, and then by linting that text: a fix that does not make its own finding go away is not a fix.
import { lint, type Fix } from '../server/src/linter.js';
import { documentSymbols, foldingRanges } from '../server/src/outline.js';
import { check, report } from './check.js';

type OutlineSymbol = ReturnType<typeof documentSymbols>[number];

const fixFor = (src: string, code: string): Fix | null => lint(src).diagnostics.find(d => d.code === code)?.data?.fix ?? null;
const codesIn = (src: string) => lint(src).diagnostics.map(d => d.code);

function apply(src: string, fix: Fix) {
	const starts = [0];
	for (let i = 0; i < src.length; i++) if (src[i] === '\n') starts.push(i + 1);
	const offset = (p: { line: number; character: number }) => starts[p.line] + p.character;
	let out = src;
	for (const e of [...fix.edits].sort((a, b) => offset(b.range.start) - offset(a.range.start)))
		out = out.slice(0, offset(e.range.start)) + e.newText + out.slice(offset(e.range.end));
	return out;
}

// Applies the fix for `code` and checks both the text and that the finding is gone.
function fixed(label: string, src: string, code: string, expected: string) {
	const fix = fixFor(src, code);
	check(`${label}: has a fix`, !!fix, true);
	if (!fix) return;
	const after = apply(src, fix);
	check(`${label}: text`, after, expected);
	check(`${label}: resolved`, codesIn(after).includes(code), false);
}

// --- implicit-private: declare as LOCAL ------------------------------------
fixed('a file-level assignment', 'lnA = 1\n', 'implicit-private', 'LOCAL lnA\nlnA = 1\n');
fixed('CRLF is kept', 'lnA = 1\r\n', 'implicit-private', 'LOCAL lnA\r\nlnA = 1\r\n');
fixed('the declaration goes after the preamble, not above the write',
	'#DEFINE MAX 3\nLOCAL lnB\nlnB = 2\nlnA = 1\n', 'implicit-private',
	'#DEFINE MAX 3\nLOCAL lnB\nLOCAL lnA\nlnB = 2\nlnA = 1\n');
// A LOCAL inside the loop would reset the total on every pass, so the fix has to reach the top of the routine.
fixed('a write inside a loop is declared at the routine top',
	'PROCEDURE Sum\n\tLPARAMETERS tnCount\n\tLOCAL i\n\tFOR i = 1 TO tnCount\n\t\tlnTotal = lnTotal + i\n\tENDFOR\n\tRETURN lnTotal\nENDPROC\n', 'implicit-private',
	'PROCEDURE Sum\n\tLPARAMETERS tnCount\n\tLOCAL i\n\tLOCAL lnTotal\n\tFOR i = 1 TO tnCount\n\t\tlnTotal = lnTotal + i\n\tENDFOR\n\tRETURN lnTotal\nENDPROC\n');
fixed('a method declares in the method', 'DEFINE CLASS A AS Custom\n\tPROCEDURE Init\n\t\tlnX = 1\n\tENDPROC\nENDDEFINE\n', 'implicit-private',
	'DEFINE CLASS A AS Custom\n\tPROCEDURE Init\n\t\tLOCAL lnX\n\t\tlnX = 1\n\tENDPROC\nENDDEFINE\n');
check('the fix keeps the spelling of the first write', fixFor('lnCamelCase = 1\n', 'implicit-private')!.title, "Declare 'lnCamelCase' as LOCAL");

// --- unused-local: remove the declaration ----------------------------------
fixed('the only name on the line removes the line', 'LOCAL lcUnused\n? 1\n', 'unused-local', '? 1\n');
fixed('an indented line goes with its indentation', '\tLOCAL lcUnused\n\t? 1\n', 'unused-local', '\t? 1\n');
fixed('a name in a list is removed from the list', 'LOCAL lcA, lcUnused, lcB\nlcA = 1\nlcB = 2\n', 'unused-local', 'LOCAL lcA, lcB\nlcA = 1\nlcB = 2\n');
fixed('array dimensions and a trailing comment survive', 'LOCAL laRows(2, 3), lcUnused && grid\nlaRows(1, 1) = 1\n', 'unused-local', 'LOCAL laRows(2, 3) && grid\nlaRows(1, 1) = 1\n');
fixed('an AS clause goes with its name', 'LOCAL lcUnused AS Character, loX AS Object\nloX = 1\n', 'unused-local', 'LOCAL loX AS Object\nloX = 1\n');
fixed('the last line of the file', '? 1\nLOCAL lcUnused', 'unused-local', '? 1\n');
check('a declaration continued across lines is left alone', fixFor('LOCAL lcUnused, ;\n\tlcOther\nlcOther = 1\n', 'unused-local'), null);

// --- missing-memvar-prefix: insert m. --------------------------------------
const shadowed = 'USE customer\nLOCAL cust_id\ncust_id = 1\nREPLACE cust_id WITH cust_id + 1\n';
check('the collision is reported twice', lint(shadowed).diagnostics.filter(d => d.code === 'missing-memvar-prefix').map(d => `${d.range.start.line + 1}:${d.range.start.character + 1}`), ['3:1', '4:22']);
check('each site gets its own fix', lint(shadowed).diagnostics.filter(d => d.code === 'missing-memvar-prefix').map(d => apply(shadowed, d.data!.fix)),
	['USE customer\nLOCAL cust_id\nm.cust_id = 1\nREPLACE cust_id WITH cust_id + 1\n', 'USE customer\nLOCAL cust_id\ncust_id = 1\nREPLACE cust_id WITH m.cust_id + 1\n']);
// STORE names its target as a plain string, so the reference is pinned to the whole statement; a prefix inserted there would land on the STORE keyword.
check('a statement-wide reference offers no fix', fixFor('USE customer\nLOCAL cust_id\nSTORE 1 TO cust_id\n', 'missing-memvar-prefix'), null);

// --- the Outline --------------------------------------------------------------
const source = [
	'#DEFINE MAX_ROWS 10',
	'PROCEDURE Alpha',
	'\tLPARAMETERS tcName, tnCount',
	'\t? tcName',
	'FUNCTION Beta(tnX AS Integer) AS Integer',
	'\tRETURN tnX',
	'ENDFUNC',
	'',
	'DEFINE CLASS Widget AS Custom',
	'\tcName = ""',
	'\tPROCEDURE Init',
	'\t\tTHIS.cName = "x"',
	'\tENDPROC',
	'ENDDEFINE',
	''
].join('\n');
const lines = source.split('\n');
const { ast } = lint(source);
const brief = (s: OutlineSymbol): string => `${s.name}:${s.kind}:${s.detail ?? ''}:${s.range.start.line}-${s.range.end.line}` + (s.children ? `[${s.children.map(brief).join(' ')}]` : '');
// SymbolKind: Function 12, Method 6, Class 5, Property 7, Constant 14.
check('outline', documentSymbols(ast!, lines).map(brief), [
	'MAX_ROWS:14:10:0-0',
	'Alpha:12:PROCEDURE (tcName, tnCount):1-3',
	'Beta:12:FUNCTION (tnX):4-6',
	'Widget:5:AS Custom:8-13[cName:7::9-9 Init:6:PROCEDURE:10-12]'
]);
check('a selection range sits on the first line', documentSymbols(ast!, lines)[1].selectionRange, { start: { line: 1, character: 0 }, end: { line: 1, character: 'PROCEDURE Alpha'.length } });

// The member declarations name properties too, and a method carrying PROTECTED used to leave the outline with the rest of the class.
const memberSource = ['DEFINE CLASS Poster AS Custom', '\tPROTECTED cName, nAge', '\tADD OBJECT cmdPost AS CommandButton', '\tPROTECTED PROCEDURE Post', '\tENDPROC', 'ENDDEFINE', ''].join('\n');
check('class members reach the outline',
	documentSymbols(lint(memberSource).ast!, memberSource.split('\n'))[0].children!.map(brief),
	['cName:7:PROTECTED:1-1', 'nAge:7:PROTECTED:1-1', 'cmdPost:19:AS CommandButton:2-2', 'Post:6:PROCEDURE:3-4']);

// --- folding --------------------------------------------------------------------
const folds = (src: string) => foldingRanges(lint(src).ast!, src.split('\n')).map(r => `${r.startLine}-${r.endLine}`);
check('a routine without ENDPROC folds to its last statement; one with it keeps the terminator visible',
	folds(source), ['1-3', '4-5', '8-12', '10-11']);
check('IF keeps ENDIF visible', folds('IF .T.\n? 1\n? 2\nENDIF\n'), ['0-2']);
check('every CASE folds', folds('DO CASE\nCASE x = 1\n\t? 1\nCASE x = 2\n\t? 2\nENDCASE\n'), ['0-4', '1-2', '3-4']);
check('a one-line block does not fold', folds('FOR i = 1 TO 3\nENDFOR\n'), []);
check('a query over several lines folds', folds('SELECT a ;\n\tFROM b ;\n\tINTO CURSOR c\n? 1\n'), ['0-1']);
// A stray terminator used to throw, which cost the file its outline and its folding along with every diagnostic below the line. lint() still returns a null ast if the parser ever does throw and server.ts guards for it, but no input reaches that path now.
const strayOutline = 'ENDIF\nPROCEDURE Foo\n? 1\nENDPROC\n';
check('a stray terminator no longer costs the file its outline',
	documentSymbols(lint(strayOutline).ast!, strayOutline.split('\n')).map(s => s.name), ['Foo']);

report('Fix, outline and folding checks');
