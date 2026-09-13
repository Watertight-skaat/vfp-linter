// The suite for everything that needs more than one file: the index, how a name resolves across the tree, and the diagnostics a file only produces when the rest of the workspace is visible.
// A case is a directory under test-files/workspace/. Every .prg in it is linted with an index built over that directory alone, and each one's findings are diffed against its .expected, exactly as run-all-tests.ts does for a single file. `case.json` can set a searchPath, per-rule severities, or the tier to index at.
// Below the fixtures are assertions about the index itself -- resolution order, file resolution, what an edit invalidates -- and the parity check that holds the header scan to the parser over every fixture in the repository.

import fs from 'fs';
import os from 'os';
import path from 'path';
import { lint, type LinterOptions } from '../server/src/linter.js';
import { extract, headerRefKinds, scanHeader, upper, WorkspaceIndex, type FileRecord, type Reference } from '../server/src/index.js';
import { isDynamic, staticName } from '../server/src/dynamic.js';
import { isBuiltin } from '../server/src/builtins.js';
import { completionsAt, definitionAt, hoverAt, referencesAt, signatureAt, workspaceSymbols } from '../server/src/navigation.js';
import { buildSymbolTable, openAliasesOf } from '../server/src/scope.js';
import { parse } from '../server/src/parser.js';
import { buildIndex, cachePath, globToRegExp, indexedExtensions, loadCache, promoteToTier2, recordFrom, refresh, saveCache } from '../server/src/workspace.js';
import { check, report } from './check.js';
import { format } from './format.js';

const root = './test-files/workspace';
const update = process.argv.includes('--update');

interface CaseSettings {
	searchPath?: string[];
	rules?: LinterOptions['rules'];
	tier?: 1 | 2;
}

const read = (file: string) => fs.readFileSync(file, 'utf-8');
const lines = (file: string) => read(file).split(/\r\n|\r|\n/);

// --- the fixture cases -------------------------------------------------------

const cases = fs.readdirSync(root, { withFileTypes: true }).filter(e => e.isDirectory()).map(e => e.name).sort();
const failures: [string, string[]][] = [];
let matched = 0;
let total = 0;

for (const name of cases) {
	const dir = path.join(root, name);
	const settingsPath = path.join(dir, 'case.json');
	const settings: CaseSettings = fs.existsSync(settingsPath) ? JSON.parse(read(settingsPath)) : {};
	const index = await buildIndex({
		roots: [dir],
		searchPath: (settings.searchPath ?? []).map(p => path.resolve(dir, p)),
		yieldEvery: 0
	});
	// The fixtures are small enough to index at tier 2, which is what makes a case exercising calls or CREATEOBJECT possible.
	if (settings.tier !== 1) for (const file of [...index.files.values()].map(r => r.file)) index.upsert(recordFrom(file, read(file), { mtime: 0, size: 0 }, 2));

	for (const file of filesIn(dir)) {
		total++;
		const options: LinterOptions = { unsupportedSyntaxSeverity: 'error', rules: settings.rules, workspace: { index, file: path.resolve(file) } };
		const actual = lint(read(file), options).diagnostics.flatMap(d => format(d).split('\n').map(l => l.trim()));
		const expectedPath = `${file}.expected`;

		if (update) {
			if (actual.length) fs.writeFileSync(expectedPath, actual.join('\n') + '\n');
			else if (fs.existsSync(expectedPath)) fs.unlinkSync(expectedPath);
			continue;
		}

		const expected = fs.existsSync(expectedPath) ? read(expectedPath).split('\n').map(l => l.trim()).filter(Boolean) : [];
		const diff: string[] = [];
		for (let i = 0; i < Math.max(actual.length, expected.length); i++) {
			if (actual[i] === expected[i]) continue;
			if (expected[i] === undefined) diff.push(`  + ${actual[i]}`);
			else if (actual[i] === undefined) diff.push(`  - ${expected[i]}`);
			else diff.push(`  - ${expected[i]}\n  + ${actual[i]}`);
		}
		if (diff.length) failures.push([file, diff]); else matched++;
	}
}

function filesIn(dir: string): string[] {
	return fs.readdirSync(dir, { withFileTypes: true })
		.flatMap(entry => (entry.isDirectory() ? filesIn(path.join(dir, entry.name)) : entry.name.endsWith('.prg') ? [path.join(dir, entry.name)] : []))
		.sort();
}

if (update) {
	console.log(`Recorded the workspace fixtures across ${cases.length} cases.`);
	process.exit(0);
}
for (const [file, diff] of failures) {
	console.log(`==== ${file} ====`);
	console.log(diff.join('\n'));
}
check('every workspace fixture matches its expected diagnostics', `${matched}/${total}`, `${total}/${total}`);

// --- the index ---------------------------------------------------------------

const basic = await buildIndex({ roots: ['./test-files/workspace/basic'], yieldEvery: 0 });
const at = (name: string) => path.resolve('./test-files/workspace/basic', name);
const names = (list: { name: string }[]) => list.map(r => r.name).sort();

check('a crawl indexes every readable file', [...basic.files.values()].map(r => path.basename(r.file)).sort(), ['console.prg', 'ledger.prg', 'log.prg', 'shared.h']);
check('the header scan reads a routine and its parameters', basic.get(at('log.prg'))!.routines.map(r => `${r.name}(${r.params.join(', ')})`), ['LogEntry(tcAccount)', 'Describe(tcAccount, tnWidth)']);
check('a function is marked as one', basic.get(at('log.prg'))!.routines.map(r => r.isFunction), [false, true]);
check('the constants of a header file are indexed', names(basic.get(at('shared.h'))!.constants), ['CRLF', 'MAX_ROWS']);
check('the comment block above a routine is its documentation', basic.get(at('ledger.prg'))!.routines[0].doc, 'Posts a charge to the ledger.\nReturns .T. when the post succeeded.');
check('a routine is found by name from another file', basic.resolveRoutine('logentry', at('console.prg')).map(r => path.basename(r.file)), ['log.prg']);
check('an unknown name resolves to nothing', basic.resolveRoutine('NoSuchThing', at('console.prg')), []);

// The references a file makes, which is what every cross-file rule reads.
const consoleRefs = (kind: string) => basic.get(at('console.prg'))!.refs.filter(r => r.kind === kind).map(r => `${r.name}/${r.argc}`);
check('a DO names its routine and counts its arguments', consoleRefs('do'), ['PostCharge/2']);
check('a DO FORM names its form', consoleRefs('form'), ['ledgerview/0']);

// --- resolving a file --------------------------------------------------------

check('an #INCLUDE resolves beside the file that names it', basic.resolveFile('shared.h', 'include', at('log.prg')), at('shared.h'));
check('a name with no extension takes the one its kind defaults to', basic.resolveFile('log', 'procedure', at('console.prg')), at('log.prg'));
check('a form that does not exist resolves to nothing', basic.resolveFile('ledgerview', 'form', at('console.prg')), null);
check('resolution is case-insensitive, as the filesystem VFP runs on is', basic.resolveFile('SHARED.H', 'include', at('log.prg')), at('shared.h'));

const searchPathDir = './test-files/workspace/search-path';
const searched = await buildIndex({ roots: [searchPathDir], searchPath: [path.resolve(searchPathDir, 'lib')], yieldEvery: 0 });
check('the search path is looked in after the roots', path.basename(searched.resolveFile('toolkit', 'procedure', path.resolve(searchPathDir, 'main.prg')) ?? ''), 'toolkit.prg');
check('a routine in a search-path file is still indexed', searched.resolveRoutine('Shared', path.resolve(searchPathDir, 'main.prg')).length, 1);

// A tree laid out by what a file is rather than by who calls it puts the caller and the callee in cousin folders, and FoxPro resolves by name over SET PATH and never by folder. So a name nothing above it matched falls back to the same name anywhere under the roots, which is the whole of the 493 missing-file findings on the Watertight tree.
const treeDir = './test-files/workspace/resolve-across-tree';
const tree = await buildIndex({ roots: [treeDir], yieldEvery: 0 });
const fromApp = path.resolve(treeDir, 'programs/app/APPMAIN.prg');
const resolvedIn = (name: string, kind: Parameters<typeof tree.resolveFile>[1]) => (tree.resolveFile(name, kind, fromApp) ?? '').replace(/\\/g, '/').split('/').slice(-2).join('/');
check('a header file in a cousin folder is found', resolvedIn('EMAILLIB.h', 'include'), 'framework/EMAILLIB.h');
check('a procedure file is found by name, extension and all', resolvedIn('mainset', 'procedure'), 'framework/MAINSET.prg');
check('so is a class library the index cannot read', resolvedIn('QbInt', 'classlib'), 'framework/QBINT.vcx');
check('and a form', resolvedIn('datasrch', 'form'), 'framework/DATASRCH.scx');
check('a name carrying a folder matches that folder anywhere in the tree', resolvedIn('framework\\mainset', 'procedure'), 'framework/MAINSET.prg');
check('a name nothing in the tree spells is still missing', tree.resolveFile('nosuchthing', 'procedure', fromApp), null);
check('and the fallback does not cross a folder boundary mid-name', tree.resolveFile('app\\mainset', 'procedure', fromApp), null);

// The ordered directories still come first, and where only the fallback answers the match nearest the asking file is the one taken. Two folders holding the same name is what makes either assertion mean anything, so the index is built by hand rather than from a fixture.
const shadowed = new WorkspaceIndex({ roots: [path.resolve('/tree')] });
for (const f of ['app/util.prg', 'far/util.prg']) shadowed.addKnown(path.resolve('/tree', f));
const folderOf = (file: string | null) => (file ?? '').replace(/\\/g, '/').split('/').slice(-2)[0];
check('a file beside the caller wins over the same name elsewhere', folderOf(shadowed.resolveFile('util', 'do', path.resolve('/tree/app/caller.prg'))), 'app');
check('and the nearest match is taken when only the fallback answers', folderOf(shadowed.resolveFile('util', 'do', path.resolve('/tree/far/deep/caller.prg'))), 'far');

// --- resolution order --------------------------------------------------------

const procDir = './test-files/workspace/procedure-file';
const procedureFile = await buildIndex({ roots: [procDir], yieldEvery: 0 });
const app = path.resolve(procDir, 'app.prg');
check('the library the file loads is preferred over one merely present in the tree',
	procedureFile.resolveRoutine('Greet', app).map(r => path.basename(r.file)), ['toolkit.prg', 'stray.prg']);
check('a definition in the asking file wins over every other',
	procedureFile.resolveRoutine('Greet', path.resolve(procDir, 'stray.prg')).map(r => path.basename(r.file)), ['stray.prg', 'toolkit.prg']);

// --- what an edit invalidates ------------------------------------------------

const churn = new WorkspaceIndex({ roots: [path.resolve('/work')] });
const record = (file: string, text: string, tier: 1 | 2 = 1) => recordFrom(path.resolve('/work', file), text, { mtime: 0, size: 0 }, tier);
churn.upsert(record('lib.prg', 'PROCEDURE Greet\nLPARAMETERS tcWho\nENDPROC\n'));
churn.upsert(record('caller.prg', 'DO Greet WITH "a"\n'));
check('a caller is a dependent of what it calls', [...churn.dependentsOf(['GREET'])].map(f => path.basename(f)), ['caller.prg']);
check('an unchanged re-index invalidates nothing',
	[...churn.upsert(record('lib.prg', 'PROCEDURE Greet\nLPARAMETERS tcWho\nENDPROC\n')).changed], []);
check('a changed signature invalidates the name',
	[...churn.upsert(record('lib.prg', 'PROCEDURE Greet\nLPARAMETERS tcWho, tcHow\nENDPROC\n')).changed], ['GREET']);
check('and the caller is what has to be looked at again',
	[...churn.dependentsOf(churn.upsert(record('lib.prg', 'PROCEDURE Greet\nENDPROC\n')).changed)].map(f => path.basename(f)), ['caller.prg']);
check('deleting the file invalidates it too', [...churn.remove(path.resolve('/work', 'lib.prg')).changed].sort(), ['GREET', 'file:lib', 'file:lib.prg']);
check('and the name then resolves to nothing', churn.resolveRoutine('Greet', path.resolve('/work', 'caller.prg')), []);

// A file appearing or vanishing is an invalidation of its own, separate from the names inside it: a `SET PROCEDURE TO lib` that was reported as missing stops being missing the moment lib.prg is created. The two are filed under the name with and without its extension so they meet without the index having to resolve a search path on every re-index.
const loading = new WorkspaceIndex({ roots: [path.resolve('/work')] });
loading.upsert(record('app.prg', 'SET PROCEDURE TO lib ADDITIVE\n#INCLUDE "shared.h"\n'));
check('a file appearing reaches the file that named it without its extension',
	[...loading.dependentsOf(loading.upsert(record('lib.prg', 'PROCEDURE Greet\nENDPROC\n')).changed)].map(f => path.basename(f)), ['app.prg']);
check('and one named with its extension too',
	[...loading.dependentsOf(loading.upsert(record('shared.h', '#DEFINE MAX 1\n')).changed)].map(f => path.basename(f)), ['app.prg']);
check('a file nothing names has no dependents',
	[...loading.dependentsOf(loading.upsert(record('other.prg', 'PROCEDURE Unrelated\nENDPROC\n')).changed)], []);

// --- the watcher path --------------------------------------------------------
// What the server does when a file changes on disk outside the editor. It touches the filesystem, so it gets a directory of its own rather than one of the fixtures.

const scratch = fs.mkdtempSync(path.join(os.tmpdir(), 'vfp-workspace-'));
try {
	const libFile = path.join(scratch, 'lib.prg');
	fs.writeFileSync(libFile, 'PROCEDURE Greet\nLPARAMETERS tcWho\nENDPROC\n');
	fs.writeFileSync(path.join(scratch, 'caller.prg'), 'DO Greet WITH "a"\n');
	const watched = await buildIndex({ roots: [scratch], yieldEvery: 0 });
	check('a crawled routine is there to begin with', watched.resolveRoutine('Greet', libFile).length, 1);

	fs.writeFileSync(libFile, 'PROCEDURE Greet\nLPARAMETERS tcWho, tcHow\nENDPROC\n');
	const afterEdit = refresh(watched, libFile);
	check('an edit on disk is picked up', [...afterEdit], ['GREET']);
	check('and the new signature is what resolves', watched.resolveRoutine('Greet', libFile)[0].params, ['tcWho', 'tcHow']);
	check('the caller is what has to be re-linted', [...watched.dependentsOf(afterEdit)].map(f => path.basename(f)), ['caller.prg']);

	fs.unlinkSync(libFile);
	check('a deleted file is dropped', [...refresh(watched, libFile)].sort(), ['GREET', 'file:lib', 'file:lib.prg']);
	check('and its routine is gone with it', watched.resolveRoutine('Greet', libFile), []);
	// A file the index does not read still has to be recorded, or SET CLASSLIB TO a .vcx reports as missing.
	const library = path.join(scratch, 'controls.vcx');
	fs.writeFileSync(library, '');
	refresh(watched, library);
	check('a file of a kind the index cannot read is still known to exist', watched.resolveFile('controls', 'classlib', path.join(scratch, 'caller.prg')), library);
} finally {
	fs.rmSync(scratch, { recursive: true, force: true });
}

// --- a finding that belongs to the file that did not change ------------------
// The hazard the whole reverse map exists for: rename or re-sign a routine in A and the finding appears in B. Nothing about B changed, so without this its diagnostics would keep saying what was true before the edit.

const stale = fs.mkdtempSync(path.join(os.tmpdir(), 'vfp-stale-'));
try {
	const lib = path.join(stale, 'lib.prg');
	const caller = path.join(stale, 'caller.prg');
	fs.writeFileSync(lib, 'PROCEDURE Greet\nLPARAMETERS tcWho, tnLevel\nENDPROC\n');
	fs.writeFileSync(caller, 'DO Greet WITH "hello", 1\n');
	const index = await buildIndex({ roots: [stale], yieldEvery: 0 });
	const lintCaller = () => lint(read(caller), { workspace: { index, file: caller } }).diagnostics.map(d => d.code);
	check('the call is within its arity to begin with', lintCaller(), []);

	fs.writeFileSync(lib, 'PROCEDURE Greet\nLPARAMETERS tcWho\nENDPROC\n');
	const changed = refresh(index, lib);
	check('dropping a parameter in the library names the caller as needing another look',
		[...index.dependentsOf(changed)].map(f => path.basename(f)), ['caller.prg']);
	check('and the caller now reports, though nothing in it was touched', lintCaller(), ['too-many-arguments']);
} finally {
	fs.rmSync(stale, { recursive: true, force: true });
}

// --- navigation --------------------------------------------------------------

const consoleFile = at('console.prg');
const consoleLines = lines(path.join('./test-files/workspace/basic', 'console.prg'));
const consoleRecord = extract(consoleFile, parse(consoleLines.join('\n')) as never, consoleLines);
const view = basic.viewFor(consoleFile);
const where = (line: number, character: number) => definitionAt(consoleRecord, consoleLines, { line, character }, view).map(l => `${path.basename(l.file)}:${l.range.start.line}`);

check('go to definition follows a DO to another file', where(1, 5), ['ledger.prg:2']);
check('go to definition follows a call', where(3, 12), ['log.prg:7']);
check('a form with no file behind it goes nowhere', where(2, 10), []);

const ledgerLines = lines('./test-files/workspace/basic/ledger.prg');
const ledgerRecord = extract(at('ledger.prg'), parse(ledgerLines.join('\n')) as never, ledgerLines);
const ledgerView = basic.viewFor(at('ledger.prg'));
check('a routine in the same file wins', definitionAt(ledgerRecord, ledgerLines, { line: 4, character: 5 }, ledgerView).map(l => path.basename(l.file)), ['log.prg']);
check('hover shows the signature and the comment block above it',
	hoverAt(consoleRecord, consoleLines, { line: 1, character: 5 }, view)?.markdown.split('\n').filter(l => l && !l.startsWith('```')),
	['PROCEDURE PostCharge(tcAccount, tnAmount)', '*ledger.prg*', 'Posts a charge to the ledger.', 'Returns .T. when the post succeeded.']);
check('hover on a constant shows its value',
	hoverAt(ledgerRecord, ['MAX_ROWS'], { line: 0, character: 2 }, ledgerView)?.markdown.split('\n')[1], '#DEFINE MAX_ROWS 500');
check('a macro target says so rather than guessing',
	hoverAt(...macro())?.markdown, 'Named at run time, so the linter cannot say what this refers to.');

function macro(): [FileRecord, string[], { line: number; character: number }, typeof view] {
	const text = ['DO &lcProc'];
	const rec = extract(consoleFile, parse(text.join('\n')) as never, text);
	return [rec, text, { line: 0, character: 5 }, view];
}

check('workspace symbols list every definition in the tree',
	workspaceSymbols({ index: basic }, '').map(s => s.name), ['CRLF', 'Describe', 'LogEntry', 'MAX_ROWS', 'PostCharge']);
check('a query narrows them, prefix first', workspaceSymbols({ index: basic }, 'log').map(s => s.name), ['LogEntry']);
check('a symbol carries the shape the editor groups by',
	workspaceSymbols({ index: basic }, 'Describe').map(s => `${s.sort}${s.detail}`), ['function(tcAccount, tnWidth)']);

// --- the header scan against the parser --------------------------------------
// The regex is what makes indexing a large tree possible, and it is the thing most likely to drift. Every fixture in the repository is read both ways and the two must agree on what the file defines and what it names.
// A fixture that records a parse gap is left out: the parser is known not to read it, so holding the scan to it would only assert that the regex is broken in the same place. Those files are where the two legitimately disagree -- `gap-keyword-as-routine-name.prg` is indexed at tier 1 and loses its methods at tier 2 -- and that divergence is tracked in TODO.md rather than frozen here.

const recordsAParseGap = (file: string) => fs.existsSync(`${file}.expected`)
	&& /\b(unsupported-syntax|syntax-error|unterminated-block)\b/.test(read(`${file}.expected`));

const corpus = collect('./test-files').filter(f => !f.includes('workspace') && !recordsAParseGap(f));
const divergent: string[] = [];
for (const file of corpus) {
	const text = read(file);
	let parsed;
	try {
		parsed = parse(text);
	} catch {
		continue; // a fixture that records a syntax error has no tree to compare against
	}
	const tier2 = extract(file, parsed as never, text.split(/\r\n|\r|\n/));
	const tier1 = scanHeader(file, text);
	const difference = compare(tier1, tier2);
	if (difference) divergent.push(`${file}: ${difference}`);
}
check('the header scan finds what the parser finds', divergent, []);

// A declaration rather than a const: the parity loop above runs before this point in the file, and only a declaration is hoisted to meet it.
function refKey(ref: Reference): string {
	return `${ref.kind}:${ref.dynamic ? '&' : upper(ref.name)}@${ref.range.start.line}`;
}

/** What the two tiers must agree on: the definitions, and the references tier 1 claims to cover. Ranges are compared by line, since one reads a whole line and the other a node. */
function compare(tier1: FileRecord, tier2: FileRecord): string | null {
	const routines = (r: FileRecord) => [...r.routines, ...r.classes.flatMap(c => c.methods)].map(x => `${x.owner ?? ''}.${x.key}(${x.params.join(',')})${x.isFunction ? '!' : ''}@${x.range.start.line}`).sort();
	const classes = (r: FileRecord) => r.classes.map(c => `${c.key} AS ${c.base ?? ''}@${c.range.start.line}`).sort();
	const constants = (r: FileRecord) => r.constants.map(c => `${c.key}=${c.value ?? ''}@${c.range.start.line}`).sort();
	const refs = (r: FileRecord) => r.refs.filter(x => headerRefKinds.has(x.kind)).map(refKey).sort();
	for (const [what, of] of [['routines', routines], ['classes', classes], ['constants', constants], ['references', refs]] as const) {
		const a = of(tier1).join(' | ');
		const b = of(tier2).join(' | ');
		if (a !== b) return `${what}\n    scan   ${a}\n    parse  ${b}`;
	}
	return tier1.mainParams?.join(',') === tier2.mainParams?.join(',') ? null : `main parameters\n    scan   ${tier1.mainParams}\n    parse  ${tier2.mainParams}`;
}

function collect(dir: string): string[] {
	return fs.readdirSync(dir, { withFileTypes: true }).flatMap(entry =>
		entry.isDirectory() ? collect(`${dir}/${entry.name}`) : entry.name.endsWith('.prg') ? [`${dir}/${entry.name}`] : []);
}

// --- find all references -----------------------------------------------------
// Only the parser sees a call, so the tree is promoted to tier 2 first. That is the whole reason the promotion exists, and asserting the answer before and after is what shows it.

const refDir = './test-files/workspace/references';
const referenced = await buildIndex({ roots: [refDir], yieldEvery: 0 });
const refAt = (name: string) => path.resolve(refDir, name);
const sites = (file: string, line: number, character: number, withDeclaration = true) => {
	const text = lines(path.join(refDir, file));
	const record = extract(refAt(file), parse(text.join('\n')) as never, text);
	return referencesAt(record, text, { line, character }, referenced.viewFor(refAt(file)), withDeclaration)
		.map(l => `${path.basename(l.file)}:${l.range.start.line}`);
};

check('over a tree the parser has not read, a call is invisible', sites('lib.prg', 1, 12), ['lib.prg:1', 'alpha.prg:1']);
const promoted = await promoteToTier2(referenced);
check('promotion reads every file the crawl left at tier 1', promoted.parsed.map(f => path.basename(f)).sort(), ['alpha.prg', 'beta.prg', 'lib.prg']);
check('and the call is then found too', sites('lib.prg', 1, 12), ['lib.prg:1', 'alpha.prg:1', 'beta.prg:2']);
check('the declaration can be left out', sites('lib.prg', 1, 12, false), ['alpha.prg:1', 'beta.prg:2']);
check('a name assembled at run time is not a reference to anything', sites('beta.prg', 5, 5), []);
check('a file is referenced by the SET PROCEDURE that loads it', sites('alpha.prg', 0, 20), ['lib.prg:0', 'alpha.prg:0']);
check('nothing is promoted twice', (await promoteToTier2(referenced)).parsed, []);

// --- completion --------------------------------------------------------------

const completions = (file: string, line: number, character: number) => {
	const text = lines(path.join(refDir, file));
	const record = extract(refAt(file), parse(text.join('\n')) as never, text);
	return completionsAt(record, text, { line, character }, referenced.viewFor(refAt(file))).map(c => `${c.sort}:${c.label}`);
};
check('the routines of the workspace are offered', completions('beta.prg', 2, 10), ['procedure:Ping']);

// A file with a table open is what makes field completion possible: there is no table to ask at edit time, so the file's own evidence is the whole of it.
const fieldSource = ['USE customer', 'SELECT customer', '? customer.cust_id', '? customer.balance', 'REPLACE customer.balance WITH 0', '? customer.'];
const fieldAst = parse(fieldSource.join('\n')) as never;
const fieldRecord = extract('fields.prg', fieldAst, fieldSource);
const fieldTable = buildSymbolTable(fieldAst);
const fieldContext = { ast: fieldAst as never, aliases: openAliasesOf(fieldTable) };
check('after an alias, the fields the file shows against it',
	completionsAt(fieldRecord, fieldSource, { line: 5, character: 11 }, referenced.viewFor('fields.prg'), fieldContext).map(c => c.label),
	['balance', 'cust_id']);
check('after something that is not an open alias, nothing is guessed at',
	completionsAt(fieldRecord, fieldSource, { line: 5, character: 11 }, referenced.viewFor('fields.prg'), { ast: fieldAst as never, aliases: new Set<string>() }), []);

// --- signature help ----------------------------------------------------------

const signature = (src: string) => {
	const text = src.split('\n');
	const at = { line: text.length - 1, character: text[text.length - 1].length };
	const record = extract(refAt('beta.prg'), parse('PROCEDURE Ping\nLPARAMETERS tcWho\nENDPROC\n') as never, []);
	const found = signatureAt(record, text, at, referenced.viewFor(refAt('beta.prg')));
	return found && `${found.label} @${found.activeParameter}`;
};
check('a call being typed names its routine', signature('x = Ping('), 'PROCEDURE Ping(tcWho) @0');
check('a comma advances the parameter', signature('x = Ping(1, '), 'PROCEDURE Ping(tcWho) @1');
check('a name nothing defines has no signature to show', signature('x = NoSuchRoutine('), null);
check('DO ... WITH counts its arguments too', signature('DO Ping WITH 1, '), 'PROCEDURE Ping(tcWho) @1');
check('a nested call is the one being typed', signature('x = Ping(Ping('), 'PROCEDURE Ping(tcWho) @0');
check('a closed call is not', signature('x = Ping(1) + '), null);
check('a comma inside a string does not advance anything', signature('DO Ping WITH "a, b"'), 'PROCEDURE Ping(tcWho) @0');

// --- the disk cache ----------------------------------------------------------
// A restart must not re-read the tree. The records are cached, never the trees, and each is pinned to the file it came from.

const cacheDir = fs.mkdtempSync(path.join(os.tmpdir(), 'vfp-cache-'));
try {
	fs.writeFileSync(path.join(cacheDir, 'one.prg'), 'PROCEDURE One\nENDPROC\n');
	fs.writeFileSync(path.join(cacheDir, 'two.prg'), 'DO One\n');
	const first = await buildIndex({ roots: [cacheDir], yieldEvery: 0 });
	check('a fresh tree is parsed once', (await promoteToTier2(first)).parsed.length, 2);

	const cache = cachePath(cacheDir, [cacheDir]);
	saveCache(first, cache);

	const second = await buildIndex({ roots: [cacheDir], yieldEvery: 0 });
	check('the cache restores every unchanged file', loadCache(second, cache), 2);
	check('and nothing has to be parsed again', (await promoteToTier2(second)).parsed, []);

	// mtime is compared to the millisecond, so a rewrite has to be a visible one.
	await new Promise(resolve => setTimeout(resolve, 15));
	fs.writeFileSync(path.join(cacheDir, 'two.prg'), 'DO One\n? 1\n');
	const third = await buildIndex({ roots: [cacheDir], yieldEvery: 0 });
	check('a file changed since is not restored from the cache', loadCache(third, cache), 1);
	check('and it is the only one parsed again', (await promoteToTier2(third)).parsed.map(f => path.basename(f)), ['two.prg']);
	check('two trees do not share a cache file', cachePath(cacheDir, [cacheDir]) === cachePath(cacheDir, [cacheDir, 'elsewhere']), false);
} finally {
	fs.rmSync(cacheDir, { recursive: true, force: true });
}

// --- the macro guard ---------------------------------------------------------
// One function decides what every cross-file rule refuses, rather than each re-deriving the same evasion. The position matters: only DO and DO FORM accept the parenthesised runtime form, so an Identifier is a name written out everywhere else.

const node = (src: string) => (parse(src).body[0] as { expression?: unknown; target?: unknown });
const target = (src: string) => isDynamic(node(src).target as never, 'target');

check('a macro target is dynamic', target('DO &lcProc'), true);
check('a plain name is not', target('DO Foo'), false);
check('a path is not', target('DO lib\\foo.prg'), false);
check('the parenthesised runtime form is', target('DO (lcProc)'), true);
check('an assembled target is', target('DO "dir\\" + m.lcFile'), true);
check('a quoted name is not', target('DO "foo.prg"'), false);
check('the same identifier written as a name is not dynamic', isDynamic({ type: 'Identifier', name: 'Foo' } as never, 'name'), false);
check('a method call is', isDynamic({ type: 'MemberExpression' } as never, 'name'), true);
check('a macro callee is', isDynamic({ type: 'MacroSubstitute', name: 'f' } as never, 'name'), true);
check('a bare string carrying an ampersand is', isDynamic('pre&post', 'name'), true);
check('and nothing at all is', isDynamic(null), true);
check('a static name comes back as written', staticName({ type: 'Path', path: 'lib\\foo.prg' } as never), 'lib\\foo.prg');
check('a dynamic one comes back null', staticName({ type: 'MacroSubstitute', name: 'f' } as never), null);

// --- the built-in names ------------------------------------------------------
// FoxPro resolves an intrinsic before it looks for a user routine, so a routine named like one is unreachable and nothing may resolve to it. The framework has carried FUNCTION VARTYPE since the 1990s and legacy275 has PROCEDURE DATETIME beside it; measuring calls against those two was 73% of everything too-many-arguments reported.

check('the names a routine cannot take are known', [isBuiltin('VARTYPE'), isBuiltin('datetime'), isBuiltin('Error'), isBuiltin('MessageBox')], [true, true, true, true]);
check('an ordinary routine name is not one', [isBuiltin('PostCharge'), isBuiltin('LogEntry'), isBuiltin('Announce'), isBuiltin('')], [false, false, false, false]);
// The list is measured against, so a name that is not really a built-in would silence a real finding. These are the command words that have no function of the same name.
check('a command word alone does not make a built-in', [isBuiltin('REPLACE'), isBuiltin('SCATTER'), isBuiltin('LOCATE'), isBuiltin('THISFORM')], [false, false, false, false]);

// --- the exclude globs -------------------------------------------------------

check('a ** glob crosses directories', globToRegExp('**/node_modules/**').test('c:/src/app/node_modules/x/y.prg'), true);
check('it matches at the root too', globToRegExp('**/node_modules/**').test('node_modules/x.prg'), true);
check('a * stops at a separator', globToRegExp('*.prg').test('a/b.prg'), false);
check('an unrelated path is kept', globToRegExp('**/out/**').test('c:/src/app/outbound/x.prg'), false);

// The client watches one glob and the server indexes one list of extensions. If they drift the index quietly stops hearing about a whole kind of file, which nothing else here would catch.
const watcherGlob = /createFileSystemWatcher\('([^']+)'\)/.exec(read('./client/src/extension.ts'))?.[1];
check('the client watches exactly the extensions the server indexes',
	watcherGlob, `**/*.{${indexedExtensions.map(e => e.slice(1)).join(',')}}`);

report('Workspace checks');
