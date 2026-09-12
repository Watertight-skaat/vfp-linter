// Asserts that unsupported syntax follows foxpro.unsupportedSyntaxSeverity while genuinely broken code does not.
// The fixture harness runs everything at 'error', so without this suite the default reporting level has no coverage.
import fs from 'fs';
import { lint, ruleDefaults, type LinterOptions, type SeverityName } from '../server/src/linter.js';
import pkg from '../package.json';
import { check, report } from './check.js';

// A statement the grammar does not cover, so it reaches UnknownStatement. Deliberately not a real FoxPro command: this suite tests the severity mapping, and it should not need editing every time the grammar learns another construct. test-files/diagnostics/still-unsupported.prg tracks the real ones.
const unsupported = 'ZZNOTACOMMAND 1\n';
// An unterminated block: the catch-all absorbs the opening line, so the parser never throws.
const unterminated = 'IF .T.\n? 1\n';
// A dangling terminator: a block terminator with nothing open for it to close. The parser absorbs it rather than throwing, so it is a rule's finding like any other.
const broken = 'ENDIF\n';

const codes = (src: string, options?: LinterOptions) => lint(src, options).diagnostics.map(d => `${d.severity} ${d.code}`);

check('unsupported syntax defaults to information', codes(unsupported), ['3 unsupported-syntax']);
check('it can be raised to error', codes(unsupported, { unsupportedSyntaxSeverity: 'error' }), ['1 unsupported-syntax']);
check('it can be a warning', codes(unsupported, { unsupportedSyntaxSeverity: 'warning' }), ['2 unsupported-syntax']);
check('it can be a hint', codes(unsupported, { unsupportedSyntaxSeverity: 'hint' }), ['4 unsupported-syntax']);
check('it can be turned off', codes(unsupported, { unsupportedSyntaxSeverity: 'off' }), []);

// An unterminated block is wrong rather than unsupported, so the setting must not be able to quiet it.
for (const severity of ['information', 'error', 'hint', 'off'] as const) {
	check(`an unterminated block stays an error at '${severity}'`,
		codes(unterminated, { unsupportedSyntaxSeverity: severity }), ['1 unterminated-block']);
}

// Broken code rather than syntax the grammar has not learned, so no setting can quiet it.
check('a dangling terminator is a syntax error', codes(broken, { rules: { 'unsupported-syntax': 'off' } }), ['1 syntax-error']);
check('the syntax error carries the position of the terminator', lint(broken).diagnostics[0].range.start, { line: 0, character: 0 });
check('it stays an error whatever the settings say', codes(broken, { rules: { 'syntax-error': 'off' }, unsupportedSyntaxSeverity: 'off' }), ['1 syntax-error']);
// The point of absorbing it rather than throwing: the rest of the file is still checked while the line is being typed.
check('the rest of the file is still checked', codes(broken + 'LOCAL lcUnused\n'), ['1 syntax-error', '2 unused-local']);

// --- per-rule severities -----------------------------------------------------
const twoRules = 'LOCAL lcUnused\nlnUndeclared = 1\n';
check('rules default', codes(twoRules), ['2 unused-local', '2 implicit-private']);
check('a rule can be lowered', codes(twoRules, { rules: { 'unused-local': 'hint' } }), ['4 unused-local', '2 implicit-private']);
check('a rule can be raised', codes(twoRules, { rules: { 'implicit-private': 'error' } }), ['2 unused-local', '1 implicit-private']);
check('a rule can be turned off', codes(twoRules, { rules: { 'unused-local': 'off' } }), ['2 implicit-private']);
check('an unknown code is ignored', codes(twoRules, { rules: { 'no-such-rule': 'error' } }), ['2 unused-local', '2 implicit-private']);
check('rules wins over the deprecated setting', codes(unsupported, { unsupportedSyntaxSeverity: 'error', rules: { 'unsupported-syntax': 'hint' } }), ['4 unsupported-syntax']);
check('the deprecated setting still applies on its own', codes(unsupported, { unsupportedSyntaxSeverity: 'warning', rules: {} }), ['2 unsupported-syntax']);
check('a locked rule ignores the settings', codes(unterminated, { rules: { 'unterminated-block': 'off', 'unsupported-syntax': 'off' } }), ['1 unterminated-block']);

// The settings schema in package.json must name exactly the rules that can be configured, with each default the rule declares.
const schema = pkg.contributes.configuration.properties['foxpro.rules'].properties as Record<string, { default: SeverityName }>;
check('package.json lists every configurable rule',
	Object.keys(schema).sort(), ruleDefaults.filter(r => !r.locked).map(r => r.code).sort());
check('package.json defaults match the rules',
	Object.fromEntries(Object.entries(schema).map(([code, p]) => [code, p.default])),
	Object.fromEntries(ruleDefaults.filter(r => !r.locked).map(r => [r.code, r.severity])));
// The README's rule table is the user-facing list, so it must name every rule too.
const readme = fs.readFileSync('./README.md', 'utf-8');
check('README documents every rule', ruleDefaults.map(r => r.code).filter(c => !readme.includes('`' + c + '`')), []);

// --- suppression comments ---------------------------------------------------
const lines = (src: string, options?: LinterOptions) => lint(src, options).diagnostics.map(d => `${d.range.start.line + 1} ${d.code}`);
check('disable-next-line silences one code on the next line',
	lines('LOCAL lcUnused\n* vfp-lint-disable-next-line implicit-private\nlnA = 1\nlnB = 2\n'), ['1 unused-local', '4 implicit-private']);
check('disable-next-line with no code silences everything on that line',
	lines('* vfp-lint-disable-next-line\nSELECT * FROM a, b\nlnB = 2\n'), ['3 implicit-private']);
check('disable-line silences the line it sits on',
	lines('lnA = 1 && vfp-lint-disable-line implicit-private -- deliberate\nlnB = 2\n'), ['2 implicit-private']);
check('a reason after -- is not read as a code',
	lines('lnA = 1 && vfp-lint-disable-line -- implicit-private is fine here\nlnB = 2\n'), ['2 implicit-private']);
check('disable / enable bracket a region',
	lines('lnA = 1\n* vfp-lint-disable implicit-private\nlnB = 2\nlnC = 3\n* vfp-lint-enable implicit-private\nlnD = 4\n'), ['1 implicit-private', '6 implicit-private']);
check('an unclosed disable runs to the end of the file',
	lines('* vfp-lint-disable\nlnB = 2\nSELECT * FROM a, b\n'), []);
check('a region for one code leaves the others alone',
	lines('* vfp-lint-disable implicit-private\nlnB = 2\nSELECT * FROM a\n'), ['3 select-without-into']);
check('codes are case-insensitive and may be comma separated',
	lines('* VFP-LINT-DISABLE-NEXT-LINE Implicit-Private,select-without-into\nlnB = 2\n'), []);
check('an unrelated comment is not a directive',
	lines('* disable the lint here please\nlnB = 2\n'), ['2 implicit-private']);

// Every block opener the grammar cannot match without its terminator.
const openers = [
	['IF .T.\n? 1\n', 'ENDIF'],
	['FOR lnI = 1 TO 3\n? lnI\n', 'ENDFOR or NEXT'],
	['DO CASE\nCASE .T.\n? 1\n', 'ENDCASE'],
	['TRY\n? 1\n', 'ENDTRY'],
	['WITH oX\n.a = 1\n', 'ENDWITH'],
	['DEFINE CLASS A AS B\nPROCEDURE P\nENDPROC\n', 'ENDDEFINE']
];
for (const [src, terminator] of openers) {
	const messages = lint(src).diagnostics
		.filter(d => d.code === 'unterminated-block')
		.map(d => d.message.slice(d.message.indexOf('missing ') + 8).split(':')[0]);
	check(`an unterminated ${terminator.split(' ')[0].slice(3)} block names its terminator`, messages, [terminator]);
}

// The same blocks, closed properly, must stay silent.
const closed = [
	'IF .T.\n? 1\nENDIF\n',
	'IF .T.\n? 1\nELSE\n? 2\nENDIF\n',
	// The loop variables are declared so that "clean" can keep meaning no diagnostic at all: an undeclared one is a real implicit-private finding and has nothing to do with block terminators.
	'LOCAL lnI\nFOR lnI = 1 TO 3\n? lnI\nENDFOR\n',
	'LOCAL lnI\nFOR lnI = 1 TO 3\n? lnI\nNEXT\n',
	'LOCAL x\nFOR EACH x IN y\n? x\nENDFOR\n',
	'DO WHILE .T.\n? 1\nENDDO\n',
	'DO CASE\nCASE .T.\n? 1\nOTHERWISE\n? 2\nENDCASE\n',
	// Declared for the same reason as the loop variables above: CATCH TO creates the variable, so an undeclared one is a real implicit-private finding and not a block-terminator problem.
	'LOCAL oErr\nTRY\n? 1\nCATCH TO oErr\n? 2\nENDTRY\n',
	'TRY\n? 1\nFINALLY\n? 2\nENDTRY\n',
	'WITH oX\n.a = 1\nENDWITH\n',
	'DEFINE CLASS A AS B\nPROCEDURE P\nENDPROC\nENDDEFINE\n',
	'SCAN\n? 1\nENDSCAN\n'
];
for (const src of closed) {
	check(`a closed block is clean: ${JSON.stringify(src.replace(/\n/g, ' / '))}`, codes(src), []);
}

report('Severity checks');
