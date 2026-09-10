// Asserts that unsupported syntax follows foxpro.unsupportedSyntaxSeverity while genuinely broken code does not.
// The fixture harness runs everything at 'error', so without this suite the default reporting level has no coverage.
const parser = require('../server/src/parser.js');
const { runLinterRules } = require('../server/src/linter.ts');
const { check, report } = require('./check.js');

// Valid FoxPro the grammar does not cover, so it reaches UnknownStatement.
const unsupported = 'THROW "boom"\n';
// An unterminated block: the catch-all absorbs the opening line, so the parser never throws.
const unterminated = 'IF .T.\n? 1\n';
// A dangling terminator, which UnknownStatement refuses to match, so the parser does throw.
const broken = 'ENDIF\n';

const lint = (src, options) => runLinterRules(parser.parse(src), options).map(d => `${d.severity} ${d.code}`);

check('unsupported syntax defaults to information', lint(unsupported), ['3 unsupported-syntax']);
check('it can be raised to error', lint(unsupported, { unsupportedSyntaxSeverity: 'error' }), ['1 unsupported-syntax']);
check('it can be a warning', lint(unsupported, { unsupportedSyntaxSeverity: 'warning' }), ['2 unsupported-syntax']);
check('it can be a hint', lint(unsupported, { unsupportedSyntaxSeverity: 'hint' }), ['4 unsupported-syntax']);
check('it can be turned off', lint(unsupported, { unsupportedSyntaxSeverity: 'off' }), []);

// An unterminated block is wrong rather than unsupported, so the setting must not be able to quiet it.
for (const severity of ['information', 'error', 'hint', 'off']) {
	check(`an unterminated block stays an error at '${severity}'`,
		lint(unterminated, { unsupportedSyntaxSeverity: severity }), ['1 unterminated-block']);
}

// A real syntax error never reaches the rules at all: the parser throws and the server reports it.
let threw = false;
try {
	parser.parse(broken);
} catch {
	threw = true;
}
check('a dangling terminator fails the parse', threw, true);

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
	const messages = runLinterRules(parser.parse(src))
		.filter(d => d.code === 'unterminated-block')
		.map(d => d.message.slice(d.message.indexOf('missing ') + 8).split(':')[0]);
	check(`an unterminated ${terminator.split(' ')[0].slice(3)} block names its terminator`, messages, [terminator]);
}

// The same blocks, closed properly, must stay silent.
const closed = [
	'IF .T.\n? 1\nENDIF\n',
	'IF .T.\n? 1\nELSE\n? 2\nENDIF\n',
	'FOR lnI = 1 TO 3\n? lnI\nENDFOR\n',
	'FOR lnI = 1 TO 3\n? lnI\nNEXT\n',
	'FOR EACH x IN y\n? x\nENDFOR\n',
	'DO WHILE .T.\n? 1\nENDDO\n',
	'DO CASE\nCASE .T.\n? 1\nOTHERWISE\n? 2\nENDCASE\n',
	'TRY\n? 1\nCATCH TO oErr\n? 2\nENDTRY\n',
	'TRY\n? 1\nFINALLY\n? 2\nENDTRY\n',
	'WITH oX\n.a = 1\nENDWITH\n',
	'DEFINE CLASS A AS B\nPROCEDURE P\nENDPROC\nENDDEFINE\n',
	'SCAN\n? 1\nENDSCAN\n'
];
for (const src of closed) {
	check(`a closed block is clean: ${JSON.stringify(src.replace(/\n/g, ' / '))}`, lint(src), []);
}

report('Severity checks');
