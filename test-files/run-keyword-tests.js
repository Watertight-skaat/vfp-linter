// None of FoxPro's command words is reserved: CLEAR, LIST, SEEK, COUNT and the rest are all legal variable and function names. A statement rule whose keyword literal has no word boundary therefore matches the start of a longer identifier -- "DO"i swallows the DO in DoSomething() -- and because the statement rules sit above AssignmentStatement, the wrong one wins silently.
// This re-derives every keyword literal from the grammar and probes each one as an identifier prefix, so adding a command word without a boundary fails the build rather than the user's file.
const fs = require('fs');
const parser = require('../server/src/parser.js');
const { check, report } = require('./check.js');

const grammar = fs.readFileSync('./server/src/foxpro.pegjs', 'utf-8');

// Every single-word case-insensitive literal the grammar matches. Multi-word literals ("SET ORDER TO") cannot start an identifier, and the character classes are not keywords.
const literals = [...new Set([...grammar.matchAll(/"([A-Za-z][A-Za-z0-9_]*)"i/g)].map(m => m[1].toUpperCase()))].sort();

const parseType = src => {
	try {
		const body = parser.parse(src).body;
		const first = Array.isArray(body[0]) ? body[0][0] : body[0];
		return first ? first.type : 'empty';
	} catch (e) {
		return 'syntax-error';
	}
};

// Three statement shapes, because each reaches a different part of the Statement list. A bare call is the one that matters: an assignment is decided by AssignmentStatement, which sits above most command rules and hides a missing boundary below it, while a bare call has to get past every one of them.
const assigned = [];
const called = [];
const printed = [];
for (const word of literals) {
	const name = word + 'zzz';
	if (parseType(`${name} = 1`) !== 'Assignment') assigned.push(word);
	if (parseType(`${name}(1)`) !== 'ExpressionStatement') called.push(word);
	if (parseType(`? ${name}`) !== 'PrintStatement') printed.push(word);
}

check(`all ${literals.length} keyword literals allow an assignment to a name starting with them`, assigned, []);
check('a bare call whose name starts with one', called, []);
check('and a name starting with one as an expression', printed, []);

// The boundary has to hold in the other direction too: the command itself must still parse.
check('the command words themselves still parse', [
	parseType('CLEAR ALL'), parseType('FLUSH FORCE'), parseType('SCATTER MEMVAR'),
	parseType('GATHER MEMVAR'), parseType('COUNT TO lnN'), parseType('CONTINUE'),
	parseType('NODEFAULT'), parseType('REINDEX'), parseType('MD datalog')
], [
	'ClearStatement', 'FlushStatement', 'ScatterStatement',
	'GatherStatement', 'AggregateStatement', 'ContinueLocateStatement',
	'NoDefaultStatement', 'ReindexStatement', 'DirectoryStatement'
]);

report('Keyword boundary checks');
