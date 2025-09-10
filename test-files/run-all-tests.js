/* global console process */
import * as parser from '../server/src/parser.js';
import * as fs from 'fs';
import { runLinterRules } from '../server/out/linter.js';

// for each .prg in this directory, run the linter and output results
const files = fs.readdirSync('./test-files').filter(f => f.endsWith('.prg'));
let successes = 0;
let failedTests = [];
for (const file of files) {
	const src = fs.readFileSync('./test-files/' + file, 'utf-8');
	try {
		const ast = parser.parse(src, { grammarSource: file });
		const diagnostics = runLinterRules(ast);
		const errors = diagnostics.filter(d => d.severity == 1 || d.severity === 'error');
		if (errors.length > 0) {
			const messages = errors.map(e =>`LINTER: ${e.message} (line ${e.range.start.line + 1})`);
			failedTests.push([file, messages]);
		} else {
			successes++;
		}
	} catch (e) {
		failedTests.push([file, [`PARSE Error: ${e.message || e.toString()}`]]);
		if (e && typeof e.format === 'function') {
			console.error(e.format([{ source: file, text: src }]));
		} else {
			console.error(e.toString ? e.toString() : e);
		}
	}
}
for (const failure of failedTests) {
	console.log(`==== ${failure[0]} (x${failure[1].length}) ====`);
	for (const msg of failure[1])
		console.log(msg);
}

console.log(`\nSuccesses: ${successes}\nFailures: ${failedTests.length}`);
process.exit(failedTests > 0 ? 1 : 0);