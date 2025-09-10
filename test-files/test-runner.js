/* global console process */
import * as parser from '../server/src/parser.js';
import * as fs from 'fs';
import { runLinterRules } from '../server/out/linter.js';

// for each .prg in this directory, run the linter and output results
const files = fs.readdirSync('./test-files').filter(f => f.endsWith('.prg'));
let successes = 0;
let failures = [];
for (const file of files) {
	const src = fs.readFileSync('./test-files/' + file, 'utf-8');
	try {
		const ast = parser.parse(src, { grammarSource: file });
		const diagnostics = runLinterRules(ast);
		const firstError = diagnostics.find(d => d.severity == 1 || d.severity === 'error');
		if (firstError) {
			const message= `LINTER: ${firstError.message} (line ${firstError.range.start.line + 1})`;
			failures.push([file, message]);
		} else {
			successes++;
		}
	} catch (e) {
		failures.push([file, `PARSE Error: ${e.message || e.toString()}`]);
		if (e && typeof e.format === 'function') {
			console.error(e.format([{ source: file, text: src }]));
		} else {
			console.error(e.toString ? e.toString() : e);
		}
	}
}
for (const failure of failures) {
	console.log(`=== ${failure[0]} ===`);
	console.log(failure[1]);
}

console.log(`\nSuccesses: ${successes}\nFailures: ${failures.length}`);
process.exit(failures > 0 ? 1 : 0);