/* global console */
import * as parser from '../server/out/parser.js';
import * as fs from 'fs';
import { runLinterRules } from '../server/out/linter.js';

// for each .prg in this directory, run the linter and output results
const files = fs.readdirSync('./test-files').filter(f => f.endsWith('.prg'));
let successes = 0;
let failures = 0;
for (const file of files) {
	const src = fs.readFileSync('./test-files/' + file, 'utf-8');
	console.log(`\n=== ${file} ===`);
	try {
		const ast = parser.parse(src, { grammarSource: file });
		const diagnostics = runLinterRules(ast);
		const firstError = diagnostics.find(d => d.severity == 1 || d.severity === 'error');
		if (firstError) {
			console.error(`LINTER: ${firstError.message} (line ${firstError.range.start.line + 1})`);
			failures++;
		} else {
			console.log('PARSED AST OK');
			successes++;
		}
	} catch (e) {
		console.error('PARSE ERROR');
		failures++;
		if (e && typeof e.format === 'function') {
			console.error(e.format([{ source: file, text: src }]));
		} else {
			console.error(e.toString ? e.toString() : e);
		}
	}
}
console.log(`Successes: ${successes}, Failures: ${failures}`);