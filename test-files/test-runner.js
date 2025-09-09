/* global console */
import * as parser from '../server/src/parser.js';
import * as fs from 'fs';
import { runLinterRules } from '../server/out/server.js';

// for each .prg in this directory, run the linter and output results
const testDir = './';
const files = fs.readdirSync(testDir).filter(f => f.endsWith('.prg'));
let successes = 0;
let failures = 0;
for (const file of files) {
	const src = fs.readFileSync(testDir + file, 'utf-8');
	console.log(`\n=== ${file} ===`);
	try {
		const ast = parser.parse(src, { grammarSource: file });
		const diagnostics = runLinterRules(ast);
		const firstError = diagnostics.find(d => d.severity == 1);
		if (firstError)
			throw new Error(`Linter found ${diagnostics.length} problems, first: ${firstError.message} at line ${firstError.range.start.line + 1}`);
		console.log('PARSED AST OK');
		successes++;
	} catch (e) {
		console.error('PARSE ERROR');
		failures++;
		if (e && typeof e.format === 'function') {
			console.error(e.format([{ source: file, text: src }]));
		} else {
			console.error(e);
		}
	}
	console.log(`Successes: ${successes}, Failures: ${failures}`);
}