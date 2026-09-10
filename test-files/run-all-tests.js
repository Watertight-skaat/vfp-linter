const parser = require('../server/src/parser.js');
const fs = require('fs');
const { runLinterRules } = require('../server/src/linter.ts');

// This harness is a grammar-coverage probe, so it asks for the strict reading: a statement the grammar cannot parse is an error here, even though users get it as advisory information.
const strict = { unsupportedSyntaxSeverity: 'error' };

// for each .prg in this directory, run the linter and output results
const files = fs.readdirSync('./test-files').filter(f => f.endsWith('.prg'));
let successes = 0;
const failedTests = [];
for (const file of files) {
	const src = fs.readFileSync('./test-files/' + file, 'utf-8');
	try {
		const ast = parser.parse(src, { grammarSource: file });
		const diagnostics = runLinterRules(ast, strict);
		const errors = diagnostics.filter(d => d.severity == 1 || d.severity === 'error');
		if (errors.length > 0) {
			const messages = errors.map(e =>`LINTER: ${e.message} (line ${e.range.start.line + 1})`);
			failedTests.push([file, messages]);
		} else {
			successes++;
		}
	} catch (e) {
		failedTests.push([file, [`PARSE Error: ${e.message || e.toString()}`]]);
	}
}
for (const failure of failedTests) {
	console.log(`==== ${failure[0]} (x${failure[1].length}) ====`);
	for (const msg of failure[1])
		console.log(msg);
}

console.log(`\nSuccesses: ${successes}\nFailures: ${failedTests.length}`);
process.exit(failedTests.length > 0 ? 1 : 0);