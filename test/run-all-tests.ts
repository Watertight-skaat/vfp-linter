// Diffs every fixture's diagnostics against a recorded expectation, so a rule that is supposed to fire can be regression-tested and a severity change shows up in review.
// A fixture with no `.expected` file must produce no diagnostics at all. Run `bun run test:update` to rewrite the expectations, then review the diff.
import fs from 'fs';
import { lint, type LinterOptions } from '../server/src/linter.js';
import { format } from './format.js';

// This harness is also a grammar-coverage probe, so it asks for the strict reading: a statement the grammar cannot parse is an error here, even though users get it as advisory information.
const strict: LinterOptions = { unsupportedSyntaxSeverity: 'error' };
const update = process.argv.includes('--update');

// Every .prg under test-files/ is a fixture. The top level and test-files/watertight/ are corpora that must parse cleanly; the diagnostics/ directories hold fixtures written to make a rule fire, or to record a construct the grammar cannot read yet. test-files/workspace/ is run-workspace-tests.ts's, whose cases only mean something with an index over their directory.
const collect = (dir: string): string[] => fs.readdirSync(dir, { withFileTypes: true }).flatMap(entry =>
	entry.isDirectory() ? (entry.name === 'workspace' ? [] : collect(`${dir}/${entry.name}`)) : entry.name.endsWith('.prg') ? [`${dir}/${entry.name}`] : []
);
const fixtures = collect('./test-files');

// The same entry point the server calls, so a parse failure is recorded through the same path the editor shows it. A related location is a line of its own, so the diff below compares line by line either way.
const diagnose = (file: string) => lint(fs.readFileSync(file, 'utf-8'), strict).diagnostics.flatMap(d => format(d).split('\n').map(l => l.trim()));

const failures: [string, string[]][] = [];
let passed = 0;
let recorded = 0;

for (const file of fixtures) {
	const expectedPath = `${file}.expected`;
	const actual = diagnose(file);

	if (update) {
		if (actual.length) {
			fs.writeFileSync(expectedPath, actual.join('\n') + '\n');
			recorded += actual.length;
		} else if (fs.existsSync(expectedPath)) {
			fs.unlinkSync(expectedPath);
		}
		continue;
	}

	const expected = fs.existsSync(expectedPath)
		? fs.readFileSync(expectedPath, 'utf-8').split('\n').map(l => l.trim()).filter(Boolean)
		: [];

	const diff: string[] = [];
	for (let i = 0; i < Math.max(actual.length, expected.length); i++) {
		if (actual[i] === expected[i]) continue;
		if (expected[i] === undefined) diff.push(`  + ${actual[i]}`);
		else if (actual[i] === undefined) diff.push(`  - ${expected[i]}`);
		else diff.push(`  - ${expected[i]}\n  + ${actual[i]}`);
	}
	if (diff.length) failures.push([file, diff]);
	else passed++;
}

if (update) {
	console.log(`Recorded ${recorded} expected diagnostics across ${fixtures.length} fixtures.`);
	process.exit(0);
}

for (const [file, diff] of failures) {
	console.log(`==== ${file} ====`);
	console.log(diff.join('\n'));
}
console.log(`\nFixtures: ${passed}/${fixtures.length} match their expected diagnostics`);
if (failures.length) console.log('If the change is intended, run `bun run test:update` and review the diff.');
process.exit(failures.length > 0 ? 1 : 0);
