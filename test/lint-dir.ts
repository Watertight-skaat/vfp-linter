// Runs the linter over a whole directory with a real workspace index behind it, and prints what it finds.
// This is how a rule is judged before it ships: a cross-file rule cannot be trusted on fixtures alone, because the thing that decides whether it is usable is how often it fires on a real tree. TODO.md asks for exactly this discipline for the SET EXACT rule, and every rule in phase 4 of the workspace plan is gated on a run of it.
//
//   bun run lint:dir <directory> [--rule code] [--severity error|warning|information|hint] [--tier 1|2] [--quiet]
//
// --quiet prints only the per-rule counts, which is the number that decides whether a rule is shippable.

import fs from 'fs';
import path from 'path';
import { lint, type LinterOptions, type SeverityName } from '../server/src/linter.js';
import { buildIndex, isIndexed, readRecord } from '../server/src/workspace.js';
import { format } from './format.js';

// The flags that take a value, so the directory is not mistaken for one of their arguments.
const valued = new Set(['rule', 'severity', 'tier']);
const args = process.argv.slice(2);
const positional: string[] = [];
for (let i = 0; i < args.length; i++) {
	if (!args[i].startsWith('--')) { positional.push(args[i]); continue; }
	if (valued.has(args[i].slice(2))) i++;
}
const flag = (name: string) => {
	const at = args.indexOf(`--${name}`);
	return at < 0 ? undefined : args[at + 1];
};
const dir = positional[0];

if (!dir || !fs.existsSync(dir)) {
	console.error('Usage: bun run lint:dir <directory> [--rule code] [--severity level] [--tier 1|2] [--quiet]');
	process.exit(2);
}

const onlyRule = flag('rule');
const onlySeverity = flag('severity');
const tier = flag('tier') === '2' ? 2 : 1;
const quiet = args.includes('--quiet');

const started = Date.now();
const index = await buildIndex({ roots: [dir], yieldEvery: 0 });
const crawled = Date.now() - started;

// The whole tree at tier 2 is the slow path, and only the rules that read calls need it.
if (tier === 2) for (const record of [...index.files.values()]) {
	const upgraded = readRecord(record.file, 2);
	if (upgraded) index.upsert(upgraded);
}
const indexed = Date.now() - started;

const severityNames: Record<number, SeverityName> = { 1: 'error', 2: 'warning', 3: 'information', 4: 'hint' };
const counts = new Map<string, number>();
const files = [...index.files.values()].map(r => r.file).filter(isIndexed).sort();
let reported = 0;
let failed = 0;

for (const file of files) {
	let text: string;
	try {
		text = fs.readFileSync(file, 'utf-8');
	} catch {
		failed++;
		continue;
	}
	const options: LinterOptions = { workspace: { index, file } };
	const found = lint(text, options).diagnostics.filter(d =>
		(!onlyRule || d.code === onlyRule) && (!onlySeverity || severityNames[d.severity] === onlySeverity));
	for (const diagnostic of found) counts.set(diagnostic.code, (counts.get(diagnostic.code) ?? 0) + 1);
	reported += found.length;
	if (!quiet && found.length) {
		console.log(`\n${path.relative(dir, file)}`);
		for (const diagnostic of found) console.log(`  ${format(diagnostic)}`);
	}
}

const linted = Date.now() - started;
console.log(`\n${files.length} files, ${reported} findings.`);
for (const [code, count] of [...counts].sort((a, b) => b[1] - a[1])) console.log(`  ${String(count).padStart(6)}  ${code}`);
if (failed) console.log(`  ${failed} files could not be read.`);
console.log(`\ncrawl ${crawled} ms, index ${indexed - crawled} ms (tier ${tier}), lint ${linted - indexed} ms.`);
