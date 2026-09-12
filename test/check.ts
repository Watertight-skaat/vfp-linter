// Minimal assertion helper shared by the suites that test structures rather than fixture diagnostics.
// `actual` and `expected` are deliberately `unknown`: the suites assert partial node shapes against location-stripped trees, so a generic signature would buy nothing but casts. The types worth having here are on the imports into server/src, not on the comparison.
const failures: string[] = [];
let checks = 0;

export function check(label: string, actual: unknown, expected: unknown): void {
	checks++;
	const a = JSON.stringify(actual);
	const e = JSON.stringify(expected);
	if (a !== e) failures.push(`${label}\n    expected ${e}\n    actual   ${a}`);
}

export function report(name: string): never {
	for (const failure of failures) console.log(`FAIL: ${failure}`);
	console.log(`\n${name}: ${checks - failures.length}/${checks} passed`);
	process.exit(failures.length > 0 ? 1 : 0);
}
