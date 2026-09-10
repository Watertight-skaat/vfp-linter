// Minimal assertion helper shared by the suites that test structures rather than fixture diagnostics.
const failures = [];
let checks = 0;

function check(label, actual, expected) {
	checks++;
	const a = JSON.stringify(actual);
	const e = JSON.stringify(expected);
	if (a !== e) failures.push(`${label}\n    expected ${e}\n    actual   ${a}`);
}

function report(name) {
	for (const failure of failures) console.log(`FAIL: ${failure}`);
	console.log(`\n${name}: ${checks - failures.length}/${checks} passed`);
	process.exit(failures.length > 0 ? 1 : 0);
}

module.exports = { check, report };
