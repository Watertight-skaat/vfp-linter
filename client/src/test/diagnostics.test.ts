import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate } from './helper';

suite('Should get diagnostics', () => {
	const docUri = getDocUri('diagnostics.prg');

	test('Reports an unsupported statement in a .prg file', async () => {
		await activate(docUri);

		const diagnostics = vscode.languages.getDiagnostics(docUri);

		assert.strictEqual(diagnostics.length, 1, `expected 1 diagnostic, got ${diagnostics.length}`);
		assert.strictEqual(diagnostics[0].source, 'VFP Linter');
		assert.strictEqual(diagnostics[0].code, 'unsupported-syntax');
		// Advisory by default: valid FoxPro the grammar has not learned is not an error.
		assert.strictEqual(diagnostics[0].severity, vscode.DiagnosticSeverity.Information);
		assert.strictEqual(diagnostics[0].range.start.line, 5);
	});

	test('Reports nothing for a file that is entirely valid', async () => {
		const cleanUri = getDocUri('clean.prg');
		await activate(cleanUri);

		assert.deepStrictEqual(vscode.languages.getDiagnostics(cleanUri), []);
	});
});
