import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate, waitForDiagnostics, doc, setTestContent } from './helper';

const codeActions = (uri: vscode.Uri, range: vscode.Range) =>
	vscode.commands.executeCommand<vscode.CodeAction[]>('vscode.executeCodeActionProvider', uri, range);

suite('Should get quick fixes', () => {
	const docUri = getDocUri('features.prg');

	test('Offers the rule\'s own fix and the suppression beside it', async () => {
		await activate(docUri);
		const [diagnostic] = await waitForDiagnostics(docUri);
		assert.strictEqual(diagnostic.code, 'implicit-private');

		const actions = await codeActions(docUri, diagnostic.range);
		assert.deepStrictEqual(actions.map(a => a.title).sort(), ['Declare \'lnCount\' as LOCAL', 'Suppress implicit-private on this line']);
		const fix = actions.find(a => a.title.startsWith('Declare'))!;
		assert.strictEqual(fix.kind?.value, vscode.CodeActionKind.QuickFix.value);
		assert.strictEqual(fix.isPreferred, true);
	});

	test('Applying the fix declares the LOCAL and clears the finding', async () => {
		await activate(docUri);
		const [diagnostic] = await waitForDiagnostics(docUri);
		const before = doc.getText();

		const actions = await codeActions(docUri, diagnostic.range);
		assert.strictEqual(await vscode.workspace.applyEdit(actions.find(a => a.title.startsWith('Declare'))!.edit!), true);
		assert.ok(/^\tLOCAL lnCount$/m.test(doc.getText()), doc.getText());
		await waitForDiagnostics(docUri, 0);

		// Put the fixture back: the Outline suite reads the same file, and the edit is only in the editor's buffer.
		await setTestContent(before);
		await waitForDiagnostics(docUri, 1);
	});

	test('Applying the suppression silences the line', async () => {
		await activate(docUri);
		const [diagnostic] = await waitForDiagnostics(docUri);
		const before = doc.getText();

		const actions = await codeActions(docUri, diagnostic.range);
		assert.strictEqual(await vscode.workspace.applyEdit(actions.find(a => a.title.startsWith('Suppress'))!.edit!), true);
		assert.ok(doc.getText().includes('* vfp-lint-disable-next-line implicit-private'), doc.getText());
		await waitForDiagnostics(docUri, 0);

		await setTestContent(before);
		await waitForDiagnostics(docUri, 1);
	});
});
