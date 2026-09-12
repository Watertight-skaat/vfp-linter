// The only test that proves the workspace index reaches the editor: everything else drives lint() and the index directly, in process.
// It needs the fixture folder open as a workspace, which runTest.ts passes on the command line -- with no folder there is nothing to crawl and every request below correctly answers nothing.

import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate } from './helper';

const callerUri = getDocUri('workspace-caller.prg');
const libraryUri = getDocUri('workspace-lib.prg');

suite('Should navigate across files', () => {
	test('Go to Definition jumps from a DO to the routine in another file', async () => {
		await activate(callerUri);
		const found = await vscode.commands.executeCommand<vscode.Location[]>('vscode.executeDefinitionProvider', callerUri, new vscode.Position(1, 5));

		assert.strictEqual(found.length, 1, 'expected exactly one definition');
		assert.strictEqual(found[0].uri.fsPath, libraryUri.fsPath);
		assert.strictEqual(found[0].range.start.line, 3, 'the PROCEDURE line');
	});

	test('Hover shows the signature and the comment block above the routine', async () => {
		await activate(callerUri);
		const hovers = await vscode.commands.executeCommand<vscode.Hover[]>('vscode.executeHoverProvider', callerUri, new vscode.Position(1, 5));

		const text = hovers.flatMap(h => h.contents.map(c => (typeof c === 'string' ? c : c.value))).join('\n');
		assert.ok(text.includes('PROCEDURE AuditLine(tcMessage, tnLevel)'), `signature missing from: ${text}`);
		assert.ok(text.includes('Writes one line to the audit trail.'), `docstring missing from: ${text}`);
	});

	test('Go to Symbol in Workspace finds a routine by name', async () => {
		await activate(callerUri);
		const symbols = await vscode.commands.executeCommand<vscode.SymbolInformation[]>('vscode.executeWorkspaceSymbolProvider', 'AuditLine');

		const mine = symbols.filter(s => s.name === 'AuditLine');
		assert.strictEqual(mine.length, 1, `expected one AuditLine, got ${symbols.map(s => s.name).join(', ')}`);
		assert.strictEqual(mine[0].location.uri.fsPath, libraryUri.fsPath);
	});
});
