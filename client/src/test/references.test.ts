// Find all references, completion and signature help over the protocol. A call is only visible to the parser, so these are also the proof that the background read of the whole tree finishes and that the request waits for it.

import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate } from './helper';

const callerUri = getDocUri('workspace-caller.prg');
const libraryUri = getDocUri('workspace-lib.prg');

suite('Should find references across the workspace', () => {
	test('A routine lists its declaration and every call of it', async () => {
		await activate(callerUri);
		const found = await vscode.commands.executeCommand<vscode.Location[]>('vscode.executeReferenceProvider', callerUri, new vscode.Position(1, 5));

		const sites = found.map(l => `${l.uri.fsPath.split(/[\\/]/).pop()}:${l.range.start.line}`).sort();
		assert.deepStrictEqual(sites, ['workspace-caller.prg:1', 'workspace-lib.prg:3']);
	});
});

suite('Should complete names from the workspace', () => {
	test('A routine defined in another file is offered', async () => {
		await activate(callerUri);
		const list = await vscode.commands.executeCommand<vscode.CompletionList>('vscode.executeCompletionItemProvider', callerUri, new vscode.Position(1, 5));

		const mine = list.items.filter(i => (typeof i.label === 'string' ? i.label : i.label.label) === 'AuditLine');
		assert.strictEqual(mine.length, 1, `AuditLine missing from ${list.items.length} items`);
		assert.strictEqual(mine[0].kind, vscode.CompletionItemKind.Function);
	});
});

suite('Should offer signature help', () => {
	test('Typing the arguments of a DO names the routine and the parameter', async () => {
		await activate(libraryUri);
		// `DO AuditLine WITH "opened", ` -- the cursor sits in the second argument.
		const help = await vscode.commands.executeCommand<vscode.SignatureHelp>(
			'vscode.executeSignatureHelpProvider', callerUri, new vscode.Position(1, 28));

		assert.ok(help, 'no signature help returned');
		assert.strictEqual(help.signatures[0].label, 'PROCEDURE AuditLine(tcMessage, tnLevel)');
		assert.strictEqual(help.activeParameter, 1);
	});
});
