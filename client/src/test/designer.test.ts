// The editor half of opening a designer: that the command exists, that a form opens in the view built for it rather than as a binary file, and that the hover over a form's name offers the link that runs the command.
// What happens after the link is pressed is Visual FoxPro's, and test/run-vfp-tests.ts covers the text sent to it.

import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate } from './helper';

const formUri = getDocUri('DESIGNER.SCX');
const callerUri = getDocUri('workspace-form.prg');

suite('Should open a designer document', () => {
	test('The command is registered', async () => {
		await activate(callerUri);
		const commands = await vscode.commands.getCommands(true);
		assert.ok(commands.includes('foxpro.openInVfp'), 'foxpro.openInVfp is not registered');
	});

	// The name is upper case, as VFP writes it and as the Watertight tree holds it, because the pattern that claims the file is matched against a lower-cased name.
	test('A form opens in the designer view rather than as a binary file', async () => {
		await activate(callerUri);
		await vscode.commands.executeCommand('vscode.open', formUri);

		const tab = vscode.window.tabGroups.activeTabGroup.activeTab;
		assert.ok(tab?.input instanceof vscode.TabInputCustom, `expected a custom editor, got ${tab?.input?.constructor.name}`);
		assert.strictEqual((tab.input as vscode.TabInputCustom).viewType, 'foxpro.designerDocument');
		assert.strictEqual((tab.input as vscode.TabInputCustom).uri.fsPath, formUri.fsPath);
		await vscode.commands.executeCommand('workbench.action.closeActiveEditor');
	});

	test('Hovering the name of a form offers to open it', async function () {
		// The link runs Visual FoxPro, so it is only offered where Visual FoxPro can run.
		if (process.platform !== 'win32') return this.skip();
		await activate(callerUri);
		const hovers = await vscode.commands.executeCommand<vscode.Hover[]>('vscode.executeHoverProvider', callerUri, new vscode.Position(1, 10));

		const text = hovers.flatMap(h => h.contents.map(c => (typeof c === 'string' ? c : c.value))).join('\n');
		assert.ok(text.includes('DESIGNER.SCX'), `the form's path is missing from: ${text}`);
		assert.ok(text.includes('(command:foxpro.openInVfp?'), `the link is missing from: ${text}`);
	});
});
