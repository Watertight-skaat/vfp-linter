/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as vscode from 'vscode';
import * as path from 'path';

export let doc: vscode.TextDocument;
export let editor: vscode.TextEditor;
export let documentEol: string;
export let platformEol: string;

/**
 * Activates the FoxPro extension and opens the given document.
 */
export async function activate(docUri: vscode.Uri) {
	// The extensionId is `publisher.name` from package.json
	const ext = vscode.extensions.getExtension('Watertight.foxpro-linter')!;
	await ext.activate();
	try {
		doc = await vscode.workspace.openTextDocument(docUri);
		editor = await vscode.window.showTextDocument(doc);
		await sleep(2000); // Wait for server activation
	} catch (e) {
		console.error(e);
	}
}

async function sleep(ms: number) {
	return new Promise(resolve => setTimeout(resolve, ms));
}

export const getDocPath = (p: string) => {
	return path.resolve(__dirname, '../../testFixture', p);
};
export const getDocUri = (p: string) => {
	return vscode.Uri.file(getDocPath(p));
};

export async function setTestContent(content: string): Promise<boolean> {
	const all = new vscode.Range(
		doc.positionAt(0),
		doc.positionAt(doc.getText().length)
	);
	return editor.edit(eb => eb.replace(all, content));
}

/** Polls until the document has exactly `count` diagnostics, so a test waits on the server rather than on a fixed sleep. */
export async function waitForDiagnostics(uri: vscode.Uri, count = 1, timeout = 20000): Promise<vscode.Diagnostic[]> {
	const deadline = Date.now() + timeout;
	for (;;) {
		const diagnostics = vscode.languages.getDiagnostics(uri);
		if (diagnostics.length === count) return diagnostics;
		if (Date.now() > deadline) throw new Error(`timed out waiting for ${count} diagnostics on ${uri.fsPath}, got ${diagnostics.length}`);
		await sleep(100);
	}
}
