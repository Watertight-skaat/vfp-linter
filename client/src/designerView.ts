// What the editor shows for a file it cannot read.
//
// A form, a class library and a report are DBF tables with a memo file beside them, so opening one in VS Code gets the binary-file placeholder and a button offering to show it anyway. This replaces that with the one thing there is to say about such a file -- what it is, where it is, and that Visual FoxPro is what opens it.

import { CancellationToken, CustomDocument, CustomDocumentOpenContext, CustomReadonlyEditorProvider, ExtensionContext, Uri, WebviewPanel, commands, window } from 'vscode';
import { designerFor, type Designer } from '../../server/src/vfp.js';

const names: Record<Designer, string> = {
	form: 'Form',
	class: 'Class library',
	report: 'Report',
	label: 'Label',
	menu: 'Menu',
	database: 'Database',
	project: 'Project'
};

export const viewType = 'foxpro.designerDocument';

export function registerDesignerView(context: ExtensionContext) {
	const provider: CustomReadonlyEditorProvider = {
		openCustomDocument(uri: Uri, _context: CustomDocumentOpenContext, _token: CancellationToken): CustomDocument {
			return { uri, dispose: () => { /* nothing is held open: the file is never read */ } };
		},
		resolveCustomEditor(document: CustomDocument, panel: WebviewPanel): void {
			panel.webview.options = { enableScripts: true };
			panel.webview.html = page(document.uri);
			panel.webview.onDidReceiveMessage(() => commands.executeCommand('foxpro.openInVfp', document.uri), undefined, context.subscriptions);
		}
	};
	context.subscriptions.push(window.registerCustomEditorProvider(viewType, provider, { webviewOptions: { retainContextWhenHidden: false } }));
}

function page(uri: Uri): string {
	const kind = names[designerFor(uri.fsPath) ?? 'form'];
	const name = uri.path.split('/').pop() ?? '';
	// The path is the only untrusted text here, and it is put in through textContent rather than into the markup.
	const nonce = Math.random().toString(36).slice(2);
	return `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; script-src 'nonce-${nonce}';">
<style>
	body { font-family: var(--vscode-font-family); color: var(--vscode-foreground); padding: 2rem 2.5rem; }
	h1 { font-size: 1.3rem; font-weight: 600; margin: 0 0 .25rem; }
	p { color: var(--vscode-descriptionForeground); margin: .35rem 0; }
	#path { font-family: var(--vscode-editor-font-family); font-size: .85rem; word-break: break-all; }
	button { font-family: inherit; font-size: inherit; margin-top: 1.5rem; padding: .45rem 1rem; border: none; cursor: pointer;
		color: var(--vscode-button-foreground); background: var(--vscode-button-background); }
	button:hover { background: var(--vscode-button-hoverBackground); }
</style>
</head>
<body>
	<h1 id="name"></h1>
	<p id="kind"></p>
	<p id="path"></p>
	<button id="open">Open in Visual FoxPro</button>
	<p>Visual FoxPro keeps a ${kind.toLowerCase()} in its own format, which this editor can show by name only. Its designer opens in the Visual FoxPro you already have running.</p>
	<script nonce="${nonce}">
		const vscode = acquireVsCodeApi();
		document.getElementById('name').textContent = ${JSON.stringify(name)};
		document.getElementById('kind').textContent = ${JSON.stringify(kind)};
		document.getElementById('path').textContent = ${JSON.stringify(uri.fsPath)};
		document.getElementById('open').addEventListener('click', () => vscode.postMessage('open'));
	</script>
</body>
</html>`;
}
