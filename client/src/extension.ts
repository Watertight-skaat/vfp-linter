import * as path from 'path';
import { commands, ExtensionContext, ProgressLocation, Uri, window, workspace } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';
import { classListExpression, designerFor, vfpCommandFor } from '../../server/src/vfp.js';
import { registerDesignerView } from './designerView.js';
import { findVfpExe, runVfp, startupCommands, type VfpResult } from './vfp.js';

let client: LanguageClient;
let started: Promise<void>;

export function activate(context: ExtensionContext) {
	const serverModule = context.asAbsolutePath(
		path.join('server', 'out', 'server.js')
	);

	// If the extension is launched in debug mode then the debug server options are used, otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: { module: serverModule, transport: TransportKind.ipc },
		debug: {
			module: serverModule,
			transport: TransportKind.ipc,
		}
	};

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'foxpro' }],
		synchronize: {
			// The index has to hear about files that are edited, added or deleted outside the editor -- a branch switch, a build, another tool. Header files are in the list because a #DEFINE lives in one.
			fileEvents: workspace.createFileSystemWatcher('**/*.{prg,mpr,spr,h}')
		},
		// Where the index caches what it has read, so a restart does not re-read the whole tree.
		initializationOptions: { storagePath: context.globalStorageUri.fsPath },
		// The hover over a form or a class library offers to open it in VFP's designer, and a link in a hover can only run a command the client has trusted. This one, and nothing else.
		markdown: { isTrusted: { enabledCommands: ['foxpro.openInVfp'] } }
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'foxpro',
		'FoxPro Language Server',
		serverOptions,
		clientOptions
	);

	started = client.start(); // This will also launch the server
	context.subscriptions.push(commands.registerCommand('foxpro.openInVfp', (target?: Uri | string) => openInVfp(context, target)));
	registerDesignerView(context);
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}

// --- opening a designer -------------------------------------------------------
// A form, a class library and a report are VFP's own binary formats: the editor can show their names and nothing else, and the program that can show the rest of them is the one the developer already has open.

async function openInVfp(context: ExtensionContext, target?: Uri | string): Promise<void> {
	if (process.platform !== 'win32') return void window.showWarningMessage('Visual FoxPro runs on Windows only.');
	const file = await fileToOpen(target);
	if (!file) return;

	const designer = designerFor(file);
	if (!designer) return void window.showWarningMessage(`Visual FoxPro has no designer for ${path.basename(file)}.`);

	// A library holds many classes and the class designer opens one of them, so which one has to be settled before there is a command to send.
	let className: string | undefined;
	if (designer === 'class') {
		className = await pickClass(context, file);
		if (!className) return;
	}

	const command = vfpCommandFor(file, className);
	if (!command) return void window.showWarningMessage(`${path.basename(file)} cannot be named in a Visual FoxPro command.`);
	await send(context, { mode: 'run', payload: command }, `Opening ${path.basename(file)} in Visual FoxPro`);
}

/** What the caller means: the file the explorer or the hover named, the one the editor is showing, or the form the cursor is standing on the name of. */
async function fileToOpen(target?: Uri | string): Promise<string | null> {
	if (typeof target === 'string') return target;
	if (target) return target.fsPath;

	// A form the editor is showing is its own target. It is not shown in a text editor -- designerView.ts is what it opens in -- so what is on the tab is what settles it.
	const showing = window.activeTextEditor?.document.uri ?? tabFile();
	if (showing && designerFor(showing.fsPath)) return showing.fsPath;

	// Otherwise it is a .prg, and what it names under the cursor is the target.
	const editor = window.activeTextEditor;
	if (!editor) {
		window.showWarningMessage('Select a form or class library, or put the cursor on the name of one.');
		return null;
	}

	await started;
	const found = await client.sendRequest<{ file: string } | null>('foxpro/fileTarget', {
		textDocument: { uri: editor.document.uri.toString() },
		position: { line: editor.selection.active.line, character: editor.selection.active.character }
	});
	if (!found) {
		window.showWarningMessage('No file of the workspace is named under the cursor.');
		return null;
	}
	return found.file;
}

/** What the focused tab is showing, whatever kind of editor is showing it. */
function tabFile(): Uri | undefined {
	return (window.tabGroups.activeTabGroup.activeTab?.input as { uri?: Uri } | undefined)?.uri;
}

/** The classes in a library, asked of VFP itself: it is the one thing that can read a .vcx, and opening the class needs it running anyway. */
async function pickClass(context: ExtensionContext, file: string): Promise<string | undefined> {
	const expression = classListExpression(file);
	if (!expression) return undefined;
	const result = await send(context, { mode: 'classes', payload: expression }, `Reading ${path.basename(file)}`);
	if (!result) return undefined;
	if (!result.lines.length) {
		window.showWarningMessage(`${path.basename(file)} holds no classes.`);
		return undefined;
	}
	if (result.lines.length === 1) return result.lines[0];
	return window.showQuickPick(result.lines, { placeHolder: `Which class in ${path.basename(file)}?` });
}

async function send(context: ExtensionContext, request: { mode: 'run' | 'classes'; payload: string }, title: string): Promise<VfpResult | null> {
	const settings = workspace.getConfiguration('foxpro');
	const root = workspace.workspaceFolders?.[0]?.uri.fsPath ?? '';
	const searchPath = (settings.get<string[]>('workspace.searchPath') ?? []).map(directory => (path.isAbsolute(directory) ? directory : path.join(root, directory)));
	const exe = findVfpExe((settings.get<string>('vfp.path') ?? '').trim());

	try {
		const result = await window.withProgress({ location: ProgressLocation.Window, title }, () => runVfp({
			...request,
			exe: exe ?? '',
			cwd: root,
			startup: startupCommands(root, searchPath, settings.get<string[]>('vfp.startupCommands') ?? [])
		}, {
			script: context.asAbsolutePath(path.join('resources', 'vfp-open.vbs')),
			workDir: context.globalStorageUri.fsPath
		}));
		// Worth saying once: a session started here has the workspace on its path and nothing else, so a form that reaches further will ask where the rest of it is.
		if (result.created) window.showInformationMessage('Visual FoxPro was not running, so it was started with the workspace on its path. Open your own environment there if a designer asks to locate a class.');
		return result;
	} catch (error) {
		window.showErrorMessage(error instanceof Error ? error.message : String(error));
		return null;
	}
}
