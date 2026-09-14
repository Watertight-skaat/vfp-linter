import * as path from 'path';
import { commands, ExtensionContext, ProgressLocation, Uri, window, workspace } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';
import { classListExpression, designerFor, vfpCommandFor } from '../../server/src/vfp.js';
import { registerDesignerView } from './designerView.js';
import { environmentExpression, findVfpExe, isSetUpFor, parseEnvironment, runVfp, startupCommands, type VfpResult } from './vfp.js';

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

	// Asked before anything is sent, because the answer decides whether the designer opens or stops on a dialog nothing here can reach.
	if (!await settle(context)) return;

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

/**
 * Whether to go on: that the Visual FoxPro this will open in can find the tree's own files, settled with the developer when it cannot.
 *
 * A session started from the Start menu knows nothing about the workspace, and the form designer it opens stops on a modal Locate dialog for the first class the form is built from -- inside VFP, where the editor can neither see it nor cancel it, and where the call it is holding up simply never returns. Asking first costs one round trip and is the difference between a dialog the developer can answer and a designer that looks hung.
 *
 * A session started here is asked the same question rather than taken on trust: it was told what the settings hold, and if they hold nothing it is every bit as lost as one the developer left open.
 */
async function settle(context: ExtensionContext): Promise<boolean> {
	const { root, configured } = settings();
	const result = await send(context, { mode: 'value', payload: environmentExpression }, 'Asking Visual FoxPro where it looks');
	if (!result) return false; // the failure has been reported already

	const consequence = 'Its designer will open the file and then stop to ask where each class the file is built from lives -- one dialog at a time, inside Visual FoxPro, where this editor cannot answer them.';
	if (isSetUpFor(root, parseEnvironment(result.lines[0] ?? ''))) {
		// Worth saying once, because a session started here is not the developer's own: it has the workspace on its path and nothing else their environment would have given it.
		if (result.created) window.showInformationMessage('Visual FoxPro was not running, so it was started on this workspace.');
		return true;
	}

	// Nothing left to offer: either it has just been given everything the settings hold and is still lost, or there was never anything but a default directory to give it -- and for a tree spread over twenty folders that is no better than what it has. The setting is the fix either way.
	if (result.created || !configured) {
		const started = result.created ? 'Visual FoxPro was not running, so it was started on this workspace -- but it still looks nowhere inside it.' : 'Visual FoxPro is running, but it looks nowhere inside this workspace.';
		const choice = await window.showWarningMessage(started, { modal: true, detail: `${consequence}\n\nList the folders this tree's code is in under foxpro.workspace.searchPath, or start Visual FoxPro the way your tree expects.` }, 'Open the setting', 'Open anyway');
		if (choice === 'Open the setting') await commands.executeCommand('workbench.action.openSettings', 'foxpro.workspace.searchPath');
		return choice === 'Open anyway';
	}

	const choice = await window.showWarningMessage('Visual FoxPro is running, but it looks nowhere inside this workspace.', { modal: true, detail: `${consequence}\n\nThe workspace folder and foxpro.workspace.searchPath can be set on it first. That lasts as long as the session does.` }, 'Set the path', 'Open anyway');
	if (choice !== 'Set the path') return choice === 'Open anyway';

	// Asked again afterwards rather than assumed: a search path that does not actually reach the code is worth hearing about here rather than from the designer.
	const after = await send(context, { mode: 'value', payload: environmentExpression, setup: true }, 'Setting up Visual FoxPro');
	if (!after) return false;
	if (!isSetUpFor(root, parseEnvironment(after.lines[0] ?? ''))) window.showWarningMessage('Visual FoxPro still looks nowhere inside this workspace. Check foxpro.workspace.searchPath against where the code actually is.');
	return true;
}

/** The workspace as Visual FoxPro has to be told about it: where the tree is, which VFP to start, and what to say to a session that does not know. */
function settings() {
	const configuration = workspace.getConfiguration('foxpro');
	const root = workspace.workspaceFolders?.[0]?.uri.fsPath ?? '';
	const searchPath = (configuration.get<string[]>('workspace.searchPath') ?? []).map(directory => (path.isAbsolute(directory) ? directory : path.join(root, directory)));
	const extra = configuration.get<string[]>('vfp.startupCommands') ?? [];
	return {
		root,
		exe: findVfpExe((configuration.get<string>('vfp.path') ?? '').trim()) ?? '',
		startup: startupCommands(root, searchPath, extra),
		/** Whether there is anything worth setting on a session that is already running. A default directory on its own is not: the tree is spread over more folders than one. */
		configured: searchPath.length > 0 || extra.length > 0
	};
}

async function send(context: ExtensionContext, request: { mode: 'run' | 'classes' | 'value'; payload: string; setup?: boolean }, title: string): Promise<VfpResult | null> {
	const { root, exe, startup } = settings();

	try {
		const result = await window.withProgress({ location: ProgressLocation.Window, title }, () => runVfp({
			...request,
			exe,
			cwd: root,
			startup
		}, {
			script: context.asAbsolutePath(path.join('resources', 'vfp-open.vbs')),
			workDir: context.globalStorageUri.fsPath
		}));
		// Whether it had to be started is settle()'s to report: it is the one that knows whether the session that came back can see the tree.
		return result;
	} catch (error) {
		window.showErrorMessage(error instanceof Error ? error.message : String(error));
		return null;
	}
}
