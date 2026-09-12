import * as path from 'path';
import { ExtensionContext, workspace } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

let client: LanguageClient;

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
		initializationOptions: { storagePath: context.globalStorageUri.fsPath }
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'foxpro',
		'FoxPro Language Server',
		serverOptions,
		clientOptions
	);

	client.start(); // This will also launch the server
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
