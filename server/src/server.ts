import { createConnection, TextDocuments, Diagnostic, DiagnosticSeverity, ProposedFeatures, InitializeParams, DidChangeConfigurationNotification, TextDocumentSyncKind, InitializeResult } from 'vscode-languageserver/node';

import { TextDocument } from 'vscode-languageserver-textdocument';
import { parse } from './parser.js'; // Import our Peggy.js parser
import { runLinterRules, type SeverityName } from './linter.js';
import type { Program } from './ast.js';

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;
	hasConfigurationCapability = !!(capabilities.workspace && !!capabilities.workspace.configuration);
	hasWorkspaceFolderCapability = !!(capabilities.workspace && !!capabilities.workspace.workspaceFolders);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental
		}
	};
	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			workspaceFolders: {
				supported: true
			}
		};
	}
	return result;
});

connection.onInitialized(() => {
	if (hasConfigurationCapability) {
		// Register for all configuration changes.
		connection.client.register(DidChangeConfigurationNotification.type, undefined);
	}
	if (hasWorkspaceFolderCapability) {
		connection.workspace.onDidChangeWorkspaceFolders(() => {
			connection.console.log('Workspace folder change event received.');
		});
	}
});

interface FoxProSettings {
	maxNumberOfProblems: number;
	unsupportedSyntaxSeverity: SeverityName;
}

const defaultSettings: FoxProSettings = { maxNumberOfProblems: 100, unsupportedSyntaxSeverity: 'information' };
let globalSettings: FoxProSettings = defaultSettings;

// Cache the settings of all open documents.
const documentSettings = new Map<string, Thenable<FoxProSettings>>();

function getDocumentSettings(resource: string): Thenable<FoxProSettings> {
	if (!hasConfigurationCapability) {
		return Promise.resolve(globalSettings);
	}
	let result = documentSettings.get(resource);
	if (!result) {
		result = connection.workspace
			.getConfiguration({ scopeUri: resource, section: 'foxpro' })
			.then((settings: Partial<FoxProSettings> | null) => ({ ...defaultSettings, ...(settings ?? {}) }));
		documentSettings.set(resource, result);
	}
	return result;
}

connection.onDidChangeConfiguration(change => {
	if (hasConfigurationCapability) {
		documentSettings.clear();
	} else {
		globalSettings = { ...defaultSettings, ...(change.settings?.foxpro ?? {}) };
	}
	// Settings can change how many problems we report, so re-lint everything that is open.
	for (const document of documents.all()) scheduleValidation(document.uri);
});

// Only keep settings for open documents.
documents.onDidClose(e => {
	cancelValidation(e.document.uri);
	documentSettings.delete(e.document.uri);
	// Clear any diagnostics we published for a document that is no longer open.
	connection.sendDiagnostics({ uri: e.document.uri, diagnostics: [] });
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
	scheduleValidation(change.document.uri);
});

// How long a document has to stop changing before it is re-linted.
// Parsing is not a bottleneck -- a 27,000-line file takes about 250 ms, and linting it under 9 ms --
// but without this every keystroke queues a parse of the whole file, and on a large one the editor
// spends the whole typing burst doing work that the next keystroke throws away.
const debounceDelay = 300;

// Per document, so typing in one file does not hold back diagnostics for another.
const pendingValidations = new Map<string, NodeJS.Timeout>();

function scheduleValidation(uri: string): void {
	cancelValidation(uri);
	pendingValidations.set(uri, setTimeout(() => {
		pendingValidations.delete(uri);
		// Re-read the document rather than capturing it: it may have changed or closed while the timer ran.
		const document = documents.get(uri);
		if (document) void validateAndSend(document);
	}, debounceDelay));
}

function cancelValidation(uri: string): void {
	const pending = pendingValidations.get(uri);
	if (pending === undefined) return;
	clearTimeout(pending);
	pendingValidations.delete(uri);
}

async function validateAndSend(document: TextDocument): Promise<void> {
	const diagnostics = await validateTextDocument(document);
	connection.sendDiagnostics({ uri: document.uri, diagnostics });
}

async function validateTextDocument(textDocument: TextDocument): Promise<Diagnostic[]> {
	const settings = await getDocumentSettings(textDocument.uri);
	const diagnostics: Diagnostic[] = [];
	const text = textDocument.getText();

	try {
		const ast = parse(text) as Program;
		diagnostics.push(...runLinterRules(ast, settings));
	} catch (error) {
		const location = (error as { location?: { start: { line: number; column: number }; end: { line: number; column: number } } })?.location;
		if (location) {
			diagnostics.push({
				severity: DiagnosticSeverity.Error,
				range: {
					start: { line: location.start.line - 1, character: location.start.column - 1 },
					end: { line: location.end.line - 1, character: location.end.column - 1 }
				},
				message: (error as Error).message,
				source: 'VFP Linter (Syntax)'
			});
		} else {
			diagnostics.push({
				severity: DiagnosticSeverity.Error,
				range: {
					start: { line: 0, character: 0 },
					end: { line: 0, character: 1 }
				},
				message: 'Error while linting: ' + error,
				source: 'VFP Linter (Syntax)'
			});
		}
	}

	return diagnostics.slice(0, Math.max(0, settings.maxNumberOfProblems));
}

documents.listen(connection);
connection.listen();
