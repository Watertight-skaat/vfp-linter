import { createConnection, TextDocuments, ProposedFeatures, InitializeParams, DidChangeConfigurationNotification, TextDocumentSyncKind, InitializeResult, CodeAction, CodeActionKind, type Diagnostic } from 'vscode-languageserver/node';
import { pathToFileURL } from 'url';

import { TextDocument } from 'vscode-languageserver-textdocument';
import { lint, type Fix, type LintDiagnostic, type SeverityName } from './linter.js';
import { documentSymbols, foldingRanges } from './outline.js';
import type { Program } from './ast.js';

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;

connection.onInitialize((params: InitializeParams) => {
	hasConfigurationCapability = !!params.capabilities.workspace?.configuration;
	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			codeActionProvider: { codeActionKinds: [CodeActionKind.QuickFix] },
			documentSymbolProvider: true,
			foldingRangeProvider: true
		}
	};
	return result;
});

connection.onInitialized(() => {
	if (hasConfigurationCapability) connection.client.register(DidChangeConfigurationNotification.type, undefined);
});

interface FoxProSettings {
	maxNumberOfProblems: number;
	unsupportedSyntaxSeverity: SeverityName;
	rules: Partial<Record<string, SeverityName>>;
}

const defaultSettings: FoxProSettings = { maxNumberOfProblems: 100, unsupportedSyntaxSeverity: 'information', rules: {} };
let globalSettings: FoxProSettings = defaultSettings;

// Cache the settings of all open documents.
const documentSettings = new Map<string, Thenable<FoxProSettings>>();

function getDocumentSettings(resource: string): Thenable<FoxProSettings> {
	if (!hasConfigurationCapability) return Promise.resolve(globalSettings);
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
	if (hasConfigurationCapability) documentSettings.clear();
	else globalSettings = { ...defaultSettings, ...(change.settings?.foxpro ?? {}) };
	// Settings change what is reported, so re-lint everything that is open.
	for (const document of documents.all()) scheduleValidation(document.uri);
});

// Only keep settings for open documents.
documents.onDidClose(e => {
	cancelValidation(e.document.uri);
	documentSettings.delete(e.document.uri);
	trees.delete(e.document.uri);
	// Clear any diagnostics we published for a document that is no longer open.
	connection.sendDiagnostics({ uri: e.document.uri, diagnostics: [] });
});

documents.onDidChangeContent(change => scheduleValidation(change.document.uri));

// How long a document has to stop changing before it is re-linted.
// Parsing is not a bottleneck -- a 27,000-line file takes about 250 ms, and linting it under 9 ms -- but without this every keystroke queues a parse of the whole file, and on a large one the editor spends the whole typing burst doing work that the next keystroke throws away.
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
	const settings = await getDocumentSettings(document.uri);
	const { diagnostics, ast } = lint(document.getText(), settings);
	trees.set(document.uri, { version: document.version, ast });
	connection.sendDiagnostics({ uri: document.uri, diagnostics: diagnostics.slice(0, Math.max(0, settings.maxNumberOfProblems)).map(toDiagnostic) });
}

// The linter names a related place by file path so it needs no language-server import; the protocol wants a URI.
function toDiagnostic({ relatedInformation, ...rest }: LintDiagnostic): Diagnostic {
	return relatedInformation ? { ...rest, relatedInformation: relatedInformation.map(r => ({ location: { uri: pathToFileURL(r.file).href, range: r.range }, message: r.message })) } : rest;
}

// The last tree per document, so the outline and folding requests that follow every edit do not each parse the file again.
const trees = new Map<string, { version: number; ast: Program | null }>();

function treeFor(document: TextDocument): Program | null {
	const cached = trees.get(document.uri);
	if (cached && cached.version === document.version) return cached.ast;
	const { ast } = lint(document.getText());
	trees.set(document.uri, { version: document.version, ast });
	return ast;
}

const linesOf = (document: TextDocument) => document.getText().split(/\r\n|\r|\n/);

connection.onDocumentSymbol(params => {
	const document = documents.get(params.textDocument.uri);
	const ast = document && treeFor(document);
	return ast ? documentSymbols(ast, linesOf(document)) : [];
});

connection.onFoldingRanges(params => {
	const document = documents.get(params.textDocument.uri);
	const ast = document && treeFor(document);
	return ast ? foldingRanges(ast, linesOf(document)) : [];
});

// Every diagnostic can be suppressed in place; the ones that know how to fix themselves carry the edit along.
connection.onCodeAction(params => {
	const document = documents.get(params.textDocument.uri);
	if (!document) return [];
	const uri = document.uri;
	const actions: CodeAction[] = [];
	for (const diagnostic of params.context.diagnostics) {
		if (typeof diagnostic.code !== 'string' || diagnostic.code === 'syntax-error') continue;
		const fix = (diagnostic.data as { fix?: Fix } | undefined)?.fix;
		if (fix) actions.push({ title: fix.title, kind: CodeActionKind.QuickFix, diagnostics: [diagnostic], isPreferred: true, edit: { changes: { [uri]: fix.edits } } });
		const line = diagnostic.range.start.line;
		const text = document.getText({ start: { line, character: 0 }, end: { line: line + 1, character: 0 } });
		const indent = /^[ \t]*/.exec(text)![0];
		const eol = document.getText().includes('\r\n') ? '\r\n' : '\n';
		actions.push({
			title: `Suppress ${diagnostic.code} on this line`,
			kind: CodeActionKind.QuickFix,
			diagnostics: [diagnostic],
			edit: { changes: { [uri]: [{ range: { start: { line, character: 0 }, end: { line, character: 0 } }, newText: `${indent}* vfp-lint-disable-next-line ${diagnostic.code}${eol}` }] } }
		});
	}
	return actions;
});

documents.listen(connection);
connection.listen();
