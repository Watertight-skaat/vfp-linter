import { createConnection, TextDocuments, ProposedFeatures, InitializeParams, DidChangeConfigurationNotification, TextDocumentSyncKind, InitializeResult, CodeAction, CodeActionKind, SymbolKind, type Diagnostic, type Location as LspLocation, type WorkspaceSymbol as LspWorkspaceSymbol } from 'vscode-languageserver/node';
import { fileURLToPath, pathToFileURL } from 'url';

import { TextDocument } from 'vscode-languageserver-textdocument';
import { lint, type Fix, type LintDiagnostic, type SeverityName } from './linter.js';
import { extract, normalizePath, WorkspaceIndex, type FileRecord } from './index.js';
import { definitionAt, hoverAt, workspaceSymbols, type Location, type SymbolSort } from './navigation.js';
import { documentSymbols, foldingRanges } from './outline.js';
import { buildIndex, refresh } from './workspace.js';
import type { Program } from './ast.js';

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasProgressCapability = false;
let workspaceRoots: string[] = [];

connection.onInitialize((params: InitializeParams) => {
	hasConfigurationCapability = !!params.capabilities.workspace?.configuration;
	hasProgressCapability = !!params.capabilities.window?.workDoneProgress;
	workspaceRoots = (params.workspaceFolders ?? []).map(folder => fileURLToPath(folder.uri));
	if (!workspaceRoots.length && params.rootUri) workspaceRoots = [fileURLToPath(params.rootUri)];
	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			codeActionProvider: { codeActionKinds: [CodeActionKind.QuickFix] },
			documentSymbolProvider: true,
			foldingRangeProvider: true,
			definitionProvider: true,
			hoverProvider: true,
			workspaceSymbolProvider: true
		}
	};
	return result;
});

connection.onInitialized(() => {
	if (hasConfigurationCapability) connection.client.register(DidChangeConfigurationNotification.type, undefined);
	void startIndexing();
});

interface FoxProSettings {
	maxNumberOfProblems: number;
	unsupportedSyntaxSeverity: SeverityName;
	rules: Partial<Record<string, SeverityName>>;
	workspace: { enabled: boolean; exclude: string[]; searchPath: string[] };
}

const defaultSettings: FoxProSettings = {
	maxNumberOfProblems: 100,
	unsupportedSyntaxSeverity: 'information',
	rules: {},
	workspace: { enabled: true, exclude: ['**/node_modules/**', '**/.git/**'], searchPath: [] }
};
let globalSettings: FoxProSettings = defaultSettings;

// Cache the settings of all open documents.
const documentSettings = new Map<string, Thenable<FoxProSettings>>();

function getDocumentSettings(resource: string): Thenable<FoxProSettings> {
	if (!hasConfigurationCapability) return Promise.resolve(globalSettings);
	let result = documentSettings.get(resource);
	if (!result) {
		result = connection.workspace
			.getConfiguration({ scopeUri: resource, section: 'foxpro' })
			// `workspace` is merged a level deeper: a settings file that names only one of its three keys must keep the defaults for the other two.
			.then((settings: Partial<FoxProSettings> | null) => ({ ...defaultSettings, ...(settings ?? {}), workspace: { ...defaultSettings.workspace, ...(settings?.workspace ?? {}) } }));
		documentSettings.set(resource, result);
	}
	return result;
}

connection.onDidChangeConfiguration(change => {
	if (hasConfigurationCapability) documentSettings.clear();
	else globalSettings = { ...defaultSettings, ...(change.settings?.foxpro ?? {}) };
	// Settings change what is reported, so re-lint everything that is open.
	for (const document of documents.all()) scheduleValidation(document.uri);
	// Which files are indexed, and where a name is looked for, are settings too.
	void startIndexing();
});

// --- the workspace index ------------------------------------------------------
// Null until the first crawl finishes, and null again whenever the setting turns it off; every cross-file feature checks for it rather than waiting, so the editor works from the first keystroke and gets better a second later.

let index: WorkspaceIndex | null = null;
let indexing: Promise<void> | null = null;

async function startIndexing(): Promise<void> {
	if (!workspaceRoots.length) return; // a single file opened with no folder: there is no tree to read
	const settings = await getDocumentSettings(pathToFileURL(workspaceRoots[0]).href);
	if (!settings.workspace.enabled) {
		index = null;
		return;
	}
	// A second crawl while one is running would index the same tree twice; the settings that start one are already debounced by the editor.
	if (indexing) return;
	const progress = hasProgressCapability ? await connection.window.createWorkDoneProgress() : null;
	progress?.begin('Indexing FoxPro workspace', 0);
	indexing = buildIndex({
		roots: workspaceRoots,
		exclude: settings.workspace.exclude,
		searchPath: settings.workspace.searchPath,
		onProgress: (done, total) => progress?.report(Math.round((done / total) * 100), `${done} of ${total} files`)
	}).then(built => {
		index = built;
		// Anything already open is ahead of what is on disk, and its findings were produced without the tree. Re-linting it publishes both: validateAndSend puts the document's own reading back into the index on its way past.
		for (const document of documents.all()) scheduleValidation(document.uri);
	}).catch(error => {
		connection.console.error(`FoxPro: indexing the workspace failed: ${error}`);
	}).finally(() => {
		progress?.done();
		indexing = null;
	});
	return indexing;
}

connection.onDidChangeWatchedFiles(params => {
	if (!index) return;
	const changed = new Set<string>();
	for (const event of params.changes) {
		const file = fileOf(event.uri);
		// An open document is the editor's to report; re-reading it from disk would undo an unsaved edit.
		if (documents.get(event.uri)) continue;
		for (const key of refresh(index, file)) changed.add(key);
	}
	relintDependents(changed);
});

/**
 * Re-lints the open files whose findings an edit elsewhere may have changed.
 *
 * Only open files: a diagnostic is published for a document the editor is showing, so a closed dependent has nothing to update. That is what keeps this cheap enough to need no cap -- and the existing per-document debounce absorbs a burst of watcher events.
 */
function relintDependents(changed: Set<string>): void {
	if (!index || !changed.size) return;
	// Normalised on both sides: the same file reaches the index from a crawl and from a URI, and Windows spells the two differently.
	const dependents = new Set([...index.dependentsOf(changed)].map(normalizePath));
	for (const document of documents.all()) {
		if (dependents.has(normalizePath(fileOf(document.uri)))) scheduleValidation(document.uri);
	}
}

const fileOf = (uri: string) => fileURLToPath(uri);

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
	const file = fileOf(document.uri);
	// Named field by field rather than spread: `foxpro.workspace` is the user's settings for the index, while LinterOptions.workspace is the index itself, and the two must not be confused for each other.
	const { diagnostics, ast, record } = lint(document.getText(), {
		rules: settings.rules,
		unsupportedSyntaxSeverity: settings.unsupportedSyntaxSeverity,
		...(index ? { workspace: { index, file } } : {})
	});
	trees.set(document.uri, { version: document.version, ast, record });
	// What this file now says about itself goes back into the index, so the file that calls it is reading the edit rather than the last save. The keys that changed are what the other open files have to be told about.
	if (index && record) relintDependents(index.upsert(record).changed);
	connection.sendDiagnostics({ uri: document.uri, diagnostics: diagnostics.slice(0, Math.max(0, settings.maxNumberOfProblems)).map(toDiagnostic) });
}

// The linter names a related place by file path so it needs no language-server import; the protocol wants a URI.
function toDiagnostic({ relatedInformation, ...rest }: LintDiagnostic): Diagnostic {
	return relatedInformation ? { ...rest, relatedInformation: relatedInformation.map(r => ({ location: { uri: pathToFileURL(r.file).href, range: r.range }, message: r.message })) } : rest;
}

// The last tree per document, so the outline and folding requests that follow every edit do not each parse the file again. The extracted record rides along, because every cross-file request wants it and it is derived from the same parse.
const trees = new Map<string, { version: number; ast: Program | null; record?: FileRecord }>();

function treeFor(document: TextDocument): Program | null {
	const cached = trees.get(document.uri);
	if (cached && cached.version === document.version) return cached.ast;
	const { ast } = lint(document.getText());
	trees.set(document.uri, { version: document.version, ast });
	return ast;
}

/** This document's own definitions and references, from the cached parse when there is one. */
function recordFor(document: TextDocument): FileRecord {
	const cached = trees.get(document.uri);
	if (cached?.version === document.version && cached.record) return cached.record;
	const ast = treeFor(document);
	const record = extract(fileOf(document.uri), ast, linesOf(document));
	trees.set(document.uri, { version: document.version, ast, record });
	return record;
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

// --- across the workspace -----------------------------------------------------

connection.onDefinition(params => {
	const document = documents.get(params.textDocument.uri);
	if (!document || !index) return null;
	const record = recordFor(document);
	const found = definitionAt(record, linesOf(document), params.position, index.viewFor(fileOf(document.uri), record));
	return found.map(toLspLocation);
});

connection.onHover(params => {
	const document = documents.get(params.textDocument.uri);
	if (!document || !index) return null;
	const record = recordFor(document);
	const found = hoverAt(record, linesOf(document), params.position, index.viewFor(fileOf(document.uri), record));
	return found ? { contents: { kind: 'markdown' as const, value: found.markdown }, range: found.range } : null;
});

connection.onWorkspaceSymbol(params => {
	if (!index) return [];
	return workspaceSymbols({ index }, params.query).map<LspWorkspaceSymbol>(symbol => ({
		name: symbol.name,
		kind: symbolKinds[symbol.sort],
		location: toLspLocation(symbol.location),
		...(symbol.container ? { containerName: symbol.container } : {})
	}));
});

// navigation.ts stays free of the protocol, so the mapping to its numbers lives here.
const symbolKinds: Record<SymbolSort, SymbolKind> = {
	procedure: SymbolKind.Function,
	function: SymbolKind.Function,
	class: SymbolKind.Class,
	method: SymbolKind.Method,
	property: SymbolKind.Property,
	constant: SymbolKind.Constant
};

const toLspLocation = (location: Location): LspLocation => ({ uri: pathToFileURL(location.file).href, range: location.range });

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
