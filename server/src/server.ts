/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
import {
	createConnection,
	TextDocuments,
	Diagnostic,
	DiagnosticSeverity,
	ProposedFeatures,
	InitializeParams,
	DidChangeConfigurationNotification,
	TextDocumentSyncKind,
	InitializeResult,
	DocumentDiagnosticReportKind,
	type DocumentDiagnosticReport
} from 'vscode-languageserver/node';

import { TextDocument } from 'vscode-languageserver-textdocument';
import { parse } from './parser.js'; // Import our Peggy.js parser

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
// let hasDiagnosticRelatedInformationCapability = false;

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;
	hasConfigurationCapability = !!(capabilities.workspace && !!capabilities.workspace.configuration);
	hasWorkspaceFolderCapability = !!(capabilities.workspace && !!capabilities.workspace.workspaceFolders);
	// hasDiagnosticRelatedInformationCapability = !!(capabilities.textDocument && capabilities.textDocument.publishDiagnostics && capabilities.textDocument.publishDiagnostics.relatedInformation);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			// completionProvider: {
			// 	resolveProvider: true
			// },
			// diagnosticProvider: {
			// 	interFileDependencies: false,
			// 	workspaceDiagnostics: false
			// }
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
		connection.workspace.onDidChangeWorkspaceFolders(_event => {
			connection.console.log('Workspace folder change event received.');
		});
	}
});

// The example settings
interface ExampleSettings {
	maxNumberOfProblems: number;
}

// The global settings, used when the `workspace/configuration` request is not supported by the client.
// Please note that this is not the case when using this server with the client provided in this example
// but could happen with other clients.
// const defaultSettings: ExampleSettings = { maxNumberOfProblems: 1000 };
// let globalSettings: ExampleSettings = defaultSettings;

// Cache the settings of all open documents
const documentSettings = new Map<string, Thenable<ExampleSettings>>();

// Only keep settings for open documents
documents.onDidClose(e => {
	documentSettings.delete(e.document.uri);
});


connection.languages.diagnostics.on(async (params) => {
	const document = documents.get(params.textDocument.uri);
	if (document !== undefined) {
		return {
			kind: DocumentDiagnosticReportKind.Full,
			items: await validateTextDocument(document)
		} satisfies DocumentDiagnosticReport;
	} else {
		// We don't know the document. We can either try to read it from disk
		// or we don't report problems for it.
		return {
			kind: DocumentDiagnosticReportKind.Full,
			items: []
		} satisfies DocumentDiagnosticReport;
	}
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
	const diagnostics = validateTextDocument(change.document);
	// Send the computed diagnostics to VSCode.
	connection.sendDiagnostics({ uri: change.document.uri, diagnostics });
});

function validateTextDocument(textDocument: TextDocument): Diagnostic[] {
	// const settings = await getDocumentSettings(textDocument.uri);
	const diagnostics: Diagnostic[] = [];
	const text = textDocument.getText();
	console.log(`Validating document: ${textDocument.uri}`);

	try {
		const ast = parse(text);
		const linterRules = runLinterRules(ast);
		diagnostics.push(...linterRules);
	} catch (error: any) {
		console.log(error);

		if (error?.location) {
			const diagnostic: Diagnostic = {
				severity: DiagnosticSeverity.Error,
				range: {
					start: { line: error.location.start.line - 1, character: error.location.start.column - 1 },
					end: { line: error.location.end.line - 1, character: error.location.end.column - 1 }
				},
				message: error.message,
				source: 'VFP Linter (Syntax)'
			};
			diagnostics.push(diagnostic);
		} else {
			const diagnostic: Diagnostic = {
				severity: DiagnosticSeverity.Error,
				range: {
					start: { line: 0, character: 0 },
					end: { line: 0, character: 1 }
				},
				message: "Error while linting: " + error,
				source: 'VFP Linter (Syntax)'
			};
			diagnostics.push(diagnostic);
		}
	}
	return diagnostics;
}

// This function walks the AST and checks for custom rules.
interface AstNode {
	type: string;
	name?: string;
	location?: {
		start: { line: number; column: number };
		end: { line: number; column: number }
	};
	[k: string]: unknown;
}
interface ProgramAst { body?: AstNode[] }
export function runLinterRules(ast: ProgramAst): Diagnostic[] {
	const problems: Diagnostic[] = [];
	if (!ast.body) {
		console.log("No body in AST");
		return problems;
	}
	// Recursively traverse the AST and collect problems from every node.
	function traverse(node: AstNode | AstNode[]) {
		if (!node)
			return;
		if (Array.isArray(node)) {
			for (const item of node)
				traverse(item);
			return;
		}
		if (typeof node !== 'object' || node === null)
			return;
		const maybeNode = node as AstNode;
		if (maybeNode.type) // If this object looks like an AST node, check it for problems.
			problems.push(...getProblemsFromNode(maybeNode));

		// Recurse into all object properties to find nested nodes or arrays of nodes.
		for (const key in maybeNode) {
			const prop: any = maybeNode[key];
			if (!prop) continue;
			if (Array.isArray(prop)) {
				for (const p of prop)
					traverse(p);
			} else if (typeof prop === 'object' && prop?.['type']) {
				traverse(prop as AstNode);
			}
		}
	}

	traverse(ast.body);
	return problems;
}
function getProblemsFromNode(node: AstNode) {
	const problems: Diagnostic[] = [];
	if (node.type === 'UnknownStatement') {
		const diagnostic: Diagnostic = {
			severity: DiagnosticSeverity.Error,
			range: {
				start: { line: (node.location?.start.line ?? 1) - 1, character: (node.location?.start.column ?? 1) - 1 },
				end: { line: (node.location?.end.line ?? 1) - 1, character: (node.location?.end.column ?? 2) - 1 }
			},
			message: `Unknown or unsupported statement: '${node.raw}'`,
			source: 'VFP Linter (Syntax)'
		};
		problems.push(diagnostic);
	}
	return problems;
}

documents.listen(connection);
connection.listen();
