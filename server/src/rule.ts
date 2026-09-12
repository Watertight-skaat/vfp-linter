// What a rule is, and what the linter hands it. Rules live in rules/ and are registered in rules/index.ts; linter.ts drives them, resolves each one's severity from settings and applies suppression comments.
// Nothing here imports the language server, so the rules and the tests can run without a connection.

import type { AstNode, AstNodeType, Loc, Program } from './ast.js';
// Type-only: index.ts imports walk() from here, and a value import back would be a cycle. Nothing in this file needs one.
import type { WorkspaceView } from './index.js';
import type { SymbolTable } from './scope.js';

// The LSP DiagnosticSeverity values, inlined so this file needs no language-server import. Typed as literals rather than an enum so the result is assignable to Diagnostic[] without a cast.
export const Severity = { Error: 1, Warning: 2, Information: 3, Hint: 4 } as const;
export type Severity = (typeof Severity)[keyof typeof Severity];

export type SeverityName = 'error' | 'warning' | 'information' | 'hint' | 'off';

export const severityByName: Record<SeverityName, Severity | null> = {
  error: Severity.Error,
  warning: Severity.Warning,
  information: Severity.Information,
  hint: Severity.Hint,
  off: null
};

export interface Position { line: number; character: number }
export interface Range { start: Position; end: Position }
export interface TextEdit { range: Range; newText: string }

/** A quick fix: the edits that resolve the finding, and the title the editor shows for them. */
export interface Fix {
  title: string;
  edits: TextEdit[];
}

/** Another place the finding involves: the other definition of a duplicated routine, the declaration a call over-supplies. A file path rather than a URI, so nothing here needs the language server. */
export interface RelatedLocation {
  file: string;
  range: Range;
  message: string;
}

export interface LintDiagnostic {
  severity: Severity;
  range: Range;
  code: string;
  message: string;
  source: string;
  relatedInformation?: RelatedLocation[];
  /** Round-tripped through the client untouched, so a code-action request can hand the fix straight back. */
  data?: { fix: Fix };
}

export interface RuleContext {
  ast: Program;
  /** One table, built once per file and shared by every rule that asks for it. */
  table: SymbolTable;
  /** The source split into lines, for fixes that must see the text as written. */
  lines: string[];
  /** The line ending the file uses, so an inserted line matches its neighbours. */
  eol: string;
  /** What the rest of the workspace holds, bound to this file. Undefined when the linter was given no index -- a rule that reads across files must stay silent then rather than report on half the evidence. */
  workspace?: WorkspaceView;
  report(loc: Loc | undefined, message: string, fix?: Fix, related?: RelatedLocation[]): void;
}

export interface Rule {
  code: string;
  /** The severity when the settings say nothing. */
  severity: Severity;
  /** Settings cannot change it: the finding is broken code rather than a style choice. */
  locked?: true;
  /** Node types the rule wants to see, one call per node. Absent, the rule runs once per file and is handed the Program. */
  on?: AstNodeType[];
  check(node: AstNode, ctx: RuleContext): void;
}

type RuleHeader = Pick<Rule, 'code' | 'severity' | 'locked'>;

/** A rule that sees every node of the given types. The check is typed to exactly those nodes. */
export function onNode<T extends AstNodeType>(rule: RuleHeader & { on: T[]; check(node: Extract<AstNode, { type: T }>, ctx: RuleContext): void }): Rule {
  return rule as unknown as Rule;
}

/** A rule that runs once per file. */
export function onFile(rule: RuleHeader & { check(ctx: RuleContext): void }): Rule {
  return { ...rule, on: undefined, check: (_node, ctx) => rule.check(ctx) };
}

// Parser locations are 1-based and optional; LSP ranges are 0-based and required.
export function toRange(loc: Loc | undefined): Range {
  const startLine = loc?.start ? loc.start.line - 1 : 0;
  const startCol = loc?.start ? loc.start.column - 1 : 0;
  const endLine = loc?.end ? loc.end.line - 1 : startLine;
  const endCol = loc?.end ? loc.end.column - 1 : startCol + 1;
  return { start: { line: startLine, character: startCol }, end: { line: endLine, character: endCol } };
}

/**
 * A canonical string for an expression, so two CASE conditions can be compared by structure rather than by source text. Identifiers are upper-cased because FoxPro names are case-insensitive; string literals are left alone because their contents are not.
 */
export function expressionKey(value: unknown): string {
  if (value === null || value === undefined) return '';
  if (Array.isArray(value)) return `[${value.map(expressionKey).join(',')}]`;
  if (typeof value !== 'object') return JSON.stringify(value);
  const node = value as AstNode;
  if (node.type === 'Identifier' || node.type === 'ImplicitGlobal' || node.type === 'MacroSubstitute')
    return `${node.type}(${String(node.name).toUpperCase()})`;
  const bag = node as unknown as Record<string, unknown>;
  const keys = Object.keys(bag).filter(k => k !== 'location').sort();
  return `${node.type}(${keys.map(k => `${k}:${expressionKey(bag[k])}`).join(',')})`;
}

/** Depth-first over every node in a subtree, arrays and option bags included. */
export function walk(value: unknown, visit: (node: AstNode) => void): void {
  if (Array.isArray(value)) {
    for (const item of value) walk(item, visit);
    return;
  }
  if (!value || typeof value !== 'object') return;
  const node = value as AstNode;
  if (typeof node.type === 'string') visit(node);
  // A generic walk over a union has to reach the properties reflectively.
  for (const child of Object.values(node as unknown as Record<string, unknown>)) {
    if (child && typeof child === 'object') walk(child, visit);
  }
}
