// What the editor shows beside the diagnostics: the Outline and breadcrumbs, and the folding ranges. Both come straight off the tree, so a file that fails to parse gets neither until it parses again.

import { SymbolKind, type DocumentSymbol, type FoldingRange } from 'vscode-languageserver';
import type { DefineClass, Loc, ProcedureStatement, Program, Statement } from './ast.js';
import { toRange, walk } from './rule.js';

export function documentSymbols(ast: Program, lines: string[]): DocumentSymbol[] {
  const out: DocumentSymbol[] = [];
  for (const statement of ast.body) {
    if (statement?.type === 'ProcedureStatement') out.push(routine(statement, lines, SymbolKind.Function));
    else if (statement?.type === 'DefineClass') out.push(classSymbol(statement, lines));
    else if (statement?.type === 'DefineStatement') out.push(symbol(statement.name, SymbolKind.Constant, statement.location, lines, statement.value ?? undefined));
  }
  return out;
}

function routine(node: ProcedureStatement, lines: string[], kind: SymbolKind): DocumentSymbol {
  // An LPARAMETERS line below the PROCEDURE line is the routine's first statement rather than part of its header.
  const declared = node.body.body.find(s => s?.type === 'ParametersDeclaration');
  const names = node.parameters.length ? node.parameters.map(p => (typeof p === 'string' ? p : p.name))
    : declared?.type === 'ParametersDeclaration' ? declared.names : [];
  const params = names.join(', ');
  const detail = `${node.isFunction ? 'FUNCTION' : 'PROCEDURE'}${params ? ` (${params})` : ''}`;
  return symbol(node.name, kind, node.location, lines, detail);
}

function classSymbol(node: DefineClass, lines: string[]): DocumentSymbol {
  const children: DocumentSymbol[] = [];
  for (const statement of node.body as Statement[]) {
    if (statement?.type === 'ProcedureStatement') children.push(routine(statement, lines, SymbolKind.Method));
    else if (statement?.type === 'Assignment' && statement.target.type === 'Identifier')
      children.push(symbol(statement.target.name, SymbolKind.Property, statement.location, lines));
  }
  return { ...symbol(node.name, SymbolKind.Class, node.location, lines, node.base ? `AS ${node.base}` : undefined), children };
}

function symbol(name: string, kind: SymbolKind, loc: Loc | undefined, lines: string[], detail?: string): DocumentSymbol {
  const range = trimmed(loc, lines);
  // The name's own position is not in the tree, so the first line of the node stands in for it.
  const selectionRange = { start: range.start, end: { line: range.start.line, character: (lines[range.start.line] ?? '').length } };
  return { name, kind, range, selectionRange, ...(detail ? { detail } : {}) };
}

// Everything with a body, and a query written over several lines.
const foldable = new Set(['IfStatement', 'DoWhileStatement', 'ForStatement', 'ForEachStatement', 'DoCaseStatement', 'CaseClause', 'ScanStatement', 'TryStatement', 'WithStatement', 'DefineClass', 'ProcedureStatement', 'TextBlockStatement', 'PreprocessorIfStatement', 'SelectStatement']);

export function foldingRanges(ast: Program, lines: string[]): FoldingRange[] {
  const out: FoldingRange[] = [];
  walk(ast, node => {
    if (!foldable.has(node.type)) return;
    const range = trimmed(node.location, lines);
    // The closing line stays visible, as the editor does for a brace, unless the block has none: a CASE ends where the next one starts, and a routine without ENDPROC ends on its last statement.
    const closed = node.type === 'CaseClause' ? false
      : node.type !== 'ProcedureStatement' || /^\s*(ENDPROC|ENDFUNC)\b/i.test(lines[range.end.line] ?? '');
    const endLine = closed ? range.end.line - 1 : range.end.line;
    if (endLine > range.start.line) out.push({ startLine: range.start.line, endLine });
  });
  return out;
}

// A block rule swallows the whitespace and comments after its terminator, so a node can end several blank lines below its last token. Pull the end back to the last line with something on it, and off a line it only touches at column 0.
function trimmed(loc: Loc | undefined, lines: string[]) {
  const range = toRange(loc);
  let line = range.end.character === 0 && range.end.line > range.start.line ? range.end.line - 1 : range.end.line;
  while (line > range.start.line && !(lines[line] ?? '').trim()) line--;
  if (line !== range.end.line) range.end = { line, character: (lines[line] ?? '').length };
  return range;
}
