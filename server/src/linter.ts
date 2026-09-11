// This file intentionally avoids creating a language-server connection so it can be imported by test scripts.

import type {
  AstNode, BlockStatement, DoCaseStatement, IfStatement, Loc, Program, SelectStatement,
  TableRef, TryStatement, UnknownStatement
} from './ast.js';
import { aliasInEffectAt, aliasName, buildSymbolTable, isWorkAreaSwitch, type Scope } from './scope.js';

// The LSP DiagnosticSeverity values, inlined so this file needs no language-server import.
// Typed as literals rather than an enum so the result is assignable to Diagnostic[] without a cast.
const Severity = { Error: 1, Warning: 2, Information: 3, Hint: 4 } as const;
type Severity = (typeof Severity)[keyof typeof Severity];

export interface LintDiagnostic {
  severity: Severity;
  range: { start: { line: number; character: number }; end: { line: number; character: number } };
  code: string;
  message: string;
  source: string;
}

// A construct the grammar does not cover yet is not the same thing as a construct that is wrong, so unsupported syntax is advisory by default: valid FoxPro the grammar has not learned should not look like a mistake.
// 'off' suppresses it entirely, while the test harness passes 'error' so the probe corpus still fails on any construct the grammar cannot read.
export type SeverityName = 'error' | 'warning' | 'information' | 'hint' | 'off';

export interface LinterOptions {
  unsupportedSyntaxSeverity?: SeverityName;
}

const defaultOptions: Required<LinterOptions> = { unsupportedSyntaxSeverity: 'information' };

const severities: Record<SeverityName, Severity | null> = {
  error: Severity.Error,
  warning: Severity.Warning,
  information: Severity.Information,
  hint: Severity.Hint,
  off: null
};

// Statements whose grammar rule cannot match without its terminator. SCAN is absent on purpose: ENDSCAN is optional in the grammar, so an unterminated SCAN never reaches the catch-all.
const blockOpeners = [
  { opener: /^IF\b/i, terminator: 'ENDIF' },
  { opener: /^FOR\b/i, terminator: 'ENDFOR or NEXT' },
  { opener: /^DO\s+WHILE\b/i, terminator: 'ENDDO' },
  { opener: /^DO\s+CASE\b/i, terminator: 'ENDCASE' },
  { opener: /^TRY\b/i, terminator: 'ENDTRY' },
  { opener: /^WITH\b/i, terminator: 'ENDWITH' },
  { opener: /^DEFINE\s+CLASS\b/i, terminator: 'ENDDEFINE' },
  { opener: /^TEXT\b/i, terminator: 'ENDTEXT' }
];

// Statements after one of these in the same block can never run.
const terminators = new Set(['ReturnStatement', 'ExitStatement', 'ContinueStatement']);

// A nested routine is not unreachable code: the grammar nests PROCEDURE and DEFINE CLASS inside the
// previous routine's body whenever ENDPROC is left off, which is common and legal.
const routineTypes = new Set(['ProcedureStatement', 'DefineClass']);

export function runLinterRules(ast: Program | null | undefined, options: LinterOptions = {}): LintDiagnostic[] {
  const problems: LintDiagnostic[] = [];
  if (!ast || !ast.body) return problems;

  const opts = { ...defaultOptions, ...options };

  function traverse(value: unknown) {
    if (Array.isArray(value)) {
      for (const item of value) traverse(item);
      return;
    }
    if (!value || typeof value !== 'object') return;
    const node = value as AstNode;
    if (typeof node.type === 'string') problems.push(...getProblemsFromNode(node, opts));

    // A generic walk over a union has to reach the properties reflectively.
    for (const child of Object.values(node as unknown as Record<string, unknown>)) {
      if (child && typeof child === 'object') traverse(child);
    }
  }

  traverse(ast);
  problems.push(...missingMemvarPrefix(ast));
  problems.push(...undirectedSelects(ast));

  // One pass appends to another, so order by position rather than by which rule produced what.
  return problems.sort((a, b) =>
    a.range.start.line - b.range.start.line || a.range.start.character - b.range.start.character);
}

function getProblemsFromNode(node: AstNode, opts: Required<LinterOptions>): LintDiagnostic[] {
  const out: LintDiagnostic[] = [];

  switch (node.type) {
    case 'SelectStatement':
      havingWithoutGroupBy(node, out);
      unlinkedTables(node, out);
      break;
    case 'UnknownStatement': unsupportedOrBroken(node, opts, out); break;
    case 'Program':
    case 'BlockStatement': unreachableCode(node, out); break;
    case 'DoCaseStatement': duplicateCaseConditions(node, out); break;
    case 'IfStatement': emptyBranches(node, out); break;
    case 'TryStatement': tryWithoutHandler(node, out); break;
    case 'PrivateAll':
      out.push(problem(Severity.Warning, node.location, 'private-all',
        'PRIVATE ALL hides every variable of the caller from this routine and everything it calls. Name the variables it needs to hide, or declare the ones this routine owns as LOCAL.'));
      break;
  }
  // todo some day: warn about unused locals
  // todo: warn about naming variables badly (VFP Hungarian prefixes: lc/ln/ll/ld/lo/la, and flag a prefix that disagrees with what is assigned, e.g. lcCount = 0)
  return out;
}

// --- SQL -------------------------------------------------------------------

function havingWithoutGroupBy(node: SelectStatement, out: LintDiagnostic[]) {
  if (!node.having || node.groupBy) return;
  out.push(problem(Severity.Information, node.having.location ?? node.location, 'having-without-group-by',
    'There is no group by clause, so this is simply a post-filter on the result set.'));
}

/**
 * A query with no INTO or TO sends its result set to a Browse window at run time, which inside a .prg
 * is nearly always an unfinished query rather than an intention.
 *
 * Only a SELECT standing on its own as a statement is reported. In an expression it is a subquery, and
 * in an INSERT ... SELECT the INSERT is the destination, so neither has anywhere to put a result.
 */
function undirectedSelects(ast: Program): LintDiagnostic[] {
  const out: LintDiagnostic[] = [];

  function check(node: SelectStatement) {
    if (isWorkAreaSwitch(node) || !node.from || node.destination) return;
    // A UNION carries its INTO on the last SELECT, which leaves the outer node looking undirected.
    if (node.unions.some(u => u.select.destination)) return;
    out.push(problem(Severity.Warning, node.location, 'select-without-into',
      'This query has nowhere to put its result, so VFP sends the whole result set to a Browse window at run time. Add INTO CURSOR, INTO TABLE, INTO ARRAY, or TO FILE.'));
  }

  function walk(value: unknown) {
    if (Array.isArray(value)) {
      for (const item of value) walk(item);
      return;
    }
    if (!value || typeof value !== 'object') return;
    const node = value as AstNode;
    if (node.type === 'Program' || node.type === 'BlockStatement' || node.type === 'DefineClass') {
      for (const statement of node.body) if (statement?.type === 'SelectStatement') check(statement);
    }
    for (const child of Object.values(node as unknown as Record<string, unknown>)) {
      if (child && typeof child === 'object') walk(child);
    }
  }

  walk(ast);
  return out;
}

/**
 * Tables in FROM with nothing relating them are a Cartesian product: every row of each against every
 * row of the others. VFP builds it happily, and at real table sizes it reads as a hang, not an error.
 *
 * Each table is a node and each condition that mentions two of them is an edge; if the graph comes out
 * in more than one piece, something is unrelated. A condition holding a name this query cannot
 * attribute to a table could be the missing link, so it silences the rule rather than risk a false
 * report -- which is why `WHERE cust_id = o_custid` is left alone while `WHERE c.id = 5` is not.
 */
function unlinkedTables(node: SelectStatement, out: LintDiagnostic[]) {
  const from = node.from;
  if (!from) return;

  const participants: string[] = [];
  for (const ref of [...from.tables, ...from.joins.map(j => j.target)]) {
    const key = tableKey(ref);
    if (!key) return; // a derived table: its links cannot be read from here
    participants.push(key);
  }
  if (participants.length < 2) return;

  const named = new Set(participants);
  const parent = new Map(participants.map(name => [name, name]));
  const find = (name: string): string => {
    let root = name;
    while (parent.get(root) !== root) root = parent.get(root)!;
    return root;
  };
  const link = (a: string, b: string) => parent.set(find(a), find(b));

  // Two tables compared to the same value are related through it, which is the usual way a parent and
  // its children are fetched by a key held in a variable.
  const byValue = new Map<string, string>();

  for (const condition of [...from.joins.map(j => j.condition), node.where]) {
    for (const leaf of conditionLeaves(condition)) {
      const refs = leafReferences(leaf, named);
      // A macro expands to source at run time, so it could be the join this rule is looking for.
      if (refs.macro) return;
      for (let i = 1; i < refs.tables.length; i++) link(refs.tables[0], refs.tables[i]);
      // One unattributable name alongside anything else could be the relating term.
      if (refs.unattributed && refs.unattributed + refs.tables.length > 1) return;
      if (refs.tables.length !== 1) continue;
      const value = comparisonCounterpart(leaf, named);
      if (!value) continue;
      const earlier = byValue.get(value);
      if (earlier === undefined) byValue.set(value, refs.tables[0]);
      else if (earlier !== refs.tables[0]) link(earlier, refs.tables[0]);
    }
  }

  const groups = new Map<string, string[]>();
  for (const name of participants) {
    const root = find(name);
    groups.set(root, [...(groups.get(root) ?? []), name]);
  }
  if (groups.size < 2) return;

  const described = [...groups.values()].map(g => g.join(' + ')).join(', ');
  out.push(problem(Severity.Warning, node.location, 'unlinked-tables',
    `Nothing relates these tables to each other: ${described}. VFP will build the Cartesian product -- every row of each against every row of the others -- which on real table sizes reads as a hang rather than an error.`));
}

// A literal cannot carry a key between tables: two tables filtered to the same status are still unrelated.
const literalTypes = new Set(['NumberLiteral', 'StringLiteral', 'BooleanLiteral', 'NullLiteral', 'DateTimeLiteral']);

/** For `t.col = <value>`, the canonical form of the value side, when it is something that could carry a key. */
function comparisonCounterpart(leaf: unknown, named: Set<string>): string | null {
  if (!leaf || typeof leaf !== 'object') return null;
  const node = leaf as AstNode;
  if (node.type !== 'BinaryExpression') return null;
  if (node.operator !== '=' && node.operator !== '==') return null;
  const left = leafReferences(node.left, named);
  const right = leafReferences(node.right, named);
  const value = left.tables.length === 1 && right.tables.length === 0 ? node.right
    : right.tables.length === 1 && left.tables.length === 0 ? node.left
    : null;
  if (!value || literalTypes.has(value.type)) return null;
  return expressionKey(value);
}

/** The name a column of this table would be qualified by: its alias, or the table name. */
function tableKey(ref: TableRef): string | null {
  if (ref.subquery) return null;
  if (ref.alias) return ref.alias.toUpperCase();
  return aliasName(ref.name);
}

// AND and OR only combine conditions; everything else is a term that may or may not relate two tables.
function conditionLeaves(expr: unknown): unknown[] {
  if (!expr || typeof expr !== 'object') return [];
  const node = expr as AstNode;
  if (node.type === 'LogicalExpression') return [...conditionLeaves(node.left), ...conditionLeaves(node.right)];
  return [expr];
}

interface LeafReferences {
  /** Distinct tables of this query that the term mentions. */
  tables: string[];
  /** References that cannot be pinned to one of them: a bare column name, or a qualifier the FROM does not name. */
  unattributed: number;
  /** The term contains a macro, so its real text is not known until run time. */
  macro: boolean;
}

function leafReferences(expr: unknown, named: Set<string>): LeafReferences {
  const tables = new Set<string>();
  let unattributed = 0;
  let macro = false;

  function walk(value: unknown) {
    if (Array.isArray(value)) {
      for (const item of value) walk(item);
      return;
    }
    if (!value || typeof value !== 'object') return;
    const node = value as AstNode;
    if (node.type === 'MemberExpression') {
      const object = node.object;
      if (object.type === 'Identifier') {
        const qualifier = object.name.toUpperCase();
        if (qualifier === 'M') return; // a memory variable, which relates nothing
        if (named.has(qualifier)) tables.add(qualifier);
        else unattributed++;
        return;
      }
      walk(object);
      return;
    }
    if (node.type === 'Identifier') {
      unattributed++;
      return;
    }
    if (node.type === 'MacroSubstitute') {
      macro = true;
      return;
    }
    if (node.type === 'CallExpression') {
      // The callee of a bare call is a function name, not a column.
      if (node.callee.type !== 'Identifier') walk(node.callee);
      walk(node.arguments);
      return;
    }
    for (const child of Object.values(node as unknown as Record<string, unknown>)) {
      if (child && typeof child === 'object') walk(child);
    }
  }

  walk(expr);
  return { tables: [...tables], unattributed, macro };
}

// --- unsupported syntax, and the broken code the same rule used to absorb ---

function unsupportedOrBroken(node: UnknownStatement, opts: Required<LinterOptions>, out: LintDiagnostic[]) {
  const raw = node.raw ?? '';
  const unterminated = blockOpeners.find(b => b.opener.test(raw));
  if (unterminated) {
    // The catch-all also absorbs the opening line of a block whose terminator is missing: the block rule fails and the line falls through to UnknownStatement.
    // That is broken code rather than syntax the linter has not learned, so it stays an Error: the parser never throws for it, and downgrading it with everything else would hide it.
    out.push(problem(Severity.Error, node.location, 'unterminated-block',
      `This opens a block that could not be parsed. Check for a missing ${unterminated.terminator}: '${raw}'`));
    return;
  }
  const severity = severities[opts.unsupportedSyntaxSeverity];
  if (severity === null) return;
  out.push(problem(severity, node.location, 'unsupported-syntax',
    `This statement is valid FoxPro that the linter does not parse yet, so it is not being checked: '${raw}'`));
}

// --- structural smells -----------------------------------------------------

function unreachableCode(node: Program | BlockStatement, out: LintDiagnostic[]) {
  const body = node.body;
  if (!Array.isArray(body)) return;
  for (let i = 0; i < body.length - 1; i++) {
    const statement = body[i];
    const next = body[i + 1];
    if (!statement || !next || !terminators.has(statement.type)) continue;
    if (routineTypes.has(next.type)) continue;
    const keyword = statement.type === 'ReturnStatement' ? 'RETURN'
      : statement.type === 'ContinueStatement' ? 'LOOP' : 'EXIT';
    out.push(problem(Severity.Warning, next.location, 'unreachable-code',
      `This cannot run: the ${keyword} above it leaves the block first.`));
    return; // one report per block is enough to make the point
  }
}

function duplicateCaseConditions(node: DoCaseStatement, out: LintDiagnostic[]) {
  const seen = new Map<string, number>();
  for (const clause of node.cases) {
    if (!clause || clause.type !== 'CaseClause') continue;
    const key = expressionKey(clause.test);
    const first = seen.get(key);
    if (first === undefined) {
      seen.set(key, clause.location?.start?.line ?? 0);
    } else {
      out.push(problem(Severity.Warning, clause.location, 'duplicate-case',
        `This condition is identical to the CASE on line ${first}, so this branch can never be reached.`));
    }
    if (clause.consequent && clause.consequent.body.length === 0)
      out.push(problem(Severity.Information, clause.location, 'empty-branch', emptyBranchMessage('CASE')));
  }
  if (node.otherwise && node.otherwise.body.length === 0)
    out.push(problem(Severity.Information, node.otherwise.location, 'empty-branch', emptyBranchMessage('OTHERWISE')));
}

function emptyBranches(node: IfStatement, out: LintDiagnostic[]) {
  if (node.consequent && node.consequent.body.length === 0)
    out.push(problem(Severity.Information, node.location, 'empty-branch', emptyBranchMessage('IF')));
  if (node.alternate && node.alternate.body.length === 0)
    out.push(problem(Severity.Information, node.location, 'empty-branch', emptyBranchMessage('ELSE')));
}

// Comments are not part of the AST, so a branch holding only a comment reads as empty. That is why
// this is advisory rather than a warning.
const emptyBranchMessages: Record<string, string> = {
  IF: 'This IF branch is empty, so the test decides nothing.',
  ELSE: 'This ELSE branch is empty and can be removed.',
  CASE: 'This CASE branch is empty, so matching it does nothing.',
  OTHERWISE: 'This OTHERWISE branch is empty and can be removed.'
};

function emptyBranchMessage(kind: string) {
  return emptyBranchMessages[kind];
}

function tryWithoutHandler(node: TryStatement, out: LintDiagnostic[]) {
  if (node.catchClause || node.finallyBlock) return;
  out.push(problem(Severity.Information, node.location, 'try-without-catch',
    'This TRY has neither CATCH nor FINALLY, so it handles nothing and any error still propagates to the caller.'));
}

// --- the m. prefix ---------------------------------------------------------

/**
 * When a memory variable and a field of an open table share a name, a bare reference resolves to the
 * field, so `lcName = "x"` can update the record instead of the variable.
 *
 * Which names are fields cannot be known without the table, so this reports only names the file
 * itself shows being used as a field: a column in a CREATE, a REPLACE target, an INSERT column list,
 * or a reference qualified by an alias the file opens. That keeps it quiet unless there is real
 * evidence of a collision.
 */
function missingMemvarPrefix(ast: Program): LintDiagnostic[] {
  const table = buildSymbolTable(ast);
  const aliases = new Set<string>();
  for (const scope of table.scopes) for (const alias of scope.openAliases) aliases.add(alias);
  // With no table in play there is nothing for a bare name to resolve to but the variable.
  if (!aliases.size) return [];

  const fields = collectFieldNames(ast, aliases);
  if (!fields.size) return [];

  const out: LintDiagnostic[] = [];
  for (const scope of table.scopes) {
    for (const symbol of scope.symbols.values()) {
      if (!symbol.declaredAt || !fields.has(symbol.name)) continue;
      for (const ref of [...symbol.writes, ...symbol.reads]) {
        // An m. prefix is already correct, and inside SQL a bare name is expected to be a column.
        if (ref.memvarPrefix || ref.sqlContext || !ref.location?.start) continue;
        out.push(problem(Severity.Warning, ref.location, 'missing-memvar-prefix',
          `'${symbol.declaredAs}' is declared as a variable and is also used as a field name in this file. ` +
          `A bare name resolves to the field${openTableSuffix(scope, ref.location.start.line)}, so write 'm.${symbol.declaredAs}' to be sure of reaching the variable.`));
      }
    }
  }
  return out;
}

// Work areas are shared across routines, so a table opened elsewhere still shadows a name here. The
// alias is only named when this routine is the one that selected it.
function openTableSuffix(scope: Scope, line: number) {
  const alias = aliasInEffectAt(scope, line);
  return alias ? ` while ${alias} is open` : '';
}

function collectFieldNames(ast: Program, aliases: Set<string>): Set<string> {
  const fields = new Set<string>();
  const add = (raw: unknown) => {
    if (typeof raw !== 'string') return;
    const parts = raw.replace(/^[@&]+/, '').split(/\.|->/).filter(Boolean);
    if (parts.length) fields.add(parts[parts.length - 1].toUpperCase());
  };

  function walk(value: unknown) {
    if (Array.isArray(value)) {
      for (const item of value) walk(item);
      return;
    }
    if (!value || typeof value !== 'object') return;
    const node = value as AstNode;
    switch (node.type) {
      case 'ColumnDefinition': add(node.name); break;
      case 'ReplaceStatement': for (const f of node.fields) add(f.field); break;
      case 'InsertStatement': for (const c of node.columns ?? []) add(c); break;
      case 'MemberExpression': {
        // `customer.cust_id` names a field only when `customer` is a work area the file opens.
        const object = node.object;
        if (object.type === 'Identifier' && aliases.has(object.name.toUpperCase())) add(node.property.name);
        break;
      }
    }
    for (const child of Object.values(node as unknown as Record<string, unknown>)) {
      if (child && typeof child === 'object') walk(child);
    }
  }

  walk(ast);
  return fields;
}

// --- shared ----------------------------------------------------------------

function problem(severity: Severity, loc: Loc | undefined, code: string, message: string): LintDiagnostic {
  return { severity, range: toRange(loc), code, message, source: 'VFP Linter' };
}

// Parser locations are 1-based and optional; LSP ranges are 0-based and required.
function toRange(loc: Loc | undefined) {
  const startLine = loc?.start ? loc.start.line - 1 : 0;
  const startCol = loc?.start ? loc.start.column - 1 : 0;
  const endLine = loc?.end ? loc.end.line - 1 : startLine;
  const endCol = loc?.end ? loc.end.column - 1 : startCol + 1;
  return { start: { line: startLine, character: startCol }, end: { line: endLine, character: endCol } };
}

/**
 * A canonical string for an expression, so two CASE conditions can be compared by structure rather
 * than by source text. Identifiers are upper-cased because FoxPro names are case-insensitive; string
 * literals are left alone because their contents are not.
 */
function expressionKey(value: unknown): string {
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
