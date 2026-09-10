// Per-routine symbol table: what each routine declares, where each name is read and written, and which work area is open as the routine runs.
// The rules in linter.ts are a stateless AST walk -- getProblemsFromNode sees one node and knows nothing about what came before it.
// The scope-dependent rules (implicit PRIVATE from an undeclared assignment, unused LOCAL, a missing m. prefix on a name that is also a field, work-area handling) all read this structure instead of re-walking the tree themselves.

import type { AstNode, DefineClass, Expr, Loc, ProcedureStatement, Program, SelectStatement } from './ast.js';

/** Which statement brought the name into being. */
export type SymbolKind =
  | 'local'      // LOCAL
  | 'public'     // PUBLIC
  | 'private'    // PRIVATE
  | 'parameter'  // LPARAMETERS / PARAMETERS / FUNCTION f(a, b)
  | 'dimension'  // DIMENSION -- private to the routine unless a LOCAL of the same name precedes it
  | 'property'   // assigned directly in a DEFINE CLASS body
  | 'implicit';  // never declared: a write creates a PRIVATE at runtime, a read may belong to the caller, to a field of an open table, or to a typo

export type ScopeKind = 'main' | 'procedure' | 'function' | 'method' | 'class';

/** One read or write of a name. */
export interface SymbolRef {
  /** The narrowest node the reference could be pinned to; statement-wide for names the grammar returns as plain strings. */
  location: Loc | null;
  /** Written `m.nTotal` rather than bare `nTotal`. Only the prefixed form reliably reaches the variable when a field shares the name. */
  memvarPrefix: boolean;
  /** Inside a SQL statement, where a bare unprefixed name may be a column rather than a variable. Treat such a reference as ambiguous: enough to call a local "used", not enough to claim the variable was really touched. */
  sqlContext: boolean;
}

export interface SymbolEntry {
  /** Upper-cased: VFP identifiers are case-insensitive. */
  name: string;
  /** The spelling at the declaration, or at the first reference when nothing declared it. */
  declaredAs: string;
  kind: SymbolKind;
  /** The AS clause of the declaration, when it had one. */
  declaredType: string | null;
  isArray: boolean;
  /** The declaring statement. Null for 'implicit'. */
  declaredAt: Loc | null;
  reads: SymbolRef[];
  writes: SymbolRef[];
}

export type WorkAreaEventKind =
  | 'open'    // USE, CREATE CURSOR/TABLE, SELECT ... INTO CURSOR/TABLE
  | 'select'  // SELECT <alias> | SELECT <n>
  | 'close';  // bare USE

export interface WorkAreaEvent {
  kind: WorkAreaEventKind;
  /** Upper-cased alias, or null when it cannot be resolved statically (a macro, an expression, SELECT 0). */
  alias: string | null;
  /** An IN clause aimed the statement at another work area, so the current one is left alone. */
  targeted: boolean;
  location: Loc | null;
  /** The statement responsible, for diagnostic messages. */
  via: string;
}

export interface Scope {
  /** 'MyProc', 'MyClass.Init', or '(main)' for file-level code. */
  name: string;
  kind: ScopeKind;
  location: Loc | null;
  /** Keyed by upper-cased name. */
  symbols: Map<string, SymbolEntry>;
  /** PRIVATE ALL [LIKE] hides the caller's variables, so an undeclared name here is not inherited. */
  privateAll: boolean;
  /** Work-area changes, in source order. */
  workArea: WorkAreaEvent[];
  /** Every alias this routine is known to open. */
  openAliases: Set<string>;
  /** Textual containment only. VFP scoping is dynamic, so this is not a name-resolution chain. */
  parent: Scope | null;
  children: Scope[];
}

export interface SymbolTable {
  /** File-level code, outside any PROCEDURE or FUNCTION. */
  main: Scope;
  /** Every scope, `main` first, in source order. */
  scopes: Scope[];
}

// THIS and friends are always in scope and are never memory variables.
const pseudoVariables = new Set(['THIS', 'THISFORM', 'THISFORMSET', 'PARENT']);

export function buildSymbolTable(ast: Program | null | undefined): SymbolTable {
  const scopes: Scope[] = [];
  // Depth rather than a flag, because subqueries nest.
  let sqlDepth = 0;
  const main = newScope('(main)', 'main', ast?.location ?? null, null);
  if (ast?.body) visit(ast.body, main);
  return { main, scopes };

  function newScope(name: string, kind: ScopeKind, location: Loc | null, parent: Scope | null): Scope {
    const scope: Scope = {
      name, kind, location,
      symbols: new Map(),
      privateAll: false,
      workArea: [],
      openAliases: new Set(),
      parent,
      children: []
    };
    if (parent) parent.children.push(scope);
    scopes.push(scope);
    return scope;
  }

  function declare(scope: Scope, raw: unknown, kind: SymbolKind, at: Loc | null, type: unknown = null, isArray = false) {
    const parsed = parseName(raw);
    if (!parsed || parsed.qualifier) return; // a field or object property, not a memvar
    const existing = scope.symbols.get(parsed.name);
    if (existing) {
      // Referenced above its own declaration, or declared twice. Keep the refs and the first site.
      if (existing.kind === 'implicit') {
        existing.kind = kind;
        existing.declaredAs = parsed.text;
        existing.declaredAt = at;
        existing.declaredType = str(type);
        existing.isArray = existing.isArray || isArray;
      }
      return;
    }
    scope.symbols.set(parsed.name, {
      name: parsed.name,
      declaredAs: parsed.text,
      kind,
      declaredType: str(type),
      isArray,
      declaredAt: at,
      reads: [],
      writes: []
    });
  }

  function reference(scope: Scope, raw: unknown, mode: 'read' | 'write', at: Loc | null, memvarPrefix = false) {
    const parsed = parseName(raw);
    if (!parsed || parsed.qualifier) return;
    if (pseudoVariables.has(parsed.name)) return;
    let entry = scope.symbols.get(parsed.name);
    if (!entry) {
      entry = {
        name: parsed.name,
        declaredAs: parsed.text,
        kind: 'implicit',
        declaredType: null,
        isArray: false,
        declaredAt: null,
        reads: [],
        writes: []
      };
      scope.symbols.set(parsed.name, entry);
    }
    const ref: SymbolRef = { location: at, memvarPrefix: memvarPrefix || parsed.memvarPrefix, sqlContext: sqlDepth > 0 };
    if (mode === 'write') entry.writes.push(ref); else entry.reads.push(ref);
  }

  function workArea(scope: Scope, kind: WorkAreaEventKind, alias: unknown, via: string, at: Loc | null, targeted = false) {
    const name = aliasName(alias);
    scope.workArea.push({ kind, alias: name, targeted, location: at, via });
    if (kind !== 'close' && name) scope.openAliases.add(name);
  }

  // A routine boundary. PROCEDURE/FUNCTION do not nest in VFP, but the grammar nests them when ENDPROC is omitted, so a nested routine becomes its own scope either way.
  function visitRoutine(node: ProcedureStatement, parent: Scope) {
    const inClass = parent.kind === 'class';
    const name = node.name;
    const scope = newScope(
      inClass ? parent.name + '.' + name : name,
      inClass ? 'method' : (node.isFunction ? 'function' : 'procedure'),
      node.location ?? null,
      parent
    );
    for (const param of node.parameters) {
      // Function-style params are { name, type }; the LPARAMETERS form is a bare string.
      const typed = typeof param === 'string' ? null : param;
      declare(scope, typed ? typed.name : param, 'parameter', node.location ?? null, typed ? typed.type : null);
    }
    visit(node.body, scope);
    visit(node.returnExpression, scope);
  }

  function visitClass(node: DefineClass, parent: Scope) {
    const scope = newScope(node.name, 'class', node.location ?? null, parent);
    for (const stmt of node.body) {
      if (!stmt || typeof stmt !== 'object') continue;
      // `cName = ""` at class-body level declares a property, not a variable.
      if (stmt.type === 'Assignment' && stmt.target.type === 'Identifier') {
        declare(scope, stmt.target.name, 'property', stmt.location ?? null);
        visit(stmt.expression, scope);
        continue;
      }
      visit(stmt, scope);
    }
  }

  // Assignment targets: a bare name or m.name is a write, anything else (THIS.x, oObj.x) is a read of the object it hangs off.
  function visitTarget(target: Expr | string | null | undefined, scope: Scope, at: Loc | null) {
    if (!target || typeof target !== 'object') {
      reference(scope, target, 'write', at);
      return;
    }
    if (target.type === 'Identifier') {
      reference(scope, target.name, 'write', target.location ?? at);
      return;
    }
    if (target.type === 'MemberExpression') {
      if (isMemvarPrefix(target.object)) {
        reference(scope, target.property.name, 'write', target.location ?? at, true);
        return;
      }
      visit(target.object, scope);
      return;
    }
    if (target.type === 'ArrayIndexExpression') {
      visitTarget(target.object, scope, at);
      visit(target.indexes, scope);
      return;
    }
    visit(target, scope);
  }

  function visit(value: unknown, scope: Scope): void {
    if (Array.isArray(value)) {
      for (const item of value) visit(item, scope);
      return;
    }
    if (!value || typeof value !== 'object') return;
    // A few of the grammar's plain option objects also carry a `type` field; they match no case below and fall through to visitChildren.
    const node = value as AstNode;
    const at = node.location ?? null;

    switch (node.type) {
      // --- routine and class boundaries ---
      case 'ProcedureStatement': visitRoutine(node, scope); return;
      case 'DefineClass': visitClass(node, scope); return;

      // --- declarations ---
      case 'LocalDeclaration':
        declare(scope, node.name, 'local', at, node.asType);
        return;
      case 'LocalArrayDeclaration':
        declare(scope, node.name, 'local', at, node.asType, true);
        visit(node.rows, scope);
        visit(node.columns, scope);
        return;
      case 'PublicDeclaration': declare(scope, node.name, 'public', at); return;
      case 'PrivateDeclaration': declare(scope, node.name, 'private', at); return;
      case 'PrivateAll':
      case 'PrivateAllLike':
        scope.privateAll = true;
        return;
      case 'ParametersDeclaration':
        for (const name of node.names) declare(scope, name, 'parameter', at);
        return;
      case 'DimensionStatement':
        for (const item of node.items) {
          declare(scope, item.name, 'dimension', at, item.asType, true);
          visit(item.rows, scope);
          visit(item.columns, scope);
        }
        return;

      // --- writes ---
      case 'Assignment':
        visitTarget(node.target, scope, at);
        visit(node.expression, scope);
        return;
      case 'StoreStatement': {
        const target = node.target;
        if (target.type === 'VarList') {
          for (const name of target.vars) reference(scope, name, 'write', at);
        } else if (target.type === 'ArrayIndexed') {
          reference(scope, target.array, 'write', at);
          visit(target.indexes, scope);
        } else {
          reference(scope, target.target, 'write', at);
          visit(target.expression, scope);
        }
        visit(node.expression, scope);
        return;
      }
      case 'ForStatement':
        reference(scope, node.variable, 'write', at);
        visit(node.init, scope);
        visit(node.final, scope);
        visit(node.step, scope);
        visit(node.body, scope);
        return;
      case 'ForEachStatement':
        reference(scope, node.variable, 'write', at);
        visit(node.collection, scope);
        visit(node.body, scope);
        return;
      case 'CalculateStatement':
      case 'SumStatement':
        if (node.to?.kind === 'ARRAY') reference(scope, node.to.name, 'write', at);
        else for (const name of node.to?.vars ?? []) reference(scope, name, 'write', at);
        visit(node.expressions, scope);
        visit(node.forCondition, scope);
        visit(node.whileCondition, scope);
        return;

      // --- work area ---
      case 'UseStatement': {
        if (!node.target) {
          workArea(scope, 'close', null, 'USE', at);
          return;
        }
        // An explicit ALIAS names the area; otherwise it takes the name of the table.
        const named = node.target.kind === 'TABLE' ? node.target.name : null;
        workArea(scope, 'open', node.alias ?? named, 'USE', at, node.inTarget != null);
        visit(node.inTarget, scope);
        return;
      }
      case 'CreateStatement':
        workArea(scope, 'open', node.name, 'CREATE ' + node.kind, at);
        if (node.fromArray) reference(scope, node.fromArray, 'read', at);
        return;
      case 'SelectStatement': {
        const switched = workAreaSwitch(node);
        if (switched !== undefined) {
          workArea(scope, 'select', switched, 'SELECT', at);
          return; // `SELECT myalias` names a work area, not a column
        }
        const destination = node.destination;
        if (destination?.kind === 'ARRAY') {
          reference(scope, destination.name, 'write', at);
        } else if (destination?.kind === 'CURSOR' || destination?.kind === 'TABLE' || destination?.kind === 'DBF') {
          workArea(scope, 'open', destination.name, 'SELECT INTO ' + destination.kind, at);
        }
        visitSql(node, scope, ['destination']);
        return;
      }
      case 'InsertStatement':
      case 'UpdateStatement':
      case 'DeleteStatement':
        visitSql(node, scope);
        return;

      // --- expressions ---
      case 'Identifier':
        reference(scope, node.name, 'read', at);
        return;
      case 'MemberExpression':
        // `m.nTotal` is the variable; `tbl.field` and `oObj.prop` are not.
        if (isMemvarPrefix(node.object)) {
          reference(scope, node.property.name, 'read', at, true);
          return;
        }
        visit(node.object, scope);
        return;
      case 'CallExpression':
        // The callee of a bare call is a function name, not a variable read.
        if (node.callee.type !== 'Identifier') visit(node.callee, scope);
        visit(node.arguments, scope);
        return;

      default:
        visitChildren(node, scope);
        return;
    }
  }

  function visitSql(node: AstNode, scope: Scope, skip: string[] = []) {
    sqlDepth++;
    visitChildren(node, scope, skip);
    sqlDepth--;
  }

  // A generic walk over a union has to reach the properties reflectively.
  function visitChildren(node: AstNode, scope: Scope, skip: string[] = []) {
    for (const [key, child] of Object.entries(node as unknown as Record<string, unknown>)) {
      if (key === 'location' || skip.includes(key)) continue;
      if (child && typeof child === 'object') visit(child, scope);
    }
  }
}

/** `SELECT <alias>` and `SELECT <n>` switch work areas rather than querying. Returns the alias, null when it is `SELECT 0` or otherwise unknowable statically, or undefined when the statement is a real query. */
function workAreaSwitch(node: SelectStatement): string | null | undefined {
  const isQuery = node.from ?? node.where ?? node.groupBy ?? node.having ?? node.orderBy ?? node.destination ?? node.top ?? node.quantifier;
  if (isQuery != null || node.unions.length) return undefined;
  if (node.list.length !== 1) return undefined;
  const item = node.list[0];
  if (item.type !== 'SelectItem' || item.alias) return undefined;
  if (item.expression.type === 'Identifier') return item.expression.name.toUpperCase();
  if (item.expression.type === 'NumberLiteral') return null; // SELECT 0 takes the lowest free area
  return undefined;
}

/** The alias in effect at a 1-based source line, as far as static analysis can tell: the alias name, null when no table is open, or undefined when it cannot be determined. */
export function aliasInEffectAt(scope: Scope, line: number): string | null | undefined {
  let current: string | null | undefined = null;
  for (const event of scope.workArea) {
    if ((event.location?.start?.line ?? 0) > line) break;
    if (event.targeted) continue; // USE ... IN <other area> leaves the current one alone
    if (event.kind === 'close') current = null;
    else current = event.alias ?? undefined;
  }
  return current;
}

interface ParsedName {
  /** Upper-cased bare name. */
  name: string;
  /** The name as written, prefixes stripped. */
  text: string;
  memvarPrefix: boolean;
  /** A non-`m` qualifier, i.e. `tbl` in `tbl.field` or `tbl->field`. */
  qualifier: string | null;
}

// ParameterName accepts @/& prefixes and dotted or -> qualified names, so declarations and some assignment targets arrive as raw strings like '@lcName', 'm.nTotal' or 'tbl->field'.
function parseName(raw: unknown): ParsedName | null {
  const text = str(raw);
  if (!text) return null;
  const parts = text.replace(/^[@&]+/, '').split(/\.|->/).filter(Boolean);
  if (!parts.length) return null;
  const name = parts[parts.length - 1];
  if (parts.length === 1) return { name: name.toUpperCase(), text: name, memvarPrefix: false, qualifier: null };
  const qualifier = parts[0].toUpperCase();
  const isMemvar = qualifier === 'M' && parts.length === 2;
  return { name: name.toUpperCase(), text: name, memvarPrefix: isMemvar, qualifier: isMemvar ? null : qualifier };
}

// `m.name` reaches the memory variable even when a field of an open table shares the name.
function isMemvarPrefix(object: Expr): boolean {
  return object.type === 'Identifier' && object.name.toUpperCase() === 'M';
}

// USE and SELECT INTO name their target as a string, a Path node, an Identifier or a QualifiedTable wrapper, depending on the clause.
function aliasName(value: unknown): string | null {
  if (typeof value === 'string') return value.toUpperCase();
  if (!value || typeof value !== 'object') return null;
  const node = value as AstNode;
  if (node.type === 'Path') return node.path.toUpperCase();
  if (node.type === 'Identifier') return node.name.toUpperCase();
  if (node.type === 'StringLiteral') return node.value.toUpperCase();
  // A QualifiedTable wrapper: { database, table }.
  const bag = value as { name?: unknown; table?: unknown };
  if (bag.name) return aliasName(bag.name);
  if (bag.table) return aliasName(bag.table);
  return null;
}

function str(value: unknown): string | null {
  return typeof value === 'string' ? value : null;
}

