// Per-routine symbol table: what each routine declares, where each name is read and written, and which work area is open as the routine runs.
// A node rule in rules/ sees one node and knows nothing about what came before it.
// The scope-dependent rules (implicit PRIVATE from an undeclared assignment, unused LOCAL, a missing m. prefix on a name that is also a field, work-area handling) all read this structure instead of re-walking the tree themselves.

import type { AstNode, DefineClass, Expr, Loc, ProcedureStatement, Program, SelectStatement, Statement } from './ast.js';
import { walk } from './rule.js';

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
  /** The routine's own statements, in source order, so a fix can find where a declaration belongs. */
  body: Statement[];
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
  const main = newScope('(main)', 'main', ast?.location ?? null, null, ast?.body ?? []);
  if (ast?.body) visit(ast.body, main);
  return { main, scopes };

  function newScope(name: string, kind: ScopeKind, location: Loc | null, parent: Scope | null, body: Statement[]): Scope {
    const scope: Scope = {
      name, kind, location,
      symbols: new Map(),
      privateAll: false,
      workArea: [],
      openAliases: new Set(),
      parent,
      children: [],
      body
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
        existing.declaredType = typeName(type);
        existing.isArray = existing.isArray || isArray;
      }
      return;
    }
    scope.symbols.set(parsed.name, {
      name: parsed.name,
      declaredAs: parsed.text,
      kind,
      declaredType: typeName(type),
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

  // A routine boundary: a PROCEDURE or FUNCTION at file level, or a method inside a DEFINE CLASS.
  function visitRoutine(node: ProcedureStatement, parent: Scope) {
    const inClass = parent.kind === 'class';
    const name = node.name;
    const scope = newScope(
      inClass ? parent.name + '.' + name : name,
      inClass ? 'method' : (node.isFunction ? 'function' : 'procedure'),
      node.location ?? null,
      parent,
      node.body.body
    );
    for (const param of node.parameters) {
      // Function-style params are { name, type }; the LPARAMETERS form is a bare string.
      const typed = typeof param === 'string' ? null : param;
      declare(scope, typed ? typed.name : param, 'parameter', node.location ?? null, typed ? typed.type : null);
    }
    visit(node.body, scope);
  }

  function visitClass(node: DefineClass, parent: Scope) {
    const scope = newScope(node.name, 'class', node.location ?? null, parent, node.body);
    for (const stmt of node.body) {
      if (!stmt || typeof stmt !== 'object') continue;
      // `cName = ""` at class-body level declares a property, not a variable.
      if (stmt.type === 'Assignment' && stmt.target.type === 'Identifier') {
        declare(scope, stmt.target.name, 'property', stmt.location ?? null);
        visit(stmt.expression, scope);
        continue;
      }
      // So do the member declarations: PROTECTED and HIDDEN name properties, ADD OBJECT names one and gives its class.
      if (stmt.type === 'ClassAccessStatement') {
        for (const name of stmt.names) declare(scope, name, 'property', stmt.location ?? null);
        continue;
      }
      if (stmt.type === 'AddObjectStatement') {
        declare(scope, stmt.name, 'property', stmt.location ?? null, stmt.base);
        visit(stmt.properties, scope);
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

  // Inside WITH, the chain after the dot hangs off the WITH target: `.Objects(m.n).Caption` names two properties and reads one variable. Walk it for the arguments and subscripts, but never book the root identifier -- that is a property name.
  function visitWithMember(expr: Expr | null | undefined, scope: Scope) {
    if (!expr || typeof expr !== 'object') return;
    switch (expr.type) {
      case 'Identifier': return;
      case 'MemberExpression': visitWithMember(expr.object, scope); return;
      case 'CallExpression': visitWithMember(expr.callee, scope); visit(expr.arguments, scope); return;
      case 'ArrayIndexExpression': visitWithMember(expr.object, scope); visit(expr.indexes, scope); return;
      default: visit(expr, scope);
    }
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
      case 'PublicDeclaration': declare(scope, node.name, 'public', at, node.asType, node.isArray); return;
      case 'PrivateDeclaration': declare(scope, node.name, 'private', at, node.asType, node.isArray); return;
      case 'PrivateAll':
      case 'PrivateAllLike':
      case 'PrivateAllExcept':
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
        for (const target of node.targets) {
          if (target.type === 'Var') {
            reference(scope, target.name, 'write', at);
          } else if (target.type === 'ArrayIndexed') {
            reference(scope, target.array, 'write', at);
            visit(target.indexes, scope);
          } else {
            reference(scope, target.target, 'write', at);
            visit(target.expression, scope);
          }
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
      case 'TextBlockStatement':
        // TEXT TO builds the variable's contents, so it is a write. The body is output text rather than code, so a name that appears only inside a <<...>> merge is invisible here -- which cannot produce a false 'unused' because a variable worth merging has to have been assigned somewhere first.
        reference(scope, node.to, 'write', at);
        return;
      case 'WaitStatement':
        // WAIT ... TO puts the key the user pressed into the variable, so it creates the name like any other write.
        reference(scope, node.to, 'write', at);
        visitChildren(node, scope);
        return;
      case 'ConsoleInputStatement':
        // INPUT and ACCEPT put what the user typed in the variable, so they create the name like any other write.
        reference(scope, node.to, 'write', at);
        visit(node.message, scope);
        return;
      case 'MenuToStatement':
        // MENU TO puts the number of the bar the user chose in the variable, so it creates the name like any other write.
        reference(scope, node.to, 'write', at);
        return;
      case 'MenuBarStatement':
        // MENU BAR builds the bar from the array, which is a read of it.
        reference(scope, node.array, 'read', at);
        visit(node.count, scope);
        return;
      case 'SaveScreenStatement':
        // SAVE SCREEN TO puts the screen image in the variable, so it creates the name like any other write; RESTORE SCREEN FROM reads it back.
        reference(scope, node.to, 'write', at);
        return;
      case 'RestoreScreenStatement':
        reference(scope, node.from, 'read', at);
        return;
      case 'DoFormStatement':
        // NAME creates the form object and TO receives what the form returns. Both create the name.
        reference(scope, node.name, 'write', at);
        reference(scope, node.to, 'write', at);
        visit(node.arguments, scope);
        return;
      case 'ScatterStatement':
        // TO and NAME both create the name they are handed. MEMVAR spreads the record over one variable per field, none of which is named here.
        reference(scope, node.name, 'write', at);
        return;
      case 'GatherStatement':
        reference(scope, node.name, 'read', at);
        return;
      case 'CalculateStatement':
      case 'AggregateStatement':
        if (node.to?.kind === 'ARRAY') reference(scope, node.to.name, 'write', at);
        else for (const name of node.to?.vars ?? []) reference(scope, name, 'write', at);
        visit(node.expressions, scope);
        visit(node.forCondition, scope);
        visit(node.whileCondition, scope);
        return;

      case 'TryStatement':
        visit(node.tryBlock, scope);
        for (const clause of node.catchClauses) {
          // CATCH TO creates the error object, so an undeclared name there is an implicit PRIVATE like any other.
          reference(scope, clause.to, 'write', at);
          visit(clause.when, scope);
          visit(clause.body, scope);
        }
        visit(node.thrown, scope);
        visit(node.finallyBlock, scope);
        return;

      // --- work area ---
      case 'UseStatement': {
        if (!node.target) {
          // `USE IN <area>` closes that area and leaves the current one alone; a bare USE closes the current one.
          workArea(scope, 'close', node.inTarget, 'USE', at, node.inTarget != null);
          visit(node.inTarget, scope);
          return;
        }
        // An explicit ALIAS names the area; otherwise it takes the name of the table.
        const named = node.target.kind === 'TABLE' ? node.target.name : null;
        workArea(scope, 'open', node.alias ?? named, 'USE', at, node.inTarget != null);
        // A table or alias named by an expression -- `USE (m.cPath) ALIAS (m.cAlias)` -- reads whatever the expression holds.
        if (node.target.kind === 'EXPR') visit(node.target.value, scope);
        visit(node.alias, scope);
        visit(node.inTarget, scope);
        return;
      }
      case 'CreateStatement':
        workArea(scope, 'open', node.name, 'CREATE ' + node.kind, at);
        if (node.fromArray) reference(scope, node.fromArray, 'read', at);
        return;
      // ALTER TABLE's clauses name columns, not variables, so they are read in SQL context: a bare name is taken for a field, while `DEFAULT m.cValue` still records the variable it reads.
      case 'AlterTableStatement':
        visitSql(node, scope);
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
      case 'MacroSubstitute':
        // `&lcCmd` evaluates the variable's contents as code, which is a read of it like any other. Without this the name is invisible to the symbol table, and a local used only through a macro looks unused.
        reference(scope, node.name, 'read', at);
        return;
      case 'WithMemberExpression':
        visitWithMember(node.expression, scope);
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

/** True when a SELECT names a work area rather than querying, so rules about queries leave it alone. */
export function isWorkAreaSwitch(node: SelectStatement): boolean {
  return workAreaSwitch(node) !== undefined;
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
export function aliasName(value: unknown): string | null {
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

// IdentifierOrString: a bare type name arrives as a string, a quoted one -- `LOCAL loX AS "Custom"` -- as a node.
function typeName(value: unknown): string | null {
  if (typeof value === 'string') return value;
  const node = value as { type?: string; value?: unknown } | null;
  return node && node.type === 'StringLiteral' && typeof node.value === 'string' ? node.value : null;
}

function str(value: unknown): string | null {
  return typeof value === 'string' ? value : null;
}


// --- field names -------------------------------------------------------------
// Which names a file shows being used as columns. There is no table to ask at lint time, so the file's own evidence is all there is: a column in a CREATE, a REPLACE target, an INSERT column list, or a reference qualified by an alias the file opens.

/** Every name this file uses as a field, upper-cased. `aliases` says which qualifiers name a work area rather than an object. */
export function collectFieldNames(ast: AstNode, aliases: Set<string>): Set<string> {
  const fields = new Set<string>();
  const add = (raw: unknown) => {
    if (typeof raw !== 'string') return;
    const parts = raw.replace(/^[@&]+/, '').split(/\.|->/).filter(Boolean);
    if (parts.length) fields.add(parts[parts.length - 1].toUpperCase());
  };
  walk(ast, node => {
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
  });
  return fields;
}

/** The fields written against one alias by name, as they were spelled. Narrower than collectFieldNames, which pools every field the file mentions: this is what completion after `customer.` can offer without guessing. */
export function fieldsQualifiedBy(ast: AstNode, alias: string): Set<string> {
  const wanted = alias.toUpperCase();
  const fields = new Set<string>();
  walk(ast, node => {
    if (node.type !== 'MemberExpression') return;
    const object = node.object;
    if (object.type === 'Identifier' && object.name.toUpperCase() === wanted) fields.add(node.property.name);
  });
  return fields;
}

/** Every alias any routine in the file is seen opening. */
export function openAliasesOf(table: SymbolTable): Set<string> {
  const out = new Set<string>();
  for (const scope of table.scopes) for (const alias of scope.openAliases) out.add(alias);
  return out;
}
