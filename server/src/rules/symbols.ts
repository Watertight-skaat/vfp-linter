// The rules that read the symbol table: a name nothing declared, a declaration nothing uses, and a variable that shares its name with a field.

import type { AstNode, Loc } from '../ast.js';
import { onFile, Severity, walk, type Fix, type RuleContext } from '../rule.js';
import { aliasInEffectAt, type Scope, type SymbolEntry, type SymbolRef } from '../scope.js';

// VFP has no declaration requirement: assigning to a name nothing declared creates a PRIVATE at run time, which every routine called from here can see and assign. So a mistyped name silently becomes a new variable, and state leaks downstream instead of staying where it was written. Reported once per name, at the first write -- the missing declaration is the finding, not each use of it.
export const implicitPrivate = onFile({
  code: 'implicit-private',
  severity: Severity.Warning,
  check(ctx) {
    for (const scope of ctx.table.scopes) {
      for (const symbol of scope.symbols.values()) {
        if (symbol.kind !== 'implicit') continue;
        // A bare name inside SQL may be a column, so it is not evidence that a variable was created.
        const first = symbol.writes.find(w => !w.sqlContext && w.location?.start);
        if (!first?.location) continue;
        ctx.report(first.location,
          `'${symbol.declaredAs}' is assigned but never declared, so FoxPro creates it as a PRIVATE: ` +
          `it stays visible to everything ${scopeLabel(scope)} calls, and a mistyped name becomes a new variable rather than an error. ` +
          `Declare it LOCAL to keep it here, or PRIVATE to say the visibility is deliberate.`,
          declareLocal(scope, symbol, ctx));
      }
    }
  }
});

// The other half of the pair: implicit-private reports an entry with no declaration, this one a declaration with no reference. LOCAL only -- PUBLIC and PRIVATE exist to be seen by the routines this one calls, so silence here says nothing about them, and an unused parameter is usually a signature the caller still passes. A reference inside SQL counts: a bare name there may be a column rather than the variable, which is not enough to claim the variable was touched but is enough to stop calling it unused.
export const unusedLocal = onFile({
  code: 'unused-local',
  severity: Severity.Warning,
  check(ctx) {
    for (const scope of ctx.table.scopes) {
      for (const symbol of scope.symbols.values()) {
        if (symbol.kind !== 'local' || !symbol.declaredAt?.start) continue;
        if (symbol.reads.length || symbol.writes.length) continue;
        ctx.report(symbol.declaredAt,
          `'${symbol.declaredAs}' is declared LOCAL in ${scopeLabel(scope)} and then never read or written. ` +
          `Either it is left over and can go, or the name it was meant to be used under is misspelled somewhere below ` +
          `-- in which case that spelling is creating a PRIVATE of its own.`,
          removeLocal(symbol, ctx));
      }
    }
  }
});

/**
 * When a memory variable and a field of an open table share a name, a bare reference resolves to the field, so `lcName = "x"` can update the record instead of the variable.
 *
 * Which names are fields cannot be known without the table, so this reports only names the file itself shows being used as a field: a column in a CREATE, a REPLACE target, an INSERT column list, or a reference qualified by an alias the file opens. That keeps it quiet unless there is real evidence of a collision.
 */
export const missingMemvarPrefix = onFile({
  code: 'missing-memvar-prefix',
  severity: Severity.Warning,
  check(ctx) {
    const aliases = new Set<string>();
    for (const scope of ctx.table.scopes) for (const alias of scope.openAliases) aliases.add(alias);
    // With no table in play there is nothing for a bare name to resolve to but the variable.
    if (!aliases.size) return;

    const fields = collectFieldNames(ctx.ast, aliases);
    if (!fields.size) return;

    for (const scope of ctx.table.scopes) {
      for (const symbol of scope.symbols.values()) {
        if (!symbol.declaredAt || !fields.has(symbol.name)) continue;
        for (const ref of [...symbol.writes, ...symbol.reads]) {
          // An m. prefix is already correct, and inside SQL a bare name is expected to be a column.
          if (ref.memvarPrefix || ref.sqlContext || !ref.location?.start) continue;
          ctx.report(ref.location,
            `'${symbol.declaredAs}' is declared as a variable and is also used as a field name in this file. ` +
            `A bare name resolves to the field${openTableSuffix(scope, ref.location.start.line)}, so write 'm.${symbol.declaredAs}' to be sure of reaching the variable.`,
            prefixMemvar(symbol, ref, ctx));
        }
      }
    }
  }
});

function scopeLabel(scope: Scope) {
  return scope.kind === 'main' ? 'this file' : scope.name;
}

// Work areas are shared across routines, so a table opened elsewhere still shadows a name here. The alias is only named when this routine is the one that selected it.
function openTableSuffix(scope: Scope, line: number) {
  const alias = aliasInEffectAt(scope, line);
  return alias ? ` while ${alias} is open` : '';
}

function collectFieldNames(ast: AstNode, aliases: Set<string>): Set<string> {
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

// --- quick fixes -----------------------------------------------------------

// Statements that belong at the top of a routine, ahead of the first real statement, so a new LOCAL goes after them rather than before.
const preamble = new Set(['ParametersDeclaration', 'LocalDeclaration', 'LocalArrayDeclaration', 'PrivateDeclaration', 'PublicDeclaration', 'PrivateAll', 'PrivateAllLike', 'PrivateAllExcept', 'DimensionStatement', 'DefineStatement', 'IncludeStatement', 'ExternalStatement']);

// The declaration goes at the top of the routine rather than above the first write: LOCAL resets the variable to .F., so placed inside a loop it would wipe the value on every pass.
function declareLocal(scope: Scope, symbol: SymbolEntry, ctx: RuleContext): Fix | undefined {
  if (scope.kind === 'class') return undefined; // a class body holds properties, not variables
  const first = scope.body.find(s => s && !preamble.has(s.type));
  if (!first?.location?.start) return undefined;
  const line = first.location.start.line - 1;
  const indent = /^\s*/.exec(ctx.lines[line] ?? '')![0];
  return {
    title: `Declare '${symbol.declaredAs}' as LOCAL`,
    edits: [{ range: { start: { line, character: 0 }, end: { line, character: 0 } }, newText: `${indent}LOCAL ${symbol.declaredAs}${ctx.eol}` }]
  };
}

// Some references arrive with a statement-wide location because the grammar returned the name as a plain string. The fix is only offered where the source at the location is the name itself, so the prefix can never land in the middle of a statement.
function prefixMemvar(symbol: SymbolEntry, ref: SymbolRef, ctx: RuleContext): Fix | undefined {
  const start = ref.location?.start;
  if (!start) return undefined;
  const text = ctx.lines[start.line - 1] ?? '';
  const col = start.column - 1;
  if (text.substr(col, symbol.name.length).toUpperCase() !== symbol.name) return undefined;
  if (/[\w.>]/.test(text.charAt(col - 1)) || /\w/.test(text.charAt(col + symbol.name.length))) return undefined;
  const at = { line: start.line - 1, character: col };
  return { title: `Write 'm.${symbol.declaredAs}'`, edits: [{ range: { start: at, end: at }, newText: 'm.' }] };
}

// Removes the name from its LOCAL line, or the whole line when it was the only name. A declaration continued across lines is left alone rather than guessed at.
function removeLocal(symbol: SymbolEntry, ctx: RuleContext): Fix | undefined {
  const line = (symbol.declaredAt as Loc).start!.line - 1;
  const text = ctx.lines[line] ?? '';
  const m = /^(\s*LOCAL\b(?:\s+ARRAY\b)?\s*)(.*?)(\s*&&.*)?$/i.exec(text);
  if (!m || m[2].endsWith(';')) return undefined;
  const entries = splitTopLevel(m[2]);
  const kept = entries.filter(e => !new RegExp(`^${symbol.name}\\b`, 'i').test(e));
  if (kept.length === entries.length) return undefined;
  const title = `Remove the unused LOCAL '${symbol.declaredAs}'`;
  if (!kept.length) {
    const last = line + 1 >= ctx.lines.length;
    const end = last ? { line, character: text.length } : { line: line + 1, character: 0 };
    return { title, edits: [{ range: { start: { line, character: 0 }, end }, newText: '' }] };
  }
  const rebuilt = m[1] + kept.join(', ') + (m[3] ?? '');
  return { title, edits: [{ range: { start: { line, character: 0 }, end: { line, character: text.length } }, newText: rebuilt }] };
}

// A LOCAL list split at its commas, leaving array dimensions like `laRows(2, 3)` whole.
function splitTopLevel(list: string): string[] {
  const out: string[] = [];
  let depth = 0, buf = '';
  for (const c of list) {
    if (c === '(' || c === '[') depth++;
    else if (c === ')' || c === ']') depth--;
    if (c === ',' && depth === 0) { out.push(buf.trim()); buf = ''; } else buf += c;
  }
  if (buf.trim()) out.push(buf.trim());
  return out;
}
