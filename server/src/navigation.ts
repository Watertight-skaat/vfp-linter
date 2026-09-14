// What the editor asks about one position in one file: where a name is defined, what it is, and what the workspace holds by that name.
// Pure, and it answers from the index rather than from the tree: a reference is a reference whether it was found by the parser or by the header scan, so the same code answers for an open file and for one the crawler has only skimmed.

import type { AstNode } from './ast.js';
import { baseOf, findWord, maskStrings, upper, type ClassRecord, type ConstantRecord, type FileRecord, type RefKind, type RoutineRecord, type WorkspaceView } from './index.js';
import { fieldsQualifiedBy } from './scope.js';
import type { Position, Range } from './rule.js';

export interface Location {
  file: string;
  range: Range;
}

export type SymbolSort = 'procedure' | 'function' | 'class' | 'method' | 'property' | 'constant';

export interface WorkspaceSymbol {
  name: string;
  sort: SymbolSort;
  detail: string;
  location: Location;
  /** The class a method belongs to, for the editor's container column. */
  container: string | null;
}

export interface Hover {
  markdown: string;
  range: Range;
  /** The file the name resolves to, when it names one. What the editor needs to offer to open it in VFP's own designer. */
  file?: string;
}

/** What sits under the cursor. A reference the file makes, or a name the tree does not treat as one -- a bare word that may still be a constant or a routine. */
export interface Target {
  kind: RefKind | 'word';
  name: string;
  range: Range;
  dynamic: boolean;
}

const contains = (range: Range, position: Position): boolean => {
  if (position.line < range.start.line || position.line > range.end.line) return false;
  if (position.line === range.start.line && position.character < range.start.character) return false;
  if (position.line === range.end.line && position.character > range.end.character) return false;
  return true;
};

const width = (range: Range) => (range.end.line - range.start.line) * 1e6 + (range.end.character - range.start.character);

/** Whether a range covers the name and nothing else, so the cursor being inside it means the cursor is on the name. */
function coversOnlyTheName(range: Range, lines: string[], name: string): boolean {
  if (range.start.line !== range.end.line) return false;
  const text = (lines[range.start.line] ?? '').slice(range.start.character, range.end.character).trim().replace(/^["']|["']$/g, '');
  return upper(text) === upper(name);
}

/**
 * The reference under the cursor, or the bare word there when the file makes none.
 *
 * Most references arrive with the name's own range, and being inside one settles it. The rest carry the whole statement, because the grammar returns their name as a bare string with no position of its own -- `DO FORM custedit NAME oForm` is one range covering both names. Those are only taken when the word under the cursor is the one they name, so the cursor on `oForm` does not jump to the form.
 */
export function targetAt(record: FileRecord, lines: string[], position: Position): Target | null {
  const covering = record.refs.filter(ref => contains(ref.range, position)).sort((a, b) => width(a.range) - width(b.range));
  const best = covering.find(ref => !ref.dynamic && ref.name) ?? covering[0];
  const word = wordAt(lines, position);
  const takeRef = best && (!word || best.dynamic || coversOnlyTheName(best.range, lines, best.name) || upper(word.name) === best.key);
  if (takeRef) return { kind: best.kind, name: best.name, range: best.range, dynamic: best.dynamic };
  return word ? { kind: 'word', name: word.name, range: word.range, dynamic: false } : null;
}

/** The identifier the position sits in, as written. */
export function wordAt(lines: string[], position: Position): { name: string; range: Range } | null {
  const text = lines[position.line] ?? '';
  let start = position.character;
  let end = position.character;
  const isWord = (c: string) => /[A-Za-z0-9_]/.test(c);
  while (start > 0 && isWord(text[start - 1])) start--;
  while (end < text.length && isWord(text[end])) end++;
  if (start === end) return null;
  return { name: text.slice(start, end), range: { start: { line: position.line, character: start }, end: { line: position.line, character: end } } };
}

/** Where the name under the cursor is defined. Same file first, then the workspace, in the order VFP would find it. */
export function definitionAt(record: FileRecord, lines: string[], position: Position, view: WorkspaceView): Location[] {
  const target = targetAt(record, lines, position);
  // A name assembled at run time has no one definition, and guessing at one is worse than answering nothing.
  if (!target || target.dynamic || !target.name) return [];
  return definitionsOf(target, view);
}

function definitionsOf(target: Target, view: WorkspaceView): Location[] {
  const asFile = (kind: RefKind): Location[] => {
    const file = view.resolveFile(target.name, kind);
    return file ? [{ file, range: { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } } }] : [];
  };

  switch (target.kind) {
    case 'include': return asFile('include');
    case 'procedure': return asFile('procedure');
    case 'form': return asFile('form');
    case 'classlib': return asFile('classlib');
    case 'object': {
      const classes = view.index.classesNamed(target.name);
      return classes.length ? classes.map(toLocation) : asFile('classlib');
    }
    // A bare `DO foo` names a routine if the tree has one and a foo.prg otherwise, which is the order VFP itself resolves them in.
    case 'do': case 'call': case 'word': {
      const routines = view.routines(target.name);
      if (routines.length) return routines.map(toLocation);
      const constants = view.index.constantsNamed(target.name);
      if (constants.length) return constants.map(toLocation);
      const classes = view.index.classesNamed(target.name);
      if (classes.length) return classes.map(toLocation);
      return target.kind === 'word' ? [] : asFile(target.kind === 'do' ? 'do' : 'call');
    }
    default: return [];
  }
}

const toLocation = (record: { file: string; range: Range }): Location => ({ file: record.file, range: record.range });

/** The signature, the docstring and, for a constant, the value: what VFP code carries instead of documentation. */
export function hoverAt(record: FileRecord, lines: string[], position: Position, view: WorkspaceView): Hover | null {
  const target = targetAt(record, lines, position);
  if (!target) return null;
  if (target.dynamic) return { markdown: 'Named at run time, so the linter cannot say what this refers to.', range: target.range };
  if (!target.name) return null;

  // Standing on a definition describes the definition, not a call to it.
  const own = definitionHere(record, position);
  if (own) return { markdown: own, range: target.range };

  const routines = target.kind === 'include' || target.kind === 'procedure' || target.kind === 'classlib' || target.kind === 'form' ? [] : view.routines(target.name);
  if (routines.length) return { markdown: describeRoutine(routines[0], routines.length), range: target.range };

  const constants = view.index.constantsNamed(target.name);
  if (constants.length) return { markdown: describeConstant(constants[0]), range: target.range };

  const classes = view.index.classesNamed(target.name);
  if (classes.length) return { markdown: describeClass(classes[0]), range: target.range };

  if (target.kind !== 'word' && target.kind !== 'do' && target.kind !== 'call') {
    const file = view.resolveFile(target.name, target.kind);
    if (file) return { markdown: `\`${file}\``, range: target.range, file };
    return { markdown: `No file named \`${target.name}\` in the workspace.`, range: target.range };
  }
  return null;
}

/** The definition the position stands on, when it is one this file makes. */
function definitionHere(record: FileRecord, position: Position): string | null {
  for (const constant of record.constants) if (position.line === constant.range.start.line) return describeConstant(constant);
  for (const routine of record.routines) if (position.line === routine.range.start.line) return describeRoutine(routine, 1);
  for (const klass of record.classes) {
    if (position.line === klass.range.start.line) return describeClass(klass);
    for (const method of klass.methods) if (position.line === method.range.start.line) return describeRoutine(method, 1);
  }
  return null;
}

export function signatureOf(routine: RoutineRecord): string {
  const word = routine.isFunction ? 'FUNCTION' : 'PROCEDURE';
  const owner = routine.owner ? `${routine.owner}.` : '';
  return `${word} ${owner}${routine.name}(${routine.params.join(', ')})`;
}

function describeRoutine(routine: RoutineRecord, count: number): string {
  const parts = ['```foxpro', signatureOf(routine), '```', `*${baseOf(routine.file)}*`];
  if (routine.doc) parts.push('', routine.doc);
  // A name with more than one definition is the shape duplicate-routine reports; saying so here costs a line and explains the jump landing where it did.
  if (count > 1) parts.push('', `Defined in ${count} files.`);
  return parts.join('\n');
}

function describeConstant(constant: ConstantRecord): string {
  return ['```foxpro', `#DEFINE ${constant.name} ${constant.value ?? ''}`.trimEnd(), '```', `*${baseOf(constant.file)}*`].join('\n');
}

function describeClass(klass: ClassRecord): string {
  const parts = ['```foxpro', `DEFINE CLASS ${klass.name}${klass.base ? ` AS ${klass.base}` : ''}`, '```', `*${baseOf(klass.file)}*`];
  if (klass.methods.length) parts.push('', `Methods: ${klass.methods.map(m => m.name).join(', ')}`);
  return parts.join('\n');
}

// --- workspace symbols -------------------------------------------------------

/** Every definition in the tree whose name matches, best first. An empty query lists everything, which is what the editor asks for when its box is still empty. */
export function workspaceSymbols(view: Pick<WorkspaceView, 'index'>, query: string, limit = 500): WorkspaceSymbol[] {
  const wanted = upper(query.trim());
  const scored: { score: number; symbol: WorkspaceSymbol }[] = [];
  for (const definition of view.index.definitions()) {
    const score = match(upper(definition.name), wanted);
    if (score < 0) continue;
    scored.push({ score, symbol: toSymbol(definition) });
    // Keeping the whole tree sorted to show twenty rows is work nobody sees; a tree big enough to hit this has a more specific query behind it.
    if (scored.length > limit * 4) break;
  }
  scored.sort((a, b) => a.score - b.score || a.symbol.name.localeCompare(b.symbol.name));
  return scored.slice(0, limit).map(s => s.symbol);
}

/** Lower is better: a prefix beats a substring beats letters merely in order. Negative means no match at all. */
function match(name: string, query: string): number {
  if (!query) return name.length;
  if (name.startsWith(query)) return name.length - query.length;
  const at = name.indexOf(query);
  if (at > 0) return 1000 + at;
  let i = 0;
  for (const c of name) if (c === query[i]) i++;
  return i === query.length ? 2000 + name.length : -1;
}

function toSymbol(definition: RoutineRecord | ClassRecord | ConstantRecord): WorkspaceSymbol {
  const location = { file: definition.file, range: definition.range };
  if ('methods' in definition) return { name: definition.name, sort: 'class', detail: definition.base ? `AS ${definition.base}` : '', location, container: null };
  if ('params' in definition) {
    const routine = definition;
    return { name: routine.name, sort: routine.owner ? 'method' : routine.isFunction ? 'function' : 'procedure', detail: `(${routine.params.join(', ')})`, location, container: routine.owner };
  }
  return { name: definition.name, sort: 'constant', detail: definition.value ?? '', location, container: null };
}

// --- find all references -----------------------------------------------------

/**
 * Every place in the workspace that names what the cursor is on.
 *
 * Complete only over a tree the parser has been over: a call is invisible to the header scan, so a workspace still at tier 1 answers with the DO and SET sites alone. The caller is responsible for promoting first when it wants the whole answer.
 */
export function referencesAt(record: FileRecord, lines: string[], position: Position, view: WorkspaceView, includeDeclaration = true): Location[] {
  const target = targetAt(record, lines, position);
  if (!target || target.dynamic || !target.name) return [];

  const asFile = fileTargets.has(target.kind);
  const out = view.index.referencesTo(target.name, asFile).map(ref => ({ file: ref.file, range: ref.range }));
  if (includeDeclaration) out.unshift(...definitionsOf(target, view));

  // The same site can arrive twice -- a DO of a file is both a reference to the routine and to the file it lives in.
  const seen = new Set<string>();
  return out.filter(location => {
    const key = `${location.file}:${location.range.start.line}:${location.range.start.character}`;
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
}

const fileTargets: ReadonlySet<string> = new Set(['include', 'procedure', 'classlib', 'form']);

/** The file the position names, for the editor commands that act on a file rather than on a position -- opening a form in VFP's designer is the one. A name assembled at run time names no one file, the same reason Go to Definition answers nothing for it. */
export function fileTargetAt(record: FileRecord, lines: string[], position: Position, view: WorkspaceView): { file: string; name: string } | null {
  const target = targetAt(record, lines, position);
  if (!target || target.dynamic || !target.name || !fileTargets.has(target.kind)) return null;
  const file = view.resolveFile(target.name, target.kind as RefKind);
  return file ? { file, name: target.name } : null;
}

// --- completion --------------------------------------------------------------

export type CompletionSort = SymbolSort | 'field';

export interface Completion {
  label: string;
  sort: CompletionSort;
  detail: string;
  /** The comment block above a routine, which is the only documentation VFP code carries. */
  doc: string | null;
}

/**
 * What could be written at the cursor.
 *
 * After `alias.`, the field names the file itself shows being used against that alias -- there is no table to ask at edit time, so the file's own evidence is the whole of it. Everywhere else, the routines, classes and constants the workspace holds. Variables are deliberately absent: the editor already offers the words in the document, and repeating them buries the names it could not have known.
 */
export function completionsAt(record: FileRecord, lines: string[], position: Position, view: WorkspaceView | undefined, context?: { ast: AstNode | null; aliases: Set<string> }): Completion[] {
  const before = (lines[position.line] ?? '').slice(0, position.character);
  const qualified = /([A-Za-z_]\w*)\s*(?:\.|->)\s*\w*$/.exec(before);

  if (qualified) {
    const alias = qualified[1];
    if (!context?.ast || !context.aliases.has(upper(alias))) return [];
    return [...fieldsQualifiedBy(context.ast, alias)].sort().map(name => ({ label: name, sort: 'field' as const, detail: `field of ${alias}`, doc: null }));
  }

  const out: Completion[] = [];
  // This file first, so a routine written a moment ago is offered whether or not the index has caught up.
  for (const routine of record.routines) out.push(fromRoutine(routine));
  for (const constant of record.constants) out.push(fromConstant(constant));
  const here = new Set(out.map(c => upper(c.label)));

  if (view) {
    for (const definition of view.index.definitions()) {
      if (here.has(upper(definition.name))) continue;
      // A method is reached through its object, not written bare.
      if ('params' in definition && definition.owner) continue;
      here.add(upper(definition.name));
      out.push('methods' in definition ? fromClass(definition) : 'params' in definition ? fromRoutine(definition) : fromConstant(definition));
    }
  }
  return out;
}

const fromRoutine = (routine: RoutineRecord): Completion => ({
  label: routine.name,
  sort: routine.isFunction ? 'function' : 'procedure',
  detail: `(${routine.params.join(', ')})`,
  doc: routine.doc
});

const fromConstant = (constant: ConstantRecord): Completion => ({
  label: constant.name, sort: 'constant', detail: constant.value ?? '', doc: null
});

const fromClass = (klass: ClassRecord): Completion => ({
  label: klass.name, sort: 'class', detail: klass.base ? `AS ${klass.base}` : '', doc: null
});

// --- signature help ----------------------------------------------------------

export interface Signature {
  label: string;
  parameters: string[];
  /** Which parameter the cursor sits in, counted from zero. */
  activeParameter: number;
  doc: string | null;
}

/**
 * The routine whose arguments are being typed, and which one the cursor is in.
 *
 * Read from the text rather than the tree, because a line half-way through being typed is exactly the line that does not parse. String contents are blanked first, so a comma inside a literal does not advance the parameter.
 */
export function signatureAt(record: FileRecord, lines: string[], position: Position, view?: WorkspaceView): Signature | null {
  const prefix = maskStrings((lines[position.line] ?? '').slice(0, position.character));
  const open = openCallAt(prefix);
  if (!open) return null;

  const local = record.routines.filter(r => !r.owner && upper(r.name) === upper(open.name));
  const found = local.length ? local : (view?.routines(open.name) ?? []).filter(r => !r.owner);
  if (!found.length) return null;
  const routine = found[0];
  return { label: signatureOf(routine), parameters: routine.params, activeParameter: open.argument, doc: routine.doc };
}

/** The call whose argument list the end of this text is inside: the name, and how many arguments have been closed off before the cursor. */
function openCallAt(prefix: string): { name: string; argument: number } | null {
  // `DO Name WITH a, b` has no bracket to find, so its WITH stands in for one. Only at the top level: inside brackets a WITH belongs to something else.
  const stack: { at: number; commas: number }[] = [];
  for (let i = 0; i < prefix.length; i++) {
    const c = prefix[i];
    if (c === '(' || c === '[') stack.push({ at: i, commas: 0 });
    else if (c === ')' || c === ']') stack.pop();
    else if (c === ',' && stack.length) stack[stack.length - 1].commas++;
  }
  if (stack.length) {
    const top = stack[stack.length - 1];
    const name = /([A-Za-z_]\w*)\s*$/.exec(prefix.slice(0, top.at));
    return name ? { name: name[1], argument: top.commas } : null;
  }

  const withAt = findWord(prefix, 'WITH');
  if (withAt < 0) return null;
  const doAt = findWord(prefix.slice(0, withAt), 'DO');
  if (doAt < 0) return null;
  const name = prefix.slice(doAt + 2, withAt).trim();
  if (!/^[A-Za-z_][\w\/.:~$#@-]*$/.test(name)) return null;
  const commas = prefix.slice(withAt + 4).split(',').length - 1;
  return { name, argument: commas };
}
