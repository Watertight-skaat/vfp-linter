// What the editor asks about one position in one file: where a name is defined, what it is, and what the workspace holds by that name.
// Pure, and it answers from the index rather than from the tree: a reference is a reference whether it was found by the parser or by the header scan, so the same code answers for an open file and for one the crawler has only skimmed.

import { baseOf, upper, type ClassRecord, type ConstantRecord, type FileRecord, type RefKind, type RoutineRecord, type WorkspaceView } from './index.js';
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
    if (file) return { markdown: `\`${file}\``, range: target.range };
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
