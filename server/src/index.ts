// The workspace index: what every file in the tree defines and what it refers to, so a rule or an editor request can answer a question about a file that is not open.
// Pure -- no fs, no language server. workspace.ts crawls the disk and feeds records in, server.ts wires the queries to the protocol, and the tests drive this file directly.
// Two tiers produce the same record. scanHeader() reads the line-anchored constructs with a regex, fast enough to run over the whole tree at startup; extract() reads a parsed tree and adds what only a parse can see (calls, CREATEOBJECT). run-workspace-tests.ts holds one to the other over every fixture, which is what keeps the regex honest.

import type { AstNode, DefineClass, DefineStatement, DoFormStatement, DoStatement, Expr, IncludeStatement, Loc, ProcedureStatement, Program, SetCommand } from './ast.js';
import { staticName, type NameNode } from './dynamic.js';
import { routineParameters } from './routine.js';
import { toRange, walk, type Range } from './rule.js';

/** What a reference points at, which decides how its name is resolved to a file. */
export type RefKind = 'do' | 'call' | 'form' | 'include' | 'procedure' | 'classlib' | 'object';

/** The kinds scanHeader() finds. The rest need a parse, so a tier-1 record carries none of them. */
export const headerRefKinds: ReadonlySet<RefKind> = new Set<RefKind>(['do', 'form', 'include', 'procedure', 'classlib']);

export interface RoutineRecord {
  name: string;
  /** Upper-cased: FoxPro names are case-insensitive. */
  key: string;
  isFunction: boolean;
  params: string[];
  range: Range;
  file: string;
  /** The class whose method this is, or null for a routine at file level. */
  owner: string | null;
  /** The comment block written above the routine, which is VFP's de-facto docstring. */
  doc: string | null;
}

export interface ClassRecord {
  name: string;
  key: string;
  base: string | null;
  methods: RoutineRecord[];
  properties: string[];
  range: Range;
  file: string;
}

export interface ConstantRecord {
  name: string;
  key: string;
  value: string | null;
  range: Range;
  file: string;
}

export interface Reference {
  kind: RefKind;
  name: string;
  key: string;
  /** The narrowest range covering the name itself, or the statement when the grammar returns the name as a bare string. */
  range: Range;
  file: string;
  /** Assembled at run time, so nothing may be concluded from failing to resolve it. */
  dynamic: boolean;
  /** Arguments passed, for the arity check. Null where the reference is not a call. */
  argc: number | null;
}

export interface FileRecord {
  file: string;
  mtime: number;
  size: number;
  tier: 1 | 2;
  routines: RoutineRecord[];
  classes: ClassRecord[];
  constants: ConstantRecord[];
  /** A file-level LPARAMETERS, which is how a .prg called with `DO file WITH ...` takes its arguments. */
  mainParams: string[] | null;
  refs: Reference[];
}

/** What a file needs from the index, bound to the file asking. The rules and the editor requests take this rather than the index itself, so neither has to keep passing its own path around. */
export interface WorkspaceView {
  index: WorkspaceIndex;
  file: string;
  routines(name: string): RoutineRecord[];
  resolveFile(name: string, kind: RefKind): string | null;
  /** Whether a name says where it lives rather than being looked for. Nothing can be concluded from failing to resolve one, because it may name a drive this machine cannot see. */
  isAbsolute(name: string): boolean;
}

// --- paths -----------------------------------------------------------------
// One spelling for a path wherever it is used as a key: forward slashes and lower case, because VFP runs on Windows and the same file is written both ways in the same tree.

export const upper = (name: string) => name.toUpperCase();

export function normalizePath(file: string): string {
  return file.replace(/\\/g, '/').toLowerCase();
}

/** Whether this path is a header file. A .h is a constant file rather than a program: #INCLUDE pulls its #DEFINEs in and nothing ever compiles its lines, so it is scanned for what it defines and never parsed as FoxPro. */
export function isHeaderFile(file: string | undefined | null): boolean {
  return !!file && file.toLowerCase().endsWith('.h');
}

export function dirOf(file: string): string {
  const at = file.replace(/\\/g, '/').lastIndexOf('/');
  return at < 0 ? '' : file.replace(/\\/g, '/').slice(0, at);
}

export function baseOf(file: string): string {
  return file.replace(/\\/g, '/').split('/').pop() ?? file;
}

const join = (dir: string, name: string) => (dir ? `${dir.replace(/[\\/]$/, '')}/${name}` : name).replace(/\\/g, '/');

export const isAbsolute = (name: string) => /^([A-Za-z]:[\\/]|[\\/]{2}|[\\/])/.test(name);

const hasExtension = (name: string) => /\.[A-Za-z0-9]+$/.test(baseOf(name));

// What VFP looks for when a name arrives without one. A DO target may be a compiled or built form, which is why those are here even though the index never reads them.
const extensionsByKind: Record<RefKind, string[]> = {
  do: ['.prg', '.fxp', '.app', '.exe'],
  call: ['.prg', '.fxp'],
  form: ['.scx', '.sct'],
  include: ['.h', '.prg'],
  procedure: ['.prg', '.fxp'],
  classlib: ['.vcx', '.prg'],
  object: ['.vcx', '.prg']
};

// --- the index -------------------------------------------------------------

export class WorkspaceIndex {
  /** The workspace folders, longest first so the innermost wins when they nest. */
  roots: string[] = [];
  /** VFP's SET PATH: extra directories a bare file name is looked for in. */
  searchPath: string[] = [];
  /** Keyed by normalised path. */
  readonly files = new Map<string, FileRecord>();
  /** Every file under the roots, whatever its extension, so `SET CLASSLIB TO x` can find an x.vcx the index cannot read. Keyed by normalised path, valued by the path as written on disk. */
  readonly known = new Map<string, string>();
  /** The same files keyed by base name, lower-cased, so a name can be looked for anywhere in the tree without walking it. */
  private readonly knownByBase = new Map<string, string[]>();
  private readonly routinesByKey = new Map<string, RoutineRecord[]>();
  private readonly classesByKey = new Map<string, ClassRecord[]>();
  private readonly constantsByKey = new Map<string, ConstantRecord[]>();
  /** Definition key -> the files that name it. The reverse map, so an edit to one file can find the files whose findings it changes. */
  private readonly referencing = new Map<string, Set<string>>();

  constructor(options: { roots?: string[]; searchPath?: string[] } = {}) {
    this.roots = [...(options.roots ?? [])].sort((a, b) => b.length - a.length);
    this.searchPath = [...(options.searchPath ?? [])];
  }

  addKnown(file: string): void {
    const key = normalizePath(file);
    // The base-name map is what a name nothing else matched falls back to, so a file has to enter it once however often the crawl, an upsert and the watcher each report it.
    if (!this.known.has(key)) push(this.knownByBase, baseOf(key), file);
    this.known.set(key, file);
  }

  removeKnown(file: string): void {
    const key = normalizePath(file);
    if (!this.known.delete(key)) return;
    const base = baseOf(key);
    const kept = (this.knownByBase.get(base) ?? []).filter(other => normalizePath(other) !== key);
    if (kept.length) this.knownByBase.set(base, kept); else this.knownByBase.delete(base);
  }

  /**
   * Replaces what the index holds for one file.
   *
   * Returns the definition keys whose meaning changed -- appeared, vanished, moved file, or changed arity -- which is exactly the set whose dependents may now report something different.
   */
  upsert(record: FileRecord): { changed: Set<string> } {
    const before = this.signatures(this.get(record.file));
    this.detach(record.file);
    this.files.set(normalizePath(record.file), record);
    this.addKnown(record.file);
    this.attach(record);
    return { changed: difference(before, this.signatures(record)) };
  }

  remove(file: string): { changed: Set<string> } {
    const before = this.signatures(this.get(file));
    this.detach(file);
    this.files.delete(normalizePath(file));
    this.removeKnown(file);
    return { changed: difference(before, new Map()) };
  }

  get(file: string): FileRecord | undefined {
    return this.files.get(normalizePath(file));
  }

  /** Every file that names any of these definitions, so the caller can re-lint the ones it cares about. */
  dependentsOf(keys: Iterable<string>): Set<string> {
    const out = new Set<string>();
    for (const key of keys) for (const file of this.referencing.get(key) ?? []) out.add(file);
    return out;
  }

  /**
   * Every reference to a name, anywhere in the tree.
   *
   * Only the files the reverse map already names are read, so this costs the size of the answer rather than the size of the workspace. Calls are found only in a file the parser has been over, which is what the tier-2 promotion is for: across a tree still at tier 1 this returns the DO and SET sites and nothing else.
   */
  referencesTo(name: string, asFile = false): Reference[] {
    const key = upper(name);
    const candidates = new Set<string>(this.referencing.get(key) ?? []);
    // A file is filed with and without its extension, and the two have to meet: `SET PROCEDURE TO lib` and `DO lib.prg` name the same file.
    if (asFile) for (const alias of fileKeys(name)) for (const file of this.referencing.get(alias) ?? []) candidates.add(file);
    const out: Reference[] = [];
    for (const file of candidates) {
      for (const ref of this.get(file)?.refs ?? []) {
        if (ref.dynamic) continue;
        if (ref.key === key || (asFile && fileRefKinds.has(ref.kind) && sameFileName(ref.name, name))) out.push(ref);
      }
    }
    return out.sort((a, b) => a.file.localeCompare(b.file) || a.range.start.line - b.range.start.line);
  }

  /**
   * Every definition of a routine name, in the order VFP would find them: the calling file first, then the files its SET PROCEDURE names, then the rest of the tree.
   *
   * The caller stops at the first group that answers; a name defined both here and elsewhere is not ambiguous, it is shadowed.
   */
  resolveRoutine(name: string, fromFile: string): RoutineRecord[] {
    const all = this.routinesByKey.get(upper(name)) ?? [];
    if (all.length < 2) return all;
    const here = normalizePath(fromFile);
    const libs = this.procedureFilesOf(fromFile);
    const rank = (r: RoutineRecord) => (normalizePath(r.file) === here ? 0 : libs.has(normalizePath(r.file)) ? 1 : 2);
    return [...all].sort((a, b) => rank(a) - rank(b));
  }

  classesNamed(name: string): ClassRecord[] {
    return this.classesByKey.get(upper(name)) ?? [];
  }

  constantsNamed(name: string): ConstantRecord[] {
    return this.constantsByKey.get(upper(name)) ?? [];
  }

  /** Every top-level routine, class and constant, for the workspace symbol list. */
  *definitions(): Generator<RoutineRecord | ClassRecord | ConstantRecord> {
    for (const record of this.files.values()) {
      yield* record.routines;
      for (const klass of record.classes) {
        yield klass;
        yield* klass.methods;
      }
      yield* record.constants;
    }
  }

  /**
   * The file a name refers to, or null when nothing in the tree matches.
   *
   * The name is tried as written and then with each extension the kind defaults to, in the directory of the file that named it, then each workspace root, then each search-path entry -- VFP's own order, with `SET PATH` last -- and finally anywhere under the roots at all, which is what a tree filed by what a file is rather than by who calls it needs.
   */
  resolveFile(name: string, kind: RefKind, fromFile: string): string | null {
    const cleaned = name.trim().replace(/^["']|["']$/g, '');
    if (!cleaned) return null;
    const names = hasExtension(cleaned) ? [cleaned] : [cleaned, ...extensionsByKind[kind].map(ext => cleaned + ext)];
    // An absolute name says where it is; the search directories have no bearing on it.
    if (isAbsolute(cleaned)) return names.map(n => this.known.get(normalizePath(n))).find(Boolean) ?? null;
    for (const dir of [dirOf(fromFile), ...this.roots, ...this.searchPath]) {
      for (const candidate of names) {
        const found = this.known.get(normalizePath(join(dir, candidate)));
        if (found) return found;
      }
    }
    // Nothing the name could be relative to holds it, so the last thing tried is the name on its own, anywhere under the roots. FoxPro resolves a file by name over SET PATH and never by folder, and a tree laid out by what a file *is* rather than by who calls it puts the caller and the callee in cousin folders: without this, every #INCLUDE and SET PROCEDURE across such a tree reads as missing. Asking the user to list the folders instead is no answer -- a first open that reports 493 files absent gets the rule turned off, not the setting filled in.
    for (const candidate of names) {
      const found = this.nearestNamed(candidate, fromFile);
      if (found) return found;
    }
    return null;
  }

  /**
   * The known file whose path ends with this relative name, nearest the file that asked.
   *
   * The match is on whole segments, so `app\mainset` does not answer with `framework/mainset.prg`. Nearness is only a tie-break: a name spelled in two folders is ambiguous to VFP too, where which one wins depends on the order `SET PATH` was written in, so the one closest to the asking file is as good an answer as there is and at least a stable one.
   */
  private nearestNamed(name: string, fromFile: string): string | null {
    const tail = normalizePath(name).replace(/^\/+/, '');
    const matches = (this.knownByBase.get(baseOf(tail)) ?? []).filter(file => {
      const full = normalizePath(file);
      return full === tail || full.endsWith(`/${tail}`);
    });
    if (matches.length < 2) return matches[0] ?? null;
    const from = normalizePath(dirOf(fromFile));
    return [...matches].sort((a, b) =>
      sharedDepth(normalizePath(b), from) - sharedDepth(normalizePath(a), from)
      || a.length - b.length
      || (normalizePath(a) < normalizePath(b) ? -1 : 1))[0];
  }

  /** A view of the index bound to one file, which is what the rules and the editor requests are handed. */
  viewFor(file: string): WorkspaceView {
    return {
      index: this,
      file,
      routines: name => this.resolveRoutine(name, file),
      resolveFile: (name, kind) => this.resolveFile(name, kind, file),
      isAbsolute
    };
  }

  /** The libraries a file loads with SET PROCEDURE, resolved to paths. One level: a library that loads another is vanishingly rare and the cycle guard would cost more than it buys. */
  private procedureFilesOf(fromFile: string): Set<string> {
    const record = this.get(fromFile);
    const out = new Set<string>();
    for (const ref of record?.refs ?? []) {
      if (ref.kind !== 'procedure' || ref.dynamic) continue;
      const resolved = this.resolveFile(ref.name, 'procedure', fromFile);
      if (resolved) out.add(normalizePath(resolved));
    }
    return out;
  }

  private attach(record: FileRecord): void {
    for (const routine of record.routines) push(this.routinesByKey, routine.key, routine);
    for (const klass of record.classes) {
      push(this.classesByKey, klass.key, klass);
      for (const method of klass.methods) push(this.routinesByKey, `${klass.key}.${method.key}`, method);
    }
    for (const constant of record.constants) push(this.constantsByKey, constant.key, constant);
    for (const key of referencedKeys(record)) {
      let set = this.referencing.get(key);
      if (!set) this.referencing.set(key, (set = new Set()));
      set.add(record.file);
    }
  }

  /** Undoes attach() for one file, entry by entry rather than by sweeping every key: this runs on each keystroke's re-index, and a scan of the whole workspace to remove one file's handful of names would put the cost of every edit on the size of the tree. */
  private detach(file: string): void {
    const existing = this.get(file);
    if (!existing) return;
    const here = normalizePath(file);
    const drop = (map: Map<string, { file: string }[]>, key: string) => {
      const kept = (map.get(key) ?? []).filter(r => normalizePath(r.file) !== here);
      if (kept.length) map.set(key, kept); else map.delete(key);
    };
    for (const routine of existing.routines) drop(this.routinesByKey as Map<string, { file: string }[]>, routine.key);
    for (const klass of existing.classes) {
      drop(this.classesByKey as Map<string, { file: string }[]>, klass.key);
      for (const method of klass.methods) drop(this.routinesByKey as Map<string, { file: string }[]>, `${klass.key}.${method.key}`);
    }
    for (const constant of existing.constants) drop(this.constantsByKey as Map<string, { file: string }[]>, constant.key);
    for (const key of referencedKeys(existing)) {
      const set = this.referencing.get(key);
      if (!set) continue;
      set.delete(existing.file);
      if (!set.size) this.referencing.delete(key);
    }
  }

  /** Every definition a file makes, as key -> a string that changes when what a caller sees changes. */
  private signatures(record: FileRecord | undefined): Map<string, string> {
    const out = new Map<string, string>();
    if (!record) return out;
    for (const routine of record.routines) out.set(routine.key, `${routine.file}:${routine.params.length}:${routine.isFunction}`);
    for (const klass of record.classes) {
      out.set(klass.key, `${klass.file}:${klass.base ?? ''}`);
      for (const method of klass.methods) out.set(`${klass.key}.${method.key}`, `${method.file}:${method.params.length}`);
    }
    for (const constant of record.constants) out.set(constant.key, `${constant.file}:${constant.value ?? ''}`);
    // Existing is itself a definition: an #INCLUDE of this file stops being missing the moment it appears, and starts again when it is deleted.
    for (const key of fileKeys(record.file)) out.set(key, record.file);
    return out;
  }
}

/** The kinds whose reference is to a file rather than to a name inside one. */
const fileRefKinds: ReadonlySet<RefKind> = new Set<RefKind>(['include', 'procedure', 'classlib', 'form']);

/**
 * The keys a file's references are filed under, so an edit anywhere can find the files it affects.
 *
 * A reference to a file is filed with and without its extension, because `SET PROCEDURE TO toolkit` and the toolkit.prg it resolves to have to meet somewhere, and resolving it here would mean walking the search path on every re-index.
 */
function referencedKeys(record: FileRecord): Set<string> {
  const out = new Set<string>();
  for (const ref of record.refs) {
    if (ref.dynamic || !ref.key) continue;
    out.add(ref.key);
    if (fileRefKinds.has(ref.kind)) for (const key of fileKeys(ref.name)) out.add(key);
  }
  return out;
}

/** Whether two names denote the same file, with or without the extension either was written with. */
function sameFileName(a: string, b: string): boolean {
  const forms = (name: string) => new Set(fileKeys(name));
  const mine = forms(a);
  for (const key of forms(b)) if (mine.has(key)) return true;
  return false;
}

function fileKeys(nameOrPath: string): string[] {
  const base = normalizePath(baseOf(nameOrPath));
  const bare = base.replace(/\.[^.]+$/, '');
  return bare === base ? [`file:${base}`] : [`file:${base}`, `file:${bare}`];
}

/** How many leading segments two normalised paths share, which is how near one file is to another. */
function sharedDepth(a: string, b: string): number {
  const left = a.split('/');
  const right = b.split('/');
  let i = 0;
  while (i < left.length && i < right.length && left[i] === right[i]) i++;
  return i;
}

function push<T>(map: Map<string, T[]>, key: string, value: T): void {
  const list = map.get(key);
  if (list) list.push(value); else map.set(key, [value]);
}

/** The keys present in one map and absent or different in the other, both ways round. */
function difference(before: Map<string, string>, after: Map<string, string>): Set<string> {
  const out = new Set<string>();
  for (const [key, value] of before) if (after.get(key) !== value) out.add(key);
  for (const [key, value] of after) if (before.get(key) !== value) out.add(key);
  return out;
}

// --- tier 2: from a parsed tree --------------------------------------------

export function extract(file: string, ast: Program | null, lines: string[], stat: { mtime: number; size: number } = { mtime: 0, size: 0 }): FileRecord {
  const record: FileRecord = { file, mtime: stat.mtime, size: stat.size, tier: 2, routines: [], classes: [], constants: [], mainParams: null, refs: [] };
  if (!ast) return record;

  for (const statement of ast.body) {
    if (!statement) continue;
    if (statement.type === 'ProcedureStatement') record.routines.push(routineOf(statement, file, lines, null));
    else if (statement.type === 'DefineClass') record.classes.push(classOf(statement, file, lines));
    else if (statement.type === 'DefineStatement') record.constants.push(constantOf(statement, file));
    else if (statement.type === 'ParametersDeclaration' && !record.mainParams) record.mainParams = statement.names;
  }

  walk(ast, node => {
    const ref = referenceOf(node, file);
    if (ref) record.refs.push(...ref);
  });
  return record;
}

function routineOf(node: ProcedureStatement, file: string, lines: string[], owner: string | null): RoutineRecord {
  const range = toRange(node.location);
  return { name: node.name, key: upper(node.name), isFunction: node.isFunction, params: routineParameters(node), range, file, owner, doc: docAbove(lines, range.start.line) };
}

function classOf(node: DefineClass, file: string, lines: string[]): ClassRecord {
  const methods: RoutineRecord[] = [];
  const properties: string[] = [];
  for (const statement of node.body) {
    if (!statement) continue;
    if (statement.type === 'ProcedureStatement') methods.push(routineOf(statement, file, lines, node.name));
    else if (statement.type === 'Assignment' && statement.target.type === 'Identifier') properties.push(statement.target.name);
    else if (statement.type === 'ClassAccessStatement') properties.push(...statement.names);
    else if (statement.type === 'AddObjectStatement') properties.push(statement.name);
  }
  return { name: node.name, key: upper(node.name), base: node.base, methods, properties, range: toRange(node.location), file };
}

function constantOf(node: DefineStatement, file: string): ConstantRecord {
  return { name: node.name, key: upper(node.name), value: node.value, range: toRange(node.location), file };
}

/** The comment block directly above a line, which is where VFP code puts what a routine is for. */
export function docAbove(lines: string[], line: number, limit = 20): string | null {
  const collected: string[] = [];
  for (let i = line - 1; i >= 0 && collected.length < limit; i--) {
    const text = lines[i] ?? '';
    if (!text.trim() && collected.length) break;
    if (!text.trim()) continue;
    const m = /^\s*(?:\*|&&|NOTE\b)[-*\s]?(.*)$/i.exec(text);
    if (!m) break;
    collected.unshift(m[1].trimEnd());
  }
  while (collected.length && !collected[0].trim()) collected.shift();
  while (collected.length && !collected[collected.length - 1].trim()) collected.pop();
  return collected.length ? collected.join('\n') : null;
}

/** Every outbound reference one node makes, or null when it makes none. */
function referenceOf(node: AstNode, file: string): Reference[] | null {
  switch (node.type) {
    case 'DoStatement': return doReference(node as DoStatement, file);
    case 'DoFormStatement': return formReference(node as DoFormStatement, file);
    case 'IncludeStatement': return [makeRef('include', (node as IncludeStatement).path, node.location, file)];
    case 'SetCommand': return setReferences(node as SetCommand, file);
    case 'CallExpression': return callReferences(node as AstNode & { callee: Expr; arguments: (Expr | null)[] }, file);
    default: return null;
  }
}

function doReference(node: DoStatement, file: string): Reference[] {
  const out = [makeRef('do', node.target as NameNode, node.location, file, node.arguments.length)];
  // `DO routine IN library.prg` names the file the routine lives in, which is a file reference of its own.
  if (node.inSession && typeof node.inSession !== 'number') out.push(makeRef('procedure', node.inSession as NameNode, node.location, file));
  return out;
}

function formReference(node: DoFormStatement, file: string): Reference[] {
  if (node.target === '?') return [];
  return [makeRef('form', node.target as NameNode, node.location, file, node.arguments.length)];
}

function setReferences(node: SetCommand, file: string): Reference[] | null {
  const command = upper(typeof node.command === 'string' ? node.command : String(node.command));
  const kind: RefKind | null = command === 'PROCEDURE' ? 'procedure' : command === 'CLASSLIB' ? 'classlib' : null;
  if (!kind) return null;
  return node.arguments.map(argument => makeRef(kind, argument as NameNode, node.location, file));
}

// CREATEOBJECT("Thing") and NEWOBJECT("Thing", "lib.vcx") name a class by string, which is the only way a .prg reaches one.
const objectFactories = new Set(['CREATEOBJECT', 'NEWOBJECT']);

function callReferences(node: AstNode & { callee: Expr; arguments: (Expr | null)[] }, file: string): Reference[] | null {
  const callee = node.callee;
  if (callee?.type !== 'Identifier') return null;
  const argc = node.arguments.length;
  if (objectFactories.has(upper(callee.name))) {
    const first = node.arguments[0];
    if (first?.type !== 'StringLiteral') return null;
    const out = [makeRef('object', first, first.location ?? node.location, file)];
    const library = node.arguments[1];
    if (upper(callee.name) === 'NEWOBJECT' && library?.type === 'StringLiteral') out.push(makeRef('classlib', library, library.location ?? node.location, file));
    return out;
  }
  return [makeRef('call', callee, callee.location ?? node.location, file, argc)];
}

// Only DO and DO FORM accept the parenthesised runtime form; everywhere else a bare identifier is the name written out.
const positionOf = (kind: RefKind) => (kind === 'do' || kind === 'form' ? 'target' : 'name');

function makeRef(kind: RefKind, target: NameNode, fallback: Loc | undefined, file: string, argc: number | null = null): Reference {
  const name = staticName(target, positionOf(kind));
  const own = target && typeof target === 'object' ? (target as { location?: Loc }).location : undefined;
  return { kind, name: name ?? '', key: name ? upper(name) : '', range: toRange(own ?? fallback), file, dynamic: name === null, argc };
}

// --- tier 1: from the text ---------------------------------------------------
// Microseconds per file against a quarter of a second to parse one, which is what makes indexing the whole tree at startup possible. It reads only line-anchored constructs, so calls and CREATEOBJECT are left to tier 2 -- everything the phase-one editor features and the arity check need is here.

const reRoutine = /^[ \t]*(?:(?:PROTECTED|HIDDEN)[ \t]+)?(PROCEDURE|FUNCTION)[ \t]+([A-Za-z_]\w*)[ \t]*(\(([^)]*)\))?/i;
const reParameters = /^[ \t]*(?:LPARAMETERS|LPARAMETER|PARAMETERS|PARAMETER|PARAM)\b[ \t]*(.*)$/i;
const reDefineClass = /^[ \t]*DEFINE[ \t]+CLASS[ \t]+([A-Za-z_]\w*)(?:[ \t]+AS[ \t]+([\w.]+))?/i;
const reEndDefine = /^[ \t]*ENDDEFINE\b/i;
const reEndRoutine = /^[ \t]*(?:ENDPROC|ENDFUNC)\b/i;
const reDefine = /^[ \t]*#DEFINE[ \t]+([A-Za-z_]\w*)[ \t]*(.*)$/i;
const reInclude = /^[ \t]*#INCLUDE[ \t]+(.*)$/i;

const reSetTo = /^[ \t]*SET[ \t]+(PROCEDURE|CLASSLIB)[ \t]+TO[ \t]*(.*)$/i;
const reComment = /^[ \t]*(?:\*|&&|NOTE\b)/i;
const reText = /^[ \t]*TEXT\b/i;
const reEndText = /^[ \t]*ENDTEXT\b/i;
const reClassMembers = /^[ \t]*(?:PROTECTED|HIDDEN)[ \t]+(?!PROCEDURE\b|FUNCTION\b)(.*)$/i;
const reAddObject = /^[ \t]*ADD[ \t]+OBJECT[ \t]+(?:PROTECTED[ \t]+)?([A-Za-z_]\w*)/i;
const reProperty = /^[ \t]*([A-Za-z_]\w*)[ \t]*=/;

export function scanHeader(file: string, text: string, stat: { mtime: number; size: number } = { mtime: 0, size: 0 }): FileRecord {
  const record: FileRecord = { file, mtime: stat.mtime, size: stat.size, tier: 1, routines: [], classes: [], constants: [], mainParams: null, refs: [] };
  const raw = text.split(/\r\n|\r|\n/);
  const lines = joinContinuations(raw);

  let klass: ClassRecord | null = null;
  let routine: RoutineRecord | null = null;
  let inText = false;
  // A routine takes the LPARAMETERS line that follows it, across blank lines and comments, exactly as the grammar does. This says we are still in that window.
  let awaitingParameters = false;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (line === null) continue;
    if (inText) {
      if (reEndText.test(line)) inText = false;
      continue;
    }
    if (reText.test(line) && !reEndText.test(line)) { inText = true; continue; }
    if (!line.trim() || reComment.test(line)) continue;

    const routineMatch = reRoutine.exec(line);
    if (routineMatch) {
      const name = routineMatch[2];
      routine = {
        name, key: upper(name),
        isFunction: upper(routineMatch[1]) === 'FUNCTION',
        params: routineMatch[4] !== undefined ? splitNames(routineMatch[4]).map(stripType) : [],
        range: rangeOf(i, 0, line.length), file,
        owner: klass ? klass.name : null,
        doc: docAbove(raw, i)
      };
      (klass ? klass.methods : record.routines).push(routine);
      awaitingParameters = routineMatch[3] === undefined;
      continue;
    }

    if (awaitingParameters) {
      const parameters = reParameters.exec(line);
      awaitingParameters = false;
      if (parameters && routine) { routine.params = splitNames(parameters[1]).map(stripType); continue; }
    } else if (!routine && !klass && !record.mainParams) {
      const parameters = reParameters.exec(line);
      // At file level the same line is how a .prg called with DO ... WITH takes its arguments.
      if (parameters) { record.mainParams = splitNames(parameters[1]).map(stripType); continue; }
    }

    const classMatch = reDefineClass.exec(line);
    if (classMatch) {
      klass = { name: classMatch[1], key: upper(classMatch[1]), base: classMatch[2] ?? null, methods: [], properties: [], range: rangeOf(i, 0, line.length), file };
      record.classes.push(klass);
      routine = null;
      continue;
    }
    if (reEndDefine.test(line)) { klass = null; routine = null; continue; }
    if (reEndRoutine.test(line)) { routine = null; continue; }

    if (klass && !routine) {
      const members = reClassMembers.exec(line);
      if (members) { klass.properties.push(...splitNames(members[1]).map(stripType)); continue; }
      const added = reAddObject.exec(line);
      if (added) { klass.properties.push(added[1]); continue; }
      const property = reProperty.exec(line);
      if (property) { klass.properties.push(property[1]); continue; }
    }

    const define = reDefine.exec(line);
    if (define) {
      record.constants.push({ name: define[1], key: upper(define[1]), value: define[2].trim() || null, range: rangeOf(i, 0, line.length), file });
      continue;
    }

    for (const ref of headerReferences(line, i, file)) record.refs.push(ref);
  }
  return record;
}

function headerReferences(line: string, index: number, file: string): Reference[] {
  // Searching a copy whose string contents are blanked keeps `lcMsg = "DO NOT EDIT"` from reading as a call, while every offset still lines up with the real text.
  const mask = maskStrings(line);

  const include = reInclude.exec(line);
  if (include) return [textRef('include', line, mask, line.length - include[1].length, line.length, index, file)];

  const setTo = reSetTo.exec(line);
  if (setTo) {
    const kind = upper(setTo[1]) === 'PROCEDURE' ? 'procedure' : 'classlib';
    const from = line.length - setTo[2].length;
    return splitRanges(mask, from, from + optionTail(mask.slice(from))).map(([start, end]) => textRef(kind, line, mask, start, end, index, file));
  }

  // A DO need not open its line: every ON handler -- ON ERROR, ON KEY LABEL, ON SELECTION BAR -- ends in the command it runs, and those are real calls.
  const doAt = findWord(mask, 'DO');
  if (doAt < 0) return [];
  const afterDo = doAt + 2;
  if (/^[ \t]*(CASE|WHILE)\b/i.test(mask.slice(afterDo))) return [];

  const formAt = findWord(mask, 'FORM', afterDo);
  if (formAt >= 0 && !mask.slice(afterDo, formAt).trim()) {
    const from = formAt + 4;
    return [textRef('form', line, mask, from, endOf(mask, from, formClauses), index, file, countArguments(mask))];
  }

  const end = endOf(mask, afterDo, doClauses);
  const out = [textRef('do', line, mask, afterDo, end, index, file, countArguments(mask))];
  // `DO routine IN library.prg` names the file the routine lives in.
  const inAt = findWord(mask, 'IN', afterDo);
  if (inAt >= 0) {
    const token = /^[ \t]*(\S+)/.exec(mask.slice(inAt + 2));
    if (token) {
      const start = inAt + 2 + token[0].length - token[1].length;
      out.push(textRef('procedure', line, mask, start, start + token[1].length, index, file));
    }
  }
  return out;
}

// What ends a target and begins the clauses after it. A DO FORM carries more of them, and every one of them is a word a form could otherwise be mistaken for.
const doClauses = ['WITH', 'IN'];
const formClauses = ['WITH', 'NAME', 'TO', 'LINKED', 'NOREAD', 'NOSHOW'];

/** Where a target ends: at the first of its clause words written outside brackets, or at the end of the line. */
function endOf(mask: string, from: number, clauses: string[]): number {
  const found = clauses.map(word => findWord(mask, word, from)).filter(at => at >= 0);
  return found.length ? Math.min(...found) : mask.length;
}

/** A comma-separated list as the ranges of its entries, so each name keeps its place on the line. */
function splitRanges(mask: string, from: number, to: number): [number, number][] {
  const out: [number, number][] = [];
  let start = from;
  let depth = 0;
  for (let i = from; i < to; i++) {
    const c = mask[i];
    if (c === '(' || c === '[') depth++;
    else if (c === ')' || c === ']') depth--;
    else if (c === ',' && depth === 0) { out.push([start, i]); start = i + 1; }
  }
  out.push([start, to]);
  return out.filter(([a, b]) => mask.slice(a, b).trim());
}

/** The first index of a whole word, outside brackets, or -1. */
export function findWord(mask: string, word: string, from = 0): number {
  let depth = 0;
  for (let i = from; i <= mask.length - word.length; i++) {
    const c = mask[i];
    if (c === '(' || c === '[') { depth++; continue; }
    if (c === ')' || c === ']') { depth--; continue; }
    if (depth > 0) continue;
    if (mask.substr(i, word.length).toUpperCase() !== word) continue;
    if (i > 0 && /[\w.]/.test(mask[i - 1])) continue;
    if (/[\w.]/.test(mask[i + word.length] ?? '')) continue;
    return i;
  }
  return -1;
}

// Where a SET's setting list stops and its options begin: `SET PROCEDURE TO lib1, lib2 ADDITIVE` names two files, not a file called "lib2 ADDITIVE".
const setOptions = ['ADDITIVE', 'IN', 'INTO', 'ALIAS', 'ON', 'OFF', 'EXCLUSIVE', 'SHARED', 'NOUPDATE', 'AGAIN'];

function optionTail(mask: string): number {
  const found = setOptions.map(word => findWord(mask, word)).filter(at => at >= 0);
  return found.length ? Math.min(...found) : mask.length;
}

/**
 * One reference, from the span of the line the name occupies.
 *
 * The span is trimmed and classified against the mask rather than the text: `DO "dir\" + m.cFile + ".prg"` opens and closes with a quote but is three strings and two operators, and only the mask can tell that from a name that was simply written in quotes.
 */
function textRef(kind: RefKind, line: string, mask: string, start: number, end: number, index: number, file: string, argc: number | null = null): Reference {
  const span = mask.slice(start, end);
  const at = start + /^\s*/.exec(span)![0].length;
  const to = Math.max(at, end - /\s*$/.exec(span)![0].length);
  const raw = line.slice(at, to);
  const quoted = /^(["'])[^"']*\1$/.test(mask.slice(at, to));
  const name = quoted ? raw.slice(1, -1) : raw;
  const dynamic = !name || name.includes('&') || (!quoted && !/^[\w\\/.:~$#@-]+$/.test(name));
  return { kind, name: dynamic ? '' : name, key: dynamic ? '' : upper(name), range: rangeOf(index, at, to), file, dynamic, argc };
}

/** The arguments a WITH clause passes, counted at its top-level commas. */
function countArguments(mask: string): number {
  const at = findWord(mask, 'WITH');
  if (at < 0) return 0;
  const tail = mask.slice(at + 4);
  if (!tail.trim()) return 0;
  let depth = 0, count = 1;
  for (const c of tail) {
    if (c === '(' || c === '[') depth++;
    else if (c === ')' || c === ']') depth--;
    else if (c === ',' && depth === 0) count++;
  }
  return count;
}

/** The line with every string's contents blanked, so a search can tell code from text without moving a single offset. */
export function maskStrings(line: string): string {
  let out = '';
  let quote = '';
  for (const c of line) {
    if (quote) { out += c === quote ? c : ' '; if (c === quote) quote = ''; continue; }
    if (c === '"' || c === "'") quote = c;
    out += c;
  }
  return out;
}

/** A statement continued with a semicolon is one line to the grammar, so it is one line here. The continuation's own slot is nulled rather than removed, to keep every index a real line number. */
function joinContinuations(raw: string[]): (string | null)[] {
  const out: (string | null)[] = raw.map(line => stripTail(line));
  for (let i = 0; i < out.length; i++) {
    const line = out[i];
    if (line === null || !/;[ \t]*$/.test(line)) continue;
    let joined = line.replace(/;[ \t]*$/, ' ');
    let j = i + 1;
    while (j < out.length) {
      const next = out[j];
      out[j] = null;
      if (next === null) { j++; continue; }
      joined += next.trim();
      j++;
      if (!/;[ \t]*$/.test(next)) break;
      joined = joined.replace(/;[ \t]*$/, ' ');
    }
    out[i] = joined;
    i = j - 1;
  }
  return out;
}

/** Drops an `&&` comment, leaving anything inside a string alone. */
function stripTail(line: string): string {
  let quote = '';
  for (let i = 0; i < line.length; i++) {
    const c = line[i];
    if (quote) { if (c === quote) quote = ''; continue; }
    if (c === '"' || c === "'") quote = c;
    else if (c === '&' && line[i + 1] === '&') return line.slice(0, i);
  }
  return line;
}

function splitNames(list: string): string[] {
  return list.split(',').map(part => part.trim()).filter(Boolean);
}

/** `tnRows AS Integer` declares one name; the type is not part of it. */
function stripType(name: string): string {
  return name.replace(/\s+AS\s+.*$/i, '').trim();
}

function rangeOf(line: number, start: number, end: number): Range {
  return { start: { line, character: start }, end: { line, character: end } };
}
