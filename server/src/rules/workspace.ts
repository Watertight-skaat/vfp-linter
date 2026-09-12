// The rules that read more than one file: a call that over-supplies a routine, a name two files both define, and a file that is named but not there.
// All three consult the index through ctx.workspace, and all three stay silent without one rather than reporting on half the evidence. `too-many-arguments` is the exception that still works alone, because a call and the routine it names are usually in the same file and ctx.record holds this file either way.

import type { AstNode, DoStatement, Expr } from '../ast.js';
import { isDynamic, staticName } from '../dynamic.js';
import { baseOf, normalizePath, upper, type RefKind, type RoutineRecord } from '../index.js';
import { onFile, Severity, walk, type RelatedLocation, type RuleContext } from '../rule.js';


/**
 * A call passing more arguments than the routine declares, which FoxPro refuses at run time with "Too many arguments".
 *
 * Passing fewer is legal and common -- the parameters that were not supplied arrive as .F. -- so only over-supply is reported. Where a name has more than one definition the most permissive is measured against, because the rule is worth having only if it is never wrong: a definition in the calling file wins outright, since that is the one FoxPro finds first, and otherwise the largest arity anywhere in the tree has to be allowed for.
 */
export const tooManyArguments = onFile({
  code: 'too-many-arguments',
  severity: Severity.Warning,
  check(ctx) {
    const local = new Map<string, RoutineRecord[]>();
    for (const routine of ctx.record.routines) local.set(routine.key, [...(local.get(routine.key) ?? []), routine]);
    // A subscript is written like a call, so an array of the same name would otherwise look over-supplied. Every name the file declares as a variable is left alone; the scopes are not walked separately because a name declared anywhere here is enough doubt.
    const variables = new Set<string>();
    for (const scope of ctx.table.scopes) for (const key of scope.symbols.keys()) variables.add(key);

    walk(ctx.ast, node => {
      const call = callOf(node);
      if (!call || variables.has(upper(call.name))) return;

      const here = local.get(upper(call.name)) ?? [];
      const declared = here.length ? here : ctx.workspace?.routines(call.name).filter(r => !r.owner) ?? [];
      // Nothing to measure against. What to say about a name nothing defines belongs to a rule that knows the built-ins.
      const takes = declared.length ? Math.max(...declared.map(r => r.params.length)) : fileParameters(ctx, call);
      if (takes === null || call.argc <= takes) return;

      const first = declared[0];
      ctx.report(node.location,
        `'${first?.name ?? call.name}' takes ${count(takes, 'parameter')}, but ${call.argc} are passed. FoxPro raises "Too many arguments" at run time.`,
        undefined,
        first ? [{ file: first.file, range: first.range, message: `'${first.name}' is declared here` }] : undefined);
    });
  }
});

/** `DO thing.prg WITH ...` runs a file, whose arguments come from the LPARAMETERS at the top of it. Null when the target is not a file this index has read. */
function fileParameters(ctx: RuleContext, call: Call): number | null {
  if (call.kind !== 'do' || !ctx.workspace) return null;
  const resolved = ctx.workspace.resolveFile(call.name, 'do');
  const params = resolved && ctx.workspace.index.get(resolved)?.mainParams;
  return params ? params.length : null;
}

interface Call {
  kind: 'do' | 'call';
  name: string;
  argc: number;
}

/** The name and argument count of a call, or null for anything that is not one this rule can measure. */
function callOf(node: AstNode): Call | null {
  if (node.type === 'DoStatement') {
    const name = staticName((node as DoStatement).target as Expr, 'target');
    return name ? { kind: 'do', name, argc: (node as DoStatement).arguments.length } : null;
  }
  if (node.type !== 'CallExpression') return null;
  const call = node as AstNode & { callee: Expr; arguments: (Expr | null)[] };
  // A method call goes through an object, whose class the index cannot follow; a macro callee is assembled as it runs.
  if (call.callee?.type !== 'Identifier' || isDynamic(call.callee, 'name')) return null;
  return { kind: 'call', name: call.callee.name, argc: call.arguments.length };
}

/**
 * The same routine name defined in two indexed files. FoxPro resolves it by search order, so which one runs depends on what was loaded when rather than on anything written at either site.
 *
 * Methods are keyed by their class, so a class with a method named like a routine is not a collision.
 */
export const duplicateRoutine = onFile({
  code: 'duplicate-routine',
  severity: Severity.Warning,
  check(ctx) {
    const workspace = ctx.workspace;
    if (!workspace) return;
    for (const routine of ctx.record.routines) {
      const others = workspace.routines(routine.name).filter(other => !other.owner && !sameFile(other.file, workspace.file));
      if (!others.length) continue;
      ctx.report(rangeToLoc(routine.range),
        `'${routine.name}' is also defined in ${others.map(o => baseOf(o.file)).join(', ')}. FoxPro runs whichever it finds first.`,
        undefined,
        others.map<RelatedLocation>(other => ({ file: other.file, range: other.range, message: `'${other.name}' is also defined here` })));
    }
  }
});

/**
 * A file named by the code that is not in the workspace: an #INCLUDE, a SET PROCEDURE or SET CLASSLIB, a DO of a path, a DO FORM.
 *
 * A bare `DO Foo` is a routine rather than a file and is left to the rule that knows the built-ins. An absolute path is left alone whether or not it resolves, because it may name a share this machine cannot see -- which is how the real code writes an include of a library on a server.
 */
export const missingFile = onFile({
  code: 'missing-file',
  severity: Severity.Warning,
  check(ctx) {
    const workspace = ctx.workspace;
    if (!workspace) return;
    for (const ref of ctx.record.refs) {
      if (!fileKinds.has(ref.kind) || ref.dynamic || !ref.name) continue;
      // A DO of a bare name is a routine reference; only a name carrying a directory or an extension is claiming to be a file.
      if (ref.kind === 'do' && !/[\\/]|\.[A-Za-z0-9]+$/.test(ref.name)) continue;
      if (workspace.resolveFile(ref.name, ref.kind) || workspace.isAbsolute(ref.name)) continue;
      ctx.report(rangeToLoc(ref.range), `No ${describe(ref.kind)} named '${ref.name}' in the workspace.`);
    }
  }
});

const fileKinds: ReadonlySet<RefKind> = new Set<RefKind>(['include', 'procedure', 'classlib', 'form', 'do']);

function describe(kind: RefKind): string {
  switch (kind) {
    case 'include': return 'header file';
    case 'form': return 'form';
    case 'classlib': return 'class library';
    case 'procedure': return 'procedure file';
    default: return 'file';
  }
}

const sameFile = (a: string, b: string) => normalizePath(a) === normalizePath(b);

const count = (n: number, noun: string) => `${n} ${noun}${n === 1 ? '' : 's'}`;


// The index records a Range, which is what the editor wants; ctx.report takes the parser's Loc. One conversion back rather than a second reporting path.
const rangeToLoc = (range: { start: { line: number; character: number }; end: { line: number; character: number } }) => ({
  start: { line: range.start.line + 1, column: range.start.character + 1, offset: 0 },
  end: { line: range.end.line + 1, column: range.end.character + 1, offset: 0 }
});
