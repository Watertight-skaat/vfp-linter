// The one test for "this name is assembled at run time", so every cross-file rule refuses the same set of targets rather than each re-deriving it.
// VFP builds names as it goes: `&lcProc` substitutes a variable's contents as source text, and `DO (lcName)` names the routine by expression. Neither can be resolved by reading the file, so a rule that reports on one is guessing.

import type { AstNode, Expr, Path, StringLiteral } from './ast.js';

/** The target of a DO, DO FORM, #INCLUDE or SET PROCEDURE, or the callee of a call. A bare string is the grammar's spelling of a plain identifier and is never dynamic. */
export type NameNode = AstNode | Expr | Path | StringLiteral | string | null | undefined;

/**
 * Which position the node sits in, because an `Identifier` means opposite things in the two.
 *
 * `target` is where VFP accepts the parenthesised runtime form -- a DO or DO FORM -- so an Identifier there is `DO (lcName)` and names nothing readable, a plain `DO foo` having arrived as a Path. `name` is everywhere else a name is written out: a call's callee, a SET PROCEDURE argument, an #INCLUDE path, where an Identifier is the name itself.
 */
export type NamePosition = 'target' | 'name';

/** Whether the name this node gives cannot be known without running the program. */
export function isDynamic(node: NameNode, as: NamePosition = 'target'): boolean {
  if (node === null || node === undefined) return true;
  if (typeof node === 'string') return node.includes('&');
  switch (node.type) {
    case 'MacroSubstitute': return true;
    // The grammar keeps the ampersand in the path text, so `DO &lcName` arrives here as a Path.
    case 'Path': return (node as Path).path.includes('&');
    case 'StringLiteral': return (node as StringLiteral).value.includes('&');
    case 'Identifier': return as === 'target';
    // A call on an object is a method rather than a routine, and an expression is assembled as it runs. Neither is ours to resolve.
    default: return true;
  }
}

/** The name a node gives, when it gives one statically. Null for anything `isDynamic` refuses. */
export function staticName(node: NameNode, as: NamePosition = 'target'): string | null {
  if (isDynamic(node, as)) return null;
  if (typeof node === 'string') return node;
  switch (node!.type) {
    case 'Path': return (node as Path).path;
    case 'StringLiteral': return (node as StringLiteral).value;
    case 'Identifier': return (node as { name: string }).name;
    default: return null;
  }
}
