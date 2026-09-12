// What a routine declares, read the same way everywhere: the outline, the workspace index and the arity rule all need the parameter list, and the LPARAMETERS line can sit in two places.

import type { ProcedureStatement } from './ast.js';

/** The routine's parameter names, from the header, or from an LPARAMETERS/PARAMETERS line that is its first statement rather than part of its header. */
export function routineParameters(node: ProcedureStatement): string[] {
  if (node.parameters.length) return node.parameters.map(p => (typeof p === 'string' ? p : p.name));
  const declared = node.body.body.find(s => s?.type === 'ParametersDeclaration');
  return declared?.type === 'ParametersDeclaration' ? declared.names : [];
}
