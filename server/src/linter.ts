// Lightweight JS copy of runLinterRules used by tests.
// This file intentionally avoids creating a language-server connection so it can be

// imported by test scripts.
interface AstNode {
  type: string;
  name?: string;
  location?: {
    start: { line: number; column: number };
    end: { line: number; column: number }
  };
  [k: string]: unknown;
}
interface ProgramAst { body?: AstNode[] }
export function runLinterRules(ast: ProgramAst) {
  const problems: any[] = [];
  if (!ast || !ast.body) return problems;

  function traverse(node: AstNode | AstNode[] | null | undefined) {
    if (!node) return;
    if (Array.isArray(node)) {
      for (const item of node) traverse(item);
      return;
    }
    if (typeof node !== 'object' || node === null) return;
    if (node.type) problems.push(...getProblemsFromNode(node));

    for (const key in node) {
      const prop = node[key];
      if (!prop) continue;
      if (Array.isArray(prop)) {
        for (const p of prop) traverse(p);
      } else if (typeof prop === 'object' && prop && (prop as any)?.['type']) {
        traverse(prop as AstNode);
      }
    }
  }

  function getProblemsFromNode(node: AstNode) {
    const out = [];
    if (node.type === 'UnknownStatement') {
      const startLine = node.location && node.location.start ? (node.location.start.line - 1) : 0;
      const startCol = node.location && node.location.start ? (node.location.start.column - 1) : 0;
      const endLine = node.location && node.location.end ? (node.location.end.line - 1) : startLine;
      const endCol = node.location && node.location.end ? (node.location.end.column - 1) : (startCol + 1);
      out.push({
        severity: 1, // DiagnosticSeverity.Error
        range: { start: { line: startLine, character: startCol }, end: { line: endLine, character: endCol } },
        message: `Unknown or unsupported statement: '${node.raw}'`,
        source: 'VFP Linter (Syntax)'
      });
    }
    return out;
  }

  traverse(ast.body);
  return problems;
}
