// This file intentionally avoids creating a language-server connection so it can be imported by test scripts.

interface ProgramAst { 
  body?: AstNode[] 
}
interface AstNode {
  type: string;
  name?: string;
  location?: Loc;
  [k: string]: unknown;
}
interface Loc {
  start?: {
    line: number;
    column: number
  };
  end?: {
    line: number;
    column: number
  }
}
enum DiagnosticSeverity {
  Error = 1,
  Warning = 2,
  Information = 3,
  Hint = 4
}

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

  traverse(ast.body);
  return problems;
}

function getProblemsFromNode(node: AstNode) {
  const out = [];
  // SQL: report HAVING without GROUP BY
  if (node.type === 'SelectStatement') {
    const n = node as unknown as Record<string, unknown>;

    const havingClause = n['having'] as Record<string, unknown> | undefined;
    if (havingClause && !n['groupBy']) {
      const locNode = getLocation(havingClause) || getLocation(node) || { start: { line: 1, column: 1 }, end: { line: 1, column: 2 } };
      const startLine = locNode.start ? (locNode.start.line - 1) : 0;
      const endLine = locNode.end ? (locNode.end.line - 1) : startLine;
      const startCol = locNode.start ? (locNode.start.column - 1) : 0;
      const endCol = locNode.end ? (locNode.end.column - 1) : (startCol + 1);
      out.push({
        severity: DiagnosticSeverity.Information,
        range: { start: { line: startLine, character: startCol }, end: { line: endLine, character: endCol } },
        message: `There is no group by clause, so this is simply a post-filter on the result set.`,
        source: 'VFP Linter'
      });
    }
  } else if (node.type === 'UnknownStatement') {
    const startLine = node.location && node.location.start ? (node.location.start.line - 1) : 0;
    const startCol = node.location && node.location.start ? (node.location.start.column - 1) : 0;
    const endLine = node.location && node.location.end ? (node.location.end.line - 1) : startLine;
    const endCol = node.location && node.location.end ? (node.location.end.column - 1) : (startCol + 1);
    out.push({
      severity: DiagnosticSeverity.Error,
      range: { start: { line: startLine, character: startCol }, end: { line: endLine, character: endCol } },
      message: `Unknown or unsupported statement: '${node.raw}'`,
      source: 'VFP Linter'
    });
  }
  return out;
}

function getLocation(obj: unknown): Loc | undefined {
  if (!obj || typeof obj !== 'object') return undefined;
  const o = obj as Record<string, unknown>;
  const loc = o['location'];
  if (!loc || typeof loc !== 'object') return undefined;
  return loc as Loc;
}