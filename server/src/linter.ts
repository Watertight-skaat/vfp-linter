// This file intentionally avoids creating a language-server connection so it can be imported by test scripts.

import type { AstNode, Loc, Program } from './ast.js';

// The LSP DiagnosticSeverity values, inlined so this file needs no language-server import.
// Typed as literals rather than an enum so the result is assignable to Diagnostic[] without a cast.
const Severity = { Error: 1, Warning: 2, Information: 3, Hint: 4 } as const;
type Severity = (typeof Severity)[keyof typeof Severity];

export interface LintDiagnostic {
  severity: Severity;
  range: { start: { line: number; character: number }; end: { line: number; character: number } };
  code: string;
  message: string;
  source: string;
}

// A construct the grammar does not cover yet is not the same thing as a construct that is wrong, so unsupported syntax is advisory by default: valid FoxPro the grammar has not learned should not look like a mistake.
// 'off' suppresses it entirely, while the test harness passes 'error' so the probe corpus still fails on any construct the grammar cannot read.
export type SeverityName = 'error' | 'warning' | 'information' | 'hint' | 'off';

export interface LinterOptions {
  unsupportedSyntaxSeverity?: SeverityName;
}

const defaultOptions: Required<LinterOptions> = { unsupportedSyntaxSeverity: 'information' };

const severities: Record<SeverityName, Severity | null> = {
  error: Severity.Error,
  warning: Severity.Warning,
  information: Severity.Information,
  hint: Severity.Hint,
  off: null
};

// Statements whose grammar rule cannot match without its terminator. SCAN is absent on purpose: ENDSCAN is optional in the grammar, so an unterminated SCAN never reaches the catch-all.
const blockOpeners = [
  { opener: /^IF\b/i, terminator: 'ENDIF' },
  { opener: /^FOR\b/i, terminator: 'ENDFOR or NEXT' },
  { opener: /^DO\s+WHILE\b/i, terminator: 'ENDDO' },
  { opener: /^DO\s+CASE\b/i, terminator: 'ENDCASE' },
  { opener: /^TRY\b/i, terminator: 'ENDTRY' },
  { opener: /^WITH\b/i, terminator: 'ENDWITH' },
  { opener: /^DEFINE\s+CLASS\b/i, terminator: 'ENDDEFINE' }
];

export function runLinterRules(ast: Program | null | undefined, options: LinterOptions = {}): LintDiagnostic[] {
  const problems: LintDiagnostic[] = [];
  if (!ast || !ast.body) return problems;

  const opts = { ...defaultOptions, ...options };

  function traverse(value: unknown) {
    if (Array.isArray(value)) {
      for (const item of value) traverse(item);
      return;
    }
    if (!value || typeof value !== 'object') return;
    const node = value as AstNode;
    if (typeof node.type === 'string') problems.push(...getProblemsFromNode(node, opts));

    // A generic walk over a union has to reach the properties reflectively.
    for (const child of Object.values(node as unknown as Record<string, unknown>)) {
      if (child && typeof child === 'object') traverse(child);
    }
  }

  traverse(ast.body);
  return problems;
}

function getProblemsFromNode(node: AstNode, opts: Required<LinterOptions>): LintDiagnostic[] {
  const out: LintDiagnostic[] = [];

  if (node.type === 'SelectStatement') { // SQL: report HAVING without GROUP BY
    if (node.having && !node.groupBy) {
      out.push({
        severity: Severity.Information,
        range: toRange(node.having.location ?? node.location),
        code: 'having-without-group-by',
        message: `There is no group by clause, so this is simply a post-filter on the result set.`,
        source: 'VFP Linter'
      });
    }
  } else if (node.type === 'UnknownStatement') {
    const raw = node.raw ?? '';
    const unterminated = blockOpeners.find(b => b.opener.test(raw));
    if (unterminated) {
      // The catch-all also absorbs the opening line of a block whose terminator is missing: the block rule fails and the line falls through to UnknownStatement.
      // That is broken code rather than syntax the linter has not learned, so it stays an Error: the parser never throws for it, and downgrading it with everything else would hide it.
      out.push({
        severity: Severity.Error,
        range: toRange(node.location),
        code: 'unterminated-block',
        message: `This opens a block that could not be parsed. Check for a missing ${unterminated.terminator}: '${raw}'`,
        source: 'VFP Linter'
      });
    } else {
      const severity = severities[opts.unsupportedSyntaxSeverity];
      if (severity !== null) {
        out.push({
          severity,
          range: toRange(node.location),
          code: 'unsupported-syntax',
          message: `This statement is valid FoxPro that the linter does not parse yet, so it is not being checked: '${raw}'`,
          source: 'VFP Linter'
        });
      }
    }
  }
  // todo some day: warn about unused locals
  // todo: warn about naming variables badly (VFP Hungarian prefixes: lc/ln/ll/ld/lo/la, and flag a prefix that disagrees with what is assigned, e.g. lcCount = 0)
  // Both of the above, plus implicit PRIVATE and work-area handling, read buildSymbolTable() in scope.ts.
  return out;
}

// Parser locations are 1-based and optional; LSP ranges are 0-based and required.
function toRange(loc: Loc | undefined) {
  const startLine = loc?.start ? loc.start.line - 1 : 0;
  const startCol = loc?.start ? loc.start.column - 1 : 0;
  const endLine = loc?.end ? loc.end.line - 1 : startLine;
  const endCol = loc?.end ? loc.end.column - 1 : startCol + 1;
  return { start: { line: startLine, character: startCol }, end: { line: endLine, character: endCol } };
}
