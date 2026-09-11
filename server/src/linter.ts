// The entry point the server and the tests share: parse the text, run every rule at the severity the settings give it, drop what a suppression comment covers, and sort what is left by position.
// This file deliberately creates no language-server connection, so the test scripts can import it directly.

import type { AstNode, AstNodeType, Loc, Program } from './ast.js';
import { parse } from './parser.js';
import { rules } from './rules/index.js';
import { buildSymbolTable } from './scope.js';
import { Severity, severityByName, toRange, walk, type Fix, type LintDiagnostic, type Rule, type RuleContext, type SeverityName } from './rule.js';

export type { Fix, LintDiagnostic, SeverityName } from './rule.js';

export interface LinterOptions {
  /** Per-rule severity overrides, keyed by the diagnostic code. A rule the settings do not name keeps its default. */
  rules?: Partial<Record<string, SeverityName>>;
  /** The setting that predates `rules`; `rules['unsupported-syntax']` wins when both are given. */
  unsupportedSyntaxSeverity?: SeverityName;
}

export interface LintResult {
  diagnostics: LintDiagnostic[];
  /** Null when the parse failed, in which case the only diagnostic is the syntax error. */
  ast: Program | null;
}

const source = 'VFP Linter';

/** Every rule's code and default severity, for the settings schema and the docs. */
export const ruleDefaults: ReadonlyArray<{ code: string; severity: SeverityName; locked: boolean }> =
  rules.map(r => ({ code: r.code, severity: severityName(r.severity), locked: !!r.locked }));

export function lint(text: string, options: LinterOptions = {}): LintResult {
  const lines = text.split(/\r\n|\r|\n/);
  let ast: Program;
  try {
    ast = parse(text) as Program;
  } catch (error) {
    return { diagnostics: [syntaxError(error)], ast: null };
  }

  const diagnostics: LintDiagnostic[] = [];
  const active = rules.flatMap(rule => {
    const severity = resolveSeverity(rule, options);
    return severity === null ? [] : [{ rule, severity }];
  });
  const ctxBase = { ast, table: buildSymbolTable(ast), lines, eol: text.includes('\r\n') ? '\r\n' : '\n' };
  const contexts = active.map(({ rule, severity }) => {
    const ctx: RuleContext = {
      ...ctxBase,
      report(loc: Loc | undefined, message: string, fix?: Fix) {
        diagnostics.push({ severity, range: toRange(loc), code: rule.code, message, source, ...(fix ? { data: { fix } } : {}) });
      }
    };
    return { rule, ctx };
  });

  // One traversal for every node rule, then each file rule once.
  const byType = new Map<AstNodeType, { rule: Rule; ctx: RuleContext }[]>();
  for (const entry of contexts) for (const type of entry.rule.on ?? []) byType.set(type, [...(byType.get(type) ?? []), entry]);
  walk(ast, (node: AstNode) => {
    for (const { rule, ctx } of byType.get(node.type) ?? []) rule.check(node, ctx);
  });
  for (const { rule, ctx } of contexts) if (!rule.on) rule.check(ast, ctx);

  const suppressed = suppressions(lines);
  const kept = diagnostics.filter(d => !suppressed(d.range.start.line, d.code));
  // One pass appends to another, so order by position rather than by which rule produced what.
  kept.sort((a, b) => a.range.start.line - b.range.start.line || a.range.start.character - b.range.start.character);
  return { diagnostics: kept, ast };
}

function resolveSeverity(rule: Rule, options: LinterOptions): Severity | null {
  if (rule.locked) return rule.severity;
  const name = options.rules?.[rule.code] ?? (rule.code === 'unsupported-syntax' ? options.unsupportedSyntaxSeverity : undefined);
  return name === undefined ? rule.severity : severityByName[name];
}

function severityName(severity: Severity): SeverityName {
  return (Object.keys(severityByName) as SeverityName[]).find(n => severityByName[n] === severity)!;
}

// A parse failure is a real syntax error, the one kind of finding no setting can quiet. Peggy reports where it gave up; anything else is a bug in the linter and is pinned to the first line so it is at least visible.
function syntaxError(error: unknown): LintDiagnostic {
  const location = (error as { location?: Loc })?.location;
  const message = location ? (error as Error).message : 'Error while linting: ' + error;
  return { severity: Severity.Error, range: toRange(location), code: 'syntax-error', message, source };
}

// --- suppression comments --------------------------------------------------
// `* vfp-lint-disable-next-line code, code`, `&& vfp-lint-disable-line code` on the statement itself, and `* vfp-lint-disable code` ... `* vfp-lint-enable code` around a region. No codes means every rule. Anything after ` -- ` is the reason and is ignored.
// Comments are not in the syntax tree, so this reads the lines directly. A directive inside a string literal would be honoured too, which is not worth a tokenizer to prevent.
const directive = /(?:^\s*\*|&&)\s*vfp-lint-(disable-next-line|disable-line|disable|enable)\b(.*)$/i;

/** Returns a predicate saying whether a diagnostic of `code` on the 0-based `line` is suppressed. */
export function suppressions(lines: string[]): (line: number, code: string) => boolean {
  // null means every code.
  const perLine = new Map<number, Set<string> | null>();
  const regions: { from: number; to: number; codes: Set<string> | null }[] = [];
  const open = new Map<string | null, number>(); // code (or null for all) -> line it was disabled on

  const add = (line: number, codes: Set<string> | null) => {
    const existing = perLine.get(line);
    if (existing === null) return;
    if (codes === null || existing === undefined) perLine.set(line, codes);
    else for (const c of codes) existing.add(c);
  };

  lines.forEach((text, i) => {
    const m = directive.exec(text);
    if (!m) return;
    const listed = m[2].split('--')[0].match(/[a-z][a-z0-9-]*/gi)?.map(c => c.toLowerCase()) ?? [];
    const codes = listed.length ? new Set(listed) : null;
    switch (m[1].toLowerCase()) {
      case 'disable-next-line': add(i + 1, codes); break;
      case 'disable-line': add(i, codes); break;
      case 'disable':
        for (const key of codes ?? [null]) if (!open.has(key)) open.set(key, i);
        break;
      case 'enable':
        for (const key of codes ?? [...open.keys()]) {
          const from = open.get(key);
          if (from === undefined) continue;
          regions.push({ from, to: i, codes: key === null ? null : new Set([key]) });
          open.delete(key);
        }
        break;
    }
  });
  for (const [key, from] of open) regions.push({ from, to: Infinity, codes: key === null ? null : new Set([key]) });

  return (line, code) => {
    const codes = perLine.get(line);
    if (codes === null || codes?.has(code)) return true;
    return regions.some(r => line >= r.from && line <= r.to && (r.codes === null || r.codes.has(code)));
  };
}
