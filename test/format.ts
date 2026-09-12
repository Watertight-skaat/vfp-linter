// One diagnostic as one line of an .expected file: "line:character severity code message", positions 1-based as an editor shows them. Shared by every suite that records diagnostics, so a fixture reads the same whichever harness produced it.
import path from 'path';
import type { LintDiagnostic } from '../server/src/linter.js';

const severityNames: Record<number, string> = { 1: 'error', 2: 'warning', 3: 'information', 4: 'hint' };

const oneLine = (text: string) => String(text).replace(/\s+/g, ' ').trim();

export function format(diagnostic: LintDiagnostic): string {
	const { line, character } = diagnostic.range.start;
	const severity = severityNames[diagnostic.severity] ?? diagnostic.severity;
	const head = `${line + 1}:${character + 1} ${severity} ${diagnostic.code ?? '(no code)'} ${oneLine(diagnostic.message)}`;
	// A related location follows on its own indented line, named by file rather than path so the expectation does not depend on where the checkout lives.
	const related = (diagnostic.relatedInformation ?? []).map(r => `\n  -> ${path.basename(r.file)}:${r.range.start.line + 1}:${r.range.start.character + 1} ${oneLine(r.message)}`);
	return head + related.join('');
}
