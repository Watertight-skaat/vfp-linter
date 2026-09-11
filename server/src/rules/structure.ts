// The structural smells: what a block gets wrong on its own, and the catch-all for what the grammar cannot read.

import { expressionKey, onNode, Severity } from '../rule.js';

// Statements after one of these in the same block can never run.
const terminators = new Set(['ReturnStatement', 'ExitStatement', 'ContinueStatement']);

// A routine after a file-level RETURN is a definition, not code that would have run: ending the main program with RETURN and putting the procedures below it is the normal layout.
const routineTypes = new Set(['ProcedureStatement', 'DefineClass']);

export const unreachableCode = onNode({
  code: 'unreachable-code',
  severity: Severity.Warning,
  on: ['Program', 'BlockStatement'],
  check(node, ctx) {
    const body = node.body;
    for (let i = 0; i < body.length - 1; i++) {
      const statement = body[i];
      const next = body[i + 1];
      if (!statement || !next || !terminators.has(statement.type)) continue;
      if (routineTypes.has(next.type)) continue;
      const keyword = statement.type === 'ReturnStatement' ? 'RETURN'
        : statement.type === 'ContinueStatement' ? 'LOOP' : 'EXIT';
      ctx.report(next.location, `This cannot run: the ${keyword} above it leaves the block first.`);
      return; // one report per block is enough to make the point
    }
  }
});

export const duplicateCase = onNode({
  code: 'duplicate-case',
  severity: Severity.Warning,
  on: ['DoCaseStatement'],
  check(node, ctx) {
    const seen = new Map<string, number>();
    for (const clause of node.cases) {
      if (!clause || clause.type !== 'CaseClause') continue;
      // `CASE .F.` is how a branch is switched off without deleting it, so repeating it is deliberate.
      if (clause.test.type === 'BooleanLiteral' && !clause.test.value) continue;
      const key = expressionKey(clause.test);
      const first = seen.get(key);
      if (first === undefined) seen.set(key, clause.location?.start?.line ?? 0);
      else ctx.report(clause.location, `This condition is identical to the CASE on line ${first}, so this branch can never be reached.`);
    }
  }
});

// Comments are not part of the AST, so a branch holding only a comment reads as empty. That is why this is advisory rather than a warning.
const emptyBranchMessages: Record<string, string> = {
  IF: 'This IF branch is empty, so the test decides nothing.',
  ELSE: 'This ELSE branch is empty and can be removed.',
  CASE: 'This CASE branch is empty, so matching it does nothing.',
  OTHERWISE: 'This OTHERWISE branch is empty and can be removed.'
};

export const emptyBranch = onNode({
  code: 'empty-branch',
  severity: Severity.Information,
  on: ['IfStatement', 'DoCaseStatement'],
  check(node, ctx) {
    if (node.type === 'IfStatement') {
      if (node.consequent && node.consequent.body.length === 0) ctx.report(node.location, emptyBranchMessages.IF);
      if (node.alternate && node.alternate.body.length === 0) ctx.report(node.location, emptyBranchMessages.ELSE);
      return;
    }
    for (const clause of node.cases) {
      if (clause?.type === 'CaseClause' && clause.consequent && clause.consequent.body.length === 0)
        ctx.report(clause.location, emptyBranchMessages.CASE);
    }
    if (node.otherwise && node.otherwise.body.length === 0) ctx.report(node.otherwise.location, emptyBranchMessages.OTHERWISE);
  }
});

export const tryWithoutCatch = onNode({
  code: 'try-without-catch',
  severity: Severity.Information,
  on: ['TryStatement'],
  check(node, ctx) {
    if (node.catchClause || node.finallyBlock) return;
    ctx.report(node.location, 'This TRY has neither CATCH nor FINALLY, so it handles nothing and any error still propagates to the caller.');
  }
});

export const privateAll = onNode({
  code: 'private-all',
  severity: Severity.Warning,
  on: ['PrivateAll'],
  check(node, ctx) {
    ctx.report(node.location, 'PRIVATE ALL hides every variable of the caller from this routine and everything it calls. Name the variables it needs to hide, or declare the ones this routine owns as LOCAL.');
  }
});

// Statements whose grammar rule cannot match without its terminator. SCAN is absent on purpose: ENDSCAN is optional in the grammar, so an unterminated SCAN never reaches the catch-all.
const blockOpeners = [
  { opener: /^IF\b/i, terminator: 'ENDIF' },
  { opener: /^FOR\b/i, terminator: 'ENDFOR or NEXT' },
  { opener: /^DO\s+WHILE\b/i, terminator: 'ENDDO' },
  { opener: /^DO\s+CASE\b/i, terminator: 'ENDCASE' },
  { opener: /^TRY\b/i, terminator: 'ENDTRY' },
  { opener: /^WITH\b/i, terminator: 'ENDWITH' },
  { opener: /^DEFINE\s+CLASS\b/i, terminator: 'ENDDEFINE' },
  { opener: /^TEXT\b/i, terminator: 'ENDTEXT' }
];

// The catch-all also absorbs the opening line of a block whose terminator is missing: the block rule fails and the line falls through to UnknownStatement. That is broken code rather than syntax the linter has not learned, so it is locked at Error: the parser never throws for it, and letting a setting quiet it would hide it.
export const unterminatedBlock = onNode({
  code: 'unterminated-block',
  severity: Severity.Error,
  locked: true,
  on: ['UnknownStatement'],
  check(node, ctx) {
    const raw = node.raw ?? '';
    const unterminated = blockOpeners.find(b => b.opener.test(raw));
    if (unterminated) ctx.report(node.location, `This opens a block that could not be parsed. Check for a missing ${unterminated.terminator}: '${raw}'`);
  }
});

// A construct the grammar does not cover yet is not the same thing as a construct that is wrong, so unsupported syntax is advisory by default: valid FoxPro the grammar has not learned should not look like a mistake. The test harness raises it to error so the probe corpus still fails on any construct the grammar cannot read.
export const unsupportedSyntax = onNode({
  code: 'unsupported-syntax',
  severity: Severity.Information,
  on: ['UnknownStatement'],
  check(node, ctx) {
    const raw = node.raw ?? '';
    if (blockOpeners.some(b => b.opener.test(raw))) return; // reported as unterminated-block
    ctx.report(node.location, `This statement is valid FoxPro that the linter does not parse yet, so it is not being checked: '${raw}'`);
  }
});
