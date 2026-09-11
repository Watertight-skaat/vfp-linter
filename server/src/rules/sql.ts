// The SQL rules: what a SELECT statement gets wrong on its own.

import type { AstNode, TableRef } from '../ast.js';
import { expressionKey, onFile, onNode, Severity, walk } from '../rule.js';
import { aliasName, isWorkAreaSwitch } from '../scope.js';

export const havingWithoutGroupBy = onNode({
  code: 'having-without-group-by',
  severity: Severity.Information,
  on: ['SelectStatement'],
  check(node, ctx) {
    if (!node.having || node.groupBy) return;
    ctx.report(node.having.location ?? node.location, 'There is no group by clause, so this is simply a post-filter on the result set.');
  }
});

/**
 * A query with no INTO or TO sends its result set to a Browse window at run time, which inside a .prg is nearly always an unfinished query rather than an intention.
 *
 * Only a SELECT standing on its own as a statement is reported. In an expression it is a subquery, and in an INSERT ... SELECT the INSERT is the destination, so neither has anywhere to put a result. Walking only the body arrays of Program, BlockStatement and DefineClass is what makes that fall out without a suppression list.
 */
export const selectWithoutInto = onFile({
  code: 'select-without-into',
  severity: Severity.Warning,
  check(ctx) {
    walk(ctx.ast, node => {
      if (node.type !== 'Program' && node.type !== 'BlockStatement' && node.type !== 'DefineClass') return;
      for (const statement of node.body) {
        if (statement?.type !== 'SelectStatement') continue;
        if (isWorkAreaSwitch(statement) || !statement.from || statement.destination) continue;
        // A UNION carries its INTO on the last SELECT, which leaves the outer node looking undirected.
        if (statement.unions.some(u => u.select.destination)) continue;
        ctx.report(statement.location, 'This query has nowhere to put its result, so VFP sends the whole result set to a Browse window at run time. Add INTO CURSOR, INTO TABLE, INTO ARRAY, or TO FILE.');
      }
    });
  }
});

/**
 * Tables in FROM with nothing relating them are a Cartesian product: every row of each against every row of the others. VFP builds it happily, and at real table sizes it reads as a hang, not an error.
 *
 * Each table is a node and each condition that mentions two of them is an edge; if the graph comes out in more than one piece, something is unrelated. A condition holding a name this query cannot attribute to a table could be the missing link, so it silences the rule rather than risk a false report -- which is why `WHERE cust_id = o_custid` is left alone while `WHERE c.id = 5` is not.
 */
export const unlinkedTables = onNode({
  code: 'unlinked-tables',
  severity: Severity.Warning,
  on: ['SelectStatement'],
  check(node, ctx) {
    const from = node.from;
    if (!from) return;

    const participants: string[] = [];
    for (const ref of [...from.tables, ...from.joins.map(j => j.target)]) {
      const key = tableKey(ref);
      if (!key) return; // a derived table: its links cannot be read from here
      participants.push(key);
    }
    if (participants.length < 2) return;

    const named = new Set(participants);
    const parent = new Map(participants.map(name => [name, name]));
    const find = (name: string): string => {
      let root = name;
      while (parent.get(root) !== root) root = parent.get(root)!;
      return root;
    };
    const link = (a: string, b: string) => parent.set(find(a), find(b));

    // Two tables compared to the same value are related through it, which is the usual way a parent and its children are fetched by a key held in a variable.
    const byValue = new Map<string, string>();

    for (const condition of [...from.joins.map(j => j.condition), node.where]) {
      for (const leaf of conditionLeaves(condition)) {
        const refs = leafReferences(leaf, named);
        // A macro expands to source at run time, so it could be the join this rule is looking for.
        if (refs.macro) return;
        for (let i = 1; i < refs.tables.length; i++) link(refs.tables[0], refs.tables[i]);
        // One unattributable name alongside anything else could be the relating term.
        if (refs.unattributed && refs.unattributed + refs.tables.length > 1) return;
        if (refs.tables.length !== 1) continue;
        const value = comparisonCounterpart(leaf, named);
        if (!value) continue;
        const earlier = byValue.get(value);
        if (earlier === undefined) byValue.set(value, refs.tables[0]);
        else if (earlier !== refs.tables[0]) link(earlier, refs.tables[0]);
      }
    }

    const groups = new Map<string, string[]>();
    for (const name of participants) {
      const root = find(name);
      groups.set(root, [...(groups.get(root) ?? []), name]);
    }
    if (groups.size < 2) return;

    const described = [...groups.values()].map(g => g.join(' + ')).join(', ');
    ctx.report(node.location, `Nothing relates these tables to each other: ${described}. VFP will build the Cartesian product -- every row of each against every row of the others -- which on real table sizes reads as a hang rather than an error.`);
  }
});

// A literal cannot carry a key between tables: two tables filtered to the same status are still unrelated.
const literalTypes = new Set(['NumberLiteral', 'StringLiteral', 'BooleanLiteral', 'NullLiteral', 'DateTimeLiteral']);

/** For `t.col = <value>`, the canonical form of the value side, when it is something that could carry a key. */
function comparisonCounterpart(leaf: unknown, named: Set<string>): string | null {
  if (!leaf || typeof leaf !== 'object') return null;
  const node = leaf as AstNode;
  if (node.type !== 'BinaryExpression') return null;
  if (node.operator !== '=' && node.operator !== '==') return null;
  const left = leafReferences(node.left, named);
  const right = leafReferences(node.right, named);
  const value = left.tables.length === 1 && right.tables.length === 0 ? node.right
    : right.tables.length === 1 && left.tables.length === 0 ? node.left
    : null;
  if (!value || literalTypes.has(value.type)) return null;
  return expressionKey(value);
}

/** The name a column of this table would be qualified by: its alias, or the table name. */
function tableKey(ref: TableRef): string | null {
  if (ref.subquery) return null;
  if (ref.alias) return ref.alias.toUpperCase();
  return aliasName(ref.name);
}

// AND and OR only combine conditions; everything else is a term that may or may not relate two tables.
function conditionLeaves(expr: unknown): unknown[] {
  if (!expr || typeof expr !== 'object') return [];
  const node = expr as AstNode;
  if (node.type === 'LogicalExpression') return [...conditionLeaves(node.left), ...conditionLeaves(node.right)];
  return [expr];
}

interface LeafReferences {
  /** Distinct tables of this query that the term mentions. */
  tables: string[];
  /** References that cannot be pinned to one of them: a bare column name, or a qualifier the FROM does not name. */
  unattributed: number;
  /** The term contains a macro, so its real text is not known until run time. */
  macro: boolean;
}

function leafReferences(expr: unknown, named: Set<string>): LeafReferences {
  const tables = new Set<string>();
  let unattributed = 0;
  let macro = false;

  function visit(value: unknown) {
    if (Array.isArray(value)) {
      for (const item of value) visit(item);
      return;
    }
    if (!value || typeof value !== 'object') return;
    const node = value as AstNode;
    if (node.type === 'MemberExpression') {
      const object = node.object;
      if (object.type === 'Identifier') {
        const qualifier = object.name.toUpperCase();
        if (qualifier === 'M') return; // a memory variable, which relates nothing
        if (named.has(qualifier)) tables.add(qualifier);
        else unattributed++;
        return;
      }
      visit(object);
      return;
    }
    if (node.type === 'Identifier') {
      unattributed++;
      return;
    }
    if (node.type === 'MacroSubstitute') {
      macro = true;
      return;
    }
    if (node.type === 'CallExpression') {
      // The callee of a bare call is a function name, not a column.
      if (node.callee.type !== 'Identifier') visit(node.callee);
      visit(node.arguments);
      return;
    }
    for (const child of Object.values(node as unknown as Record<string, unknown>)) {
      if (child && typeof child === 'object') visit(child);
    }
  }

  visit(expr);
  return { tables: [...tables], unattributed, macro };
}
