// Re-derives the node types and property names the grammar actually emits, and asserts server/src/ast.ts
// declares exactly those. A hand-written union is only useful while it is true, so the grammar is the
// source of truth and this suite fails the build when the two drift apart.
const fs = require('fs');
const { check, report } = require('./check.js');

const grammar = fs.readFileSync('./server/src/foxpro.pegjs', 'utf-8');
const astSource = fs.readFileSync('./server/src/ast.ts', 'utf-8');

// Read the balanced {...} that starts at `open` and return its text.
function balanced(text, open) {
	let depth = 0;
	for (let i = open; i < text.length; i++) {
		const c = text[i];
		if (c === '{' || c === '(' || c === '[') depth++;
		else if (c === '}' || c === ')' || c === ']') {
			depth--;
			if (depth === 0) return text.slice(open + 1, i);
		}
	}
	return '';
}

// Top-level keys of an object literal body, plus any `...spread` markers.
function keysOf(body) {
	const stripped = body.replace(/\/\/[^\n]*/g, '');
	const parts = [];
	let depth = 0;
	let buf = '';
	for (const c of stripped) {
		if (c === '{' || c === '(' || c === '[') depth++;
		else if (c === '}' || c === ')' || c === ']') depth--;
		if (c === ',' && depth === 0) {
			parts.push(buf);
			buf = '';
		} else buf += c;
	}
	parts.push(buf);
	return parts.map(p => p.trim()).filter(Boolean).map(p => {
		if (p.startsWith('...')) return p.split(/[\s,}]/)[0];
		const m = p.match(/^([A-Za-z_]\w*)\s*:/);
		return m ? m[1] : p.split(/\s/)[0];
	});
}

// The rule body that defines `RuleName`, up to the next rule at column 0.
function ruleBody(name) {
	const start = grammar.search(new RegExp(`^\\s*${name}\\s*("[^"]*")?\\s*$|^\\s*${name}\\s*("[^"]*")?\\s*=`, 'm'));
	if (start < 0) return '';
	const rest = grammar.slice(start + name.length);
	const end = rest.search(/\n[A-Za-z_]\w*\s*("[^"]*")?\s*\n?\s*=/);
	return end < 0 ? rest : rest.slice(0, end);
}

// A spread either names a local object built earlier in the action, or the result of another rule.
function resolveSpread(spread, context) {
	const id = spread.slice(3);
	const local = context.indexOf(`const ${id} = {`);
	if (local >= 0) return keysOf(balanced(context, context.indexOf('{', local)));
	const bound = context.match(new RegExp(`\\b${id}\\s*:\\s*([A-Z]\\w*)`));
	if (!bound) return [];
	const body = ruleBody(bound[1]);
	const ret = body.lastIndexOf('return {');
	return ret < 0 ? [] : keysOf(balanced(body, body.indexOf('{', ret)));
}

// --- what the grammar emits -------------------------------------------------
const fromGrammar = new Map();
for (const m of grammar.matchAll(/node\(\s*['"]([A-Za-z_]\w*)['"]\s*,\s*\{/g)) {
	const open = m.index + m[0].length - 1;
	// The action this node() call sits in, so a spread can be resolved against it.
	const context = grammar.slice(Math.max(0, m.index - 4000), m.index);
	const keys = keysOf(balanced(grammar, open)).flatMap(k => (k.startsWith('...') ? resolveSpread(k, context) : [k]));
	if (!fromGrammar.has(m[1])) fromGrammar.set(m[1], new Set());
	for (const k of keys) fromGrammar.get(m[1]).add(k);
}

// --- what ast.ts declares --------------------------------------------------
const fromAst = new Map();
for (const m of astSource.matchAll(/export interface (\w+) extends NodeBase \{/g)) {
	const body = balanced(astSource, m.index + m[0].length - 1);
	const typeName = body.match(/\btype:\s*'([^']+)'/);
	if (!typeName) continue;
	const props = [...body.matchAll(/^\s{2}(\w+)\??:/gm)].map(p => p[1]).filter(p => p !== 'type');
	fromAst.set(typeName[1], new Set(props));
}

// --- the unions ------------------------------------------------------------
function unionMembers(name) {
	const at = astSource.indexOf(`export type ${name} =`);
	const body = astSource.slice(at, astSource.indexOf(';', at));
	return body.split('|').slice(1).map(s => s.trim()).filter(Boolean);
}
const interfaceOfType = new Map();
for (const m of astSource.matchAll(/export interface (\w+) extends NodeBase \{/g)) {
	const body = balanced(astSource, m.index + m[0].length - 1);
	const typeName = body.match(/\btype:\s*'([^']+)'/);
	if (typeName) interfaceOfType.set(typeName[1], m[1]);
}

const sorted = set => [...set].sort();

check('every node type the grammar emits is declared in ast.ts',
	sorted(fromGrammar.keys()).filter(n => !fromAst.has(n)), []);
check('ast.ts declares no node type the grammar never emits',
	sorted(fromAst.keys()).filter(n => !fromGrammar.has(n)), []);
check('node type count', fromAst.size, fromGrammar.size);

const propMismatches = [];
for (const [name, expected] of [...fromGrammar].sort()) {
	const actual = fromAst.get(name);
	if (!actual) continue;
	const missing = sorted(expected).filter(k => !actual.has(k));
	const extra = sorted(actual).filter(k => !expected.has(k));
	if (missing.length || extra.length) propMismatches.push({ node: name, missing, extra });
}
check('every declared node has exactly the properties the grammar sets', propMismatches, []);

// AstNode is only exhaustive if every interface is reachable through Statement or Expr.
const reachable = new Set([...unionMembers('Statement'), ...unionMembers('Expr')]);
check('every node interface is a member of Statement or Expr',
	[...interfaceOfType.values()].sort().filter(i => !reachable.has(i)), []);
check('the unions name no interface that does not exist',
	[...reachable].sort().filter(i => !astSource.includes(`export interface ${i} extends NodeBase {`)), []);

report('AST checks');
