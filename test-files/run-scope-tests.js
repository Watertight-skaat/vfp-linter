// Asserts the contents of the per-routine symbol table against test-files/scope.prg.
// run-all-tests.js only checks that a fixture produces no error-severity diagnostic, which cannot test a structure; these are explicit assertions about what each routine declares.
const fs = require('fs');
const parser = require('../server/src/parser.js');
const { buildSymbolTable, aliasInEffectAt } = require('../server/src/scope.ts');
const { check, report } = require('./check.js');

const src = fs.readFileSync('./test-files/scope.prg', 'utf-8');
const table = buildSymbolTable(parser.parse(src, { grammarSource: 'scope.prg' }));

const scope = name => {
	const found = table.scopes.find(s => s.name === name);
	if (!found) throw new Error(`no scope named ${name} (have: ${table.scopes.map(s => s.name).join(', ')})`);
	return found;
};
const sym = (scopeName, varName) => scope(scopeName).symbols.get(varName) ?? null;
const names = scopeName => [...scope(scopeName).symbols.keys()].sort();
// A symbol reduced to what matters, so a mismatch prints something readable.
const shape = (scopeName, varName) => {
	const s = sym(scopeName, varName);
	if (!s) return null;
	return { kind: s.kind, type: s.declaredType, array: s.isArray, declared: !!s.declaredAt, reads: s.reads.length, writes: s.writes.length };
};

// --- scope discovery -------------------------------------------------------
check('scopes found', table.scopes.map(s => `${s.name}:${s.kind}`), [
	'(main):main',
	'CountRows:procedure',
	'Describe:function',
	'Widget:class',
	'Widget.Init:method',
	'Widget.Label:method'
]);
check('main is the root', table.main.name, '(main)');
check('methods hang off the class', scope('Widget').children.map(c => c.name), ['Widget.Init', 'Widget.Label']);

// --- file-level code -------------------------------------------------------
check('main declares lcGreeting', shape('(main)', 'LCGREETING'),
	{ kind: 'local', type: null, array: false, declared: true, reads: 0, writes: 1 });

// --- declarations ----------------------------------------------------------
check('CountRows symbols', names('CountRows'),
	['CUST_ID', 'GNTOTAL', 'LABUFFER', 'LAGRID', 'LCUNUSED', 'LNI', 'LNROWS', 'LNUNDECLARED', 'PNSEED', 'TCALIAS', 'TNSTART']);
check('LPARAMETERS are parameters', shape('CountRows', 'TCALIAS'),
	{ kind: 'parameter', type: null, array: false, declared: true, reads: 0, writes: 0 });
check('LOCAL ARRAY is an array', shape('CountRows', 'LABUFFER'),
	{ kind: 'local', type: null, array: true, declared: true, reads: 0, writes: 1 });
check('DIMENSION is an array', shape('CountRows', 'LAGRID'),
	{ kind: 'dimension', type: null, array: true, declared: true, reads: 0, writes: 0 });
check('PRIVATE', sym('CountRows', 'PNSEED').kind, 'private');
check('PUBLIC', sym('CountRows', 'GNTOTAL').kind, 'public');
check('AS clause is kept', shape('Describe', 'LCOUT'),
	{ kind: 'local', type: 'Character', array: false, declared: true, reads: 1, writes: 1 });
check('typed parameter', shape('Describe', 'TCNAME'),
	{ kind: 'parameter', type: 'Character', array: false, declared: true, reads: 1, writes: 0 });

// --- read and write sites --------------------------------------------------
// lnRows = tnStart / lnUndeclared = lnRows + 1 / m.lnRows = m.lnRows + 1 / RETURN lnRows
check('reads and writes are counted', shape('CountRows', 'LNROWS'),
	{ kind: 'local', type: null, array: false, declared: true, reads: 3, writes: 2 });
check('the m. prefix is recorded per reference', sym('CountRows', 'LNROWS').writes.map(w => w.memvarPrefix), [false, true]);
check('STORE writes every target', [sym('CountRows', 'PNSEED').writes.length, sym('CountRows', 'GNTOTAL').writes.length], [1, 1]);
check('a FOR variable is written', shape('CountRows', 'LNI'),
	{ kind: 'implicit', type: null, array: false, declared: false, reads: 2, writes: 1 });

// --- the rules this exists for --------------------------------------------
check('an unused LOCAL has no references', shape('CountRows', 'LCUNUSED'),
	{ kind: 'local', type: null, array: false, declared: true, reads: 0, writes: 0 });
check('an undeclared assignment is implicit', shape('CountRows', 'LNUNDECLARED'),
	{ kind: 'implicit', type: null, array: false, declared: false, reads: 0, writes: 1 });

// --- things that are not variables ----------------------------------------
check('a called function is not a symbol', sym('Describe', 'ALLTRIM'), null);
// A bare name in a SQL statement may be a column; the table records the ambiguity rather than guessing, so a rule can treat it as a weak reference instead of a real variable read.
check('a SQL column is flagged, not resolved',
	sym('CountRows', 'CUST_ID').reads.map(r => [r.sqlContext, r.memvarPrefix]), [[true, false]]);
check('an ordinary read is not in SQL context',
	sym('Describe', 'TCNAME').reads.map(r => r.sqlContext), [false]);
check('THIS and its properties are not symbols', names('Widget.Label'), ['LCLABEL']);
check('class-body assignments are properties', [sym('Widget', 'CNAME').kind, sym('Widget', 'NCOUNT').kind], ['property', 'property']);
check('a method body does not leak into the class', names('Widget.Init'), []);

// --- work area -------------------------------------------------------------
check('main work area', scope('(main)').workArea.map(e => `${e.kind}:${e.alias}:${e.targeted ? 'in' : 'current'}`),
	['open:CUSTOMER:current', 'select:null:current', 'open:ORD:in']);
check('main open aliases', [...scope('(main)').openAliases].sort(), ['CUSTOMER', 'ORD']);
check('a cursor destination opens an area', scope('CountRows').workArea.map(e => `${e.kind}:${e.alias}`),
	['open:CURTMP', 'select:CURTMP']);

// Lines are 1-based, as the parser reports them.
check('alias before any USE', aliasInEffectAt(scope('(main)'), 4), null);
check('alias after USE customer', aliasInEffectAt(scope('(main)'), 5), 'CUSTOMER');
check('SELECT 0 makes the alias unknowable', aliasInEffectAt(scope('(main)'), 6), undefined);
check('USE ... IN leaves the current area alone', aliasInEffectAt(scope('(main)'), 7), undefined);

report('Scope checks');
