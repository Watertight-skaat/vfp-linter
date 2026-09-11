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
	'Widget.Label:method',
	'Branching:function',
	'Filtering:procedure',
	'Subscripts:procedure',
	'Styling:procedure',
	'Records:procedure',
	'Reordered:procedure',
	'KeywordMembers:procedure',
	'ComputedAreas:procedure'
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

// A DO CASE used to drop every branch it had, contents and all, so nothing inside one reached the symbol table. These assertions fail again if that regresses.
check('a variable written inside both CASE branches', shape('Branching', 'LCBRANCH'),
	{ kind: 'local', type: null, array: false, declared: true, reads: 1, writes: 2 });
check('a parameter read inside a CASE condition', shape('Branching', 'TNTYPE'),
	{ kind: 'parameter', type: null, array: false, declared: true, reads: 1, writes: 0 });

// SCAN and REPLACE dropped their FOR and WHILE conditions, DIMENSION its column count and SET its argument, each by reading the wrong index of a PEG sequence. The statements still parsed, so no diagnostic could have caught it -- only counting the reads does.
check('a parameter read in SCAN FOR and REPLACE WITH', shape('Filtering', 'TNLIMIT'),
	{ kind: 'parameter', type: null, array: false, declared: true, reads: 2, writes: 0 });
check('a parameter read in DIMENSION, SET, SCAN WHILE and REPLACE FOR', shape('Filtering', 'TNCOLS'),
	{ kind: 'parameter', type: null, array: false, declared: true, reads: 4, writes: 0 });
check('a field in a scoped condition is not a declared variable', shape('Filtering', 'INVBAL'),
	{ kind: 'implicit', type: null, array: false, declared: false, reads: 3, writes: 0 });

// The ARRAY keyword used to be read as the first variable name on all three of these, a parenthesised subscript split an assignment into a call plus a stray literal, and STORE matched the bare name and dropped the subscript. All four parsed, so no diagnostic could have caught any of them; a symbol with the wrong kind, or a missing write, is the only visible trace. The scope names alone are the assertion for the first three -- a variable named ARRAY would appear here instead.
check('Subscripts declares only the three arrays', names('Subscripts'),
	['LABRACKETS', 'LAPRIVATE', 'LAPUBLIC']);
check('LOCAL ARRAY with brackets, written through both subscript forms', shape('Subscripts', 'LABRACKETS'),
	{ kind: 'local', type: null, array: true, declared: true, reads: 0, writes: 2 });
check('PUBLIC ARRAY, written by STORE to an element', shape('Subscripts', 'LAPUBLIC'),
	{ kind: 'public', type: null, array: true, declared: true, reads: 0, writes: 1 });
check('PRIVATE ARRAY, two-dimensional', shape('Subscripts', 'LAPRIVATE'),
	{ kind: 'private', type: null, array: true, declared: true, reads: 0, writes: 1 });

// Inside WITH, the leading dot is the only thing separating a property from a memory variable, and the grammar used to drop it. The names list is the assertion: CAPTION, COLUMNS and WIDTH appearing here would mean every property assignment in every WITH block is being booked as an implicit PRIVATE.
check('WITH properties are not variables', names('Styling'), ['LCHEADING', 'TNCOLUMN', 'TOGRID']);
check('an argument inside a WITH member is still a read', shape('Styling', 'TNCOLUMN'),
	{ kind: 'parameter', type: null, array: false, declared: true, reads: 1, writes: 0 });

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

// SCATTER, GATHER, CATCH TO, TEXT TO, DO FORM and &macro each name a variable that reached the symbol table as nothing at all: the statement either did not parse or parsed with the name thrown away. A missing reference here is invisible to every diagnostics fixture, because a statement that quietly drops its operand still reports nothing -- so the counts are the assertion.
check('SCATTER NAME writes the object, GATHER NAME reads it', shape('Records', 'LOROW'),
	{ kind: 'local', type: null, array: false, declared: true, reads: 1, writes: 1 });
check('a macro substitution is a read of the variable being run', shape('Records', 'LCCOMMAND'),
	{ kind: 'local', type: null, array: false, declared: true, reads: 1, writes: 1 });
check('CATCH TO m.name creates the error object', shape('Records', 'LOERR'),
	{ kind: 'local', type: null, array: false, declared: true, reads: 1, writes: 1 });
check('TEXT TO builds the variable it names', shape('Records', 'LCREPORT'),
	{ kind: 'local', type: null, array: false, declared: true, reads: 1, writes: 1 });
check('DO FORM ... TO receives into the variable', shape('Records', 'LOPICKED'),
	{ kind: 'local', type: null, array: false, declared: true, reads: 1, writes: 1 });
check('COUNT TO writes an undeclared name', shape('Records', 'LNSEEN'),
	{ kind: 'implicit', type: null, array: false, declared: false, reads: 0, writes: 1 });
// A name inside a <<...>> merge is output text rather than code, so it is not a reference. That cannot produce a false 'unused': a variable worth merging has to have been assigned somewhere first.
check('a name inside a TEXTMERGE body is not a reference', sym('Records', 'INVNUM'), null);

// SCAN takes FOR and WHILE in either order. The fixed order left the second clause to the catch-all, which reported a missing ENDFOR for a block that was never opened; both operands must reach the tree.
check('SCAN reads both clauses with WHILE written first',
	[shape('Reordered', 'TNCOLS').reads, shape('Reordered', 'TNLIMIT').reads], [1, 1]);

// A keyword after the dot is a property name. TO, FROM and CLASS appearing in this list would mean each reference had been cut at the dot and the keyword booked as a memory variable of its own.
check('keyword members are not variables', names('KeywordMembers'), ['LCJOINED', 'TOMESSAGE']);
check('the object is read once per member reference', shape('KeywordMembers', 'TOMESSAGE'),
	{ kind: 'parameter', type: null, array: false, declared: true, reads: 4, writes: 0 });

// `USE IN cust` is the normal way to close a work area, and it read as opening a table named IN -- every work-area event in the corpus was wrong. The alias of a computed one cannot be known, but the event is a targeted close either way, so the current area is left alone.
check('USE IN closes a targeted area', scope('ComputedAreas').workArea.map(e => `${e.kind}:${e.alias}:${e.targeted ? 'in' : 'current'}`),
	['close:null:in']);
check('the variable naming a computed area is read', shape('ComputedAreas', 'TCALIAS'),
	{ kind: 'parameter', type: null, array: false, declared: true, reads: 3, writes: 0 });
check('a targeted close leaves the current alias alone', aliasInEffectAt(scope('ComputedAreas'), 200), null);

report('Scope checks');
