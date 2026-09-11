// What the parser returns for one construct, asserted directly.
// run-all-tests.js can only say that a fixture produced no diagnostic, which a misparse satisfies just as well as a correct parse: `x = 0x1F` read as the literal zero is a clean parse and a wrong tree, and `SET TOPIC TO "x"` read as SET TO with a setting called PIC reported nothing at all. Everything here is a shape a fixture cannot check.
const parser = require('../server/src/parser.js');
const { check, report } = require('./check.js');

// Locations are dropped so a mismatch prints something readable.
const strip = value => JSON.parse(JSON.stringify(value, (k, v) => (k === 'location' ? undefined : v)) ?? 'null');
const body = src => strip(parser.parse(src).body);
const first = src => body(src)[0];
const types = src => body(src).map(s => s.type);

// --- SELECT() as a function --------------------------------------------------
// SELECT is in the Keyword list, so Identifier refuses it and the whole assignment fell to the catch-all; lnArea was then reported as an unused local.
check('SELECT("alias") is a call', first('lnArea = SELECT("customer")').expression,
	{ type: 'CallExpression', callee: { type: 'Identifier', name: 'SELECT' }, arguments: [{ type: 'StringLiteral', value: 'customer' }] });
check('SELECT(0) is a call, not the literal zero', first('lnArea = SELECT(0)').expression.type, 'CallExpression');
check('SELECT <n> is still the work-area switch', first('SELECT 0').type, 'SelectStatement');
check('SELECT (expr) is still the work-area switch', first('SELECT (m.cAlias)').type, 'SelectStatement');
check('a subquery is still a query', first('SELECT a FROM t WHERE x IN (SELECT b FROM u)').where.right.type, 'SelectStatement');

// --- number literals ----------------------------------------------------------
check('hex', first('x = 0x1F').expression, { type: 'NumberLiteral', value: 31, raw: '0x1F', currency: false });
check('scientific', first('x = 1E5').expression, { type: 'NumberLiteral', value: 100000, raw: '1E5', currency: false });
check('signed exponent', first('x = 1.5e-3').expression.value, 0.0015);
check('currency still reads', first('x = $12.50').expression, { type: 'NumberLiteral', value: 12.5, raw: '$12.50', currency: true });
check('a leading point still reads', first('x = .5').expression.value, 0.5);

// --- TRUE and FALSE are names, not literals -----------------------------------
// VFP has .T. and .F. only. Read as booleans, a variable of either name vanished from the symbol table.
check('TRUE is a name', first('x = TRUE').expression, { type: 'Identifier', name: 'TRUE' });
check('.T. is still a boolean', first('x = .T.').expression, { type: 'BooleanLiteral', value: true });

// --- NOTE, the oldest comment form -------------------------------------------
check('NOTE is a comment, not a statement', types('NOTE this is a comment\nx = 1'), ['Assignment']);
check('a semicolon carries it onto the next line', types('NOTE first ;\nsecond\nx = 1'), ['Assignment']);
check('a variable of that name is untouched', first('note = 1').type, 'Assignment');
check('so is a property of one', first('note.caption = 1').target.type, 'MemberExpression');
check('and a call to one', first('note(1)').expression.type, 'CallExpression');

// --- #IF bodies ---------------------------------------------------------------
const pp = first('#IF .T.\nLOCAL x\n#ENDIF');
check('the condition is kept raw', [pp.directive, pp.test], ['IF', '.T.']);
check('the body is parsed', pp.consequent.body.map(s => s.type), ['LocalDeclaration']);
check('a nested #IF ends at its own #ENDIF',
	first('#IF .T.\n#IF .F.\nLOCAL a\n#ENDIF\nLOCAL b\n#ENDIF').consequent.body.map(s => s.type), ['PreprocessorIfStatement', 'LocalDeclaration']);
check('#ELSE fills the branch below', first('#IF .F.\n? 1\n#ELSE\n? 2\n#ENDIF').alternate.body.map(s => s.type), ['PrintStatement']);
check('an #ELIF link stands alone in that branch', first('#IF a\n? 1\n#ELIF b\n? 2\n#ENDIF').alternate.body[0].directive, 'ELIF');
check('#IFDEF is read too', first('#IFDEF FOO\nLOCAL x\n#ENDIF').directive, 'IFDEF');

// --- dangling terminators ------------------------------------------------------
// These used to throw, which cost the file every other diagnostic in it.
check('a stray terminator is absorbed', types('x = 1\nENDIF\ny = 2'), ['Assignment', 'DanglingTerminator', 'Assignment']);
check('it names the word', first('ENDCASE').keyword, 'ENDCASE');
check('a block still claims its own terminator', types('IF x\ny = 1\nENDIF'), ['IfStatement']);
check('and a routine still claims ENDPROC', types('PROCEDURE Foo\nx = 1\nENDPROC'), ['ProcedureStatement']);
check('NEXT still closes a FOR', types('FOR i = 1 TO 3\nx = 1\nNEXT'), ['ForStatement']);
check('and is still usable as a name', first('NEXT = 1').type, 'Assignment');

// --- SET: argument lists and clauses of their own --------------------------------
const classlib = first('SET CLASSLIB TO mylib IN app ALIAS al');
check('SET ... IN ... ALIAS', [classlib.command, classlib.arguments.map(a => a.name), classlib.inTarget, classlib.alias], ['CLASSLIB', ['mylib'], 'app', 'al']);
const procs = first('SET PROCEDURE TO lib1, lib2 ADDITIVE');
check('the argument is a list', [procs.arguments.map(a => a.name), procs.additive], [['lib1', 'lib2'], true]);
check('SET ... OFF INTO ...', (({ state, into }) => ({ state, into }))(first('SET RELATION OFF INTO orders')), { state: 'OFF', into: 'orders' });
check('SET ... TO ... INTO ...', first('SET SKIP TO custid INTO orders').into, 'orders');
// Without a word boundary the TO literal matched the first two letters of TOPIC, which produced a valid tree and reported nothing.
check('a setting whose name begins with TO', first('SET TOPIC TO "x"').command, 'TOPIC');
check('a bare value is still the argument', first('SET STATUS BAR OFF').arguments.map(a => a.name), ['BAR']);
check('SET FILTER TO on its own still clears', first('SET FILTER TO').cleared, true);
check('SET TO <expr> is still its own node', first('SET TO m.x').type, 'SetTo');

// --- BROWSE options past the first ----------------------------------------------
check('every option is consumed', types('BROWSE FIELDS custid, name NOEDIT'), ['BrowseStatement']);
check('the fields are still read', first('BROWSE FIELDS custid, name NOEDIT').fields, ['custid', 'name']);
check('an option taking a value', types('BROWSE LAST NOWAIT TIMEOUT 30 TITLE "Customers" IN WINDOW wMain'), ['BrowseStatement']);

// --- the rest of the ledger -------------------------------------------------------
check('SAVE WINDOW', first('SAVE WINDOW wOut TO layout').windows, ['wOut']);
check('SAVE WINDOW ALL', first('SAVE WINDOW ALL TO layout').windows, 'ALL');
check('RESTORE WINDOW', first('RESTORE WINDOW wOut FROM layout').type, 'RestoreWindowStatement');
check('SAVE TO is untouched', first('SAVE TO memfile').type, 'SaveToStatement');
check('INSERT BEFORE BLANK', (({ blank, before }) => ({ blank, before }))(first('INSERT BEFORE BLANK')), { blank: true, before: true });
check('INSERT INTO is still the query', first('INSERT INTO t (a) VALUES (1)').type, 'InsertStatement');
check('FIND keeps its text as written', first('FIND smith').text, 'smith');
check('COPY INDEXES', (({ files, to }) => ({ files, to }))(first('COPY INDEXES a, b TO c')), { files: ['a', 'b'], to: 'c' });
check('COPY TO is untouched', first('COPY TO x').type, 'CopyToStatement');
check('CREATE VIEW parses its query', first('CREATE VIEW v AS SELECT custid FROM orders').query.type, 'SelectStatement');
check('CREATE CURSOR is untouched', first('CREATE CURSOR c (a C(10))').type, 'CreateStatement');
// DECLARE of an array is the older spelling of DIMENSION, and the one statement in the group that names a variable.
check('DECLARE of an array declares it', first('DECLARE laArr[3]'), { type: 'DimensionStatement', items: [{ name: 'laArr', rows: { type: 'NumberLiteral', value: 3, raw: '3', currency: false }, columns: null, asType: null }] });
check('DECLARE of a DLL function still reads', first('DECLARE INTEGER Sleep IN kernel32 INTEGER nMs').functionName, 'Sleep');
check('WAIT ... TO names the variable', first('WAIT "" TO lcKey').to, 'lcKey');
check('WAIT WINDOW still reads', (({ window, nowait }) => ({ window, nowait }))(first('WAIT WINDOW "hi" NOWAIT')), { window: true, nowait: true });
check('WAIT WINDOW AT', first('WAIT WINDOW AT 10, 20 "hello"').at.row.value, 10);
check('a variable called wait is untouched', first('wait = 1').type, 'Assignment');
check('DEBUGOUT', first('DEBUGOUT lcMessage').expression.name, 'lcMessage');
check('a call to a routine of that name is untouched', first('debugout(1)').expression.type, 'CallExpression');

report('Parse checks');
