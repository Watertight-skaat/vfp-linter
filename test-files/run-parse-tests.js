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

// --- DEFINE CLASS member declarations -----------------------------------------
// The access words on a method used to leave the whole class unreadable, so every method in it left the outline and the symbol table together.
const cls = first('DEFINE CLASS Poster AS Custom\nPROTECTED cName, nAge\nHIDDEN lDirty\nIMPLEMENTS IPoster IN "poster.dll"\nADD OBJECT cmdPost AS CommandButton WITH Caption = "Post", Top = 1\nPROTECTED PROCEDURE Post\nENDPROC\nFUNCTION Other\nENDFUNC\nENDDEFINE');
check('every member is read', cls.body.map(s => s.type),
	['ClassAccessStatement', 'ClassAccessStatement', 'ImplementsStatement', 'AddObjectStatement', 'ProcedureStatement', 'ProcedureStatement']);
check('PROTECTED names the properties', [cls.body[0].access, cls.body[0].names], ['PROTECTED', ['cName', 'nAge']]);
check('IMPLEMENTS keeps its library', cls.body[2].library, { type: 'StringLiteral', value: 'poster.dll' });
check('ADD OBJECT keeps its class and its WITH pairs',
	[cls.body[3].base, cls.body[3].properties.map(p => p.name)], ['CommandButton', ['Caption', 'Top']]);
// A method carrying the word is still a sibling of the one after it: read as a property list, the name was consumed and the class ran on unterminated to the end of the file.
check('a PROTECTED method is a routine, not a property list', [cls.body[4].name, cls.body[4].access], ['Post', 'PROTECTED']);
check('and the routine after it is its sibling', cls.body[5].name, 'Other');
check('neither word is reserved', first('protected = .T.').type, 'Assignment');

// --- the output commands --------------------------------------------------------
check('?? is its own style', (({ style, arguments: a }) => [style, a.length])(first('?? lcMessage')), ['??', 1]);
check('??? too', first('??? lcMessage').style, '???');
check('? is unchanged', first('? m.x').style, '?');
check('PRINT is the ? form', first('PRINT m.x').style, '?');
check('a bare ? still reads', first('?').arguments, []);
// \ starts a new line of TEXTMERGE output, \\ appends to the one before it. The rest of the line is text, not code.
check('a \\ line', first('\\ Dear <<m.cName>>,'), { type: 'TextMergeLine', newline: true, content: ' Dear <<m.cName>>,' });
check('a \\\\ line appends', (({ newline }) => newline)(first('\\\\ and the rest of it.')), false);

// --- the rest of the old ledger ---------------------------------------------------
check('APPEND MEMO', (({ field, overwrite }) => ({ field, overwrite }))(first('APPEND MEMO notes FROM notes.txt OVERWRITE')), { field: 'notes', overwrite: true });
check('APPEND on its own is untouched', first('APPEND BLANK').type, 'AppendStatement');
check('COPY MEMO', first('COPY MEMO notes TO notes.txt').field, 'notes');
check('COPY TO is still untouched', first('COPY TO x').type, 'CopyToStatement');
check('ON PAD opens a submenu', (({ what, of, activate }) => ({ what, of, activate }))(first('ON PAD pFile OF mMain ACTIVATE POPUP pFileMenu')),
	{ what: 'PAD', of: 'mMain', activate: { what: 'POPUP', name: 'pFileMenu' } });
check('ON BAR too', first('ON BAR 1 OF pFileMenu ACTIVATE POPUP pSubMenu').what, 'BAR');
check('ON SELECTION still runs a command', first('ON SELECTION BAR 1 OF pFileMenu DO Foo').command.type, 'DoStatement');
check('a quoted class library', first('LOCAL loX AS Poster OF "poster.vcx"').ofClass, { type: 'StringLiteral', value: 'poster.vcx' });
check('a quoted type', first('LOCAL loX AS "Custom"').asType, { type: 'StringLiteral', value: 'Custom' });
check('a bare one is still a string', first('LOCAL loX AS Custom').asType, 'Custom');
check('CANCEL', first('CANCEL').type, 'CancelStatement');
check('READ EVENTS', first('READ EVENTS').type, 'ReadEventsStatement');
check('COMPILE names the file', first('COMPILE program.prg').target, { type: 'Path', path: 'program.prg' });
check('BUILD APP names both', (({ what, from }) => [what, from.path])(first('BUILD APP myapp FROM myproject')), ['APP', 'myproject']);
// RETURN parses on its own, so without the TO form claimed first the tail read as a statement after it and reported as unreachable as well.
check('RETURN TO MASTER', (({ argument, to }) => ({ argument, to }))(first('RETURN TO MASTER')), { argument: null, to: 'MASTER' });
check('RETURN TO a routine', first('RETURN TO Caller').to, 'Caller');
check('RETURN with a value is untouched', (({ argument, to }) => [argument.value, to])(first('RETURN .T.')), [true, null]);

// --- SET: a file path, and a clause of its own -------------------------------------
check('a bare Windows path is the argument', first('SET DEFAULT TO c:\\temp').arguments, [{ type: 'Path', path: 'c:\\temp' }]);
check('SET ... TO FILE marks the destination', (({ file, arguments: a }) => [file, a[0].path])(first('SET PRINTER TO FILE output.txt')), [true, 'output.txt']);
check('DELIMITERS TO is a clause, not two settings',
	first('SET TEXTMERGE ON DELIMITERS TO "<<", ">>"').delimiters.map(d => d.value), ['<<', '>>']);
check('the state beside it still reads', first('SET TEXTMERGE ON DELIMITERS TO "<<", ">>"').state, 'ON');
check('an ordinary argument is still an expression', first('SET CENTURY TO 19').arguments[0].type, 'NumberLiteral');

// --- the screen buffer, SET MARK OF, and an index file with its extension -----------
// The last three lines of the unsupported ledger. Each is asserted on the field that was lost, not on the statement parsing at all: SAVE SCREEN read as nothing, SET MARK OF dropped its TO clause, and INDEX ON ... OF stopped at the dot.
check('SAVE SCREEN names the variable', first('SAVE SCREEN TO lcScreen'), { type: 'SaveScreenStatement', to: 'lcScreen' });
check('and stands on its own', first('SAVE SCREEN').to, null);
check('RESTORE SCREEN is the other half', first('RESTORE SCREEN FROM lcScreen'), { type: 'RestoreScreenStatement', from: 'lcScreen' });
check('SAVE TO is untouched by it', first('SAVE TO config.mem').type, 'SaveToStatement');
check('so is SAVE WINDOW', first('SAVE WINDOW wOut TO win.win').type, 'SaveWindowStatement');
check('SET MARK OF keeps its TO clause',
	(({ what, target, of, mark }) => ({ what, target, of, mark: mark.value }))(first('SET MARK OF BAR 1 OF pFileMenu TO .T.')),
	{ what: 'BAR', target: { type: 'NumberLiteral', value: 1, raw: '1', currency: false }, of: 'pFileMenu', mark: true });
check('a PAD is marked with a character', first('SET MARK OF PAD pFile OF mMain TO "*"').mark, { type: 'StringLiteral', value: '*' });
check('SET MARK TO is still the date delimiter', first('SET MARK TO "/"').type, 'SetCommand');
check('an index file keeps its extension', first('INDEX ON custid TAG custid OF cust.cdx ADDITIVE').of, { type: 'Path', path: 'cust.cdx' });
check('the option after it is no longer lost', first('INDEX ON custid TAG custid OF cust.cdx ADDITIVE').additive, true);
check('INDEX ON ... TO reads the same way', first('INDEX ON custid TO cust.idx').to, { type: 'Path', path: 'cust.idx' });
check('a quoted name is still a string', first('INDEX ON custid TAG custid OF "cust.cdx"').of, { type: 'StringLiteral', value: 'cust.cdx' });
check('a bare name is still an identifier', first('INDEX ON custid TAG custid OF cust').of, 'cust');
check('a drive and directory read as one path', first('INDEX ON custid TAG custid OF c:\\data\\cust.cdx').of, { type: 'Path', path: 'c:\\data\\cust.cdx' });
// The same pair sat behind every index file name, not just this one.
check('USE ... INDEX reads a list of them', first('USE customer INDEX cust.idx, ord.idx').index.files, [{ type: 'Path', path: 'cust.idx' }, { type: 'Path', path: 'ord.idx' }]);
check('SET ORDER TO TAG ... OF too', first('SET ORDER TO TAG custid OF cust.cdx').selection.of, { type: 'Path', path: 'cust.cdx' });
check('SET ORDER TO a bare tag is unchanged', first('SET ORDER TO custid').selection, { kind: 'FILE', value: 'custid' });
check('COPY INDEXES names both ends', (({ files, to }) => [files[0].path, to.path])(first('COPY INDEXES cust.idx TO cust.cdx')), ['cust.idx', 'cust.cdx']);

// --- FIELDS LIKE and FIELDS EXCEPT ------------------------------------------------
// The skeleton forms were written into the grammar below the field list, which matches LIKE as a field name of its own: neither alternative could ever be reached, and every FIELDS LIKE in a file silently read as one field called LIKE with the skeleton left behind.
check('FIELDS LIKE is a skeleton', first('COPY TO x FIELDS LIKE c*').fields, { kind: 'like', pattern: 'c*' });
check('FIELDS EXCEPT too', first('COPY TO x FIELDS EXCEPT c*').fields, { kind: 'except', pattern: 'c*' });
check('SCATTER reads it as well, and keeps the clause after it',
	(({ fields, destination }) => [fields, destination])(first('SCATTER FIELDS LIKE c* MEMVAR')), [{ kind: 'like', pattern: 'c*' }, 'MEMVAR']);
check('a plain field list is unchanged', first('COPY TO x FIELDS a, b').fields, { kind: 'list', fields: ['a', 'b'] });
check('and a field whose name starts with one', first('COPY TO x FIELDS likely, extra').fields, { kind: 'list', fields: ['likely', 'extra'] });

report('Parse checks');
