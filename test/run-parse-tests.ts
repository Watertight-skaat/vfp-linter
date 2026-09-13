// What the parser returns for one construct, asserted directly.
// run-all-tests.ts can only say that a fixture produced no diagnostic, which a misparse satisfies just as well as a correct parse: `x = 0x1F` read as the literal zero is a clean parse and a wrong tree, and `SET TOPIC TO "x"` read as SET TO with a setting called PIC reported nothing at all. Everything here is a shape a fixture cannot check.
import { parse } from '../server/src/parser.js';
import { check, report } from './check.js';

// Locations are dropped so a mismatch prints something readable.
// `any` is deliberate: every assertion below reads a partial shape off a location-stripped tree, and a real node type would only force a cast at each one.
const strip = (value: unknown): any => JSON.parse(JSON.stringify(value, (k, v) => (k === 'location' ? undefined : v)) ?? 'null');
const body = (src: string) => strip(parse(src).body);
const first = (src: string) => body(src)[0];
const types = (src: string) => body(src).map((s: { type: string }) => s.type);

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
check('the body is parsed', pp.consequent.body.map((s: any) => s.type), ['LocalDeclaration']);
check('a nested #IF ends at its own #ENDIF',
	first('#IF .T.\n#IF .F.\nLOCAL a\n#ENDIF\nLOCAL b\n#ENDIF').consequent.body.map((s: any) => s.type), ['PreprocessorIfStatement', 'LocalDeclaration']);
check('#ELSE fills the branch below', first('#IF .F.\n? 1\n#ELSE\n? 2\n#ENDIF').alternate.body.map((s: any) => s.type), ['PrintStatement']);
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
check('SET ... IN ... ALIAS', [classlib.command, classlib.arguments.map((a: any) => a.name), classlib.inTarget, classlib.alias], ['CLASSLIB', ['mylib'], 'app', 'al']);
const procs = first('SET PROCEDURE TO lib1, lib2 ADDITIVE');
check('the argument is a list', [procs.arguments.map((a: any) => a.name), procs.additive], [['lib1', 'lib2'], true]);
check('SET ... OFF INTO ...', (({ state, into }) => ({ state, into }))(first('SET RELATION OFF INTO orders')), { state: 'OFF', into: 'orders' });
check('SET ... TO ... INTO ...', first('SET SKIP TO custid INTO orders').into, 'orders');
// Without a word boundary the TO literal matched the first two letters of TOPIC, which produced a valid tree and reported nothing.
check('a setting whose name begins with TO', first('SET TOPIC TO "x"').command, 'TOPIC');
check('a bare value is still the argument', first('SET STATUS BAR OFF').arguments.map((a: any) => a.name), ['BAR']);
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
check('WAIT WINDOW AT after the message', types('WAIT WINDOW "hello" AT 10, 20 NOWAIT'), ['WaitStatement']);
check('WAIT WINDOW AT after the message keeps the position', first('WAIT WINDOW "hello" AT 10, 20').at.column.value, 20);
check('WAIT CLEAR', (({ clear, message }) => ({ clear, message }))(first('WAIT CLEAR')), { clear: true, message: null });
check('a variable called wait is untouched', first('wait = 1').type, 'Assignment');
check('DEBUGOUT', first('DEBUGOUT lcMessage').expression.name, 'lcMessage');
check('a call to a routine of that name is untouched', first('debugout(1)').expression.type, 'CallExpression');

// --- DEFINE CLASS member declarations -----------------------------------------
// The access words on a method used to leave the whole class unreadable, so every method in it left the outline and the symbol table together.
const cls = first('DEFINE CLASS Poster AS Custom\nPROTECTED cName, nAge\nHIDDEN lDirty\nIMPLEMENTS IPoster IN "poster.dll"\nADD OBJECT cmdPost AS CommandButton WITH Caption = "Post", Top = 1\nPROTECTED PROCEDURE Post\nENDPROC\nFUNCTION Other\nENDFUNC\nENDDEFINE');
check('every member is read', cls.body.map((s: any) => s.type),
	['ClassAccessStatement', 'ClassAccessStatement', 'ImplementsStatement', 'AddObjectStatement', 'ProcedureStatement', 'ProcedureStatement']);
check('PROTECTED names the properties', [cls.body[0].access, cls.body[0].names], ['PROTECTED', ['cName', 'nAge']]);
check('IMPLEMENTS keeps its library', cls.body[2].library, { type: 'StringLiteral', value: 'poster.dll' });
check('ADD OBJECT keeps its class and its WITH pairs',
	[cls.body[3].base, cls.body[3].properties.map((p: any) => p.name)], ['CommandButton', ['Caption', 'Top']]);
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
	first('SET TEXTMERGE ON DELIMITERS TO "<<", ">>"').delimiters.map((d: any) => d.value), ['<<', '>>']);
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

// --- RENAME's container forms -----------------------------------------------------
// RENAME TABLE and its three siblings rename an object inside the database or class library; the file form renames bytes on disk. Every assertion below is on which of the two matched, since both start with the same word.
check('RENAME TABLE names both ends', first('RENAME TABLE oldname TO newname'),
	{ type: 'RenameObjectStatement', kind: 'TABLE', source: 'oldname', library: null, destination: 'newname' });
check('RENAME VIEW is the same shape', (({ kind, source, destination }) => [kind, source, destination])(first('RENAME VIEW v1 TO v2')), ['VIEW', 'v1', 'v2']);
check('so is RENAME CONNECTION', first('RENAME CONNECTION c1 TO c2').kind, 'CONNECTION');
check('RENAME CLASS keeps its library, extension and all', first('RENAME CLASS poster OF posters.vcx TO banner'),
	{ type: 'RenameObjectStatement', kind: 'CLASS', source: 'poster', library: { type: 'Path', path: 'posters.vcx' }, destination: 'banner' });
check('a name expression reads on the old end', first('RENAME TABLE (lcOld) TO (lcNew)').source, { type: 'Identifier', name: 'lcOld' });
check('and on the new one', first('RENAME TABLE (lcOld) TO (lcNew)').destination, { type: 'Identifier', name: 'lcNew' });
check('the file form is untouched', first('RENAME old.dbf TO new.dbf'),
	{ type: 'RenameStatement', source: { type: 'Path', path: 'old.dbf' }, destination: { type: 'Path', path: 'new.dbf' } });
check('and a file that starts with the keyword still renames a file', first('RENAME table.dbf TO new.dbf').type, 'RenameStatement');
check('a file actually named TABLE too, which is where the two forms meet', first('RENAME TABLE TO newname').source, { type: 'Path', path: 'TABLE' });
check('a name that merely starts with it too', first('RENAME tablename TO other').source, { type: 'Path', path: 'tablename' });
check('COPY FILE is unchanged beside it', first('COPY FILE a.txt TO b.txt').type, 'CopyFileStatement');

// --- the referential-integrity side of the database container ----------------------
check('CREATE TRIGGER reads all three of its parts',
	(({ table, event, expression }) => [table, event, expression.callee.name])(first('CREATE TRIGGER ON customer FOR INSERT AS NewCustomer()')),
	['customer', 'INSERT', 'NewCustomer']);
check('the expression is code, not text', first('CREATE TRIGGER ON customer FOR UPDATE AS m.lnX > 0').expression.type, 'BinaryExpression');
// Without its own rule DELETE read TRIGGER as the record scope and left `ON customer FOR INSERT` to the catch-all.
check('DELETE TRIGGER is claimed ahead of the xbase DELETE', first('DELETE TRIGGER ON customer FOR INSERT'),
	{ type: 'DeleteTriggerStatement', table: 'customer', event: 'INSERT' });
check('the xbase DELETE is untouched beside it', first('DELETE ALL FOR amount = 0').scope, 'ALL');
check('and so is the SQL one', first('DELETE FROM orders WHERE amount = 0').type, 'DeleteStatement');
check('VALIDATE DATABASE keeps the flag that makes it write', (({ recover, options }) => ({ recover, options }))(first('VALIDATE DATABASE RECOVER')),
	{ recover: true, options: null });
check('the report tail stays raw', first('VALIDATE DATABASE NOCONSOLE TO FILE errs.txt').options, 'NOCONSOLE TO FILE errs.txt');

// --- the container itself, and the transaction frame around writes into it ---------
// All the container commands are one node carrying the verb and the object it reached, so these assert the pair rather than a shape each.
const container = (src: string) => (({ command, object, name, options }) => [command, object, name, options])(first(src));
check('CREATE DATABASE', container('CREATE DATABASE mydata'), ['CREATE', 'DATABASE', 'mydata', null]);
check('OPEN DATABASE keeps its flags as raw source', container('OPEN DATABASE mydata EXCLUSIVE NOUPDATE'),
	['OPEN', 'DATABASE', 'mydata', 'EXCLUSIVE NOUPDATE']);
check('CREATE CONNECTION', container("CREATE CONNECTION myconn DATASOURCE 'dsn'"), ['CREATE', 'CONNECTION', 'myconn', "DATASOURCE 'dsn'"]);
check('FREE TABLE', container('FREE TABLE customer'), ['FREE', 'TABLE', 'customer', null]);
check('REMOVE TABLE', container('REMOVE TABLE customer DELETE RECYCLE'), ['REMOVE', 'TABLE', 'customer', 'DELETE RECYCLE']);
// The three DELETE objects used to leave a partial node behind: DELETE parsed and the object clause after it was lost.
check('DELETE DATABASE', container('DELETE DATABASE mydata DELETETABLES'), ['DELETE', 'DATABASE', 'mydata', 'DELETETABLES']);
check('DELETE VIEW', container('DELETE VIEW myview'), ['DELETE', 'VIEW', 'myview', null]);
check('DELETE CONNECTION', container('DELETE CONNECTION myconn'), ['DELETE', 'CONNECTION', 'myconn', null]);
check('? names nothing: it asks the user to pick', container('OPEN DATABASE ?'), ['OPEN', 'DATABASE', null, null]);
check('a macro-substituted container is an expression, so the variable naming it is read',
	first('OPEN DATABASE (m.cContainer)').name.type, 'MemberExpression');
check('a container named with a path keeps the extension', first('OPEN DATABASE data\\my.dbc').name, { type: 'Path', path: 'data\\my.dbc' });
// CREATE VIEW is the SQL view, and claiming it here would turn a view whose SELECT cannot be read into a silently wrong tree.
check('CREATE SQL VIEW is untouched beside it', first('CREATE SQL VIEW v AS SELECT a FROM t').type, 'CreateViewStatement');
check('and so is CREATE TABLE', first('CREATE TABLE t (a C(10))').type, 'CreateStatement');
check('and the xbase DELETE', first('DELETE NEXT 5').scope.type, 'NEXT');

check('BEGIN TRANSACTION', first('BEGIN TRANSACTION'), { type: 'TransactionStatement', action: 'BEGIN' });
check('END TRANSACTION', first('END TRANSACTION').action, 'END');
check('ROLLBACK', first('ROLLBACK').action, 'ROLLBACK');
// ROLLBACK stands alone, so it has to refuse every shape a variable of that name takes.
check('a variable called rollback is untouched', first('rollback = .T.').type, 'Assignment');
check('and a call to one', first('rollback()').expression.type, 'CallExpression');

// --- console input ------------------------------------------------------------------
// Both put what was typed in the variable, which is the write the symbol table was losing.
check('INPUT names its message and the variable it writes', first("INPUT 'Name: ' TO lcName"),
	{ type: 'ConsoleInputStatement', command: 'INPUT', message: { type: 'StringLiteral', value: 'Name: ' }, to: 'lcName' });
check('ACCEPT is the same shape', (({ command, to }) => [command, to])(first("ACCEPT 'City: ' TO lcCity")), ['ACCEPT', 'lcCity']);
// TO is reserved, which is what keeps the greedy message from taking it and leaving the name behind.
check('the message may be left off', (({ message, to }) => [message, to])(first('INPUT TO lcName')), [null, 'lcName']);
check('a variable called input is untouched', first('input = 1').type, 'Assignment');
check('and a call to one', first('accept(1)').expression.type, 'CallExpression');

// --- the obsolete READ screen --------------------------------------------------------
check('bare READ is the command', first('READ'), { type: 'ReadStatement', cycle: false, options: null });
check('CYCLE is kept: it restarts the read rather than falling through', (({ cycle, options }) => [cycle, options])(first('READ CYCLE MODAL SAVE')),
	[true, 'MODAL SAVE']);
check('the rest of the tail stays raw', first('READ TIMEOUT 30 NOMOUSE').options, 'TIMEOUT 30 NOMOUSE');
check('READ EVENTS is still the event loop', first('READ EVENTS').type, 'ReadEventsStatement');
check('READ MENU TO is still the menu activation', first('READ MENU TO lnChoice').type, 'MenuToStatement');
// The call is the control that matters: an assignment is decided above ReadStatement, but a bare call has to get past it.
check('a call to a routine called read is untouched', first('read(1)').expression.type, 'CallExpression');
check('a variable of that name too', first('read = 1').type, 'Assignment');
check('and a property of one', first('read.enabled = .T.').target.type, 'MemberExpression');

// --- @ ... EDIT ------------------------------------------------------------------------
// The multi-line GET. Read without it, the verb fell through to the bare-coordinates form and its tail was reported as a statement of its own.
check('EDIT names the variable it edits', (({ verb, options }) => [verb, options])(first('@ 3, 2 EDIT m.cUsed SIZE 17, 75 NOEDIT')),
	['EDIT', 'SIZE 17, 75 NOEDIT']);
check('the operand is the same reference GET takes', first('@ 3, 2 EDIT m.cUsed SIZE 17, 75').target.type, 'MemberExpression');
check('the bare coordinates still only move the print head', first('@ 8, 1').verb, null);
check('and SAY is untouched', first('@ 2, 5 SAY "Name:"').verb, 'SAY');

// --- SHUTDOWN, which is not QUIT ---------------------------------------------------
// ON SHUTDOWN runs first, so it is a chance for code to run and the two cannot share a node.
check('SHUTDOWN is its own statement', first('SHUTDOWN').type, 'ShutdownStatement');
check('QUIT is still the exit', first('QUIT').type, 'ExitStatement');
check('a name that starts with it is untouched', first('shutdownhook(1)').expression.type, 'CallExpression');

// --- the Foxbase menu system -------------------------------------------------------
check('MENU BAR names the array and the count',
	(({ array, count }) => [array, count.value])(first('MENU BAR mBar, 5')), ['mBar', 5]);
check('MENU TO names the variable it writes', first('MENU TO lnChoice'), { type: 'MenuToStatement', to: 'lnChoice', read: false });
check('READ MENU TO is the same activation', (({ to, read }) => ({ to, read }))(first('READ MENU TO lnChoice')), { to: 'lnChoice', read: true });
check('READ EVENTS is untouched beside it', first('READ EVENTS').type, 'ReadEventsStatement');
check('a property called menu is untouched', first('menu.caption = "x"').target.type, 'MemberExpression');

// --- RELEASE's screen forms --------------------------------------------------------
// The only one of the four that misparsed: RELEASE read as far as the word, took MENU for the name of a variable to release and left the real name behind.
check('RELEASE MENU names the menu, not a variable called MENU',
	(({ scope, names, extended }) => ({ scope, names, extended }))(first('RELEASE MENU mMain EXTENDED')),
	{ scope: 'MENUS', names: ['mMain'], extended: true });
check('RELEASE POPUP is the same shape', (({ scope, names }) => ({ scope, names }))(first('RELEASE POPUP pFileMenu')),
	{ scope: 'POPUPS', names: ['pFileMenu'] });
check('the plural spelling reads the same way', first('RELEASE MENUS mMain').scope, 'MENUS');
check('and stands on its own', first('RELEASE MENUS').names, []);
check('RELEASE of variables is untouched', first('RELEASE loA, loB').names, ['loA', 'loB']);
check('so is RELEASE ALL EXTENDED', (({ scope, extended }) => ({ scope, extended }))(first('RELEASE ALL EXTENDED')), { scope: 'ALL', extended: true });

// --- USE's ORDER clause ------------------------------------------------------------
// OrderSpec was reachable only through `USE ... ?`, so every word of the clause fell to the connection-handle alternative and the last one won: the tag was read as a handle and nothing reported it.
check('USE ... ORDER TAG reads the tag', (({ order, connection }) => ({ order, connection }))(first('USE customer ORDER TAG custid')),
	{ order: { kind: 'TAG', tag: 'custid', of: null, direction: null }, connection: null });
check('the whole clause reads, and the option after it is no longer lost',
	(({ order, inTarget }) => [order.of, order.direction, inTarget.value])(first('USE customer ORDER TAG custid OF cust.cdx DESCENDING IN 2')),
	[{ type: 'Path', path: 'cust.cdx' }, 'DESCENDING', 2]);
check('an order number still reads', first('USE customer ORDER 1').order.kind, 'NUMBER');
check('a connection handle is still a handle', first('USE customer CONNSTRING lnHandle').connection,
	{ kind: 'CONNSTRING', value: 'lnHandle' });
check('USE with no ORDER leaves it null', first('USE customer ALIAS cust').order, null);

// --- a SET whose argument is a file ------------------------------------------------
// `SET HELP TO x.hlp` parsed, and read the file as member access on a variable called x: a read of a name that does not exist, reported by nothing.
check('the file reads as one name', first('SET HELP TO x.hlp').arguments, [{ type: 'Path', path: 'x.hlp' }]);
check('a bare name is still an expression, because it may be a variable holding the file',
	first('SET CLASSLIB TO mylib').arguments, [{ type: 'Identifier', name: 'mylib' }]);
check('an m. prefix is still a memvar, not a path', first('SET HELP TO m.cHelpFile').arguments[0].type, 'MemberExpression');
check('a parenthesised argument too', first('SET PROCEDURE TO (m.cLib) ADDITIVE').arguments[0].type, 'MemberExpression');
check('a mixed list reads each item for what it is',
	first('SET PROCEDURE TO lib1.prg, (m.cLib)').arguments.map((a: any) => a.type), ['Path', 'MemberExpression']);
check('a drive or a share reads as one path too', first('SET HELP TO \\\\srv\\share\\vfp.hlp').arguments, [{ type: 'Path', path: '\\\\srv\\share\\vfp.hlp' }]);
check('the clause after the file still reads', first('SET ALTERNATE TO out.txt ADDITIVE').additive, true);
check('a SET outside the file list keeps the expression reader',
	first('SET FILTER TO customer.state = "NY"').arguments[0].left.type, 'MemberExpression');

// --- the thirteen constructs that cost a whole file --------------------------------
// Each of these failed a block rather than a statement, so the error surfaced on an orphaned terminator far below and no rule ran on the file at all. A diagnostics fixture can only say the file is clean now; these say it is read correctly.

// A leading-dot member reference inside an expression, which was the largest single cause. The dot was read only where a statement started with it.
check('a member reference in a condition is the WITH target, not a name',
	first('IF .ChartsCount > 1\nx=1\nENDIF').test.left,
	{ type: 'WithMemberExpression', expression: { type: 'Identifier', name: 'ChartsCount' } });
check('the whole chain after the dot belongs to it',
	first('DO CASE\nCASE .Scale.Format = "@$"\nx=1\nENDCASE').cases[0].test.left.expression.type, 'MemberExpression');
check('a nested WITH may name a member of the enclosing one',
	first('WITH .Fields(1)\n.a = 1\nENDWITH').target.type, 'WithMemberExpression');
// The dangerous half: read as an expression, `.Width = 400` is the reference followed by `= 400`, which EvalStatement takes as a statement of its own -- two statements and no write recorded.
check('a member assignment under a nested block is one assignment, not two statements',
	first('WITH m.o\nIF m.b\n.Width = 400\nENDIF\nENDWITH').body.body[0].consequent.body,
	[{ type: 'Assignment', target: { type: 'WithMemberExpression', expression: { type: 'Identifier', name: 'Width' } }, expression: { type: 'NumberLiteral', value: 400, raw: '400', currency: false } }]);
check('.T. is still a boolean beside it', first('x = .T.').expression.type, 'BooleanLiteral');
check('.5 is still a number', first('x = .5').expression.type, 'NumberLiteral');
check('and .and. is still an operator', first('x = a .and. b').expression.operator, 'AND');
// A property on its own is no more a statement than a name on its own is, or the `.prg` left over from a clause the grammar stopped short of stops announcing itself.
check('a bare member reference is not a statement', types('.prg'), ['UnknownStatement']);

// PARAMETERS() the function, which the declaration keyword won.
check('PARAMETERS() is a call', first('IF PARAMETERS() < 3\nx=1\nENDIF').test.left.callee, { type: 'Identifier', name: 'PARAMETERS' });
check('the declaration is untouched beside it', first('PARAMETERS cPath, nKey'), { type: 'ParametersDeclaration', names: ['cPath', 'nKey'] });

// PROTECTED / HIDDEN methods were already read; the return type they carry without a parameter list was not, and `AS Logical` was left behind as a statement.
check('a method declares its return type without a parameter list',
	(({ name, access, isFunction, returnType }) => ({ name, access, isFunction, returnType }))(first('DEFINE CLASS X AS Session\nHIDDEN FUNCTION Rel AS Logical\nRETURN .t.\nENDFUNC\nENDDEFINE').body[0]),
	{ name: 'Rel', access: 'HIDDEN', isFunction: true, returnType: 'Logical' });

// A second CATCH, which is the shape of every retry loop.
check('every CATCH is read, with its own WHEN and body',
	first('TRY\nx=1\nCATCH TO m.e WHEN m.e.ErrorNo = 1707\ny=1\nCATCH TO m.e\nz=1\nENDTRY').catchClauses.map((c: any) => [c.to, c.when ? c.when.type : null, c.body.body.length]),
	[['m.e', 'BinaryExpression', 1], ['m.e', null, 1]]);
check('a TRY with none still says so', first('TRY\nx=1\nENDTRY').catchClauses, []);

// LOOP and CLASS as plain names. Neither is reserved in VFP, and both are metadata column names here.
check('LOOP is a variable where one is meant', first('DO WHILE loop\nx=1\nENDDO').test, { type: 'Identifier', name: 'loop' });
check('and an assignment to it is an assignment, not the loop-control word',
	first('DO WHILE .t.\nloop = .f.\nENDDO').body.body.map((s: any) => s.type), ['Assignment']);
check('LOOP on its own is still the loop-control word', first('DO WHILE .t.\nLOOP\nENDDO').body.body[0].type, 'ContinueStatement');
check('CLASS is a field name where one is meant', first('IF class == "frame"\nx=1\nENDIF').test.left, { type: 'Identifier', name: 'class' });
check('DEFINE CLASS is untouched beside it', first('DEFINE CLASS X AS Session\nENDDEFINE').base, 'Session');

// DO CASE with a trailing expression, which VFP ignores and the old code writes as documentation.
check('the expression after DO CASE is kept, so the read still reaches the symbol table',
	(({ subject, cases }) => [subject, cases.length])(first('do case m_emu\ncase m_emu = 1\nx=1\nendcase')),
	[{ type: 'Identifier', name: 'm_emu' }, 1]);
check('a bare DO CASE leaves it null', first('DO CASE\nCASE x = 1\ny=1\nENDCASE').subject, null);

// A second OTHERWISE. VFP runs the first; the rest are dead, and are kept apart from it rather than merged into it.
check('the first OTHERWISE is the branch and the rest are dead',
	(({ otherwise, deadOtherwise }) => [otherwise.body.length, deadOtherwise.map((o: any) => o.body.length)])(first('DO CASE\nCASE x=1\na=1\nOTHERWISE\nb=1\nOTHERWISE\nc=1\nENDCASE')),
	[1, [1]]);
check('one OTHERWISE leaves the dead list empty', first('DO CASE\nCASE x=1\na=1\nOTHERWISE\nb=1\nENDCASE').deadOtherwise, []);

// --- DO's target and its IN clause -------------------------------------------
// The target came back as a pair, the guard's result beside the node, so anything reading it saw an array. Nothing did until the workspace index needed the name.
check('DO names its target directly', first('DO foo').target, { type: 'Path', path: 'foo' });
check('a path target is a Path', first('DO S:\\apps\\thing.prg WITH 1').target, { type: 'Path', path: 'S:\\apps\\thing.prg' });
check('the parenthesised form is the expression itself', first('DO (lcName)').target, { type: 'Identifier', name: 'lcName' });
check('a macro target keeps its ampersand', first('DO &lcName').target, { type: 'Path', path: '&lcName' });
check('IN takes a bare name', first('DO foo IN bar').inSession, 'bar');
// IN read an identifier only, so the `.prg` of a file name was left behind as a statement of its own.
check('IN takes a file', first('DO foo IN lib\\bar.prg').inSession, { type: 'Path', path: 'lib\\bar.prg' });
check('nothing is left behind after a file in IN', types('DO foo IN bar.prg\nx = 1'), ['DoStatement', 'Assignment']);
check('IN and WITH in either order', (({ inSession, arguments: a }) => [inSession, a.length])(first('DO foo WITH 1, 2 IN bar.prg')), [{ type: 'Path', path: 'bar.prg' }, 2]);

// DEFINE CLASS with no AS: VFP defaults the parent to Custom.
check('a class with no parent still reads its body',
	(({ name, base, body }) => [name, base, body.map((s: any) => s.type)])(first('DEFINE CLASS X\nPROCEDURE Run\nENDPROC\nENDDEFINE')),
	['X', null, ['ProcedureStatement']]);

// COPY TO ... NEXT n. NEXT is also a loop terminator, so the leftover closed the enclosing DO WHILE.
check('the record count is read, and as an expression because the chunking loops write one',
	first('COPY TO (m.cDir) NEXT (m.nChunk)').scope,
	{ type: 'NEXT', count: { type: 'MemberExpression', object: { type: 'Identifier', name: 'm' }, property: { type: 'Identifier', name: 'nChunk' } } });
check('a COPY TO without one leaves it null', first('COPY TO out.dbf FIELDS a, b FOR x = 1').scope, null);
check('and the clauses after it still read', first('COPY TO out.dbf NEXT 5 FOR x = 1').for.type, 'BinaryExpression');

// A #IF fence that does not nest with the block structure around it. The preprocessor is a text pass, so VFP allows it; a block node cannot represent it, and the code's own blocks matter more than the fence.
check('the code blocks nest and the directives stand alone',
	body('IF m.n > 0\nx=1\n#IF R\ny=1\nENDIF\n#ENDIF').map((s: any) => s.type === 'IfStatement' ? ['IfStatement', s.consequent.body.map((c: any) => c.type)] : [s.type, s.directive]),
	[['IfStatement', ['Assignment', 'PreprocessorDirective', 'Assignment']], ['PreprocessorDirective', 'ENDIF']]);
check('a fence that does nest is still one block', first('#IF A\nx=1\n#ELSE\n#IF B\ny=1\n#ENDIF\n#ENDIF').alternate.body.map((s: any) => s.type), ['PreprocessorIfStatement']);

// --- the ledger's volume half ------------------------------------------------------
// Nine constructs that announced themselves rather than parsing, in the order they were measured over the corpus. Each is asserted on the field that was lost, and each has a control beside it: the shape that already worked has to keep working, because every one of these widens a rule that a name of its own could now be eaten by.

// @ with no verb, which just moves the print head: 147 uses, the most common @ there is.
check('@ with no clause is still the statement', (({ type, verb, options }) => ({ type, verb, options }))(first('@ PROW()+1, 1')),
	{ type: 'AtStatement', verb: null, options: null });
check('and its coordinates are read', (({ row, column }) => [row.type, column.value])(first('@ PROW()+1, 1')), ['BinaryExpression', 1]);
// The bare form has to end the line, or a verb the grammar has not learned reads as it and reports the gap on the tail instead of the statement.
check('a verb it has not learned is one gap, not a statement and a remainder', types('@ 3, 2 FILL TO 8, 40'), ['UnknownStatement']);
check('a verb it has is untouched', first('@ 2,5 SAY "Name:"').verb, 'SAY');

// The console commands.
check('EJECT', first('EJECT'), { type: 'EjectStatement', page: false });
check('EJECT PAGE is the in-band form', first('EJECT PAGE').page, true);
check('RETRY', first('RETRY').type, 'RetryStatement');
check('SHOW GETS', first('SHOW GETS'), { type: 'ShowGetsStatement', target: null, options: null });
check('SHOW GET names one, which is a read the symbol table needs',
	(({ target, options }) => [target, options])(first('SHOW GET m.answer DISABLE')),
	[{ type: 'MemberExpression', object: { type: 'Identifier', name: 'm' }, property: { type: 'Identifier', name: 'answer' } }, 'DISABLE']);
check('SHOW WINDOW is still the window command', first('SHOW WINDOW wOut').type, 'ScreenCommandStatement');

// AS on a declaration other than LOCAL, and the OF clause naming the file the type is defined in.
check('PRIVATE reads a type', first('PRIVATE cHeaderHTML as String'),
	{ type: 'PrivateDeclaration', name: 'cHeaderHTML', isArray: false, asType: 'String', ofClass: null });
check('PUBLIC does too', first('PUBLIC pnMailAccountID as Integer').asType, 'Integer');
check('the class library keeps its extension', first('LOCAL loSec as SECURITY_ATTRIBUTES OF oplocks.prg').ofClass, { type: 'Path', path: 'oplocks.prg' });
check('an untyped name is unchanged', first('PRIVATE cName'), { type: 'PrivateDeclaration', name: 'cName', isArray: false, asType: null, ofClass: null });

// DELETE's scope clause, which names a record number as often as a word.
check('DELETE RECORD reads the number as an expression, because the corpus writes RECNO()',
	first('DELETE RECORD RECNO("rec2inv") IN rec2inv').scope,
	{ type: 'RECORD', number: { type: 'CallExpression', callee: { type: 'Identifier', name: 'RECNO' }, arguments: [{ type: 'StringLiteral', value: 'rec2inv' }] } });
check('and the clause after it', first('DELETE RECORD RECNO("rec2inv") IN rec2inv').inTarget, 'rec2inv');
check('RECALL is the same shape', first('RECALL RECORD 5').scope, { type: 'RECORD', number: { type: 'NumberLiteral', value: 5, raw: '5', currency: false } });
check('a word scope is still a word', first('DELETE ALL').scope, 'ALL');
check('and DELETE FROM is still the SQL form', first('DELETE FROM cities').type, 'DeleteStatement');

// The clauses whose operand is an expression rather than a name.
check('FLUSH IN reads the work area', (({ inTarget, force }) => [inTarget, force])(first('FLUSH IN (m.inWorkArea) FORCE')),
	[{ type: 'MemberExpression', object: { type: 'Identifier', name: 'm' }, property: { type: 'Identifier', name: 'inWorkArea' } }, true]);
check('a bare FLUSH is unchanged', first('FLUSH'), { type: 'FlushStatement', inTarget: null, force: false });
check('SET ORDER TO a call is an expression, not an index file named IIF',
	first('SET ORDER TO IIF(TYPE("m.cOrder") = "U", "servsnum", m.cOrder)').selection.kind, 'EXPR');
check('a bare tag is still a file name', first('SET ORDER TO servsnum').selection, { kind: 'FILE', value: 'servsnum' });
check('MD over an expression', first('MD (ADDBS(m.m_tpath) + "temp")').target.type, 'BinaryExpression');
check('MD over a path is unchanged', first('MD datalog').target, { type: 'Path', path: 'datalog' });
check('and MD glued to the parenthesis is still a call', first('MD(1)').type, 'ExpressionStatement');
check('MODIFY COMMAND over an expression', (({ what, options }) => [what, options])(first('MODIFY COMMAND (m.cFile) NOWAIT')), ['COMMAND', '(m.cFile) NOWAIT']);

// The optional THEN, which the symbol table booked as a read of a variable named THEN.
check('THEN is punctuation, not the first statement of the branch',
	first('IF m.stat = 3 THEN\n? 1\nENDIF').consequent.body.map((s: any) => s.type), ['PrintStatement']);
check('the condition is unchanged beside it', first('IF m.stat = 3 THEN\n? 1\nENDIF').test.operator, '=');
// It is claimed on the condition's own line only, so the word on the line below is still a name.
check('a variable called THEN is still a variable', first('IF x\nTHEN = 1\nENDIF').consequent.body, [{ type: 'Assignment', target: { type: 'Identifier', name: 'THEN' }, expression: { type: 'NumberLiteral', value: 1, raw: '1', currency: false } }]);

// DIMEN, the four-letter abbreviation VFP allows on every command word.
check('DIMEN is DIMENSION', first('DIMEN invarr(1, 16)'),
	{ type: 'DimensionStatement', items: [{ name: 'invarr', rows: { type: 'NumberLiteral', value: 1, raw: '1', currency: false }, columns: { type: 'NumberLiteral', value: 16, raw: '16', currency: false }, asType: null }] });
check('the full spelling is unchanged', first('DIMENSION invarr(1, 16)').type, 'DimensionStatement');
check('and a longer word starting with it is still a name', first('DIMENSIONS = 1').type, 'Assignment');

// TOTAL takes its two halves in either order, and only the reverse of the documented one was read.
const totalTo = first('TOTAL TO totals ON custid FIELDS invbal FOR invbal > 0');
check('TOTAL TO ... ON ... reads both halves', [totalTo.target, totalTo.key.name], [{ type: 'Path', path: 'totals' }, 'custid']);
check('and the option tail behind them', [totalTo.fields, totalTo.for.operator], [{ kind: 'list', fields: ['invbal'] }, '>']);
check('TOTAL ON ... TO ... is unchanged',
	(({ target, key }) => [target, key.name])(first('TOTAL ON custid TO totals')), [{ type: 'Path', path: 'totals' }, 'custid']);

// The SETs whose argument ran past what the setting reader claimed, each leaving its tail to the catch-all.
check('a two-word setting is one command', (({ command, arguments: a }) => [command, a[0].value])(first('SET TOPIC ID TO 5')), ['TOPIC ID', 5]);
check('SET TOPIC TO on its own is still one word', first('SET TOPIC TO "x"').command, 'TOPIC');
check('the optional second word keeps the state beside it',
	(({ command, state }) => [command, state])(first('SET NOTIFY CURSOR OFF')), ['NOTIFY CURSOR', 'OFF']);
check('and without it the setting is unchanged', (({ command, state }) => [command, state])(first('SET NOTIFY OFF')), ['NOTIFY', 'OFF']);
check('SET WINDOW OF MEMO names the field and the window',
	first('SET WINDOW OF MEMO notes TO myform'), { type: 'SetWindowOfMemo', field: 'notes', window: 'myform' });
check('a bare TO restores the default window', first('SET WINDOW OF MEMO notes TO').window, null);

// REPLACE's scope, which the documentation puts after the field list. Only the leading ALL | REST was read.
check('RECORD after the field list is the scope',
	first('REPLACE invbal WITH 0 RECORD 5').scope, { type: 'RECORD', number: { type: 'NumberLiteral', value: 5, raw: '5', currency: false } });
// Unread, NEXT fell to the dangling-terminator rule and closed the enclosing FOR, so the loop lost every statement after it.
check('NEXT there is the scope, not the end of the loop',
	first('FOR i = 1 TO 3\nREPLACE invbal WITH 0 NEXT 3\n? 1\nNEXT').body.body.map((s: any) => s.type), ['ReplaceStatement', 'PrintStatement']);
check('the clauses behind it still read',
	(({ forCondition, inTarget, noOptimize }) => [forCondition.operator, inTarget, noOptimize])(first('REPLACE invbal WITH 0 REST FOR invbal > 0 IN invinfo NOOPTIMIZE')), ['>', 'invinfo', true]);
check('the leading ALL is unchanged', first('REPLACE ALL invbal WITH 0').scope, 'ALL');
check('and a field whose name begins with one is still a field', first('REPLACE allowance WITH 0').fields[0].field, 'allowance');

// A column's nullability, which was read by the shape of what the alternative returned rather than by which one matched, so both spellings came back as NULL.
check('NOT NULL is NOT NULL', first('CREATE TABLE t (invbal N(12, 2) NOT NULL)').columns[0].nullability, 'NOT NULL');
check('NULL is still NULL', first('CREATE TABLE t (invbal N(12, 2) NULL)').columns[0].nullability, 'NULL');
check('and a column with neither has none', first('CREATE TABLE t (invbal N(12, 2))').columns[0].nullability, null);

// ALTER TABLE's tail, which used to be kept as source. The clause list is what a rule reads; `options` is the fallback, and a null there is the assertion that the tail was understood.
const alterActions = (src: any) => first(src).clauses.map((c: any) => c.action);
// The tail stopped at the physical line, so everything after the semicolon was left behind as a statement of its own and reported as a gap.
check('a clause on a continuation line belongs to the statement',
	types('ALTER TABLE items ADD COLUMN billcode C(6) ;\n ADD COLUMN ledacct C(8)'), ['AlterTableStatement']);
check('and both clauses are read', alterActions('ALTER TABLE items ADD COLUMN billcode C(6), ADD COLUMN ledacct C(8)'), ['ADD COLUMN', 'ADD COLUMN']);
check('the added column is a column definition, which is what names a field',
	first('ALTER TABLE items ADD COLUMN billcode C(6)').clauses[0].column,
	{ type: 'ColumnDefinition', name: 'billcode', fieldType: 'C', size: { width: { type: 'NumberLiteral', value: 6, raw: '6', currency: false }, precision: null }, nullability: null, check: null, autoinc: null, default: null, key: null, references: null, nocptrans: false });
check('a change that restates the type is a column definition too',
	(({ action, column }) => [action, column.fieldType, column.nullability])(first('ALTER TABLE items ALTER COLUMN ledacct C(12) NOT NULL').clauses[0]), ['ALTER COLUMN', 'C', 'NOT NULL']);
// The form that carries no type: SET would otherwise read as the field type and DEFAULT as the column's own.
check('and one that does not is a list of changes to a named column',
	(({ action, name, modifiers }) => [action, name, modifiers.map((m: any) => m.kind)])(first('ALTER TABLE items ALTER COLUMN billcode SET DEFAULT m.cCode DROP CHECK').clauses[0]),
	['ALTER COLUMN', 'billcode', ['SET DEFAULT', 'DROP CHECK']]);
check('a DEFAULT expression is kept, so the variable it reads is one the symbol table sees',
	first('ALTER TABLE items ALTER COLUMN billcode SET DEFAULT m.cCode').clauses[0].modifiers[0].expression.property.name, 'cCode');
check('the constraint forms read as constraints',
	first('ALTER TABLE items ADD FOREIGN KEY ledacct TAG ledacct REFERENCES ledger TAG acct').clauses[0].constraint,
	{ type: 'TableConstraint', kind: 'FOREIGN KEY', expression: { type: 'Identifier', name: 'ledacct' }, tag: 'ledacct', nodup: false, collate: null, for: null, references: { table: 'ledger', tag: 'acct' } });
check('FOR filters the tag it indexes', first('ALTER TABLE items ADD UNIQUE billcode TAG billcode FOR qty > 0').clauses[0].constraint.for.operator, '>');
check('a dropped constraint names which one and its tag',
	(({ action, kind, tag, save }) => [action, kind, tag, save])(first('ALTER TABLE items DROP FOREIGN KEY TAG ledacct SAVE').clauses[0]), ['DROP CONSTRAINT', 'FOREIGN KEY', 'ledacct', true]);
check('DROP CHECK is the table check, not a column called CHECK', alterActions('ALTER TABLE items DROP CHECK'), ['DROP CHECK']);
check('a column whose name begins with one of those keywords is still a column',
	(({ action, name }) => [action, name])(first('ALTER TABLE items DROP COLUMN check_flag').clauses[0]), ['DROP COLUMN', 'check_flag']);
check('clauses stand side by side without commas', alterActions('ALTER TABLE items DROP COLUMN postdate ADD PRIMARY KEY itemno TAG itemno'), ['DROP COLUMN', 'ADD CONSTRAINT']);
check('RENAME names both halves',
	(({ name, newName }) => [name, newName])(first('ALTER TABLE items RENAME COLUMN billcode TO bill_code').clauses[0]), ['billcode', 'bill_code']);
// The floor the change has to keep: a clause the list cannot read costs the tail, not the statement.
check('a tail with an unreadable clause falls back to source',
	(({ clauses, options }) => [clauses, options])(first('ALTER TABLE items ADD COLUMN whatever C(6) SOMETHING ODD')), [[], 'ADD COLUMN whatever C(6) SOMETHING ODD']);
check('and the fallback crosses a continuation as well',
	first('ALTER TABLE items SOMETHING ODD ;\n AND MORE').options, 'SOMETHING ODD AND MORE');

// CAST's width, read as a NumberLiteral. The widths come from the schema at runtime, so the query pads to whatever the target column declares -- and a variable there cost the whole SELECT its parse, not the cast alone.
const src = 'SELECT CAST(custinfo.bfname AS C(m.nFnameLen)) AS fname FROM custinfo WHERE !EMPTY(custinfo.bemail) INTO CURSOR curContacts';
const sel = first(src);
check('a computed width no longer costs the query its parse', types(src), ['SelectStatement']);
check('and its destination is visible to the rules that ask', (({ kind, name }) => [kind, name.name])(sel.destination), ['CURSOR', 'curContacts']);
check('the width is the expression it is written as', sel.list[0].expression.to, { kind: 'typed', name: 'C', size: { type: 'MemberExpression', object: { type: 'Identifier', name: 'm' }, property: { type: 'Identifier', name: 'nFnameLen' } }, scale: null });
check('a computed width may be any expression', first('x = CAST(a AS C(LEN(m.cKey) + 2))').expression.to.size.type, 'BinaryExpression');
check('a literal width is unchanged',
	first('x = CAST(a AS N(10, 2))').expression.to, { kind: 'typed', name: 'N', size: { type: 'NumberLiteral', value: 10, raw: '10', currency: false }, scale: { type: 'NumberLiteral', value: 2, raw: '2', currency: false } });
check('and a type with no width is still simple', first('x = CAST(a AS M)').expression.to, { kind: 'simple', name: 'M' });

// DO FORM's target, read as an identifier. A form is a file, so the name stopped at the first character a name cannot hold -- and the statement then looked read while naming the wrong form.
check('a hyphen is part of the form name', first('DO FORM start-up_code_mod').target, { type: 'Path', path: 'start-up_code_mod' });
check('and so is an extension', first('DO FORM myform.scx').target, { type: 'Path', path: 'myform.scx' });
check('a parenthesised name is the expression that holds it', first('DO FORM (m.cFormName)').target.property.name, 'cFormName');
check('a plain name is unchanged', first('DO FORM testform').target, 'testform');
check('a quoted one is still a string', first('DO FORM "myform"').target, { type: 'StringLiteral', value: 'myform' });
check('and the clauses behind the name still read',
	(({ target, arguments: a, to }) => [target, a.map((x: any) => x.name), to])(first('DO FORM testform WITH param1, param2 TO varname')), ['testform', ['param1', 'param2'], 'varname']);

// --- a routine named after a command word -------------------------------------
// The name position is a name, not a command word. `PROCEDURE declare` and `PROCEDURE use` are how the Windows-API wrappers name their setup and teardown, and rejecting the header inside DEFINE CLASS orphaned its ENDPROC and the ENDDEFINE with it, so the class stopped being indexed at all -- promoting such a file to tier 2 *removed* the symbols the header scan had found.
const routine = (src: string) => { const r = first(src); return `${r.type}:${r.name}${r.isFunction ? '!' : ''}`; };
const words = ['declare', 'use', 'select', 'replace', 'store', 'error'];
check('a command word can name a procedure',
	words.map(n => routine(`PROCEDURE ${n}\nRETURN\nENDPROC`)), words.map(n => `ProcedureStatement:${n}`));
check('and a function, parameters and all', routine('FUNCTION use(tcAlias)\nRETURN .t.\nENDFUNC'), 'ProcedureStatement:use!');
check('the access word in front of one still reads', first('PROTECTED PROCEDURE declare\nENDPROC').access, 'PROTECTED');
check('a class keeps its methods, and its ENDDEFINE',
	first('DEFINE CLASS Crypto AS Session\nPROTECTED PROCEDURE declare\nENDPROC\nPROCEDURE use\nENDPROC\nENDDEFINE').body.map((m: { name: string }) => m.name),
	['declare', 'use']);
// The words are names only where a name is expected. Everywhere else they open their own statement, which is what a permissive name position must not cost.
check('the same words still open their own statements',
	types('USE customer\nDECLARE INTEGER Sleep IN win32api\nSELECT 0\nSTORE 0 TO x'),
	['UseStatement', 'DeclareStatement', 'SelectStatement', 'StoreStatement']);

// --- a string literal ends at the line break ----------------------------------
// FoxPro's tokenizer ends a literal at the newline. Letting one run on meant a single stray quote swallowed every line up to the next quote anywhere in the file, and the swallowed lines produced no diagnostic of any kind -- the one failure in the corpus a user could not see.
check('a literal does not swallow the line below it', types('? "abc\n? 1'), ['PrintStatement', 'PrintStatement']);
check('it ends where the line does, and says so', first('? "abc\n? 1').arguments[0], { type: 'StringLiteral', value: 'abc', unterminated: true });
check('an apostrophe is no different', first("? 'abc\n? 1").arguments[0].unterminated, true);
check('a closed literal carries no such flag', first('? "abc"').arguments[0], { type: 'StringLiteral', value: 'abc' });
check('a stray quote after a terminator stands as a statement of its own', types("FOR i = 1 TO 3\nENDFOR'\n? 1"), ['ForStatement', 'ExpressionStatement', 'PrintStatement']);
// A bracket literal is not recovered, because `[` also opens a subscript: the line it is on is unreadable, and the point is that the lines below it are not.
check('a bracket literal stops at the line end too', types('x = [abc\ny = 1'), ['UnknownStatement', 'Assignment']);

report('Parse checks');
