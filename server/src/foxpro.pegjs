// FoxPro (VFP/VFPA) grammar for PEG.js
{
  // Helper utilities accessible inside actions
  function node(type, props) {
    return Object.assign({ type, location: location() }, props);
  }
  function flatten(list) {
    const out = [];
    for (const item of list) {
      if (Array.isArray(item)) out.push(...item); else if (item !== null) out.push(item);
    }
    return out;
  }
}

// -----------------------------
// Top Level
// -----------------------------
Start "start of program"
  = __ statements:SourceElements? __
  { return node("Program", { body: statements ? statements.body : [] }); }

SourceElements "statement list"
  = head:Statement tail:(__ Statement)* {
      const statements = [head, ...tail.map(t => t[1])].filter(s => s !== null);
      return node("BlockStatement", { body: statements });
    }

// A Statement returns either a single AST node or an Array of nodes (e.g. multiple LOCAL vars)
Statement "statement"
  = s:(LocalStatement
  / PrivateStatement
  / PublicStatement
  / DimensionStatement
  / DeclareStatement
  / TryStatement
  / DefineClass
  / LParameters
  / PrintStatement
  / WaitWindowStatement
  / UseStatement
  / AppendStatement
  / CalculateStatement
  / AggregateStatement
  / CopyStatement
  / EraseStatement
  / SetStatement
  / OnStatement
  / TextBlockStatement
  / ThrowStatement
  / AtStatement
  / PreprocessorStatement
  / IterationStatement
  / ExitStatement
  / ContinueStatement
  / CreateStatement
  / IndexOnStatement
  / InsertStatement
  / SelectStatement
  / UpdateStatement
  / DeleteStatement
  / ZapStatement
  / GoToStatement
  / SkipStatement
  / UnlockStatement
  / AssignmentStatement
  / RecallStatement
  / DoCaseStatement
  / DoFormStatement
  / DoStatement
  / ProcedureStatement
  / LocateStatement
  / ScanStatement
  / ReturnStatement
  / StoreStatement
  / ReplaceStatement
  / ScatterStatement
  / GatherStatement
  / IfStatement
  / EvalStatement
  / WithStatement
  / BrowseStatement
  / ClearStatement
  / CloseStatement
  / ReleaseStatement
  / PackStatement
  / SeekStatement
  / SuspendStatement
  / ResumeStatement
  / KeyboardStatement
  / ReportFormStatement
  / SortStatement
  / ListStatement
  / FlushStatement
  / ReindexStatement
  / DirectoryStatement
  / ContinueLocateStatement
  / NoDefaultStatement
  / PushPopStatement
  / ExternalStatement
  / ModifyStatement
  / AlterTableStatement
  / RunStatement
  / ExpressionStatement
  / UnknownStatement
  ) { return s; }

// -----------------------------
// Declarations
// -----------------------------
// LOCAL Var1 [ AS type [ OF ClassLib ] ] | [ ArrayName1( nRows1 [, nColumns1 ] ) [ AS type [ OF ClassLib ] ] ]
//   [, Var2 [ AS type [ OF ClassLib ] ] | [, ArrayName2( nRows2 [, nColumns2 ] ) [ AS type [ OF ClassLib ] ] ]
// LOCAL [ ARRAY ] ArrayName1( nRows1 [, nColumns1 ] ) [ AS type [OF ClassLib ] ]
//   [, ArrayName2( nRows2 [, nColumns2 ] ) [ AS type [ OF ClassLib ] ] ]
LocalStatement
  = "LOCAL"i WB _ "ARRAY"i _ arrs:ArrayDeclList { return arrs; }
  / "LOCAL"i WB _ entries:LocalEntryList {  return entries; }

// A comma-separated list of local entries (variables or arrays)
LocalEntryList
  = head:LocalEntry tail:(_ "," _ LocalEntry)* { return [head, ...tail.map(t => t[3])]; }

// An entry is either a variable declaration or an array declaration
LocalEntry
  = ArrayDecl
  / VarDecl

// Variable declaration: name [ AS type [ OF ClassLib ] ]
VarDecl
  = name:ParameterName _ asPart:(_ "AS"i __ t:Identifier _ ofPart:(_ "OF"i _ cl:Identifier { return cl; })? { return { type: t, of: ofPart }; })? {
      return node("LocalDeclaration", { name, asType: asPart ? asPart.type : null, ofClass: asPart ? asPart.of : null });
    }

// Array declaration: ArrayName( nRows [, nColumns ] ) [ AS type [ OF ClassLib ] ]
ArrayDims
  = "(" _ rows:Expression _ cols:(_ "," _ c:Expression { return c; })? _ ")" { return { rows, columns: cols }; }
  / "[" _ rows:Expression _ cols:(_ "," _ c:Expression { return c; })? _ "]" { return { rows, columns: cols }; }

ArrayDecl
  = name:Identifier _ dims:ArrayDims _ asPart:(_ "AS"i __ t:Identifier _ ofPart:(_ "OF"i _ cl:Identifier { return cl; })? { return { type: t, of: ofPart }; })? {
      return node("LocalArrayDeclaration", { name, rows: dims.rows, columns: dims.columns, asType: asPart ? asPart.type : null, ofClass: asPart ? asPart.of : null });
    }

ArrayDeclList
  = head:ArrayDecl tail:(_ "," _ ArrayDecl)* { return [head, ...tail.map(t => t[3])]; }

PrivateStatement
  = "PRIVATE"i WB _ decl:(
      "ALL"i _ "LIKE"i _ p:(StringLiteral / Pattern) {
        const pat = (typeof p === 'string') ? p : (p && p.value ? p.value : p);
        return node("PrivateAllLike", { pattern: pat });
      }
      / "ALL"i { return node("PrivateAll", {}); }
      / "ARRAY"i WB _ arrs:ArrayDeclList { return arrs.map(a => node("PrivateDeclaration", { name: a.name, isArray: true })); }
      / vars:IdentifierList{ return vars.map(v => node("PrivateDeclaration", { name: v, isArray: false })); }
      / _ { return node("PrivateDirective", {}); }
  ) { return decl; }

PublicStatement
  = "PUBLIC"i WB _ "ARRAY"i WB _ arrs:ArrayDeclList {
      return arrs.map(a => node("PublicDeclaration", { name: a.name, isArray: true }));
    }
  / "PUBLIC"i WB _ vars:IdentifierList {
      return vars.map(v => node("PublicDeclaration", { name: v, isArray: false }));
    }

// The singular spellings are the older ones and the app still uses them. These declare a routine's inputs, so missing one leaves the symbol table without the parameters of the whole routine. Longest first: PARAMETERS has to be tried before PARAMETER, and PARAMETER before PARAM.
LParameters
  = ("LPARAMETERS"i / "LPARAMETER"i / "PARAMETERS"i / "PARAMETER"i / "PARAM"i) WB _ vars:ParameterList {
      return node("ParametersDeclaration", { names: vars });
    }

// DIMENSION ArrayName(nRows [, nColumns]) [AS cType] [, ArrayName2(...)] ...
DimensionStatement
  = "DIMENSION"i WB __ first:DimensionItem tail:(_ "," _ DimensionItem)* {
      const items = [first, ...tail.map(t => t[3])];
      return node("DimensionStatement", { items });
    }

DimensionItem
  = name:Identifier _ dims:ArrayDims _ asPart:("AS"i __ t:IdentifierOrString)? {
      return { name, rows: dims.rows, columns: dims.columns, asType: asPart ? asPart[2] : null };
    }

IdentifierList
  = head:ParameterName tail:(_ "," _ ParameterName)* {
      return [head, ...tail.map(t => t[3])];
    }

// Parameter names can be dotted (e.g. m.UserName). Allow an optional leading @ or & immediately before the identifier (e.g. @var, &var or @m.User).
// Capture as a single name string (including the prefix when present).
ParameterName
  = name:$([@&]? [a-zA-Z_][a-zA-Z0-9_]* (("." / "->") [a-zA-Z_][a-zA-Z0-9_]*)*) { return name; }

ParameterList
  = head:ParameterName tail:(_ "," _ ParameterName)* { return [head, ...tail.map(t => t[3])]; }

// Procedure-style parameters with optional type:  name [ AS Type ]
ProcedureParam
  = name:ParameterName _ asPart:(_ "AS"i __ t:IdentifierOrString)? { return { name, type: asPart ? asPart[3] : null }; }

ProcedureParamList
  = head:ProcedureParam tail:(_ "," _ ProcedureParam)* { return [head, ...tail.map(t => t[3])]; }

// Unquoted pattern token (e.g. TestRelease*)
Pattern
  = p:$([a-zA-Z0-9_.*]+) { return p; }

// -----------------------------
// Assignment & Expressions
// -----------------------------

// Allow dotted member chains (e.g. m.test) on the left-hand side of an assignment
LValue
  = head:Identifier tail:(
      ("." / "->") _ prop:MemberName { return { type: 'member', prop: prop }; }
    / "[" _ idxs:ExpressionList _ "]" { return { type: 'index', indexes: idxs }; }
    / "(" _ idxs:ExpressionList _ ")" { return { type: 'index', indexes: idxs }; }
    )* {
      let expr = node("Identifier", { name: head });
      for (const t of tail) {
        if (t.type === 'member') {
          const propName = t.prop;
          expr = node("MemberExpression", { object: expr, property: node("Identifier", { name: propName }) });
        } else if (t.type === 'index') {
          expr = node('ArrayIndexExpression', { object: expr, indexes: t.indexes });
        }
      }
      return expr;
    }

AssignmentStatement
  = id:LValue __ "=" __ expr:Expression {
      return node("Assignment", { target: id, expression: expr });
    }

// Shorthand print statement: ? <expression> or PRINT <expression>
PrintStatement // todo: Wait window probably should be separate
  = ("?" / ("PRINT"i WB)) _ args:ExpressionList {
      return node("PrintStatement", { arguments: args });
    }

// WAIT [cMessageText] [TO VarName] [WINDOW [AT nRow, nColumn]] [NOWAIT]
//    [CLEAR | NOCLEAR] [TIMEOUT nSeconds]
// The flags may sit on either side of the message, and every call site in the app writes them after it.
// The leading list is greedy, so a flag is never mistaken for the message.
WaitWindowStatement
  = "WAIT WINDOW"i WB lead:(_ WaitOption)* msg:(_ e:Expression { return e; })? trail:(_ WaitOption)* {
      const o = { nowait: false, noclear: false, clear: false, timeout: null };
      for (const part of [...lead, ...trail].map(t => t[1])) {
        switch (part.kind) {
          case 'NOWAIT': o.nowait = true; break;
          case 'NOCLEAR': o.noclear = true; break;
          case 'CLEAR': o.clear = true; break;
          case 'TIMEOUT': o.timeout = part.value; break;
        }
      }
      return node("WaitWindowStatement", { nowait: o.nowait, noclear: o.noclear, clear: o.clear, timeout: o.timeout, message: msg });
    }

WaitOption
  = "NOWAIT"i WB { return { kind: 'NOWAIT' }; }
  / "NOCLEAR"i WB { return { kind: 'NOCLEAR' }; }
  / "CLEAR"i WB { return { kind: 'CLEAR' }; }
  / "TIMEOUT"i WB _ n:Expression { return { kind: 'TIMEOUT', value: n }; }

// USE [[DatabaseName!] TableName | SQLViewName | ?]
//  [IN nWorkArea | cTableAlias] [ONLINE] [ADMIN] [AGAIN]
//  [NOREQUERY [nDataSessionNumber]] [NODATA] 
//  [INDEX IndexFileList | ? [ORDER [nIndexNumber | IDXFileName 
//  | [TAG] TagName [OF CDXFileName] [ASCENDING | DESCENDING]]]]
//  [ALIAS cTableAlias] [EXCLUSIVE] [SHARED] [NOUPDATE] 
//  [CONNSTRING cConnectionString | nStatementHandle ]
UseStatement
  = "USE"i WB _
    tgt:(!UseOptionWord t:UseTarget { return t; })? _
    parts:(UseOption _)*
    {
      const opts = { inTarget:null, online:false, admin:false, again:false, norequery:false, dataSession:null, nodata:false, index:null, alias:null, exclusive:false, shared:false, noUpdate:false, connection:null };
      for (const p of parts.map(t => t[0])) {
        switch (p.kind) {
          case 'IN': opts.inTarget = p.value; break;
          case 'ONLINE': opts.online = true; break;
          case 'ADMIN': opts.admin = true; break;
          case 'AGAIN': opts.again = true; break;
          case 'NOREQUERY': opts.norequery = true; opts.dataSession = (p.value === true) ? null : p.value; break;
          case 'NODATA': opts.nodata = true; break;
          case 'INDEX': opts.index = p.value; break;
          case 'ALIAS': opts.alias = p.value; break;
          case 'EXCLUSIVE': opts.exclusive = true; break;
          case 'SHARED': opts.shared = true; break;
          case 'NOUPDATE': opts.noUpdate = true; break;
          case 'CONN': opts.connection = p.value; break;
        }
      }
      return node("UseStatement", {
        target: tgt || null,
        inTarget: opts.inTarget,
        online: opts.online,
        admin: opts.admin,
        again: opts.again,
        norequery: opts.norequery,
        dataSession: opts.dataSession,
        nodata: opts.nodata,
        index: opts.index,
        alias: opts.alias,
        exclusive: opts.exclusive,
        shared: opts.shared,
        noUpdate: opts.noUpdate,
        connection: opts.connection
      });
    }

// The words that can only be options, never the table.
UseOptionWord
  = ("IN"i / "ONLINE"i / "ADMIN"i / "AGAIN"i / "NOREQUERY"i / "NODATA"i / "INDEX"i / "ALIAS"i
    / "EXCLUSIVE"i / "SHARED"i / "NOUPDATE"i / "CONNSTRING"i) WB

UseTarget
  = "?" { return { kind: 'PROMPT' }; }
  / name:QualifiedTable { return { kind: 'TABLE', name }; }
  / expr:PathOrExpression { return { kind: 'EXPR', value: expr }; }

UseOption
  = inC:InClause { return { kind: 'IN', value: inC }; }
  / "ONLINE"i { return { kind: 'ONLINE', value: true }; }
  / "ADMIN"i { return { kind: 'ADMIN', value: true }; }
  / "AGAIN"i { return { kind: 'AGAIN', value: true }; }
  / "NOREQUERY"i _ ds:Expression? { return { kind: 'NOREQUERY', value: ds || true }; }
  / "NODATA"i { return { kind: 'NODATA', value: true }; }
  / idx:UseIndexPart { return { kind: 'INDEX', value: idx }; }
  / "ALIAS"i __ a:AliasRef { return { kind: 'ALIAS', value: a }; }
  / "EXCLUSIVE"i { return { kind: 'EXCLUSIVE', value: true }; }
  / "SHARED"i { return { kind: 'SHARED', value: true }; }
  / "NOUPDATE"i { return { kind: 'NOUPDATE', value: true }; }
  / conn:UseConnPart { return { kind: 'CONN', value: conn }; }

UseIndexPart
  = "INDEX"i __ files:IndexFileList { return { mode: 'INDEX', files }; }
  / "?" _ ord:OrderSpec? { return { mode: 'PROMPT', order: ord || null }; }

IndexFileList
  = head:(IdentifierOrString / UnquotedPath) tail:(_ "," _ (IdentifierOrString / UnquotedPath))* {
      return [head, ...tail.map(t => t[3])];
    }

OrderSpec
  = "ORDER"i __ sel:(
      &("TAG"i WB) tag:TagSpec { return { kind: 'TAG', ...tag }; }
      / n:Expression { return { kind: 'NUMBER', value: n }; }
      / f:(IdentifierOrString / UnquotedPath) { return { kind: 'FILE', value: f }; }
      / tag:TagSpec { return { kind: 'TAG', ...tag }; }
    ) { return sel; }

TagSpec
  = ("TAG"i _)? t:Identifier _ 
    ofPart:("OF"i __ cdx:(IdentifierOrString / UnquotedPath))? _ 
    dir:("ASCENDING"i / "DESCENDING"i / "ASC"i / "DESC"i)? {
      return { tag: t, of: ofPart ? ofPart[2] : null, direction: dir ? (typeof dir === 'string' ? dir.toUpperCase() : dir) : null };
    }

UseConnPart
  = "CONNSTRING"i __ cs:(StringLiteral / Identifier) { return { kind: 'CONNSTRING', value: cs }; }
  / h:(NumberLiteral / Identifier) { return { kind: 'HANDLE', value: h }; }

// Preprocessor directives
PreprocessorStatement
  = IncludeStatement
  / DefineStatement
  / PreprocessorIfStatement

// ON ERROR | ESCAPE | SHUTDOWN | READERROR | APLABOUT | PAGE | KEY [LABEL cLabel] [command]
// The command is parsed as a statement, and `_` does not cross a newline, so a bare ON ERROR that clears the handler cannot swallow the line below it.
OnStatement
  = "ON"i WB _ "KEY"i WB _ "LABEL"i WB _ label:$([^ \t\r\n]+) _ cmd:Statement? {
      return node("OnStatement", { event: 'KEY LABEL', label, atLine: null, command: cmd || null });
    }
  / "ON"i WB _ "KEY"i WB _ cmd:Statement? {
      return node("OnStatement", { event: 'KEY', label: null, atLine: null, command: cmd || null });
    }
  / "ON"i WB _ "PAGE"i WB _ at:("AT"i WB _ "LINE"i WB _ n:Expression { return n; })? _ cmd:Statement? {
      return node("OnStatement", { event: 'PAGE', label: null, atLine: at || null, command: cmd || null });
    }
  / "ON"i WB _ ev:("ERROR"i / "ESCAPE"i / "SHUTDOWN"i / "READERROR"i / "APLABOUT"i) WB _ cmd:Statement? {
      return node("OnStatement", { event: ev.toUpperCase(), label: null, atLine: null, command: cmd || null });
    }

// TEXT [TO VarName [ADDITIVE]] [TEXTMERGE] [NOSHOW] [FLAGS nFlags] [PRETEXT nPretext] ... ENDTEXT
// The body is raw output text, not code, so TextLine reads it verbatim up to the ENDTEXT line.
TextBlockStatement "text block"
  = "TEXT"i WB opts:(_ TextOption)* _ PartialLineComment? LineTerminatorSequence
    lines:TextLine*
    _ "ENDTEXT"i WB {
      const o = { to: null, additive: false, textmerge: false, noshow: false, flags: null, pretext: null };
      for (const part of opts.map(t => t[1])) {
        switch (part.kind) {
          case 'TO': o.to = part.value; break;
          case 'ADDITIVE': o.additive = true; break;
          case 'TEXTMERGE': o.textmerge = true; break;
          case 'NOSHOW': o.noshow = true; break;
          case 'FLAGS': o.flags = part.value; break;
          case 'PRETEXT': o.pretext = part.value; break;
        }
      }
      return node("TextBlockStatement", {
        to: o.to,
        additive: o.additive,
        textmerge: o.textmerge,
        noshow: o.noshow,
        flags: o.flags,
        pretext: o.pretext,
        content: lines.join('\n')
      });
    }

// ADDITIVE is its own option rather than a tail of TO: the option list is order-free, and the app writes `TO m.x NOSHOW ADDITIVE` as often as the adjacent form. Requiring adjacency stopped the list at NOSHOW, failed the whole TEXT rule, and left the body to be read as code -- costing the file its parse rather than one statement.
TextOption
  = "TO"i WB _ v:ParameterName { return { kind: 'TO', value: v }; }
  / "ADDITIVE"i WB { return { kind: 'ADDITIVE' }; }
  / "TEXTMERGE"i WB { return { kind: 'TEXTMERGE' }; }
  / "NOSHOW"i WB { return { kind: 'NOSHOW' }; }
  / "FLAGS"i WB _ n:Expression { return { kind: 'FLAGS', value: n }; }
  / "PRETEXT"i WB _ n:Expression { return { kind: 'PRETEXT', value: n }; }

TextLine
  = !(_ "ENDTEXT"i WB) line:$((!LineTerminator .)*) LineTerminatorSequence { return line; }

// THROW [eUserValue], which the grammar previously accepted only between a CATCH body and FINALLY.
ThrowStatement
  = "THROW"i WB _ e:Expression? {
      return node("ThrowStatement", { argument: e || null });
    }

// @ nRow, nColumn SAY | GET | TO | CLEAR, the legacy screen commands.
// Their option tails (PICTURE, FUNCTION, SIZE, FONT, VALID, WHEN, COLOR ...) are long and vary by verb, so they are kept as raw source: recognising the statement is what stops the false positive.
AtStatement "screen coordinate statement"
  = "@" _ row:Expression _ "," _ col:Expression _ body:AtBody {
      return node("AtStatement", {
        row,
        column: col,
        verb: body.verb,
        expression: body.expression,
        target: body.target,
        endRow: body.endRow,
        endColumn: body.endColumn,
        options: body.options
      });
    }

AtBody
  = "SAY"i WB _ e:Expression o:RawOptions { return { verb: 'SAY', expression: e, target: null, endRow: null, endColumn: null, options: o }; }
  / "GET"i WB _ v:LValue o:RawOptions { return { verb: 'GET', expression: null, target: v, endRow: null, endColumn: null, options: o }; }
  / "TO"i WB _ r:Expression _ "," _ c:Expression o:RawOptions { return { verb: 'TO', expression: null, target: null, endRow: r, endColumn: c, options: o }; }
  / "CLEAR"i WB o:RawOptions { return { verb: 'CLEAR', expression: null, target: null, endRow: null, endColumn: null, options: o }; }

// The rest of a command line, verbatim, for option tails the grammar does not model yet.
RawOptions
  = _ o:$((!(LineTerminator / PartialLineComment) .)*) { return o.trim() || null; }

IncludeStatement
  = "#include"i _ path:(StringLiteral / UnquotedPath) {
      return node("IncludeStatement", { path });
    }

DefineStatement
  = "#define"i _ name:Identifier _ value:$((!LineTerminator .)*) {
      return node("DefineStatement", { name, value: value.trim() });
    }

// Preprocessor if/else/endif
PreprocessorIfStatement
  = start:"#if"i rest:$((!"#endif"i .)*) end:"#endif"i {
      // capture raw preprocessor block (including any #elif/#else lines)
      return node("PreprocessorIfStatement", { raw: (start + rest + end).trim() });
    }

// DEFINE CLASS ClassName AS ParentClass [OF ClassLibrary] [OLEPUBLIC]
DefineClass
  = "DEFINE CLASS"i WB _ name:Identifier _ "AS"i _ base:Identifier
    ofPart:(_ "OF"i WB _ lib:(StringLiteral / UnquotedPath) { return lib; })?
    olePublic:(_ "OLEPUBLIC"i WB)? __
    statements:(Statement __)*
    "ENDDEFINE"i {
      return node("DefineClass", { name, base: base || null, ofClass: ofPart || null, olePublic: !!olePublic, body: flatten(statements.map(s => s[0])) });
    }

// DECLARE [cFunctionType] FunctionName IN LibraryName [AS AliasName] [cParamType1 [@] ParamName1, cParamType2 [@] ParamName2, ...]
DeclareStatement
  = "DECLARE"i WB _
    cFunctionType:("SHORT"i / "LONG"i / "INTEGER"i / "SINGLE"i / "DOUBLE"i / "STRING"i / "OBJECT"i)? _
    functionName:Identifier _ 
    "IN"i _ 
    libraryName:(UnquotedPath / IdentifierOrString) _ 
    asPart:("AS"i _ aliasName:Identifier _)?
    paramsPart:(_ head:DeclareParameter ( _ "," _ tail:DeclareParameter )*)? _
    LineTerminator? {
      const params = paramsPart ? [paramsPart[1], ...paramsPart[2].map(t => t[3])] : [];
      return node("DeclareStatement", { returnType: cFunctionType || null, functionName, libraryName, aliasName: asPart ? asPart[2] : null, parameters: params });
    }

DeclareParameter
  = type:("LONG"i / "INTEGER"i / "SINGLE"i / "DOUBLE"i / "STRING"i) _ byRef:("@" _)? name:Identifier { 
      return { type, byRef: !!byRef, name };
    }

Expression
  = head:LogicalOr { return head; }

// Basic precedence chain (can be expanded later)
LogicalOr
  = head:LogicalAnd tail:(M_ (".OR."i / ("OR"i ![a-zA-Z0-9_])) _ LogicalAnd)* {
      return tail.reduce((acc, t) => node("LogicalExpression", { operator: "OR", left: acc, right: t[3] }), head);
    }

LogicalAnd
  = head:Equality tail:(M_ (".AND."i / ("AND"i ![a-zA-Z0-9_])) _ Equality)* {
      return tail.reduce((acc, t) => node("LogicalExpression", { operator: "AND", left: acc, right: t[3] }), head);
    }

Equality
  = head:Relational tail:(M_ op:("==" / "=" / "<>" / "!=" / "#" / "$") _ Relational)* {
      return tail.reduce((acc, t) => node("BinaryExpression", { operator: t[1], left: acc, right: t[3] }), head);
    }

Relational
  = head:Additive tail:(
      M_ op:(">=" / ">" / "<=" / "<") _ rhs:Additive { return { kind: 'cmp', op, rhs }; }
      / __ "NOT IN"i __ "(" __ inRhs:(SelectStatement / ExpressionList) __ ")" { return { kind: 'in', not: true, rhs: inRhs }; }
      / __ "IN"i __ "(" __ inRhs2:(SelectStatement / ExpressionList) __ ")" { return { kind: 'in', not: false, rhs: inRhs2 }; }
    )* {
      return tail.reduce((acc, t) => {
        if (!t) return acc;
        if (t.kind === 'cmp') {
          return node("BinaryExpression", { operator: t.op, left: acc, right: t.rhs });
        }
        if (t.kind === 'in') {
          return node("InExpression", { left: acc, not: !!t.not, right: t.rhs });
        }
        return acc;
      }, head);
    }

Additive
  = head:Multiplicative tail:(M_ op:("+" / "-") _ Multiplicative)* {
      return tail.reduce((acc, t) => node("BinaryExpression", { operator: t[1], left: acc, right: t[3] }), head);
    }

Multiplicative
  = head:Unary tail:(M_ op:("*" / "/" / "%") _ Unary)* {
      return tail.reduce((acc, t) => node("BinaryExpression", { operator: t[1], left: acc, right: t[3] }), head);
    }

Unary
  = op:(".NOT."i / ("NOT"i ![a-zA-Z0-9_]) / "!" / "-" / "+") _ expr:Unary {
      return node("UnaryExpression", { operator: typeof op === 'string' ? op.toUpperCase() : op, argument: expr });
    }
  / Exponentiation

// Exponentiation (^) - right-associative and tighter than unary so that -2^2 parses as -(2^2) which matches typical VFP/SQL semantics.
Exponentiation
  = head:PostfixExpression tail:(M_ "^" _ rhs:(Exponentiation / Unary))? {
      if (!tail) return head;
      return node("BinaryExpression", { operator: '^', left: head, right: tail[3] });
    }

// Primary base (literals, identifiers, parenthesized expressions)
Primary
  = ExistsExpression
  / NumberLiteral
  / StringLiteral
  / BooleanLiteral
  / NullLiteral
  / DateTimeLiteral
  / CastExpression
  / CaseExpression
  / MacroSubstitute
  / id:Identifier { return (id && id.length && id.charAt(0) === '_') ? node("ImplicitGlobal", { name: id }) : node("Identifier", { name: id }); }
  / "(" _ e:Expression _ ")" { return e; }

// Argument list for call expressions (Allow empty arguments (i.e. consecutive commas) which are represented as null)
ArgumentList
  = first:((ArgItem)?) tail:(_ "," _ (ArgItem)? )* {
    const args = [];
    args.push(first === undefined ? null : first);
    for (const t of tail) {
      args.push(t[3] === undefined ? null : t[3]);
    }
    return args;
  }

ArgItem
  = MacroPrefixedArg
  / Expression
  / "*" { return node('SelectStar', {}); }

MacroPrefixedArg
  = m:MacroSubstitute _ e:Expression { return { type: 'MacroPrefixed', macro: m, expression: e }; }

// CAST(expr AS TypeSpec) - simple SQL style cast support
TypeSpec
  = id:Identifier _ "(" _ w:NumberLiteral _ s:("," _ NumberLiteral _)? ")" { 
    return { kind: 'typed', name: id, size: w, scale: (s != null ? s[2] : null) }; 
  }
  / id:IdentifierOrString { return { kind: 'simple', name: id }; }

// SQL CASE [operand] WHEN ... THEN ... [ELSE ...] END, which is an expression rather than a statement.
CaseExpression
  = "CASE"i WB __ operand:(!("WHEN"i WB) e:Expression __ { return e; })?
    whens:("WHEN"i WB __ w:Expression __ "THEN"i WB __ t:Expression __ { return { when: w, then: t }; })+
    alt:("ELSE"i WB __ e:Expression __ { return e; })?
    "END"i WB {
      return node("CaseExpression", { operand: operand || null, whens, otherwise: alt || null });
    }

CastExpression
  = "CAST"i _ "(" _ e:Expression _ "AS"i _ t:TypeSpec _ ")" { return node("CastExpression", { expression: e, to: t }); }

// EXISTS (subquery)
ExistsExpression
  = "EXISTS"i _ "(" _ sq:SelectStatement _ ")" { return node("ExistsExpression", { argument: sq }); }

// Postfix expressions: allow chaining of member access (.prop) and call expressions (args)
PostfixExpression
  = head:Primary tail:(
      "::" _ prop:Identifier { return { type: 'scope', prop } }
    / ("." / "->") _ prop:MemberName { return { type: 'member', prop } }
      / "(" _ args:ArgumentList? _ ")" { return { type: 'call', args: args || [] } }
      / "[" _ idxs:ExpressionList _ "]" { return { type: 'index', indexes: idxs }; }
    )*
    {
      let expr = head;
      for (const t of tail) {
        if (t.type === 'member') {
          expr = node("MemberExpression", { object: expr, property: node("Identifier", { name: t.prop }) });
        } else if (t.type === 'scope') {
          // Base::Method() reaches a parent implementation explicitly, which DODEFAULT() does implicitly.
          expr = node("ScopeResolution", { object: expr, property: node("Identifier", { name: t.prop }) });
        } else if (t.type === 'call') {
          expr = node("CallExpression", { callee: expr, arguments: t.args });
        } else if (t.type === 'index') {
          expr = node('ArrayIndexExpression', { object: expr, indexes: t.indexes });
        }
      }
      return expr;
    }

// -----------------------------
// Control Flow
// -----------------------------

// Allow a bare expression (typically a call) as a top-level statement.
ExpressionStatement "expression statement"
  = expr:PostfixExpression !{ return expr.type === 'Identifier' || expr.type === 'ImplicitGlobal'; } { return node("ExpressionStatement", { expression: expr }); }

// Leading equals can be used to evaluate/call an expression as a statement, e.g. "=func()"
EvalStatement "equals-expression statement"
  = "=" _ expr:Expression { return node("ExpressionStatement", { expression: expr }); }

IfStatement "if statement"
  = "IF"i WB __ test:Expression __ 
    consequent:(Statement __)*
    "ELSE"i __
    alternate:(Statement __)*
    "ENDIF"i __
    {
      return node("IfStatement", { test, consequent: node("BlockStatement", { body: flatten(consequent.map(s => s[0])) }), alternate: node("BlockStatement", { body: flatten(alternate.map(s => s[0])) }) });
    }
    / "IF"i WB __ test:Expression __ 
      consequent:(Statement __)* 
      "ENDIF"i __
    {
      return node("IfStatement", { test, consequent: node("BlockStatement", { body: flatten(consequent.map(s => s[0])) }), alternate: null });
    }

// `SELECT [ALL | DISTINCT] [TOP nExpr [PERCENT]] Select_List_Item [, ...]
  //  FROM [FORCE] Table_List_Item [, ...]
  //     [[JoinType] JOIN DatabaseName!]Table [[AS] Local_Alias]
  //     [ON JoinCondition [AND | OR [JoinCondition | FilterCondition] ...] 
  //  [WITH (BUFFERING = lExpr)]
  //  [WHERE JoinCondition | FilterCondition [AND | OR JoinCondition | FilterCondition] ...]
  //  [GROUP BY Column_List_Item [, ...]] [HAVING FilterCondition [AND | OR ...]]
  //  [UNION [ALL] SELECTCommand]
  //  [ORDER BY Order_Item [ASC | DESC] [, ...]]
  //  [INTO StorageDestination | TO DisplayDestination]
  //  [PREFERENCE PreferenceName] [NOCONSOLE] [PLAIN] [NOWAIT]
SelectStatement
  = sc:SelectCore unions:(ContSpace "UNION"i ContSpace all:("ALL"i ContSpace)? rhs:SelectCore { return { all: !!all, select: rhs }; })* {
      const unionParts = unions ? unions : [];
      return node('SelectStatement', { ...sc, unions: unionParts });
    }

SelectCore
  = "SELECT"i WB WS0
    quant:("ALL"i / "DISTINCT"i)? WS0
    top:("TOP"i WS0 n:Expression WS0 percent:("PERCENT"i)? { return { count: n, percent: !!percent }; })? WS0
    list:(!("FROM"i ![a-zA-Z0-9_]
          / "WITH"i ![a-zA-Z0-9_]
          / "WHERE"i ![a-zA-Z0-9_]
          / "GROUP BY"i ![a-zA-Z0-9_] 
          / "HAVING"i ![a-zA-Z0-9_] 
          / "ORDER BY"i ![a-zA-Z0-9_] 
          / "INTO"i ![a-zA-Z0-9_] 
          / "UNION"i ![a-zA-Z0-9_]) l:SelectList { return l; })?
    parts:(ContSpace SelectTailPart)* {
      let from = null, withbuf = null, where = null, group = null, having = null, order = null, destination = null, pref = null, noconsol = false, plain = false, nowait = false;
      for (const t of parts) {
        const p = t[1];
        switch (p.kind) {
          case 'FROM': if (!from) from = p.value; break;
          case 'WITHBUF': if (!withbuf) withbuf = p.value; break;
          case 'WHERE': if (!where) where = p.value; break;
          case 'DEST': if (!destination) destination = p.value; break;
          case 'GROUP': if (!group) group = p.value; break;
          case 'HAVING': if (!having) having = p.value; break;
          case 'ORDER': if (!order) order = p.value; break;
          case 'PREF': if (!pref) pref = p.value; break;
          case 'NOCONSOLE': noconsol = true; break;
          case 'PLAIN': plain = true; break;
          case 'NOWAIT': nowait = true; break;
        }
      }
      return {
        quantifier: quant ? (typeof quant === 'string' ? quant.toUpperCase() : quant) : null,
        top: top || null,
        list: list || [node('SelectStar', {})],
        from: from || null,
        // if the FromClause provided intermixed items, expose them for consumers
        fromItems: from ? from.items : null,
        withBuffering: withbuf || null,
        where: where || null,
        groupBy: group || null,
        having: having || null,
        orderBy: order || null,
        destination: destination || null,
        preference: pref || null,
        noconsol,
        plain,
        nowait
      };
    }

SelectList
  = head:SelectItem tail:(_ (MacroSubstitute _)* "," _ SelectItem)* { return [head, ...tail.map(t => t[4])]; }

// Allow SELECT tail clauses to appear in any order, at most once.
SelectTailPart
  = from:FromClause { return { kind: 'FROM', value: from }; }
  / withbuf:WithBufferingClause { return { kind: 'WITHBUF', value: withbuf }; }
  / where:WhereClause { return { kind: 'WHERE', value: where }; }
  / dest:(IntoClause / ToClause) { return { kind: 'DEST', value: dest }; }
  / group:GroupByClause { return { kind: 'GROUP', value: group }; }
  / having:HavingClause { return { kind: 'HAVING', value: having }; }
  / order:OrderByClause { return { kind: 'ORDER', value: order }; }
  / pref:PreferenceClause { return { kind: 'PREF', value: pref }; }
  / ms:MacroSubstitute { return { kind: 'MACRO', value: ms }; }
  / !(
      "FROM"i ![A-Za-z0-9_]
    / "WITH"i ![A-Za-z0-9_]
    / "WHERE"i ![A-Za-z0-9_]
    / "GROUP BY"i ![A-Za-z0-9_]
    / "HAVING"i ![A-Za-z0-9_]
    / "ORDER BY"i ![A-Za-z0-9_]
    / "INTO"i ![A-Za-z0-9_]
    / "UNION"i ![A-Za-z0-9_]
    / "PREFERENCE"i ![A-Za-z0-9_]
    / "NOCONSOLE"i ![A-Za-z0-9_]
    / "PLAIN"i ![A-Za-z0-9_]
    / "NOWAIT"i ![A-Za-z0-9_]
    / "," ![A-Za-z0-9_]
    / "JOIN"i ![A-Za-z0-9_]
    ) e:Expression { return { kind: 'EXTRA', value: e }; }
  / "NOCONSOLE"i { return { kind: 'NOCONSOLE' }; }
  / "PLAIN"i { return { kind: 'PLAIN' }; }
  / "NOWAIT"i { return { kind: 'NOWAIT' }; }

SelectItem
  = ms:MacroSubstitute _ si:SelectItem { return si; }
  / "*" { return node('SelectStar', {}); }
  / tbl:Identifier "." "*" { return node('SelectStar', { table: tbl }); }
  / expr:Expression alias:(
      _ "AS"i _ a:Identifier { return a; }
      / _ !(
          "FROM"i ![a-zA-Z0-9_]
        / "WITH"i ![a-zA-Z0-9_]
        / "WHERE"i ![a-zA-Z0-9_]
        / "GROUP"i ![a-zA-Z0-9_]
        / "HAVING"i ![a-zA-Z0-9_]
        / "ORDER"i ![a-zA-Z0-9_]
        / "INTO"i ![a-zA-Z0-9_]
        / "UNION"i ![a-zA-Z0-9_]
        / "PREFERENCE"i ![a-zA-Z0-9_]
        / "NOCONSOLE"i ![a-zA-Z0-9_]
        / "PLAIN"i ![a-zA-Z0-9_]
        / "NOWAIT"i ![a-zA-Z0-9_]
      ) a:Identifier { return a; }
    )? {
      return node('SelectItem', { expression: expr, alias: alias || null }); }
  / callee:Identifier _? "(" _ args:ArgumentList? _ ")" alias:(
      _ "AS"i _ a:Identifier { return a; }
      / _ !(
          "FROM"i ![a-zA-Z0-9_]
        / "WITH"i ![a-zA-Z0-9_]
        / "WHERE"i ![a-zA-Z0-9_]
        / "GROUP"i ![a-zA-Z0-9_]
        / "HAVING"i ![a-zA-Z0-9_]
        / "ORDER"i ![a-zA-Z0-9_]
        / "INTO"i ![a-zA-Z0-9_]
        / "UNION"i ![a-zA-Z0-9_]
        / "PREFERENCE"i ![a-zA-Z0-9_]
        / "NOCONSOLE"i ![a-zA-Z0-9_]
        / "PLAIN"i ![a-zA-Z0-9_]
        / "NOWAIT"i ![a-zA-Z0-9_]
      ) a:Identifier { return a; }
    )? {
      return node('SelectItem', { expression: node('CallExpression', { callee: node('Identifier', { name: callee }), arguments: args || [] }), alias: alias || null });
    }
  / "(" _ inner:Expression _ ")" alias:(
      _ "AS"i _ a:Identifier { return a; }
      / _ !(
          "FROM"i ![a-zA-Z0-9_]
        / "WITH"i ![a-zA-Z0-9_]
        / "WHERE"i ![a-zA-Z0-9_]
        / "GROUP"i ![a-zA-Z0-9_]
        / "HAVING"i ![a-zA-Z0-9_]
        / "ORDER"i ![a-zA-Z0-9_]
        / "INTO"i ![a-zA-Z0-9_]
        / "UNION"i ![a-zA-Z0-9_]
        / "PREFERENCE"i ![a-zA-Z0-9_]
        / "NOCONSOLE"i ![a-zA-Z0-9_]
        / "PLAIN"i ![a-zA-Z0-9_]
        / "NOWAIT"i ![a-zA-Z0-9_]
      ) a:Identifier { return a; }
    )? {
      return node('SelectItem', { expression: inner, alias: alias || null });
    }

FromClause
  = "FROM"i ContSpace force:("FORCE"i ContSpace)? seq:FromSequence {
      // seq contains ordered tables and joins; expose arrays for backwards compatibility
      return { force: !!force, tables: seq.tables, joins: seq.joins, items: seq.items };
    }

TableList
  = head:TableRef tail:(_ "," _ TableRef)* { return [head, ...tail.map(t => t[3])]; }

// FromSequence allows TableRef and JoinClause to be intermixed, e.g.
// FROM t1 ; LEFT JOIN t2 ON ... ; ,t3, t4
FromSequence
  = ContSpace? first:TableRef tail:(
      ContSpace "," ContSpace tr:TableRef { return { kind: 'table', value: tr }; }
      / ContSpace jc:JoinClause { return { kind: 'join', value: jc }; }
    )* {
    const items = [ { kind: 'table', value: first }, ...tail ];
    const tables = items.filter(i => i.kind === 'table').map(i => i.value);
    const joins = items.filter(i => i.kind === 'join').map(i => i.value);
    return { items, tables, joins };
  }

TableRef
  = tablePart:("(" __ sub:SelectStatement ContSpace? ")" { return { subquery: sub }; }
                / "(" _ tbl:Expression _ ")" { return { name: tbl }; }
                / name:QualifiedTable { return { name }; })
    InlineWS alias:(
      ("AS"i _ a:Identifier { return a; })
      / !("SET"i ![A-Za-z0-9_]
         / "WHERE"i ![A-Za-z0-9_]
         / "ORDER"i ![A-Za-z0-9_]
         / "GROUP"i ![A-Za-z0-9_]
         / "HAVING"i ![A-Za-z0-9_]
         / "UNION"i ![A-Za-z0-9_]
         / "INTO"i ![A-Za-z0-9_]
         / "WITH"i ![A-Za-z0-9_]
         / "ON"i ![A-Za-z0-9_]
         / "IN"i ![A-Za-z0-9_]
         / "JOIN"i ![A-Za-z0-9_])
        a:Identifier { return a; }
    )? {
    return { ...tablePart, alias: alias ? (typeof alias === 'string' ? alias : alias[2]) : tablePart.alias };
  }

QualifiedTable
  = db:Identifier "!" tbl:Identifier { return { database: db, table: tbl }; }
  / t:(UnquotedPath / IdentifierOrString) { return { database: null, table: t }; }

JoinClause
  = jt:JoinType? _ "JOIN"i _ tr:TableRef __ "ON"i __ cond:Expression {
      return { type: jt || null, target: tr, condition: cond };
    }

JoinType
  = "LEFT"i _ ("OUTER"i _)? { return "LEFT"; }
  / "RIGHT"i _ ("OUTER"i _)? { return "RIGHT"; }
  / "FULL"i _ ("OUTER"i _)? { return "FULL"; }
  / "INNER"i _ { return "INNER"; }

WithBufferingClause
  = "WITH"i _ "(" _ "BUFFERING"i _ "=" _ e:Expression _ ")" { return { buffering: e }; }

WhereClause
  = "WHERE"i _ e:Expression { return e; }

GroupByClause
  = "GROUP BY"i __ items:ExpressionList { return items; }

HavingClause
  = "HAVING"i __ e:Expression { return e; }

OrderByClause
  = "ORDER BY"i __ items:OrderItemList { return items; }

OrderItemList
  = head:OrderItem _ tail:("," _ OrderItem)* { return [head, ...tail.map(t => t[3])]; }

OrderItem
  = expr:Expression dir:(_ ("ASC"i / "DESC"i))? { return { expression: expr, direction: dir ? dir[1].toUpperCase() : null }; }

IntoClause
  = "INTO"i _ dest:(
      ("TABLE"i WB _ p:PathOrExpression { return { kind: 'TABLE', name: p }; })
      / ("CURSOR"i WB _ a:Expression flags:(_("READWRITE"i / "NOFILTER"i))* { return { kind: 'CURSOR', name: a }; })
      / ("ARRAY"i WB _ a:Identifier { return { kind: 'ARRAY', name: a }; })
      / ("DBF"i WB _ n:PathOrExpression { return { kind: 'DBF', name: n }; })
      / n:IdentifierOrString { return { kind: 'DEFAULT', name: n }; }
    ) { return dest; }

ToClause
  = "TO"i __ d:IdentifierOrString { return { kind: 'TO', name: d }; }

PreferenceClause
  = "PREFERENCE"i __ p:IdentifierOrString { return p; }

NoConsoleFlag
  = "NOCONSOLE"i { return true; }

PlainFlag
  = "PLAIN"i { return true; }

NowaitFlag
  = "NOWAIT"i { return true; }

// -----------------------------
// COPY/RENAME
// -----------------------------
CopyStatement "copy/rename statement"
  = CopyFileStatement / CopyToStatement

CopyFileStatement
  = action:("COPY FILE"i / "RENAME"i) WB _ src:PathOrExpression _ "TO"i _ dst:PathOrExpression {
      return node(action === 'COPY FILE' ? 'CopyFileStatement' : 'RenameStatement', { source: src, destination: dst });
    }

CopyToStatement
  = "COPY TO"i WB _
    target:(PathOrExpression) _
    db:DatabaseClause? _
    fields:FieldsClause? _
    forClause:("FOR"i __ fexp:Expression { return fexp; })? _
    whileClause:("WHILE"i __ wexp:Expression { return wexp; })? _
    idx:WithIndexClause? _
    noopt:("NOOPTIMIZE"i)? _
    t:TypeClause? _
    ascp:("AS"i __ cp:Expression { return cp; })?
    {
      return node('CopyToStatement', {
        target,
        database: db || null,
        fields: fields || null,
        for: forClause || null,
        while: whileClause || null,
        index: idx || null,
        noOptimize: !!noopt,
        exportType: t || null,
        codepage: ascp || null
      });
    }
  
// ERASE FileName | ? [RECYCLE]
EraseStatement
  = "ERASE"i WB _ target:(PathOrExpression / "?") _ recycle:(_ "RECYCLE"i)? {
      const tgt = (typeof target === 'string' && target === '?') ? { kind: 'PROMPT' } : target;
      return node('EraseStatement', { target: tgt, recycle: !!(recycle && recycle[1]) });
    }

DatabaseClause
  = "DATABASE"i __ db:IdentifierOrString _ name:("NAME"i __ ln:IdentifierOrString { return ln; })? { return { database: db, longName: name || null }; }

FieldsClause
  = "FIELDS"i __ spec:(
      list:IdentifierList { return { kind: 'list', fields: list }; }
      / "LIKE"i __ sk:Pattern { return { kind: 'like', pattern: sk }; }
      / "EXCEPT"i __ sk:Pattern { return { kind: 'except', pattern: sk }; }
    ) { return spec; }

WithIndexClause
  = ("WITH"i _)? kind:("CDX"i / "PRODUCTION"i) { return typeof kind === 'string' ? kind.toUpperCase() : kind; }

// INDEX ON eExpression TO IDXFileName | TAG TagName [BINARY]
//    [COLLATE cCollateSequence] [OF CDXFileName] [FOR lExpression]
//    [COMPACT] [ASCENDING | DESCENDING] [UNIQUE | CANDIDATE] [ADDITIVE]
// Options may appear in any order.
IndexOnStatement "index on statement"
  = "INDEX ON"i WB __ expr:Expression parts:(_ IndexOnPart)* {
      // Aggregate options from arbitrary order
      let to = null, tag = null, binary = false, collate = null, of = null, forExpr = null,
          compact = false, direction = null, uniqueness = null, additive = false;
      for (const p of parts.map(t => t[1])) {
        switch (p.kind) {
          case 'TO': to = p.value; break;
          case 'TAG': tag = p.value; break;
          case 'BINARY': binary = true; break;
          case 'COLLATE': collate = p.value; break;
          case 'OF': of = p.value; break;
          case 'FOR': forExpr = p.value; break;
          case 'COMPACT': compact = true; break;
          case 'DIR': direction = (typeof p.value === 'string' ? p.value.toUpperCase() : p.value); break;
          case 'UNIQ': uniqueness = (typeof p.value === 'string' ? p.value.toUpperCase() : p.value); break;
          case 'ADDITIVE': additive = true; break;
        }
      }
      return node('IndexOnStatement', {
        expression: expr,
        to,
        tag,
        binary,
        collate,
        of,
        for: forExpr,
        compact,
        direction,
        uniqueness,
        additive
      });
    }

IndexOnPart
  = "TO"i _ tgt:(IdentifierOrString / UnquotedPath) { return { kind: 'TO', value: tgt }; }
  / "TAG"i _ tag:Identifier { return { kind: 'TAG', value: tag }; }
  / "BINARY"i { return { kind: 'BINARY' }; }
  / "COLLATE"i __ cs:IdentifierOrString { return { kind: 'COLLATE', value: cs }; }
  / "OF"i __ cdx:(IdentifierOrString / UnquotedPath) { return { kind: 'OF', value: cdx }; }
  / "FOR"i __ fexp:Expression { return { kind: 'FOR', value: fexp }; }
  / "COMPACT"i { return { kind: 'COMPACT' }; }
  / dir:("ASCENDING"i / "DESCENDING"i) { return { kind: 'DIR', value: dir }; }
  / uniq:("UNIQUE"i / "CANDIDATE"i) { return { kind: 'UNIQ', value: uniq }; }
  / "ADDITIVE"i { return { kind: 'ADDITIVE' }; }

TypeClause
  = ("TYPE"i _)? et:ExportType { return et; }

ExportType
  = t:("FOXPLUS"i / "FOX2X"i / "DIF"i / "MOD"i / "SDF"i / "SYLK"i / "WK1"i / "WKS"i / "WR1"i / "WRK"i / "CSV"i / "XLS"i / "XL5"i) { return { format: (typeof t === 'string' ? t.toUpperCase() : t) }; }
  / "DELIMITED"i _ d:DelimitedOptions? { return { format: 'DELIMITED', options: d || null }; }

DelimitedOptions
  = "WITH"i __ opt:(
      "BLANK"i { return { mode: 'BLANK' }; }
      / "TAB"i { return { mode: 'TAB' }; }
      / "CHARACTER"i __ ch:IdentifierOrString { return { mode: 'CHARACTER', delimiter: ch }; }
      / del:IdentifierOrString { return { mode: 'DELIMITER', delimiter: del }; }
    ) { return opt; }

// -----------------------------
// GO / GOTO (record navigation)
// -----------------------------

GoToStatement "go/goto statement"
  = cmd:("GOTO"i / "GO"i) WB _
    part:(
      pos:("TOP"i / "BOTTOM"i) _ inC:InClause? { return { pos, rec: null, inTarget: inC || null }; }
      / reckw:("RECORD"i)? _ rec:Expression _ inC:InClause? { return { pos: null, rec, inTarget: inC || null }; }
    ) {
      return node("GoToStatement", {
        command: (typeof cmd === 'string' ? cmd.toUpperCase() : cmd),
        position: part.pos ? (typeof part.pos === 'string' ? part.pos.toUpperCase() : part.pos) : null,
        record: part.rec || null,
        inTarget: part.inTarget
      });
    }

InClause
  = "IN"i WB _ target:(AliasRef / SelectCore) { return target; }

// A work area can be named by an expression in parentheses wherever an alias is expected: `USE IN (D_MTPC)`, `SET ORDER TO (m.cTag) IN (m.cAlias)`, `GO TOP IN (m.cAlias)`. That is how the alias travels when it is held in a variable. Accepting the parenthesised form only where a *table* was expected is what left the rest of each of those lines to the catch-all.
AliasRef
  = "(" _ e:Expression _ ")" { return e; }
  / Identifier
  / StringLiteral
  / NumberLiteral

// SKIP [nRecords] [IN nWorkArea | cTableAlias]
SkipStatement
  = "SKIP"i WB _ n:Expression? _ 
  inPart:(_ "IN"i __ target:(NumberLiteral / PathOrExpression) { return target; })? 
  {
    return node('SkipStatement', { count: n || null, inTarget: inPart });
  }

// UNLOCK [RECORD nRecordNumber] [IN nWorkArea | cTableAlias] [ALL]
UnlockStatement
  = "UNLOCK"i WB _
    rec:(_ "RECORD"i __ n:Expression { return n; })?
    _ inPart:(_ "IN"i __ target:AliasRef { return target; })?
    _ all:("ALL"i)? {
      return node('UnlockStatement', { record: rec, inTarget: inPart, all: !!all });
    }

// Opt 1: INSERT INTO dbf_name [(FieldName1 [, FieldName2, ...])]
//    VALUES (eExpression1 [, eExpression2, ...])
// Opt 2: INSERT INTO dbf_name FROM ARRAY ArrayName | FROM MEMVAR | FROM NAME ObjectName
// Opt 3: INSERT INTO dbf_name [(FieldName1 [, FieldName2, ...])]
//    SELECT SELECTClauses [UNION UnionClause SELECT SELECTClauses ...]
InsertStatement
  = "INSERT"i WB __ "INTO"i __ target:PathOrExpression _
    cols:("(" _ cl:IdentifierList _ ")")? __
    src:(
      "VALUES"i _ "(" _ vals:ExpressionList _ ")" { return { kind: 'values', values: vals }; }
      / "FROM"i __ (
          "ARRAY"i __ arr:Identifier { return { kind: 'from', source: 'ARRAY', name: arr }; }
          / "MEMVAR"i { return { kind: 'from', source: 'MEMVAR', name: null }; }
          / "NAME"i __ obj:ParameterName { return { kind: 'from', source: 'NAME', name: obj }; }
        )
      / select:SelectStatement { return { kind: 'select', select }; }
    ) {
      return node('InsertStatement', {
        target,
        columns: cols ? cols[2] : null,
        source: src
      });
    }

// UPDATE Target
//    SET Column_Name1 = eExpression1 [, Column_Name2 = eExpression2 ...]
//    [FROM [FORCE] Table_List_Item [[, ...] | [JOIN [ Table_List_Item]]]
//    WHERE FilterCondition1 [AND | OR FilterCondition2 ...]
// Clauses may appear in any order
UpdateStatement
  = "UPDATE"i WB _ target:IdentifierOrString WS0
    parts:(WSX UpdatePart)* {
      let set = null, from = null, where = null;
      for (const p of parts.map(t => t[1])) {
        switch (p.kind) {
          case 'SET': if (!set) set = p.value; break;
          case 'FROM': if (!from) from = p.value; break;
          case 'WHERE': if (!where) where = p.value; break;
        }
      }
      return node('UpdateStatement', { target, set: set || null, from: from || null, where: where || null });
    }

UpdatePart
  = "SET"i __ assigns:UpdateAssignmentList { return { kind: 'SET', value: assigns }; }
  / from:FromClause { return { kind: 'FROM', value: from }; }
  / where:WhereClause { return { kind: 'WHERE', value: where }; }

UpdateAssignmentList
  = head:UpdateAssignment tail:(_ "," _ UpdateAssignment)* { return [head, ...tail.map(t => t[3])]; }

UpdateAssignment
  = field:ParameterName _ "=" _ expr:Expression { return { field, expression: expr }; }

// OPT 1: DELETE [Target] FROM [FORCE] Table_List [[, Table_List ...] | [JOIN [ Table_List]]]
//   [WHERE FilterCondition1 [AND | OR FilterCondition2 ...]]
// OPT 2: DELETE [Scope] [FOR lExpression1] [WHILE lExpression2]
//    [IN nWorkArea | cTableAlias] [NOOPTIMIZE]
DeleteStatement
  = "DELETE"i WB _ sel:(
      from:FromClause { return { target: null, from }; }
      / target:IdentifierOrString _ from:FromClause { return { target, from }; }
    ) _
    where:WhereClause? {
      const from = sel.from;
      return node('DeleteStatement', {
        target: sel.target || null,
        // keep backward-compatible fields
        tables: from.tables,
        joins: from.joins,
        fromItems: from.items,
        where: where || null
      });
    }
  / "DELETE"i WB _ 
      scope:IdentifierOrString? _
    forp:("FOR"i __ fexp:Expression { return fexp; })? _
    whilep:("WHILE"i __ wexp:Expression { return wexp; })? _
    inPart:("IN"i _ inTarget:(NumberLiteral / Identifier))? _
    noopt:("NOOPTIMIZE"i)? {
      return node('DeleteStatement', {
        target: null,
        tables: null,
        joins: null,
        where: null,
        scope: scope || null,
        for: forp || null,
        while: whilep || null,
        inTarget: inPart ? inPart[2] : null,
        noOptimize: !!noopt
      });
    }

// ZAP [IN nWorkArea | cTableAlias]
ZapStatement
  = "ZAP"i WB _ inPart:(_ "IN"i __ target:AliasRef { return target; })? {
    return node('ZapStatement', { inTarget: inPart });
  }

// RECALL [Scope] [FOR lExpression1] [WHILE lExpression2] [NOOPTIMIZE]
//    [IN nWorkArea | cTableAlias]
RecallStatement
  = "RECALL"i WB _
    scope:(!("IN"i) IdentifierOrString)? _
    forp:(("FOR"i __ fexp:Expression { return fexp; }))? _
    whilep:(("WHILE"i __ wexp:Expression { return wexp; }))? _
    noopt:("NOOPTIMIZE"i)? _
    inPart:(_ "IN"i __ target:AliasRef { return target; })?
    {
      return node('RecallStatement', {
        scope: scope || null,
        for: forp || null,
        while: whilep || null,
        noOptimize: !!noopt,
        inTarget: inPart ? inPart[2] : null
      });
    }

// -----------------------------
// Loops: FOR ... ENDFOR|NEXT, FOR EACH ... ENDFOR|NEXT and DO WHILE ... ENDDO
// -----------------------------
IterationStatement
  = ForEachLoop / ForLoop / DoWhileLoop

// FOR VarName = nInitialValue TO nFinalValue [STEP nIncrement] Commands [EXIT] [LOOP] ENDFOR | NEXT
ForLoop "for loop"
  = "FOR"i WB _ 
    varName:ParameterName _ "=" _ init:Expression _ "TO"i _ final:Expression _ 
    step:("STEP"i _ inc:Expression)? __
    // Avoid consuming ENDFOR/NEXT as part of the body when NEXT isn't reserved globally
    body:(!("ENDFOR"i WB / "NEXT"i WB) s:Statement __ { return s; })*
    ("ENDFOR"i / "NEXT"i) _ endVar:ParameterName?
    {
      return node("ForStatement", {
        variable: varName,
        init,
        final,
        step: step ? step[2] : null,
        endVariable: endVar || null,
        body: node("BlockStatement", { body: flatten(body) })
      });
    }

// FOR EACH Var [AS Type [OF Class-Library]] IN Group [FOXOBJECT]
//   Commands
// [EXIT]
// [LOOP]
// ENDFOR | NEXT [Var]
ForEachLoop "for-each loop"
  = "FOR EACH"i WB _ varName:ParameterName _
    typePart:("AS"i _ type:Identifier _ ofPart:("OF"i _ clslib:Identifier _ { return { library: clslib }; })? {
      return { typing: type, of: ofPart || null };
    })?
    "IN"i _ group:Expression foxobj:(_ "FOXOBJECT"i)? __
    // Avoid consuming ENDFOR/NEXT as part of the body when NEXT isn't reserved globally
    body:(!("ENDFOR"i WB / "NEXT"i WB) s:Statement __ { return s; })*
    ("ENDFOR"i / "NEXT"i) _ endVar:ParameterName? 
    {
      const asType = typePart ? typePart.typing : null;
      const ofClass = typePart ? typePart.of : null;
      return node("ForEachStatement", {
        variable: varName,
        asType,
        ofClass,
        collection: group,
        foxObject: !!foxobj,
        endVariable: endVar || null,
        body: node("BlockStatement", { body: flatten(body) })
      });
    }

// DO WHILE lExpression Commands [LOOP] [EXIT] ENDDO
DoWhileLoop "do-while loop"
  = "DO WHILE"i WB _ test:Expression __
    body:(Statement __)*
    "ENDDO"i {
      return node("DoWhileStatement", {
        test,
        body: node("BlockStatement", { body: flatten(body.map(s => s[0])) })
      });
    }

// DO CASE CASE lExpression1 [Commands] ... [OTHERWISE Commands] ENDCASE
DoCaseStatement "do case statement"
  = "DO CASE"i WB __
  cases:(CaseClause)*
  otherwise:("OTHERWISE"i __ othBody:(Statement __)* { return node('BlockStatement', { body: flatten(othBody.map(s => s[0])) }); })?
  "ENDCASE"i {
      // (CaseClause)* yields the clauses themselves, not [clause] pairs: indexing them dropped every
      // branch of every DO CASE, contents and all, so nothing downstream could see inside one.
      return node('DoCaseStatement', {
        cases,
        otherwise: otherwise ? otherwise : null
      });
    }

CaseClause
  = "CASE"i _ test:Expression __
    consequent:(!CaseBoundary s:Statement __ { return s; })* {
      return node('CaseClause', {
        test,
        consequent: node('BlockStatement', { body: flatten(consequent) })
      });
    }

// A CASE body ends at the next branch or at ENDCASE. Without this guard the catch-all swallows the next CASE line into this body, and only the first branch of a DO CASE is ever parsed.
CaseBoundary
  = ("CASE"i / "OTHERWISE"i / "ENDCASE"i) WB

// DO FORM FormName | ? [NAME VarName [LINKED]] [WITH cParameterList]
//  [TO VarName] [NOREAD] [NOSHOW]
DoFormStatement "do form statement"
  = "DO FORM"i WB _ target:(StringLiteral / Identifier / "?") _
    namePart:("NAME"i _ nameIdent:ParameterName _ link:("LINKED"i)? { return { name: nameIdent, linked: !!link }; })?
    withPart:("WITH"i _ params:ArgumentList maybeTo:(_ "TO"i _ v:ParameterName { return v; })? { return { params, to: maybeTo }; })?
    toPart:("TO"i _ v:ParameterName { return v; })?
    flags:(_ ("NOREAD"i / "NOSHOW"i))* {
      // If a TO clause was attached directly after WITH's argument list, prefer it.
      const toFromWith = withPart ? withPart.to : null;
      const explicitTo = toPart;
      return node("DoFormStatement", {
        target,
        name: namePart ? namePart.name : null,
        linked: namePart ? namePart.linked : false,
        arguments: withPart ? withPart.params : [],
        to: toFromWith || explicitTo || null,
        noread: flags ? flags.some(f => f[1].toUpperCase() === 'NOREAD') : false,
        noshow: flags ? flags.some(f => f[1].toUpperCase() === 'NOSHOW') : false
      });
    }

DoStatement "do statement"
  = "DO"i WB _ 
    target:(!("FORM"i WB / "CASE"i WB / "WHILE"i WB) PathOrExpression) _
    // Allow IN and WITH in either order
    first:(
      ("WITH"i _ params:ArgumentList { return { kind: 'WITH', params }; })
      / ("IN"i _ n:( $([0-9]+) { return Number(n); } / Identifier / StringLiteral ) { return { kind: 'IN', value: n }; })
    )?
    rest:( _ (
      ("WITH"i _ params:ArgumentList { return { kind: 'WITH', params }; })
      / ("IN"i _ n:( $([0-9]+) { return Number(n); } / Identifier / StringLiteral ) { return { kind: 'IN', value: n }; })
    ))?
    {
      let withArgs = [];
      let inSession = null;
      function apply(p) { if (!p) return; if (p.kind === 'WITH') withArgs = p.params; else if (p.kind === 'IN') inSession = p.value; }
      apply(first);
      if (rest) apply(rest[1]);
      return node("DoStatement", { target, inSession, arguments: withArgs });
    }

ExitStatement "exit"
  = ("EXIT"i / "QUIT"i) WB { return node("ExitStatement", {}); }

ContinueStatement "continue (LOOP)"
  = "LOOP"i WB { return node("ContinueStatement", {}); }

// -----------------------------
// CREATE TABLE/DBF/CURSOR
// -----------------------------
CreateStatement "create statement"
  = "CREATE"i WB _ 
    kind:("TABLE"i / "DBF"i / "CURSOR"i) _ 
    name:CreateTarget _
    nameClause:("NAME"i __ longName:Identifier _ { return longName; })? _
    free:("FREE"i _)?
    codepage:("CODEPAGE"i _ "=" _ cp:(NumberLiteral / Identifier))? _
    def:(
      _ "(" _ items:CreateDefItems _ ")" _ tail:(_ "," _ more:CreateDefItems)? {
        return { type: 'columns', items: tail ? [...items, ...tail[3]] : items };
      }
      / _ "FROM"i __ "ARRAY"i __ arr:Identifier { return { type: 'fromArray', array: arr }; }
    )
    {
      const payload = { 
        kind: (typeof kind === 'string' ? kind.toUpperCase() : kind).toUpperCase(),
        name,
        longName: nameClause || null,
        free: !!free,
        codepage: codepage ? codepage[4] : null
      };
      if (def.type === 'fromArray') {
        return node('CreateStatement', { ...payload, fromArray: def.array, columns: [], constraints: [] });
      } else {
        const cols = def.items.filter(i => i.kind === 'column').map(i => i.node);
        const cons = def.items.filter(i => i.kind === 'constraint').map(i => i.node);
        return node('CreateStatement', { ...payload, columns: cols, constraints: cons, fromArray: null });
      }
    }

// CREATE target may be a simple identifier or a parenthesized expression (macro/expr)
CreateTarget
  = "(" _ e:Expression _ ")" { return e; }
  / Identifier

CreateDefItems
  = head:CreateDefItem tail:(_ "," _ CreateDefItem)* {
      const rest = tail.map(t => t[3]);
      return [head, ...rest];
    }

CreateDefItem
  = c:ColumnDefinition { return { kind: 'column', node: c }; }
  / t:TableConstraint  { return { kind: 'constraint', node: t }; }

IdentifierOrString
  = StringLiteral / Identifier

// Column definition and options
ColumnDefinition
  = name:Identifier __ 
    ftype:FieldType _
    fsize:FieldSize? _
    nullability:("NULL"i / "NOT NULL"i)? _
    check:("CHECK"i __ expr:Expression _ err:("ERROR"i __ msg:StringLiteral)? { return { expr, error: err ? err[2] : null }; })? _
    autoinc:("AUTOINC"i _ nv:("NEXTVALUE"i __ nv:(NumberLiteral / Identifier) _ step:("STEP"i __ st:(NumberLiteral / Identifier))?)? { return { nextValue: nv ? nv[2] : null, step: (nv && nv[4]) ? nv[4][2] : null }; })? _
    def:("DEFAULT"i __ d:Expression { return d; })? _
    colkey:(
      "PRIMARY"i __ "KEY"i { return { primaryKey: true, unique: false, collate: null }; }
      / "UNIQUE"i _ coll:("COLLATE"i __ cs:IdentifierOrString { return cs; })? { return { primaryKey: false, unique: true, collate: coll }; }
    )? _
    refs:("REFERENCES"i __ tbl:IdentifierOrString _ tag:("TAG"i __ tn:Identifier { return tn; })? { return { table: tbl, tag }; })? _
    nocp:("NOCPTRANS"i)? {
      return node('ColumnDefinition', {
        name,
        fieldType: ftype,
        size: fsize || null,
        nullability: nullability ? (Array.isArray(nullability) ? 'NOT NULL' : 'NULL') : null,
        check: check || null,
        autoinc: autoinc || null,
        default: def || null,
        key: colkey || null,
        references: refs || null,
        nocptrans: !!nocp
      });
    }

FieldType
  = t:$([A-Za-z]+) { return t.toUpperCase(); }

FieldSize
  = "(" _ w:Expression _ "," _ p:Expression _ ")" { return { width: w, precision: p }; }
  / "(" _ w:Expression _ ")" { return { width: w, precision: null }; }

TableConstraint
  = "PRIMARY"i __ "KEY"i __ expr:Expression __ "TAG"i __ tag:Identifier { return node('TableConstraint', { kind: 'PRIMARY KEY', expression: expr, tag }); }
  / "UNIQUE"i __ expr:Expression __ "TAG"i __ tag:Identifier _ coll:("COLLATE"i __ cs:IdentifierOrString { return cs; })? { return node('TableConstraint', { kind: 'UNIQUE', expression: expr, tag, collate: coll }); }
  / "FOREIGN"i __ "KEY"i __ expr:Expression __ "TAG"i __ tag:Identifier _ nodup:("NODUP"i)? _ coll:("COLLATE"i __ cs:IdentifierOrString { return cs; })? __ "REFERENCES"i __ tbl:IdentifierOrString _ reftag:("TAG"i __ rt:Identifier { return rt; })? { return node('TableConstraint', { kind: 'FOREIGN KEY', expression: expr, tag, nodup: !!nodup, collate: coll, references: { table: tbl, tag: reftag } }); }
  / "CHECK"i __ expr:Expression _ err:("ERROR"i __ msg:StringLiteral)? { return node('TableConstraint', { kind: 'CHECK', expression: expr, error: err ? err[2] : null }); }


// TRY [ tryCommands ] [ CATCH [ TO VarName ] [ WHEN lExpression ] [ catchCommands ] ] [ THROW [ eUserExpression ] ] [ EXIT ] [ FINALLY [ finallyCommands ] ] ENDTRY
TryStatement "try-catch statement"
  = "TRY"i WB __
    tstmts:(Statement __)*
    cpart:(
      "CATCH"i WB
      toVar:(_ "TO"i WB _ v:ParameterName { return v; })?
      whenPart:(_ "WHEN"i WB __ wexpr:Expression { return wexpr; })?
      __
      cstmts:(Statement __)* {
        return { to: toVar, when: whenPart, body: flatten(cstmts.map(s => s[0])) };
      }
    )?
    tpart:("THROW"i _ texpr:Expression? __ { return texpr === undefined ? null : texpr; })?
    exitpart:("EXIT"i __ { return true; })?
    fpart:("FINALLY"i __ fstmts:(Statement __)* { return flatten(fstmts.map(s => s[0])); })?
    "ENDTRY"i __
    {
      return node("TryStatement", {
        tryBlock: node("BlockStatement", { body: flatten(tstmts.map(s => s[0])) }),
        catchClause: cpart ? { to: cpart.to, when: cpart.when, body: node("BlockStatement", { body: cpart.body }) } : null,
        thrown: (tpart === undefined) ? null : tpart,
        didExit: !!exitpart,
        finallyBlock: fpart ? node("BlockStatement", { body: fpart }) : null
      });
    }

// WITH ObjectName [AS <Type> [OF <Class Library>]]
//    [.cStatements]
// ENDWITH
WithStatement
  = "WITH"i WB _ target:(LValue / PostfixExpression)
    asPart:(_ "AS"i __ t:Identifier _ ofPart:(_ "OF"i _ cl:Identifier { return cl; })? { return { type: t, of: ofPart }; })? __
    body:(WithBodyEntry __)*
    "ENDWITH"i {
      return node("WithStatement", {
        target,
        asType: asPart ? asPart.type : null,
        ofClass: asPart ? asPart.of : null,
        body: node("BlockStatement", { body: flatten(body.map(b => b[0])) })
      });
    }

WithBodyEntry
  = "." _? a:DotAssignment { return a; }
    / "." _? e:PostfixExpression { return node("ExpressionStatement", { expression: node("WithMemberExpression", { expression: e }) }); }
    / c:Statement { return c; }

// Support assignments where the left side may contain call/member chains, e.g.
//   .Objects(n).Style = 1
DotAssignment
  = lhs:PostfixExpression __ "=" __ expr:Expression {
      return node("Assignment", { target: node("WithMemberExpression", { expression: lhs }), expression: expr });
    }

// -----------------------------
// Xbase housekeeping
// -----------------------------
// None of these command words are reserved, so each opens with NotCallOrAssign and keeps only the operands a rule could want. A long option tail is captured as raw source: recognising the statement is what stops the false positive, and pretending to model the tail would buy nothing.

FlushStatement
  = "FLUSH"i WB NotCallOrAssign force:(_ "FORCE"i WB)? {
      return node('FlushStatement', { force: !!force });
    }

ReindexStatement
  = "REINDEX"i WB NotCallOrAssign compact:(_ "COMPACT"i WB)? {
      return node('ReindexStatement', { compact: !!compact });
    }

// MD/RD/CD and their long spellings. Whitespace before the path is required rather than optional: these are two letters long, and without it `CD.Value` would read as a command rather than a member.
DirectoryStatement
  = cmd:("MKDIR"i / "RMDIR"i / "CHDIR"i / "MD"i / "RD"i / "CD"i) WB NotCallOrAssign Whitespace _ target:PathOrExpression {
      return node('DirectoryStatement', { command: cmd.toUpperCase(), target });
    }

// CONTINUE resumes the last LOCATE. It is not LOOP -- that is ContinueStatement -- and unlike LOOP it does not end the block, so it must stay a separate node or unreachable-code would misread it.
ContinueLocateStatement
  = "CONTINUE"i WB NotCallOrAssign { return node('ContinueLocateStatement', {}); }

// NODEFAULT suppresses the base class's own handling of the event being coded.
NoDefaultStatement
  = "NODEFAULT"i WB NotCallOrAssign { return node('NoDefaultStatement', {}); }

PushPopStatement
  = cmd:("PUSH"i / "POP"i) WB _ what:("KEY"i / "MENU"i / "POPUP"i) WB NotCallOrAssign opts:RawOptions {
      return node('PushPopStatement', { command: cmd.toUpperCase(), what: what.toUpperCase(), options: opts });
    }

// EXTERNAL declares nothing at run time -- it tells the compiler a name resolves elsewhere -- but the names in it are deliberate rather than typos, so they are kept.
ExternalStatement
  = "EXTERNAL"i WB _ kind:("ARRAY"i / "PROCEDURE"i / "FUNCTION"i / "CLASS"i / "FORM"i / "LABEL"i / "MENU"i / "QUERY"i / "REPORT"i / "SCREEN"i) WB _ names:IdentifierList {
      return node('ExternalStatement', { kind: kind.toUpperCase(), names });
    }

ModifyStatement
  = "MODIFY"i WB _ what:("STRUCTURE"i / "COMMAND"i / "CONNECTION"i / "DATABASE"i / "FILE"i / "MEMO"i / "REPORT"i / "FORM"i / "CLASS"i / "VIEW"i / "PROCEDURE"i / "LABEL"i / "MENU"i / "PROJECT"i / "QUERY"i / "WINDOW"i / "GENERAL"i) WB NotCallOrAssign opts:RawOptions {
      return node('ModifyStatement', { what: what.toUpperCase(), options: opts });
    }

// ALTER TABLE's tail is a DDL of its own. The table is the part a rule would ask about; the rest is source.
AlterTableStatement
  = "ALTER"i WB __ "TABLE"i WB __ name:IdentifierOrString opts:RawOptions {
      return node('AlterTableStatement', { name, options: opts });
    }

// RUN, and its `!` shorthand: everything after it goes to the shell, so none of it is FoxPro.
RunStatement
  = ("RUN"i WB NotCallOrAssign / "!") cmd:$((!LineTerminator .)*) {
      return node('RunStatement', { command: cmd.trim() });
    }

// -----------------------------
// Unknown/catch-all statement
// -----------------------------
// Captures a single logical line (respecting semicolon continuations) that didn't match any known statement. Protects block delimiters so structured constructs (IF/DO WHILE/FOR/TRY/DEFINE/WITH) can still recognize their endings.
UnknownStatement
  = !("ENDIF"i      ![A-Za-z0-9_]
    / "ELSE"i       ![A-Za-z0-9_]
    / "ENDDO"i      ![A-Za-z0-9_]
    / "ENDFOR"i     ![A-Za-z0-9_]
    / "NEXT"i       ![A-Za-z0-9_]
    / "ENDTRY"i     ![A-Za-z0-9_]
    / "ENDDEFINE"i  ![A-Za-z0-9_]
    / "ENDPROC"i    ![A-Za-z0-9_]
    / "ENDFUNC"i    ![A-Za-z0-9_]
    / "ENDCASE"i    ![A-Za-z0-9_]
    / "ENDWITH"i    ![A-Za-z0-9_]
    / "ENDSCAN"i    ![A-Za-z0-9_]
    / "ENDTEXT"i    ![A-Za-z0-9_]
    / "OTHERWISE"i  ![A-Za-z0-9_]
    / "CATCH"i      ![A-Za-z0-9_]
    / "FINALLY"i    ![A-Za-z0-9_]
    )
    raw:$((!LineTerminator .)+ (LineContinuation (!LineTerminator .)*)*) {
      return node("UnknownStatement", { raw: raw.trim() });
    }

SetStatement
  = SetOrderToStatement / SetRelationToStatement / SetSettingStatement

// SET ORDER TO [nIndexNumber | IDXIndexFileName | [TAG] TagName 
//   [OF CDXFileName] [IN nWorkArea | cTableAlias]
//   [ASCENDING | DESCENDING]]
SetOrderToStatement
  = "SET ORDER TO"i WB _
    sel:(
      n:NumberLiteral { return { kind: 'NUMBER', value: n }; }
      / "(" _ e:Expression _ ")" { return { kind: 'EXPR', value: e }; }
      / f:(IdentifierOrString / UnquotedPath) { return { kind: 'FILE', value: f }; }
      / t:TagSpec { return { kind: 'TAG', tag: t.tag, of: t.of, direction: t.direction }; }
    )?
    _ first:( _ ("IN"i __ target:AliasRef { return { kind: 'IN', value: target }; } 
             / dir:("ASCENDING"i / "DESCENDING"i / "ASC"i / "DESC"i) { return { kind: 'DIR', value: dir }; }) )?
    second:( _ ("IN"i __ target:AliasRef { return { kind: 'IN', value: target }; } 
              / dir:("ASCENDING"i / "DESCENDING"i / "ASC"i / "DESC"i) { return { kind: 'DIR', value: dir }; }) )?
    {
      let inTarget = null; let direction = null;
      function apply(opt) { if (!opt) return; const p = opt[1]; if (!p) return; if (p.kind === 'IN') inTarget = p.value; else if (p.kind === 'DIR') direction = typeof p.value === 'string' ? p.value.toUpperCase() : p.value; }
      apply(first); apply(second);
      if (!direction && sel && sel.kind === 'TAG') direction = sel.direction || null;
      return node('SetOrder', { selection: sel || null, inTarget, direction });
    }

  // SET RELATION TO [eExpression1 INTO nWorkArea1 | cTableAlias1
  //   [, eExpression2 INTO nWorkArea2 | cTableAlias2 ...]
  //   [IN nWorkArea | cTableAlias] [ADDITIVE]
  SetRelationToStatement
    = "SET RELATION TO"i WB _
      first:RelationPair? tail:(_ "," _ RelationPair)*
      inClause:(_ "IN"i __ target:AliasRef _ { return target; })?
      additive:(_ "ADDITIVE"i)? {
        const pairs = first ? [first, ...tail.map(t => t[3])] : [];
        return node('SetRelation', {
          pairs: pairs.map(p => ({ expression: p.expr, into: p.into })),
          inTarget: inClause,
          additive: !!(additive && additive[1])
        });
      }

  RelationPair
    = expr:Expression _ "INTO"i __ into:AliasRef {
        return { expr, into };
      }

// SET [cSetCommand] [ON | OFF | TO [eSetting]]
SetSettingStatement
  ="SET"i (Whitespace / LineContinuation)+ inner:(
    ("TO"i __ setting:Expression { return node("SetTo", { setting }); })
    / (cmd:KeywordOrIdentifier toPart:(_ "TO"i WB s:(_ e:Expression { return e; })? { return { setting: s }; })? argPart:(_ (StringLiteral / Identifier / NumberLiteral))? additive:(_ "ADDITIVE"i)? state:(_ ("ON"i / "OFF"i))? { const argument = toPart ? toPart.setting : (argPart ? argPart[1] : null); const st = state ? state[1] : null; return node("SetCommand", { command: cmd, argument: argument, cleared: !!toPart && !toPart.setting, state: st ? st.toUpperCase() : null, additive: !!additive }); })
  ) {
      // If TO form, inner is already a SetTo node and we return it directly.
      if (inner && inner.type === 'SetTo') return inner;
      // Otherwise inner is a SetCommand node; return it as the captured command node.
      return inner;
    }

// APPEND FROM FileName | ? [FIELDS FieldList] [FOR lExpression]
//   [[TYPE] [DELIMITED [WITH Delimiter | WITH BLANK | WITH TAB | WITH CHARACTER Delimiter]
//     | DIF | FW2 | MOD | PDOX | RPD | SDF | SYLK | WK1 | WK3 | WKS | WR1 | WRK | CSV | XLS | XL5 [SHEET cSheetName] | XL8 [SHEET cSheetName]]]
//   [AS nCodePage]
AppendStatement
  = "APPEND FROM"i WB _
    src:("?" { return { kind: 'PROMPT' }; } / PathOrExpression) _
    parts:(AppendFromOption _)* {
      let fields = null;
      let forExpr = null;
      let type = null;
      let codepage = null;
      for (const t of parts.map(p => p[0])) {
        if (!t) continue;
        switch (t.kind) {
          case 'FIELDS': fields = t.value; break;
          case 'FOR': forExpr = t.value; break;
          case 'TYPE': type = t.value; break;
          case 'AS': codepage = t.value; break;
        }
      }
      return node("AppendFromStatement", {
        source: src,
        fields: fields,
        for: forExpr,
        exportType: type,
        codepage: codepage
      });
    }
  / "APPEND"i WB _
    blank:("BLANK"i _)?
    inPart:("IN"i _ tableAlias:AliasRef _)?
    nomenu:("NOMENU"i _)? {
      return node("AppendStatement", {
        blank: !!blank,
        inTarget: inPart ? inPart[2] : null,
        nomenu: !!nomenu
      });
    }

// Options for APPEND FROM, order-insensitive
AppendFromOption
  = "FIELDS"i __ f:(IdentifierList / FieldsClause) { return { kind: 'FIELDS', value: f }; }
  / "FOR"i __ e:Expression { return { kind: 'FOR', value: e }; }
  / at:AppendTypeClause { return { kind: 'TYPE', value: at }; }
  / "AS"i __ cp:Expression { return { kind: 'AS', value: cp }; }

// Optional TYPE keyword before the import type
AppendTypeClause
  = ("TYPE"i _)? at:AppendType { return at; }

// Import types for APPEND FROM
AppendType
  = "DELIMITED"i _ first:AppendDelimitedOption rest:(_ AppendDelimitedOption)* {
      const opts = [first, ...rest.map(r => r[1])];
      return { format: 'DELIMITED', options: opts };
    }
  / t:("DIF"i / "FW2"i / "MOD"i / "PDOX"i / "RPD"i / "SDF"i / "SYLK"i / "WK1"i / "WK3"i / "WKS"i / "WR1"i / "WRK"i / "CSV"i / "XLS"i / "XL5"i / "XL8"i) _ sheet:("SHEET"i __ s:IdentifierOrString { return s; })? {
      return { format: (typeof t === 'string' ? t.toUpperCase() : t), sheet: sheet || null };
    }

// Allow multiple WITH options, including an unquoted single-character like *
AppendDelimitedOption
  = "WITH"i __ opt:(
      "BLANK"i { return { mode: 'BLANK' }; }
      / "TAB"i { return { mode: 'TAB' }; }
      / "CHARACTER"i __ ch:(IdentifierOrString / "*" { return "*"; }) { return { mode: 'CHARACTER', character: ch }; }
      / del:(IdentifierOrString / "*" { return "*"; }) { return { mode: 'DELIMITER', delimiter: del }; }
    ) { return opt; }

// BROWSE [FIELDS FieldList] [FONT cFontName [, nFontSize [, nFontCharSet]]] 
//    [STYLE cFontStyle] [FOR lExpression1 [REST]] [FORMAT] 
//    [FREEZE FieldName] [KEY eExpression1 [, eExpression2]] [LAST | NOINIT]
//    [LOCK nNumberOfFields] [LPARTITION] [NAME ObjectName] [NOAPPEND]
//    [NOCAPTIONS] [NODELETE] [NOEDIT | NOMODIFY] [NOLGRID] [NORGRID] 
//    [NOLINK] [NOMENU] [NOOPTIMIZE] [NOREFRESH] [NORMAL] [NOWAIT] 
//    [PARTITION nColumnNumber [LEDIT] [REDIT]]
//    [PREFERENCE PreferenceName] [SAVE] [TIMEOUT nSeconds] 
//    [TITLE cTitleText] [VALID [:F] lExpression2 [ERROR cMessageText]]
//    [WHEN lExpression3] [WIDTH nFieldWidth] [WINDOW WindowName1]
//    [IN [WINDOW] WindowName2 | IN SCREEN] [COLOR SCHEME nSchemeNumber]
BrowseStatement "browse statement"
  = "BROWSE"i WB _ parts:(BrowseOption _)* {
      let fields = null; let cond = null; let norm = false; let nowait = false;
      for (const p of parts.map(t => t[0])) {
        if (!p) continue;
        switch (p.kind) {
          case 'FIELDS': fields = p.value; break;
          case 'FOR': cond = p.value; break;
          case 'NORM': norm = true; break;
          case 'NOWAIT': nowait = true; break;
        }
      }
      return node('BrowseStatement', { fields: fields || [], for: cond || null, norm, nowait });
    }

BrowseOption
  = "FIELDS"i __ list:IdentifierList { return { kind: 'FIELDS', value: list }; }
  / "FOR"i __ e:Expression { return { kind: 'FOR', value: e }; }
  / "NORM"i { return { kind: 'NORM' }; }
  / "NOWAIT"i { return { kind: 'NOWAIT' }; }

// REPLACE [ALL | REST] FieldName1 WITH eExpression1 [ADDITIVE] [, FieldName2 WITH eExpression2 [ADDITIVE]] ... [Scope] [FOR lExpression1] [WHILE lExpression2] [IN nWorkArea | cTableAlias] [NOOPTIMIZE]
ReplaceStatement
  = "REPLACE"i WB _
    scope: ( "ALL"i { return 'ALL'; } / "REST"i { return 'REST'; })? _
    fields:ReplaceFieldList
    forClause:(_ "FOR"i __ condition:Expression _ { return condition; })?
    whileClause:(_ "WHILE"i __ condition:Expression _ { return condition; })?
    inClause:("IN"i __ target:AliasRef _ { return target; })?
    noOptimize:("NOOPTIMIZE"i)? {
      return node("ReplaceStatement", { 
        scope,
        fields, 
        forCondition: forClause,
        whileCondition: whileClause,
        inTarget: inClause,
        noOptimize: !!noOptimize
      });
    }

// SCATTER [FIELDS FieldList | FIELDS LIKE Skeleton | FIELDS EXCEPT Skeleton] [MEMO]
//   TO ArrayName [BLANK] | TO ArrayName AUTOMEM | MEMVAR [BLANK] | NAME ObjectName [BLANK | ADDITIVE]
// This is how a record becomes an object or a set of variables, so the destination is the part the rules need: TO and NAME both create the name they are handed, which makes SCATTER a write of it.
ScatterStatement
  = "SCATTER"i WB NotCallOrAssign opts:(_ ScatterOption)* {
      const o = { destination: null, name: null, fields: null, memo: false, blank: false, additive: false, autoMem: false };
      for (const part of opts.map(t => t[1])) {
        switch (part.kind) {
          case 'DEST': if (!o.destination) { o.destination = part.value; o.name = part.name; } break;
          case 'FIELDS': o.fields = part.value; break;
          case 'MEMO': o.memo = true; break;
          case 'BLANK': o.blank = true; break;
          case 'ADDITIVE': o.additive = true; break;
          case 'AUTOMEM': o.autoMem = true; break;
        }
      }
      return node("ScatterStatement", {
        destination: o.destination, name: o.name, fields: o.fields,
        memo: o.memo, blank: o.blank, additive: o.additive, autoMem: o.autoMem
      });
    }

ScatterOption
  = "MEMVAR"i WB { return { kind: 'DEST', value: 'MEMVAR', name: null }; }
  / "NAME"i WB _ n:ParameterName { return { kind: 'DEST', value: 'NAME', name: n }; }
  / "TO"i WB _ n:ParameterName { return { kind: 'DEST', value: 'ARRAY', name: n }; }
  / f:FieldsClause { return { kind: 'FIELDS', value: f }; }
  / "MEMO"i WB { return { kind: 'MEMO' }; }
  / "BLANK"i WB { return { kind: 'BLANK' }; }
  / "ADDITIVE"i WB { return { kind: 'ADDITIVE' }; }
  / "AUTOMEM"i WB { return { kind: 'AUTOMEM' }; }

// GATHER FROM ArrayName | MEMVAR | NAME ObjectName
//   [FIELDS FieldList | FIELDS LIKE Skeleton | FIELDS EXCEPT Skeleton] [MEMO]
// The mirror of SCATTER: the named object or array is read, and the record is what gets written.
GatherStatement
  = "GATHER"i WB NotCallOrAssign opts:(_ GatherOption)* {
      const o = { source: null, name: null, fields: null, memo: false };
      for (const part of opts.map(t => t[1])) {
        switch (part.kind) {
          case 'SRC': if (!o.source) { o.source = part.value; o.name = part.name; } break;
          case 'FIELDS': o.fields = part.value; break;
          case 'MEMO': o.memo = true; break;
        }
      }
      return node("GatherStatement", { source: o.source, name: o.name, fields: o.fields, memo: o.memo });
    }

GatherOption
  = "MEMVAR"i WB { return { kind: 'SRC', value: 'MEMVAR', name: null }; }
  / "NAME"i WB _ n:ParameterName { return { kind: 'SRC', value: 'NAME', name: n }; }
  / "FROM"i WB _ n:ParameterName { return { kind: 'SRC', value: 'ARRAY', name: n }; }
  / f:FieldsClause { return { kind: 'FIELDS', value: f }; }
  / "MEMO"i WB { return { kind: 'MEMO' }; }

// LOCATE [FOR lExpression1] [IN nWorkArea | cTableAlias] [WHILE lExpression2] [NOOPTIMIZE]
LocateStatement
  = "LOCATE"i WB parts:(
      _ (
        ("FOR"i __ condition:Expression { return { kind: 'FOR', value: condition }; })
      / ("ALL"i { return { kind: 'SCOPE', value: 'ALL' }; })
      / ("NEXT"i _ n:NumberLiteral { return { kind: 'SCOPE', value: { type: 'NEXT', count: n } }; })
      / ("RECORD"i _ n:NumberLiteral { return { kind: 'SCOPE', value: { type: 'RECORD', number: n } }; })
      / ("REST"i { return { kind: 'SCOPE', value: 'REST' }; })
      / ("IN"i __ target:(AliasRef / SelectCore) { return { kind: 'IN', value: target }; })
      / ("WHILE"i __ condition:Expression { return { kind: 'WHILE', value: condition }; })
      / ("NOOPTIMIZE"i { return { kind: 'NOOPTIMIZE' }; })
      )
    )* {
      let forCondition = null;
      let scope = null;
      let inTarget = null;
      let whileCondition = null;
      let noOptimize = false;
      for (const p of parts.map(t => t[1])) {
        switch (p.kind) {
          case 'FOR': if (!forCondition) forCondition = p.value; break;
          case 'SCOPE': if (!scope) scope = p.value; break;
          case 'IN': if (!inTarget) inTarget = p.value; break;
          case 'WHILE': if (!whileCondition) whileCondition = p.value; break;
          case 'NOOPTIMIZE': noOptimize = true; break;
        }
      }
      return node("LocateStatement", { forCondition, scope, inTarget, whileCondition, noOptimize });
    }

// SCAN [NOOPTIMIZE] Scope:[ALL | NEXT nRecords | RECORD nRecordNumber | REST] [FOR lExpression1] [WHILE lExpression2]
//   [Commands]
//   [LOOP]
//   [EXIT]
// ENDSCAN
// The clauses are order-free in VFP, and the app writes WHILE before FOR because the WHILE bounds the walk and the FOR is the extra filter. Reading them in a fixed order left `FOR ...` to the catch-all, which then reported a missing ENDFOR for a block that was never opened -- a false positive at error severity. `_` keeps the option list on the logical line, so the body below is never mistaken for one.
ScanStatement
  = "SCAN"i WB opts:(_ ScanOption)* __
    body:(Statement __)*
    ("ENDSCAN"i / ("LOOP"i / "EXIT"i) _? "ENDSCAN"i)? {
      const o = { noOptimize: false, scope: null, forCondition: null, whileCondition: null };
      for (const part of opts.map(t => t[1])) {
        switch (part.kind) {
          case 'NOOPTIMIZE': o.noOptimize = true; break;
          case 'SCOPE': if (!o.scope) o.scope = part.value; break;
          case 'FOR': if (!o.forCondition) o.forCondition = part.value; break;
          case 'WHILE': if (!o.whileCondition) o.whileCondition = part.value; break;
        }
      }
      return node("ScanStatement", {
        noOptimize: o.noOptimize,
        scope: o.scope || 'ALL',
        forCondition: o.forCondition,
        whileCondition: o.whileCondition,
        body: node("BlockStatement", { body: flatten(body.map(s => s[0])) })
      });
    }

ScanOption
  = "NOOPTIMIZE"i WB { return { kind: 'NOOPTIMIZE' }; }
  / "ALL"i WB { return { kind: 'SCOPE', value: 'ALL' }; }
  / "NEXT"i WB _ n:NumberLiteral { return { kind: 'SCOPE', value: { type: 'NEXT', count: n } }; }
  / "RECORD"i WB _ n:NumberLiteral { return { kind: 'SCOPE', value: { type: 'RECORD', number: n } }; }
  / "REST"i WB { return { kind: 'SCOPE', value: 'REST' }; }
  / "FOR"i WB __ c:Expression { return { kind: 'FOR', value: c }; }
  / "WHILE"i WB __ c:Expression { return { kind: 'WHILE', value: c }; }


// CALCULATE eExpressionList [Scope] [FOR lExpression1] [WHILE lExpression2]
//    [TO VarList | TO ARRAY ArrayName] [NOOPTIMIZE] [IN nWorkArea | cTableAlias]
CalculateStatement
  = ("CALCULATE"i / "Calc"i) WB __ 
    exprs:ExpressionList _
    parts:(CalcOption _)*
    {
      const opts = { scope: null, forCondition: null, whileCondition: null, to: null, noOptimize: false, inTarget: null };
      for (const p of parts.map(t => t[0])) {
        if (!p) continue;
        switch (p.kind) {
          case 'SCOPE': opts.scope = p.value; break;
          case 'FOR': opts.forCondition = p.value; break;
          case 'WHILE': opts.whileCondition = p.value; break;
          case 'TO': opts.to = p.value; break;
          case 'NOOPTIMIZE': opts.noOptimize = true; break;
          case 'IN': opts.inTarget = p.value; break;
        }
      }
      return node('CalculateStatement', { expressions: exprs, scope: opts.scope, forCondition: opts.forCondition, whileCondition: opts.whileCondition, to: opts.to, noOptimize: opts.noOptimize, inTarget: opts.inTarget });
    }

// SUM [eExpressionList]   [Scope] [FOR lExpression1] [WHILE lExpression2]
//    [TO MemVarNameList | TO ARRAY ArrayName]   [NOOPTIMIZE]
// SUM, AVERAGE and COUNT are one command with three names: the same scope, FOR/WHILE and TO tail that CALCULATE takes. COUNT simply brings no expression list, which the option loop already allows.
// `_` rather than `__` keeps the tail on the logical line, so a bare COUNT cannot reach down and read the next line's assignment as its expression list.
AggregateStatement
  = cmd:("SUM"i / "AVERAGE"i / "COUNT"i) WB NotCallOrAssign parts:(
      _ (
        (exprs:ExpressionList { return { kind: 'EXPRS', value: exprs }; })
      / (p:CalcOption { return p; })
      )
    )* {
      const opts = { scope: null, forCondition: null, whileCondition: null, to: null, noOptimize: false, inTarget: null };
      let expressions = null;
      for (const p of parts.map(t => t[1])) {
        if (!p) continue;
        if (p.kind === 'EXPRS') { expressions = p.value; continue; }
        switch (p.kind) {
          case 'SCOPE': if (!opts.scope) opts.scope = p.value; break;
          case 'FOR': if (!opts.forCondition) opts.forCondition = p.value; break;
          case 'WHILE': if (!opts.whileCondition) opts.whileCondition = p.value; break;
          case 'TO': if (!opts.to) opts.to = p.value; break;
          case 'NOOPTIMIZE': opts.noOptimize = true; break;
          case 'IN': if (!opts.inTarget) opts.inTarget = p.value; break;
        }
      }
      return node('AggregateStatement', { command: cmd.toUpperCase(), expressions: expressions, scope: opts.scope, forCondition: opts.forCondition, whileCondition: opts.whileCondition, to: opts.to, noOptimize: opts.noOptimize, inTarget: opts.inTarget });
    }

CalcOption
  = s:(
      ("ALL"i { return { kind: 'SCOPE', value: 'ALL' }; })
    / ("NEXT"i _ n:NumberLiteral { return { kind: 'SCOPE', value: { type: 'NEXT', count: n } }; })
    / ("RECORD"i _ n:NumberLiteral { return { kind: 'SCOPE', value: { type: 'RECORD', number: n } }; })
    / ("REST"i { return { kind: 'SCOPE', value: 'REST' }; })
    / ("FOR"i __ e:Expression { return { kind: 'FOR', value: e }; })
    / ("WHILE"i __ e:Expression { return { kind: 'WHILE', value: e }; })
    / ("TO"i __ to:(vars:IdentifierList { return { kind: 'TO', value: { kind: 'VARS', vars } }; } / ("ARRAY"i __ arr:Identifier { return { kind: 'TO', value: { kind: 'ARRAY', name: arr } }; })) { return to; })
    / ("NOOPTIMIZE"i { return { kind: 'NOOPTIMIZE', value: true }; })
    / ("IN"i __ target:(NumberLiteral / Identifier / StringLiteral / SelectCore) { return { kind: 'IN', value: target }; })
  ) { return s; }

ReplaceFieldList
  = head:ReplaceField tail:(_ "," _ ReplaceField)* {
      return [head, ...tail.map(t => t[3])];
    }

ReplaceField
  = field:ParameterName _ "WITH"i _ value:Expression _ additive:("ADDITIVE"i)? {
      return { field, value, additive: !!additive };
    }

// STORE eExpression TO VarNameList | ArrayNameList-or-VarName | ArrayName = eExpression
StoreStatement
  = "STORE"i WB __ expr:Expression __ "TO"i __
    toPart:(
      arr:Identifier _ "[" _ indexList:ExpressionList _ "]" { return { type: 'ArrayIndexed', array: arr, indexes: indexList }; }
      / arr:Identifier _ "(" _ indexList:ExpressionList _ ")" { return { type: 'ArrayIndexed', array: arr, indexes: indexList }; }
      / arrAssign:Identifier _ "=" _ rhs:Expression { return { type: 'ArrayAssign', target: arrAssign, expression: rhs }; }
      / vars:IdentifierList { return { type: 'VarList', vars }; }
    ) {
    return node('StoreStatement', { expression: expr, target: toPart });
  }

ExpressionList
  = head:Expression tail:(_ "," _ Expression)* { return [head, ...tail.map(t => t[3])]; }

// Procedure declarations - two styles are supported:
// 1) PROCEDURE Name [ LPARAMETERS p1, p2, ... ]   Commands [ RETURN expr ] [ ENDPROC ]
// 2) PROCEDURE Name( [ p1 [ AS type ] [, p2 [ AS type ] ... ] ) [ AS returntype ]  Commands [ RETURN expr ] [ ENDPROC ]
ProcedureStatement "procedure"
  = cw:("PROCEDURE"i / "FUNCTION"i) WB __ name:Identifier _ proc:(
      // function-style parameter list with optional typed params and optional return type
      "(" _ params:ProcedureParamList? _ ")" _ retPart:(_ "AS"i __ rt:IdentifierOrString)? __ statements:(Statement __)* end:(_ ("ENDPROC"i / "ENDFUNC"i) __)? {
        return node("ProcedureStatement", {
          name,
          isFunction: (typeof cw === 'string') ? (cw.toUpperCase() === 'FUNCTION') : false,
          parameters: params || [],
          returnType: retPart ? retPart[3] : null,
          body: node("BlockStatement", { body: flatten(statements.map(s => s[0])) }),
          lparameters: false
        });
      }
    / // alternate LPARAMETERS style (untyped, compatible with LPARAMETERS/PARAMETERS keyword)
    lparams:LParameters? __ statements:(Statement __)* end:(_ ("ENDPROC"i / "ENDFUNC"i) __)? {
        return node("ProcedureStatement", {
          name,
          isFunction: (typeof cw === 'string') ? (cw.toUpperCase() === 'FUNCTION') : false,
          parameters: lparams ? (lparams.names || []) : [],
          returnType: null,
          body: node("BlockStatement", { body: flatten(statements.map(s => s[0])) }),
          lparameters: !!lparams
        });
      }
    ) { return proc; }

ReturnStatement
  = "RETURN"i WB _ expr:Expression? _ LineTerminator? { return node("ReturnStatement", { argument: expr === undefined ? null : expr }); }


// -----------------------------
// Xbase housekeeping and output commands
// -----------------------------

// None of these command words is reserved, so each rule first refuses a call or an assignment: that keeps a variable named `list` or a call to SEEK() parsing as what it is.
NotCallOrAssign
  = !(_ ("(" / "="))

// CLEAR [ALL | CLASS cName | CLASSLIB cName | DLLS | EVENTS | FIELDS | GETS | MACROS | MEMORY
//   | MENUS | POPUPS | PROGRAM | PROMPT | READ [ALL] | RESOURCES | TYPEAHEAD | WINDOWS | DEBUG]
ClearStatement
  = "CLEAR"i WB NotCallOrAssign _ opt:ClearOption? {
      return node('ClearStatement', {
        target: opt ? opt.target : null,
        name: opt ? opt.name : null,
        all: opt ? opt.all : false
      });
    }

ClearOption
  = kw:("CLASSLIB"i / "CLASS"i) WB _ n:IdentifierOrString { return { target: kw.toUpperCase(), name: n, all: false }; }
  / "READ"i WB _ a:("ALL"i WB)? { return { target: 'READ', name: null, all: !!a }; }
  / kw:("ALL"i / "DLLS"i / "EVENTS"i / "FIELDS"i / "GETS"i / "MACROS"i / "MEMORY"i / "MENUS"i
      / "POPUPS"i / "PROGRAM"i / "PROMPT"i / "RESOURCES"i / "TYPEAHEAD"i / "WINDOWS"i / "DEBUG"i) WB {
      return { target: kw.toUpperCase(), name: null, all: false };
    }

// CLOSE ALL | ALTERNATE | DATABASES [ALL] | DEBUGGER | FORMAT | INDEXES | PROCEDURE | TABLES [ALL]
CloseStatement
  = "CLOSE"i WB NotCallOrAssign _ opt:CloseOption? {
      return node('CloseStatement', { target: opt ? opt.target : null, all: opt ? opt.all : false });
    }

CloseOption
  = kw:("ALTERNATE"i / "DATABASES"i / "DEBUGGER"i / "FORMAT"i / "INDEXES"i / "PROCEDURE"i
      / "TABLES"i / "ALL"i) WB _ a:("ALL"i WB)? {
      return { target: kw.toUpperCase(), all: !!a };
    }

// RELEASE MemVarList | ALL [EXTENDED] | ALL LIKE | EXCEPT Skeleton | WINDOWS | PROCEDURE | CLASSLIB ...
ReleaseStatement
  = "RELEASE"i WB NotCallOrAssign _ body:ReleaseBody {
      return node('ReleaseStatement', {
        scope: body.scope,
        extended: body.extended,
        mode: body.mode,
        pattern: body.pattern,
        names: body.names,
        options: body.options
      });
    }

ReleaseBody
  = "ALL"i WB _ m:("LIKE"i / "EXCEPT"i) WB _ pat:(StringLiteral / Pattern) {
      return { scope: 'ALL', extended: false, mode: m.toUpperCase(), pattern: pat, names: [], options: null };
    }
  / "ALL"i WB _ ext:("EXTENDED"i WB)? {
      return { scope: 'ALL', extended: !!ext, mode: null, pattern: null, names: [], options: null };
    }
  / kw:("WINDOWS"i / "PROCEDURE"i / "CLASSLIB"i / "LIBRARY"i / "MENUS"i / "POPUPS"i / "BAR"i / "PAD"i) WB opts:RawOptions {
      return { scope: kw.toUpperCase(), extended: false, mode: null, pattern: null, names: [], options: opts };
    }
  / names:IdentifierList {
      return { scope: null, extended: false, mode: null, pattern: null, names, options: null };
    }

// PACK [MEMO | DBF] [TableName] [IN nWorkArea | cTableAlias]
PackStatement
  = "PACK"i WB NotCallOrAssign _ what:(("MEMO"i / "DBF"i) WB)? _ tbl:IdentifierOrString? _
    inTgt:("IN"i WB __ t:AliasRef { return t; })? {
      return node('PackStatement', {
        what: what ? what[0].toUpperCase() : null,
        table: tbl || null,
        inTarget: inTgt || null
      });
    }

// SEEK eExpression [ORDER ...] [ASCENDING | DESCENDING] [IN nWorkArea | cTableAlias]
SeekStatement
  = "SEEK"i WB NotCallOrAssign _ e:Expression _ ord:OrderSpec? _ dir:(("ASCENDING"i / "DESCENDING"i) WB)? _
    inTgt:("IN"i WB __ t:AliasRef { return t; })? {
      return node('SeekStatement', {
        expression: e,
        order: ord || null,
        direction: dir ? dir[0].toUpperCase() : null,
        inTarget: inTgt || null
      });
    }

SuspendStatement
  = "SUSPEND"i WB NotCallOrAssign { return node('SuspendStatement', {}); }

ResumeStatement
  = "RESUME"i WB NotCallOrAssign { return node('ResumeStatement', {}); }

// KEYBOARD cExpression [PLAIN] [CLEAR]
KeyboardStatement
  = "KEYBOARD"i WB NotCallOrAssign _ e:Expression flags:(_ ("PLAIN"i / "CLEAR"i) WB)* {
      const names = flags.map(f => f[1].toUpperCase());
      return node('KeyboardStatement', {
        expression: e,
        plain: names.includes('PLAIN'),
        clear: names.includes('CLEAR')
      });
    }

// REPORT FORM | LABEL FORM FileName ... The option tail is long and order-free, so it is kept as
// raw source: recognising the statement is what stops the false positive.
ReportFormStatement
  = cmd:("REPORT"i / "LABEL"i) WB _ "FORM"i WB _ form:PathOrExpression opts:RawOptions {
      return node('ReportFormStatement', { command: cmd.toUpperCase(), form, options: opts });
    }

// SORT TO TableName ON FieldName [/A | /D | /C] [, ...] ... Remaining options kept as raw source.
SortStatement
  = "SORT"i WB _ "TO"i WB _ target:PathOrExpression _ "ON"i WB _ first:SortField rest:(_ "," _ SortField)* opts:RawOptions {
      return node('SortStatement', { target, fields: [first, ...rest.map(r => r[3])], options: opts });
    }

SortField
  = name:ParameterName flags:("/" [ADCadc])* {
      const f = flags.map(x => x[1].toUpperCase());
      return { name, descending: f.includes('D'), ignoreCase: f.includes('C') };
    }

// LIST | DISPLAY [subject] ... Both have large, subject-dependent option tails, kept as raw source.
ListStatement
  = cmd:("LIST"i / "DISPLAY"i) WB NotCallOrAssign _ subj:ListSubject? opts:RawOptions {
      return node('ListStatement', { command: cmd.toUpperCase(), subject: subj, options: opts });
    }

ListSubject
  = kw:("MEMORY"i / "STATUS"i / "STRUCTURE"i / "FILES"i / "DATABASE"i / "TABLES"i / "OBJECTS"i
      / "CONNECTIONS"i / "VIEWS"i / "PROCEDURES"i / "DLLS"i / "CLASSES"i / "FIELDS"i) WB {
      return kw.toUpperCase();
    }

// -----------------------------
// Lexical
// -----------------------------
Identifier
  = !Keyword pref:([@&])? name:$([a-zA-Z_][a-zA-Z0-9_]*) { return (pref ? pref : '') + name; }

KeywordOrIdentifier
  = Keyword / Identifier

// After a dot a keyword is just a name: .To, .From, .Class and .Select are all real properties, and refusing them cut the reference short and left the rest of the line to the catch-all. The dot operators are the exception, and the closing dot is what tells them apart -- `.AND.`, `.T.` and `.NULL.` are the operator or the literal, never a member, while `.Additive` and `.Note` are members.
MemberName
  = !(DotOperatorWord ".") pref:([@&])? name:$([a-zA-Z_][a-zA-Z0-9_]*) { return (pref ? pref : '') + name; }

DotOperatorWord
  = "AND"i / "OR"i / "NOT"i / "NULL"i / "T"i / "F"i / "Y"i / "N"i

// Recognized keywords to prevent them being treated as identifiers.
Keyword "keyword"
  = ("LOCAL"i       ![a-zA-Z0-9_])
  / ("PRIVATE"i     ![a-zA-Z0-9_])
  / ("PUBLIC"i      ![a-zA-Z0-9_])
  / ("PARAMETERS"i  ![a-zA-Z0-9_])
  / ("LPARAMETERS"i ![a-zA-Z0-9_])
  / ("PROCEDURE"i   ![a-zA-Z0-9_])
  / ("LOCATE"i      ![a-zA-Z0-9_])
  / ("SCAN"i        ![a-zA-Z0-9_])
  / ("SKIP"i        ![a-zA-Z0-9_])
  / ("CALCULATE"i   ![a-zA-Z0-9_])
  / ("FUNCTION"i    ![a-zA-Z0-9_])
  / ("ENDPROC"i     ![a-zA-Z0-9_])
  / ("ENDFUNC"i     ![a-zA-Z0-9_])
  / ("IF"i          ![a-zA-Z0-9_])
  / ("ELSE"i        ![a-zA-Z0-9_])
  / ("ENDIF"i       ![a-zA-Z0-9_])
  / ("AND"i         ![a-zA-Z0-9_])
  / ("OR"i          ![a-zA-Z0-9_])
  / ("NOT"i         ![a-zA-Z0-9_])
  / ("USE"i         ![a-zA-Z0-9_])
  / ("IN"i          ![a-zA-Z0-9_])
  / ("DEFINE"i      ![a-zA-Z0-9_])
  / ("INCLUDE"i     ![a-zA-Z0-9_])
  / ("DECLARE"i     ![a-zA-Z0-9_])
  / ("CLASS"i       ![a-zA-Z0-9_])
  / ("AS"i          ![a-zA-Z0-9_])
  / ("ENDDEFINE"i   ![a-zA-Z0-9_])
  / ("REPLACE"i     ![a-zA-Z0-9_])
  / ("WITH"i        ![a-zA-Z0-9_])
  / ("ADDITIVE"i    ![a-zA-Z0-9_])
  / ("STORE"i       ![a-zA-Z0-9_])
  / ("TO"i          ![a-zA-Z0-9_])
  / ("QUIT"i        ![a-zA-Z0-9_])
  / ("RETURN"i      ![a-zA-Z0-9_])
  / ("EXIT"i        ![a-zA-Z0-9_])
  / ("DO"i          ![a-zA-Z0-9_])
  / ("WHILE"i       ![a-zA-Z0-9_])
  / ("FOR"i         ![a-zA-Z0-9_])
  / ("for each"i    ![a-zA-Z0-9_])
  / ("CASE"i        ![a-zA-Z0-9_])
  / ("ENDFOR"i      ![a-zA-Z0-9_])
  // NOTE: Do not reserve NEXT globally so it can be used as an identifier in expressions.
  // / ("NEXT"i        ![a-zA-Z0-9_])
  / ("ENDDO"i       ![a-zA-Z0-9_])
  / ("LOOP"i        ![a-zA-Z0-9_])
  / ("TRY"i         ![a-zA-Z0-9_])
  / ("CATCH"i       ![a-zA-Z0-9_])
  / ("ENDTRY"i      ![a-zA-Z0-9_])
  / ("THROW"i       ![a-zA-Z0-9_])
  / ("FINALLY"i     ![a-zA-Z0-9_])
  / ("OTHERWISE"i   ![a-zA-Z0-9_])
  / ("ENDCASE"i     ![a-zA-Z0-9_])
  / ("ENDWITH"i     ![a-zA-Z0-9_])
  / ("JOIN"i        ![a-zA-Z0-9_])
  / ("FROM"i       ![a-zA-Z0-9_])
  / ("Order by"i    ![a-zA-Z0-9_])
  / ("INNER JOIN"i       ![a-zA-Z0-9_])
  / ("LEFT OUTER JOIN"i  ![a-zA-Z0-9_])
  / ("RIGHT OUTER JOIN"i ![a-zA-Z0-9_])
  / ("GROUP BY"i    ![a-zA-Z0-9_])
  / ("ON"i          ![a-zA-Z0-9_])
  / ("WHERE"i       ![a-zA-Z0-9_])
  / ("SELECT"i     ![a-zA-Z0-9_])
  / ("WITH"i       ![a-zA-Z0-9_])
  / ("WHERE"i      ![a-zA-Z0-9_])
  / ("HAVING"i     ![a-zA-Z0-9_])
  / ("UNION"i      ![a-zA-Z0-9_])
  / ("INTO"i       ![a-zA-Z0-9_])
  / ("INSERT"i     ![a-zA-Z0-9_])
  / ("UPDATE"i     ![a-zA-Z0-9_])
  / ("COPY"i       ![a-zA-Z0-9_])
  / ("CREATE"i      ![a-zA-Z0-9_])
  / ("GO"i         ![a-zA-Z0-9_])
  / ("GOTO"i       ![a-zA-Z0-9_])
  / ("RECORD"i     ![a-zA-Z0-9_])
  / ("CURSOR"i      ![a-zA-Z0-9_])
  / ("ON KEY"i      ![a-zA-Z0-9_])
  / ("ZAP"i        ![a-zA-Z0-9_])
  / ("BROWSE"i     ![a-zA-Z0-9_])
  / ("ENDSCAN"i    ![a-zA-Z0-9_])
  / ("ENDTEXT"i    ![a-zA-Z0-9_])

NumberLiteral "number"
  = "SELECT(0)"i { return node("NumberLiteral", { value: 0, raw: "SELECT(0)", currency: false });}
    / value:$("$"? ( [0-9]+ ("." [0-9]+)? / "." [0-9]+ ) ) {
      const raw = value;
      const isCurrency = raw.charAt(0) === '$';
      const num = parseFloat(isCurrency ? raw.slice(1) : raw);
      return node("NumberLiteral", { value: num, raw, currency: !!isCurrency });
    }

StringLiteral "string"
  = '"' chars:DoubleStringChar* '"' { return node("StringLiteral", { value: chars.join("") }); }
  / "'" chars:SingleStringChar* "'" { return node("StringLiteral", { value: chars.join("") }); }
  / "[" chars:BracketStringChar* "]" { return node("StringLiteral", { value: chars.join("") }); }

DoubleStringChar
  = '""' { return '"'; }
  / !'"' . { return text(); }

SingleStringChar
  = "''" { return "'"; }
  / !"'" . { return text(); }

BracketStringChar
  = "]]" { return "]"; }
  / !"]" . { return text(); }

LineTerminator
	= [\n\r\u2028\u2029]

// todo: date literal
DateTimeLiteral "datetime"
  = "{" _ d:$([^}]*) _ "}" { return node("DateTimeLiteral", { value: d.trim() }); }

// example: `s:\code\mosapi\3_3\aalib\mosapi.h` or `libs\system.app`
UnquotedPath
  = p:$([^ \t\f\v\r\n,;()+]+) { return node("Path", { path: p }); }

// If the upcoming token (up to a line terminator or , or ;) contains a plus or any spacing characters, prefer parsing an Expression instead of treating it as a path.
PathOrExpression
  = !("\"" / "'") p:UnquotedPath !(_ ("+" / "-" / "*" / "/")) { return p; }
  / "(" _ e:Expression _ ")" { return e; }
  / MacroPrefixedArg
  / Expression

LineTerminatorSequence "end of line"
  = "\n"
  / "\r\n"
  / "\r"
  / "\u2028"
  / "\u2029"

// Visual FoxPro boolean literals like .T. and .F.
BooleanLiteral "boolean"
  = b:(".T."i / ".F."i / "TRUE"i / "FALSE"i) { return node("BooleanLiteral", { value: (b.toUpperCase() === ".T.") }); }

NullLiteral "null"
  = ".NULL."i / "NULL"i { return node("NullLiteral", { }); }

// Whitespace/comments between SELECT clauses: allow both inline (&&) and full-line (*) comments
// Light whitespace/comment set used near token-sensitive locations
// A word boundary after a keyword literal, so a keyword cannot match the start of a longer identifier.
// Without it "DO"i matches the DO in DoSomething(), "SELECT"i matches SELECTED, and "USE"i matches USEr.
WB "word boundary"
  = ![a-zA-Z0-9_]

WS0
  = (Whitespace / LineContinuation / PartialLineComment / LineTerminatorSequence)*

// Rich whitespace/comments between SELECT clauses (includes full-line comments)
WSX
  = (Whitespace / LineContinuation / Comment / MacroSubstitute / LineTerminatorSequence)*

__
  = (Whitespace / LineContinuation / Comment / LineTerminatorSequence)*

_ 
  = (Whitespace / LineContinuation)*

// Continuation-aware whitespace for SELECT and FROM sections.
// - On the same physical line: allows spaces and macro substitutions.
// - If a semicolon line continuation appears: permits comments and newlines afterward.
ContSpace
  = (Whitespace / MacroSubstitute)* (LineContinuation (Whitespace / MacroSubstitute / Comment / LineTerminatorSequence)*)*

// Macro-aware lightweight spacer used inside expressions BEFORE an operator.
// Include macros so something like "expr &m OR ..." doesn't break parsing even if &m expands to an operator. We keep '_' (above) used AFTER operators so that "OR &c" still treats &c as an operand rather than being swallowed as spacing.
M_ 
  = (Whitespace / LineContinuation / MacroSubstitute)*

// Inline whitespace only (no line continuation); used to avoid swallowing semicolons
InlineWS
  = (Whitespace / MacroSubstitute)*

// using &macro allows changing the foxpro at runtime.
MacroSubstitute "macro substitution"
  = "&" name:Identifier { return node("MacroSubstitute", { name }); }

// Semicolon at end of physical line continues the logical line onto the next physical line.
LineContinuation "semicolon"
  = ";" [ \t]* (PartialLineComment? LineTerminatorSequence / LineTerminatorSequence / !.)

Whitespace "whitespace"
  = [ \t\f\v]+ 

EmptyLine "empty line"
	= __ LineTerminator

Comment "comment"
  = PartialLineComment
  / FullLineComment

PartialLineComment "&& comment"
  = "&&" (!LineTerminator .)*

FullLineComment "* comment"
  = [ \t]* "*" (!LineTerminator .)*

EOS
  = _ PartialLineComment? LineTerminatorSequence
  // / __ EOF
  / __

EOF "end of file"
	= !.
