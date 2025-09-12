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
  = __ statements:SourceElements __ 
  { return node("Program", { body: statements.body }); }

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
  / UseStatement
  / AppendStatement
  / CalculateStatement
  / SumStatement
  / CopyStatement
  / EraseStatement
  / SetStatement
  / OnKeyStatement
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
  / IfStatement
  / EvalStatement
  / WithStatement
  / BrowseStatement
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
  = "LOCAL"i _ "ARRAY"i _ arrs:ArrayDeclList { return arrs; }
  / "LOCAL"i _ entries:LocalEntryList {  return entries; }

// A comma-separated list of local entries (variables or arrays)
LocalEntryList
  = head:LocalEntry tail:(_ "," _ LocalEntry)* { return [head, ...tail.map(t => t[3])]; }

// An entry is either a variable declaration or an array declaration
LocalEntry
  = ArrayDecl
  / VarDecl

// Variable declaration: name [ AS type [ OF ClassLib ] ]
VarDecl
  = name:ParameterName _ asPart:(_ "AS"i __ t:Identifier _ ofPart:(_ "OF"i _ cl:Identifier { return cl; })? { return { type: t, of: ofPart ? ofPart[2] : null }; })? {
      return node("LocalDeclaration", { name, type: asPart ? asPart.type : null, ofClass: asPart ? asPart.of : null });
    }

// Array declaration: ArrayName( nRows [, nColumns ] ) [ AS type [ OF ClassLib ] ]
ArrayDecl
  = name:Identifier _ "(" _ rows:Expression _ cols:(_ "," _ Expression)? _ ")" _ asPart:(_ "AS"i __ t:Identifier _ ofPart:(_ "OF"i _ cl:Identifier { return cl; })? { return { type: t, of: ofPart ? ofPart[2] : null }; })? {
      return node("LocalArrayDeclaration", { name, rows, columns: cols ? cols[2] : null, type: asPart ? asPart.type : null, ofClass: asPart ? asPart.of : null });
    }

ArrayDeclList
  = head:ArrayDecl tail:(_ "," _ ArrayDecl)* { return [head, ...tail.map(t => t[3])]; }

PrivateStatement
  = "PRIVATE"i _ (
      "ALL"i _ "LIKE"i _ p:(StringLiteral / Pattern) {
        const pat = (typeof p === 'string') ? p : (p && p.value ? p.value : p);
        return node("PrivateAllLike", { pattern: pat });
      }
      / "ALL"i { return node("PrivateAll", {}); }
      / vars:IdentifierList{ return vars.map(v => node("PrivateDeclaration", { name: v })); }
      / _ { return node("PrivateDirective", {}); }
  )

PublicStatement
  = "PUBLIC"i _ vars:IdentifierList {
      return vars.map(v => node("PublicDeclaration", { name: v }));
    }

LParameters
  = ("LPARAMETERS"i / "PARAMETERS"i) _ vars:ParameterList {
      return node("ParametersDeclaration", { names: vars });
    }

// DIMENSION ArrayName(nRows [, nColumns]) [AS cType] [, ArrayName2(...)] ...
DimensionStatement
  = "DIMENSION"i __ first:DimensionItem tail:(_ "," _ DimensionItem)* {
      const items = [first, ...tail.map(t => t[3])];
      return node("DimensionStatement", { items });
    }

DimensionItem
  = name:Identifier _ "(" _ rows:Expression _ cols:(_ "," _ Expression)? _ ")" _ asPart:("AS"i __ t:IdentifierOrString)? {
      return { name, rows, columns: cols ? cols[2] : null, asType: asPart ? asPart[2] : null };
    }
  / name:Identifier _ "[" _ rows:Expression _ cols:(_ "," _ Expression)? _ "]" _ asPart:("AS"i __ t:IdentifierOrString)? {
      return { name, rows, columns: cols ? cols[2] : null, asType: asPart ? asPart[2] : null };
    }

IdentifierList
  = head:ParameterName tail:(_ "," _ ParameterName)* {
      return [head, ...tail.map(t => t[3])];
    }

// Parameter names can be dotted (e.g. m.UserName). Allow an optional leading
// @ or & immediately before the identifier (e.g. @var, &var or @m.User).
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
      ("." / "->") _ prop:Identifier { return { type: 'member', prop: prop }; }
    / "[" _ idxs:ExpressionList _ "]" { return { type: 'index', indexes: idxs }; }
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
  = ("?" / "PRINT"i / "WAIT WINDOW"i) _ args:ExpressionList {
      return node("PrintStatement", { arguments: args, argument: (args && args.length) ? args[0] : null });
    }

// USE [[DatabaseName!] TableName | SQLViewName | ?]
//  [IN nWorkArea | cTableAlias] [ONLINE] [ADMIN] [AGAIN]
//  [NOREQUERY [nDataSessionNumber]] [NODATA] 
//  [INDEX IndexFileList | ? [ORDER [nIndexNumber | IDXFileName 
//  | [TAG] TagName [OF CDXFileName] [ASCENDING | DESCENDING]]]]
//  [ALIAS cTableAlias] [EXCLUSIVE] [SHARED] [NOUPDATE] 
//  [CONNSTRING cConnectionString | nStatementHandle ]
UseStatement
  = "USE "i _
    tgt:UseTarget? _
    parts:(UseOption _)*
    {
      const opts = { inTarget:null, online:false, admin:false, again:false, norequery:false, dataSession:null, nodata:false, index:null, alias:null, exclusive:false, shared:false, noUpdate:false, connection:null };
      for (const p of parts) {
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
  / "USE"i

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
  / "ALIAS"i __ a:IdentifierOrString { return { kind: 'ALIAS', value: a }; }
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
      n:Expression { return { kind: 'NUMBER', value: n }; }
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

// ON KEY [ = expN] [command]
OnKeyStatement
  = "ON KEY"i _ eq:(_ "=" _ e:Expression { return e; })? _ cmd:Expression? _ LineTerminator? {
      return node('OnKeyStatement', { keyExpression: eq ? eq[1] : null, command: cmd || null });
    }

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

// Example: DEFINE CLASS myhandler AS Session
DefineClass
  = "DEFINE CLASS"i _ name:Identifier _ "AS"i _ base:Identifier __
    statements:(Statement __)*
    "ENDDEFINE"i {
      return node("DefineClass", { name, base: base || null, body: flatten(statements.map(s => s[0])) });
    }

// DECLARE [cFunctionType] FunctionName IN LibraryName [AS AliasName] [cParamType1 [@] ParamName1, cParamType2 [@] ParamName2, ...]
DeclareStatement
  = "DECLARE"i _
    cFunctionType:("SHORT"i / "LONG"i / "INTEGER"i / "SINGLE"i / "DOUBLE"i / "STRING"i / "OBJECT"i)? _
    functionName:Identifier _ 
    "IN"i _ 
    libraryName:Identifier _ 
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

// Exponentiation (^) - right-associative and tighter than unary so that
// -2^2 parses as -(2^2) which matches typical VFP/SQL semantics.
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

CastExpression
  = "CAST"i _ "(" _ e:Expression _ "AS"i _ t:TypeSpec _ ")" { return node("CastExpression", { expression: e, to: t }); }

// EXISTS (subquery)
ExistsExpression
  = "EXISTS"i _ "(" _ sq:SelectStatement _ ")" { return node("ExistsExpression", { argument: sq }); }

// Postfix expressions: allow chaining of member access (.prop) and call expressions (args)
PostfixExpression
  = head:Primary tail:(
      ("." / "->") _ prop:Identifier { return { type: 'member', prop } }
      / "(" _ args:ArgumentList? _ ")" { return { type: 'call', args: args || [] } }
      / "[" _ idxs:ExpressionList _ "]" { return { type: 'index', indexes: idxs }; }
    )*
    {
      let expr = head;
      for (const t of tail) {
        if (t.type === 'member') {
          expr = node("MemberExpression", { object: expr, property: node("Identifier", { name: t.prop }) });
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
  = expr:PostfixExpression { return node("ExpressionStatement", { expression: expr }); }

// Leading equals can be used to evaluate/call an expression as a statement, e.g. "=func()"
EvalStatement "equals-expression statement"
  = "=" _ expr:Expression { return node("ExpressionStatement", { expression: expr }); }

IfStatement "if statement"
  = "IF"i __ test:Expression __ 
    consequent:(Statement __)*
    "ELSE"i __
    alternate:(Statement __)*
    "ENDIF"i __
    {
      return node("IfStatement", { test, consequent: node("BlockStatement", { body: flatten(consequent.map(s => s[0])) }), alternate: node("BlockStatement", { body: flatten(alternate.map(s => s[0])) }) });
    }
    / "IF"i __ test:Expression __ 
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
  = "SELECT"i WS0
    quant:("ALL"i / "DISTINCT"i)? WS0
    top:("TOP"i WS0 n:Expression WS0 percent:("PERCENT"i)? { return { count: n, percent: !!percent }; })? WS0
    list:(!("FROM"i ![a-zA-Z0-9_]
          / "WITH"i ![a-zA-Z0-9_]
          / "WHERE"i ![a-zA-Z0-9_]
          / "GROUP BY"i ![a-zA-Z0-9_] 
          / "HAVING"i ![a-zA-Z0-9_] 
          / "ORDER BY"i ![a-zA-Z0-9_] 
          / "INTO"i ![a-zA-Z0-9_] 
          / "UNION"i ![a-zA-Z0-9_]) SelectList)?
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
      ("TABLE"i _ p:PathOrExpression { return { kind: 'TABLE', name: p }; })
      / ("CURSOR"i _ a:Expression flags:(_("READWRITE"i / "NOFILTER"i))* { return { kind: 'CURSOR', name: a }; })
      / ("ARRAY"i _ a:Identifier { return { kind: 'ARRAY', name: a }; })
      / ("DBF"i _ n:IdentifierOrString { return { kind: 'DBF', name: n }; })
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
  = action:("COPY FILE"i / "RENAME"i) _ src:PathOrExpression _ "TO"i _ dst:PathOrExpression {
      return node(action === 'COPY FILE' ? 'CopyFileStatement' : 'RenameStatement', { source: src, destination: dst });
    }

CopyToStatement
  = "COPY TO"i _
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
        type: t || null,
        codepage: ascp || null
      });
    }
  
// ERASE FileName | ? [RECYCLE]
EraseStatement
  = "ERASE"i _ target:(PathOrExpression / "?") _ recycle:(_ "RECYCLE"i)? {
      const tgt = (typeof target === 'string' && target === '?') ? { kind: 'PROMPT' } : target;
      return node('EraseStatement', { target: tgt, recycle: !!(recycle && recycle[1]) });
    }

DatabaseClause
  = "DATABASE"i __ db:IdentifierOrString _ name:("NAME"i __ ln:IdentifierOrString { return ln; })? { return { database: db, longName: name || null }; }

FieldsClause
  = "FIELDS"i __ (
      list:IdentifierList { return { kind: 'list', fields: list }; }
      / "LIKE"i __ sk:Pattern { return { kind: 'like', pattern: sk }; }
      / "EXCEPT"i __ sk:Pattern { return { kind: 'except', pattern: sk }; }
    )

WithIndexClause
  = ("WITH"i _)? kind:("CDX"i / "PRODUCTION"i) { return typeof kind === 'string' ? kind.toUpperCase() : kind; }

// INDEX ON eExpression TO IDXFileName | TAG TagName [BINARY]
//    [COLLATE cCollateSequence] [OF CDXFileName] [FOR lExpression]
//    [COMPACT] [ASCENDING | DESCENDING] [UNIQUE | CANDIDATE] [ADDITIVE]
// Options may appear in any order.
IndexOnStatement "index on statement"
  = "INDEX ON"i __ expr:Expression parts:(_ IndexOnPart)* {
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
  = "WITH"i __ (
      "BLANK"i { return { mode: 'BLANK' }; }
      / "TAB"i { return { mode: 'TAB' }; }
      / "CHARACTER"i __ ch:IdentifierOrString { return { mode: 'CHARACTER', delimiter: ch }; }
      / del:IdentifierOrString { return { mode: 'DELIMITER', delimiter: del }; }
    )

// -----------------------------
// GO / GOTO (record navigation)
// -----------------------------

GoToStatement "go/goto statement"
  = cmd:("GO"i / "GOTO"i) _
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
  = "IN"i _ target:(Identifier / StringLiteral / NumberLiteral / SelectCore) { return target; }

// SKIP [nRecords] [IN nWorkArea | cTableAlias]
SkipStatement
  = "SKIP"i _ n:Expression? _ 
  inPart:(_ "IN"i __ target:(NumberLiteral / PathOrExpression) { return target; })? 
  {
    return node('SkipStatement', { count: n || null, inTarget: inPart ? inPart[2] : null });
  }

// UNLOCK [RECORD nRecordNumber] [IN nWorkArea | cTableAlias] [ALL]
UnlockStatement
  = "UNLOCK"i _
    rec:(_ "RECORD"i __ n:Expression { return n; })?
    _ inPart:(_ "IN"i __ target:(NumberLiteral / Identifier / StringLiteral) { return target; })?
    _ all:("ALL"i)? {
      return node('UnlockStatement', { record: rec ? rec[2] : null, inTarget: inPart ? inPart[2] : null, all: !!all });
    }

// Opt 1: INSERT INTO dbf_name [(FieldName1 [, FieldName2, ...])]
//    VALUES (eExpression1 [, eExpression2, ...])
// Opt 2: INSERT INTO dbf_name FROM ARRAY ArrayName | FROM MEMVAR | FROM NAME ObjectName
// Opt 3: INSERT INTO dbf_name [(FieldName1 [, FieldName2, ...])]
//    SELECT SELECTClauses [UNION UnionClause SELECT SELECTClauses ...]
InsertStatement
  = "INSERT"i __ "INTO"i __ target:PathOrExpression _
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
  = "UPDATE"i _ target:IdentifierOrString WS0
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
  = "DELETE"i _ sel:(
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
  / "DELETE"i _ 
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
  = "ZAP"i _ inPart:(_ "IN"i __ target:(NumberLiteral / Identifier / StringLiteral) { return target; })? {
    return node('ZapStatement', { inTarget: inPart ? inPart[2] : null });
  }

// RECALL [Scope] [FOR lExpression1] [WHILE lExpression2] [NOOPTIMIZE]
//    [IN nWorkArea | cTableAlias]
RecallStatement
  = "RECALL"i _
    scope:(!("IN"i) IdentifierOrString)? _
    forp:(("FOR"i __ fexp:Expression { return fexp; }))? _
    whilep:(("WHILE"i __ wexp:Expression { return wexp; }))? _
    noopt:("NOOPTIMIZE"i)? _
    inPart:(_ "IN"i __ target:(NumberLiteral / Identifier / StringLiteral) { return target; })?
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
  = "FOR"i _ 
    varName:ParameterName _ "=" _ init:Expression _ "TO"i _ final:Expression _ 
    step:("STEP"i _ inc:Expression)? __
    body:(Statement __)*
    ("ENDFOR"i / "NEXT"i) 
    {
      return node("ForStatement", {
        variable: varName,
        init,
        final,
        step: step ? step[2] : null,
        body: node("BlockStatement", { body: flatten(body.map(s => s[0])) })
      });
    }

// FOR EACH Var [AS Type [OF Class-Library]] IN Group [FOXOBJECT]
//   Commands
// [EXIT]
// [LOOP]
// ENDFOR | NEXT [Var]
ForEachLoop "for-each loop"
  = "FOR EACH"i _ varName:ParameterName _
    typePart:("AS"i _ type:Identifier _ ofPart:("OF"i _ clslib:Identifier _ { return { library: clslib }; })? {
      return { typing: type, of: ofPart || null };
    })?
    "IN"i _ group:Expression foxobj:(_ "FOXOBJECT"i)? __
    // Avoid consuming ENDFOR/NEXT as part of the body when NEXT isn't reserved globally
    body:((!("ENDFOR"i / "NEXT"i) Statement) __)*
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
        body: node("BlockStatement", { body: flatten(body.map(s => s[0])) })
      });
    }

// DO WHILE lExpression Commands [LOOP] [EXIT] ENDDO
DoWhileLoop "do-while loop"
  = "DO WHILE"i _ test:Expression __
    body:(Statement __)*
    "ENDDO"i {
      return node("DoWhileStatement", {
        test,
        body: node("BlockStatement", { body: flatten(body.map(s => s[0])) })
      });
    }

// DO CASE CASE lExpression1 [Commands] ... [OTHERWISE Commands] ENDCASE
DoCaseStatement "do case statement"
  = "DO CASE"i __
  cases:(CaseClause)*
  otherwise:("OTHERWISE"i __ othBody:(Statement __)* { return node('BlockStatement', { body: flatten(othBody.map(s => s[0])) }); })?
  "ENDCASE"i {
      return node('DoCaseStatement', { 
        cases: cases.map(c => c[0]), 
        otherwise: otherwise ? otherwise : null 
      });
    }

CaseClause
  = "CASE"i _ test:Expression __ 
    consequent:(Statement __)* {
      return node('CaseClause', { 
        test, 
        consequent: node('BlockStatement', { body: flatten(consequent.map(s => s[0])) }) 
      });
    }

// DO FORM FormName | ? [NAME VarName [LINKED]] [WITH cParameterList]
//  [TO VarName] [NOREAD] [NOSHOW]
DoFormStatement "do form statement"
  = "DO FORM"i _ target:(StringLiteral / Identifier / "?") _
    namePart:("NAME"i _ nameIdent:ParameterName _ link:("LINKED"i)? )?
    withPart:("WITH"i _ params:ArgumentList maybeTo:(_ "TO"i _ v:ParameterName)? )?
    toPart:("TO"i _ v:ParameterName)?
    flags:(_ ("NOREAD"i / "NOSHOW"i))* {
      // If a TO clause was attached directly after WITH's argument list, prefer it.
      const toFromWith = (withPart && withPart[3]) ? withPart[3][2] : null;
      const explicitTo = toPart ? toPart[2] : null;
      return node("DoFormStatement", {
        target,
        name: namePart ? namePart[2] : null,
        linked: namePart ? !!(namePart[3]) : false,
        arguments: withPart ? withPart[2] : [],
        to: toFromWith || explicitTo || null,
        noread: flags ? flags.some(f => f[1].toUpperCase() === 'NOREAD') : false,
        noshow: flags ? flags.some(f => f[1].toUpperCase() === 'NOSHOW') : false
      });
    }

DoStatement "do statement"
  = "DO"i _ 
    target:(!("FORM"i ![A-Za-z0-9_] / "CASE"i ![A-Za-z0-9_]) PathOrExpression) _
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
  = ("EXIT"i / "QUIT"i) { return node("ExitStatement", {}); }

ContinueStatement "continue (LOOP)"
  = "LOOP"i { return node("ContinueStatement", {}); }

// -----------------------------
// CREATE TABLE/DBF/CURSOR
// -----------------------------
CreateStatement "create statement"
  = "CREATE"i _ 
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
      / "UNIQUE"i _ coll:("COLLATE"i __ cs:IdentifierOrString { return cs; })? { return { primaryKey: false, unique: true, collate: coll ? coll[2] : null }; }
    )? _
    refs:("REFERENCES"i __ tbl:IdentifierOrString _ tag:("TAG"i __ tn:Identifier { return tn; })? { return { table: tbl, tag: tag ? tag[2] : null }; })? _
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
  / "UNIQUE"i __ expr:Expression __ "TAG"i __ tag:Identifier _ coll:("COLLATE"i __ cs:IdentifierOrString { return cs; })? { return node('TableConstraint', { kind: 'UNIQUE', expression: expr, tag, collate: coll ? coll[2] : null }); }
  / "FOREIGN"i __ "KEY"i __ expr:Expression __ "TAG"i __ tag:Identifier _ nodup:("NODUP"i)? _ coll:("COLLATE"i __ cs:IdentifierOrString { return cs; })? __ "REFERENCES"i __ tbl:IdentifierOrString _ reftag:("TAG"i __ rt:Identifier { return rt; })? { return node('TableConstraint', { kind: 'FOREIGN KEY', expression: expr, tag, nodup: !!nodup, collate: coll ? coll[2] : null, references: { table: tbl, tag: reftag ? reftag[2] : null } }); }
  / "CHECK"i __ expr:Expression _ err:("ERROR"i __ msg:StringLiteral)? { return node('TableConstraint', { kind: 'CHECK', expression: expr, error: err ? err[2] : null }); }


// TRY [ tryCommands ] [ CATCH [ TO VarName ] [ WHEN lExpression ] [ catchCommands ] ] [ THROW [ eUserExpression ] ] [ EXIT ] [ FINALLY [ finallyCommands ] ] ENDTRY
TryStatement "try-catch statement"
  = "TRY"i __
    tstmts:(Statement __)*
    cpart:(
      "CATCH"i 
      toVar:(_ "TO"i _ v:Identifier { return v; })?
      whenPart:(_ "WHEN"i __ wexpr:Expression { return wexpr; })? 
      __
      cstmts:(Statement __)* {
        return { to: toVar ? toVar[2] : null, when: whenPart ? whenPart[2] : null, body: flatten(cstmts.map(s => s[0])) };
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
  = "WITH"i _ target:(LValue / PostfixExpression)
    asPart:(_ "AS"i __ t:Identifier _ ofPart:(_ "OF"i _ cl:Identifier { return cl; })? )? __
    body:(WithBodyEntry __)*
    "ENDWITH"i {
      return node("WithStatement", {
        target,
        asType: asPart ? asPart[2] : null,
        ofClass: asPart && asPart[4] ? asPart[4][2] : null,
        body: node("BlockStatement", { body: flatten(body.map(b => b[0])) })
      });
    }

WithBodyEntry
  = "." _? a:DotAssignment { return a; }
    / "." _? e:PostfixExpression { return node("ExpressionStatement", { expression: e }); }
    / c:Statement { return c; }

// Support assignments where the left side may contain call/member chains, e.g.
//   .Objects(n).Style = 1
DotAssignment
  = lhs:PostfixExpression __ "=" __ expr:Expression {
      return node("Assignment", { target: lhs, expression: expr });
    }

// -----------------------------
// Unknown/catch-all statement
// -----------------------------
// Captures a single logical line (respecting semicolon continuations) that didn't
// match any known statement. Protects block delimiters so structured constructs
// (IF/DO WHILE/FOR/TRY/DEFINE/WITH) can still recognize their endings.
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
  = "SET ORDER TO"i _
    sel:(
      n:NumberLiteral { return { kind: 'NUMBER', value: n }; }
      / f:(IdentifierOrString / UnquotedPath) { return { kind: 'FILE', value: f }; }
      / t:TagSpec { return { kind: 'TAG', tag: t.tag, of: t.of, direction: t.direction }; }
    )?
    _ first:( _ ("IN"i __ target:(Identifier / StringLiteral / NumberLiteral) { return { kind: 'IN', value: target }; } 
             / dir:("ASCENDING"i / "DESCENDING"i / "ASC"i / "DESC"i) { return { kind: 'DIR', value: dir }; }) )?
    second:( _ ("IN"i __ target:(Identifier / StringLiteral / NumberLiteral) { return { kind: 'IN', value: target }; } 
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
    = "SET RELATION TO"i _
      first:RelationPair? tail:(_ "," _ RelationPair)*
      inClause:(_ "IN"i __ target:(Identifier / StringLiteral / NumberLiteral) _)?
      additive:(_ "ADDITIVE"i)? {
        const pairs = first ? [first, ...tail.map(t => t[3])] : [];
        return node('SetRelation', {
          pairs: pairs.map(p => ({ expression: p.expr, into: p.into })),
          inTarget: inClause ? inClause[2] : null,
          additive: !!(additive && additive[1])
        });
      }

  RelationPair
    = expr:Expression _ "INTO"i __ into:(Identifier / NumberLiteral / StringLiteral) {
        return { expr, into };
      }

// SET [cSetCommand] [ON | OFF | TO [eSetting]]
SetSettingStatement
  ="SET"i (Whitespace / LineContinuation)+ inner:(
    ("TO"i __ setting:Expression { return node("SetTo", { setting }); })
    / (cmd:KeywordOrIdentifier toPart:(_ "TO"i __ setting:Expression)? argPart:(_ (StringLiteral / Identifier / NumberLiteral))? additive:(_ "ADDITIVE"i)? state:(_ ("ON"i / "OFF"i))? { const argument = toPart ? toPart[2] : (argPart ? argPart[1] : null); const st = state ? state[1] : null; return node("cSetCommand", { command: cmd, argument: argument, state: st ? st.toUpperCase() : null, additive: !!additive }); })
  ) {
      // If TO form, inner is already a SetTo node and we return it directly.
      if (inner && inner.type === 'SetTo') return inner;
      // Otherwise inner is a cSetCommand node; return it as the captured command node.
      return inner;
    }

// APPEND FROM FileName | ? [FIELDS FieldList] [FOR lExpression]
//   [[TYPE] [DELIMITED [WITH Delimiter | WITH BLANK | WITH TAB | WITH CHARACTER Delimiter]
//     | DIF | FW2 | MOD | PDOX | RPD | SDF | SYLK | WK1 | WK3 | WKS | WR1 | WRK | CSV | XLS | XL5 [SHEET cSheetName] | XL8 [SHEET cSheetName]]]
//   [AS nCodePage]
AppendStatement
  = "APPEND FROM"i _
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
        type: type,
        codepage: codepage
      });
    }
  / "APPEND"i _
    blank:("BLANK"i _)?
    inPart:("IN"i _ tableAlias:(Identifier / StringLiteral / NumberLiteral) _)?
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
  = "WITH"i __ (
      "BLANK"i { return { mode: 'BLANK' }; }
      / "TAB"i { return { mode: 'TAB' }; }
      / "CHARACTER"i __ ch:(IdentifierOrString / "*" { return "*"; }) { return { mode: 'CHARACTER', character: ch }; }
      / del:(IdentifierOrString / "*" { return "*"; }) { return { mode: 'DELIMITER', delimiter: del }; }
    )

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
  = "BROWSE"i _ parts:(BrowseOption _)* {
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
  = "REPLACE"i _
    scope: ( "ALL"i { return 'ALL'; } / "REST"i { return 'REST'; })? _
    fields:ReplaceFieldList
    forClause:(_ "FOR"i __ condition:Expression _)?
    whileClase:(_ "WHILE"i __ condition:Expression _)?
    inClause:("IN"i __ target:(Identifier / StringLiteral / NumberLiteral) _)?
    noOptimize:("NOOPTIMIZE"i)? {
      return node("ReplaceStatement", { 
        scope: scope ? scope[0] : null,
        fields, 
        forCondition: forClause ? forClause[2] : null,
        whileCondition: whileClase ? whileClase[2] : null,
        inTarget: inClause ? inClause[2] : null,
        noOptimize: !!noOptimize
      });
    }

// LOCATE [FOR lExpression1] [IN nWorkArea | cTableAlias] [WHILE lExpression2] [NOOPTIMIZE]
LocateStatement
  = "LOCATE"i parts:(
      _ (
        ("FOR"i __ condition:Expression { return { kind: 'FOR', value: condition }; })
      / ("ALL"i { return { kind: 'SCOPE', value: 'ALL' }; })
      / ("NEXT"i _ n:NumberLiteral { return { kind: 'SCOPE', value: { type: 'NEXT', count: n } }; })
      / ("RECORD"i _ n:NumberLiteral { return { kind: 'SCOPE', value: { type: 'RECORD', number: n } }; })
      / ("REST"i { return { kind: 'SCOPE', value: 'REST' }; })
      / ("IN"i __ target:(Identifier / StringLiteral / NumberLiteral / SelectCore) { return { kind: 'IN', value: target }; })
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
ScanStatement
  = "SCAN"i _
    noopt:("NOOPTIMIZE"i _)?
    scope:(
      ("ALL"i { return 'ALL'; })
      / ("NEXT"i _ n:NumberLiteral { return { type: 'NEXT', count: n }; })
      / ("RECORD"i _ n:NumberLiteral { return { type: 'RECORD', number: n }; })
      / ("REST"i { return 'REST'; })
    )? _
    forClause:(_ "FOR"i __ condition:Expression)?
    whileClause:(_ "WHILE"i __ condition:Expression)?
    __
    body:(Statement __)*
    endkw:("ENDSCAN"i / ("LOOP"i / "EXIT"i) _? "ENDSCAN"i)? {
      return node("ScanStatement", {
        noOptimize: !!(noopt && noopt[1]),
        scope: scope || 'ALL',
        forCondition: forClause ? forClause[2] : null,
        whileCondition: whileClause ? whileClause[2] : null,
        body: node("BlockStatement", { body: flatten(body.map(s => s[0])) })
      });
    }


// CALCULATE eExpressionList [Scope] [FOR lExpression1] [WHILE lExpression2]
//    [TO VarList | TO ARRAY ArrayName] [NOOPTIMIZE] [IN nWorkArea | cTableAlias]
CalculateStatement
  = ("CALCULATE"i / "Calc"i) __ 
    exprs:ExpressionList _
    parts:(CalcOption _)*
    {
      const opts = { scope: null, forCondition: null, whileCondition: null, to: null, noOptimize: false, inTarget: null };
      for (const p of parts) {
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
SumStatement
  = ("SUM"i) __? parts:(
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
      return node('SumStatement', { expressions: expressions, scope: opts.scope, forCondition: opts.forCondition, whileCondition: opts.whileCondition, to: opts.to, noOptimize: opts.noOptimize, inTarget: opts.inTarget });
    }

CalcOption
  = s:(
      ("ALL"i { return { kind: 'SCOPE', value: 'ALL' }; })
    / ("NEXT"i _ n:NumberLiteral { return { kind: 'SCOPE', value: { type: 'NEXT', count: n } }; })
    / ("RECORD"i _ n:NumberLiteral { return { kind: 'SCOPE', value: { type: 'RECORD', number: n } }; })
    / ("REST"i { return { kind: 'SCOPE', value: 'REST' }; })
    / ("FOR"i __ e:Expression { return { kind: 'FOR', value: e }; })
    / ("WHILE"i __ e:Expression { return { kind: 'WHILE', value: e }; })
    / ("TO"i __ (vars:IdentifierList { return { kind: 'TO', value: { kind: 'VARS', vars } }; } / ("ARRAY"i __ arr:Identifier { return { kind: 'TO', value: { kind: 'ARRAY', name: arr } }; })))
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
  = "STORE"i __ expr:Expression __ "TO"i __
    toPart:(
      vars:IdentifierList { return { type: 'VarList', vars }; }
      / arr:Identifier "[" _ indexList:ExpressionList _ "]" { return { type: 'ArrayIndexed', array: arr, indexes: indexList }; }
      / arrAssign:Identifier _ "=" _ rhs:Expression { return { type: 'ArrayAssign', target: arrAssign, expression: rhs }; }
    ) {
    return node('StoreStatement', { expression: expr, target: toPart });
  }

ExpressionList
  = head:Expression tail:(_ "," _ Expression)* { return [head, ...tail.map(t => t[3])]; }

// Procedure declarations - two styles are supported:
// 1) PROCEDURE Name [ LPARAMETERS p1, p2, ... ]   Commands [ RETURN expr ] [ ENDPROC ]
// 2) PROCEDURE Name( [ p1 [ AS type ] [, p2 [ AS type ] ... ] ) [ AS returntype ]  Commands [ RETURN expr ] [ ENDPROC ]
ProcedureStatement "procedure"
  = cw:("PROCEDURE"i / "FUNCTION"i) __ name:Identifier _ (
      // function-style parameter list with optional typed params and optional return type
      "(" _ params:ProcedureParamList? _ ")" _ retPart:(_ "AS"i __ rt:IdentifierOrString)? __ statements:(Statement __)* ret:(_ "RETURN"i __ expr:Expression _)? end:(_ "ENDPROC"i __)? {
        return node("ProcedureStatement", {
          name,
          isFunction: (typeof cw === 'string') ? (cw.toUpperCase() === 'FUNCTION') : false,
          parameters: params || [],
          returnType: retPart ? retPart[3] : null,
          body: node("BlockStatement", { body: flatten(statements.map(s => s[0])) }),
          returnExpression: ret ? ret[2] : null,
          lparameters: false
        });
      }
    / // alternate LPARAMETERS style (untyped, compatible with LPARAMETERS/PARAMETERS keyword)
    lparams:LParameters? __ statements:(Statement __)* ret:(_ "RETURN"i __ expr:Expression _)? end:(_ ("ENDPROC"i / "ENDFUNC"i) __)? {
        return node("ProcedureStatement", {
          name,
      isFunction: false,
      parameters: lparams ? (lparams.names || []) : [],
          returnType: null,
          body: node("BlockStatement", { body: flatten(statements.map(s => s[0])) }),
          returnExpression: ret ? ret[2] : null,
          lparameters: !!lparams
        });
      }
    )

ReturnStatement
  = "RETURN"i _ expr:Expression? _ LineTerminator? { return node("ReturnStatement", { argument: expr === undefined ? null : expr }); }

// -----------------------------
// Lexical
// -----------------------------
Identifier
  = !Keyword pref:([@&])? name:$([a-zA-Z_][a-zA-Z0-9_]*) { return (pref ? pref : '') + name; }

KeywordOrIdentifier
  = Keyword / Identifier

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

// If the upcoming token (up to a line terminator or , or ;) contains a plus or any
// spacing characters, prefer parsing an Expression instead of treating it as a path.
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
// Include macros so something like "expr &m OR ..." doesn't break parsing even if &m
// expands to an operator. We keep '_' (above) used AFTER operators so that "OR &c"
// still treats &c as an operand rather than being swallowed as spacing.
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
