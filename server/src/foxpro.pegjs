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
Program
  = __ statements:(Statement __)* EOF {
      return node("Program", { body: flatten(statements.map(s => s[0])) });
    }

// A Statement returns either a single AST node or an Array of nodes (e.g. multiple LOCAL vars)
Statement "statement"
  = s:( 
    LocalStatement
    / PrivateStatement
    / PublicStatement
    / DimensionStatement
    / DeclareStatement
    / TryStatement
    / CreateStatement
    / IndexOnStatement
    / DefineClass
    / LParameters
    / PrintStatement
    / UseStatement
    / AppendStatement
    / CopyStatement
    / ReplaceStatement
    / SetStatement
    / PreprocessorStatement
    / IterationStatement
    / ExitStatement
    / ContinueStatement
    / SelectStatement
    / GoToStatement
    / InsertStatement
    / AssignmentStatement
    / ExpressionStatement
    / DoCaseStatement
    / DoFormStatement
    / DoStatement
    / ProcedureStatement
    / LocateStatement
    / ReturnStatement
    / StoreStatement
    / ReplaceStatement
    / IfStatement
    / EvalStatement
    / UnknownStatement
    / EmptyLine
    ) { return s; }

// -----------------------------
// Declarations
// -----------------------------
// LOCAL Var1 [ AS type [ OF ClassLib ] ] | [ ArrayName1( nRows1 [, nColumns1 ] ) [ AS type [ OF ClassLib ] ] ]
//   [, Var2 [ AS type [ OF ClassLib ] ] | [, ArrayName2( nRows2 [, nColumns2 ] ) [ AS type [ OF ClassLib ] ] ]
// LOCAL [ ARRAY ] ArrayName1( nRows1 [, nColumns1 ] ) [ AS type [OF ClassLib ] ]
//   [, ArrayName2( nRows2 [, nColumns2 ] ) [ AS type [ OF ClassLib ] ] ]
LocalStatement
  = "LOCAL"i _ "ARRAY"i _ arrs:ArrayDeclList __ { return arrs; }
  / "LOCAL"i _ entries:LocalEntryList __ {  return entries; }

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
      "ALL"i _ "LIKE"i _ p:(StringLiteral / Pattern) __ {
        const pat = (typeof p === 'string') ? p : (p && p.value ? p.value : p);
        return node("PrivateAllLike", { pattern: pat });
      }
    / "ALL"i __ { return node("PrivateAll", {}); }
    / vars:IdentifierList __ { return vars.map(v => node("PrivateDeclaration", { name: v })); }
    / __ { return node("PrivateDirective", {}); }
  )

PublicStatement
  = "PUBLIC"i _ vars:IdentifierList __ {
      return vars.map(v => node("PublicDeclaration", { name: v }));
    }

LParameters
  = ("LPARAMETERS"i / "PARAMETERS"i) _ vars:ParameterList __ {
      return node("ParametersDeclaration", { names: vars });
    }

// DIMENSION ArrayName(nRows [, nColumns]) [AS cType] [, ArrayName2(...)] ...
DimensionStatement
  = "DIMENSION"i __ first:DimensionItem tail:(_ "," _ DimensionItem)* __ {
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

// Parameter names can be dotted (e.g. m.UserName). Capture as a single name string.
ParameterName
  = name:$([a-zA-Z_][a-zA-Z0-9_]* (("." / "->") [a-zA-Z_][a-zA-Z0-9_]*)*) { return name; }

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
  = head:Identifier tail:(("." / "->") _ prop:Identifier)* {
      let expr = node("Identifier", { name: head });
      for (const t of tail) {
        const propName = t[2];
        expr = node("MemberExpression", { object: expr, property: node("Identifier", { name: propName }) });
      }
      return expr;
    }

AssignmentStatement
  = id:LValue __ "=" __ expr:Expression __ {
      return node("Assignment", { target: id, expression: expr });
    }

// Shorthand print statement: ? <expression> or PRINT <expression>
PrintStatement // todo: Wait window probably should be separate
  = ("?" / "PRINT"i / "WAIT WINDOW"i) _ expr:Expression __ {
      return node("PrintStatement", { argument: expr });
    }

// USE [[DatabaseName!] TableName | SQLViewName | ?]
//  [IN nWorkArea | cTableAlias] [ONLINE] [ADMIN] [AGAIN]
//  [NOREQUERY [nDataSessionNumber]] [NODATA] 
//  [INDEX IndexFileList | ? [ORDER [nIndexNumber | IDXFileName 
//  | [TAG] TagName [OF CDXFileName] [ASCENDING | DESCENDING]]]]
//  [ALIAS cTableAlias] [EXCLUSIVE] [SHARED] [NOUPDATE] 
//  [CONNSTRING cConnectionString | nStatementHandle ]
UseStatement
  = "USE"i _
    tgt:UseTarget? _
    parts:(UseOption _)*
    __ {
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

UseTarget
  = "?" { return { kind: 'PROMPT' }; }
  / name:QualifiedTable { return { kind: 'TABLE', name }; }

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
  = ("TAG"i _)? t:Identifier _ ofPart:("OF"i __ cdx:(IdentifierOrString / UnquotedPath))? _ dir:("ASCENDING"i / "DESCENDING"i)? {
      return { tag: t, of: ofPart ? ofPart[2] : null, direction: dir ? (typeof dir === 'string' ? dir.toUpperCase() : dir) : null };
    }

UseConnPart
  = "CONNSTRING"i __ cs:(StringLiteral / Identifier) { return { kind: 'CONNSTRING', value: cs }; }
  / h:(NumberLiteral / Identifier) { return { kind: 'HANDLE', value: h }; }

// Preprocessor directives
PreprocessorStatement
  = IncludeStatement
  / DefineStatement

IncludeStatement
  = "#" _ "include"i _ path:(StringLiteral / UnquotedPath) __ {
      return node("IncludeStatement", { path });
    }

DefineStatement
  = "#" _ "define"i _ name:Identifier _ value:$((!LineTerminator .)*) __ {
      return node("DefineStatement", { name, value: value.trim() });
    }

// Example: DEFINE CLASS myhandler AS Session
DefineClass
  = "DEFINE CLASS"i _ name:Identifier _ "AS"i _ base:Identifier __
    statements:(Statement __)*
    "ENDDEFINE"i __ {
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
  = head:LogicalAnd tail:(_ (".OR."i / "OR"i) _ LogicalAnd)* {
      return tail.reduce((acc, t) => node("LogicalExpression", { operator: "OR", left: acc, right: t[3] }), head);
    }

LogicalAnd
  = head:Equality tail:(_ (".AND."i / "AND"i) _ Equality)* {
      return tail.reduce((acc, t) => node("LogicalExpression", { operator: "AND", left: acc, right: t[3] }), head);
    }

Equality
  = head:Relational tail:(_ op:("==" / "=" / "<>" / "!=" / "#" / "$") _ Relational)* {
      return tail.reduce((acc, t) => node("BinaryExpression", { operator: t[1], left: acc, right: t[3] }), head);
    }

Relational
  = head:Additive tail:(_ op:(">=" / ">" / "<=" / "<") _ Additive)* {
      return tail.reduce((acc, t) => node("BinaryExpression", { operator: t[1], left: acc, right: t[3] }), head);
    }

Additive
  = head:Multiplicative tail:(_ op:("+" / "-") _ Multiplicative)* {
      return tail.reduce((acc, t) => node("BinaryExpression", { operator: t[1], left: acc, right: t[3] }), head);
    }

Multiplicative
  = head:Unary tail:(_ op:("*" / "/") _ Unary)* {
      return tail.reduce((acc, t) => node("BinaryExpression", { operator: t[1], left: acc, right: t[3] }), head);
    }

Unary
  = op:(".NOT."i / "NOT"i / "!" / "-" / "+") _ expr:Unary {
      return node("UnaryExpression", { operator: typeof op === 'string' ? op.toUpperCase() : op, argument: expr });
    }
  / PostfixExpression

// Primary base (literals, identifiers, parenthesized expressions)
Primary
  = NumberLiteral
  / StringLiteral
  / BooleanLiteral
  / NullLiteral
  / id:Identifier { return (id && id.length && id.charAt(0) === '_') ? node("ImplicitGlobal", { name: id }) : node("Identifier", { name: id }); }
  / "(" _ e:Expression _ ")" { return e; }

// Argument list for call expressions
ArgumentList
  = head:Expression tail:(_ "," _ Expression)* { return [head, ...tail.map(t => t[3])]; }

// Postfix expressions: allow chaining of member access (.prop) and call expressions (args)
PostfixExpression
  = head:Primary tail:(("." / "->") _ prop:Identifier { return { type: 'member', prop } } 
  / "(" _ args:ArgumentList? _ ")" { return { type: 'call', args: args || [] } })*
    {
      let expr = head;
      for (const t of tail) {
        if (t.type === 'member') {
          expr = node("MemberExpression", { object: expr, property: node("Identifier", { name: t.prop }) });
        } else if (t.type === 'call') {
          expr = node("CallExpression", { callee: expr, arguments: t.args });
        }
      }
      return expr;
    }

// -----------------------------
// Control Flow
// -----------------------------

// Allow a bare expression (typically a call) as a top-level statement.
ExpressionStatement "expression statement"
  = expr:PostfixExpression __ { return node("ExpressionStatement", { expression: expr }); }

// Leading equals can be used to evaluate/call an expression as a statement, e.g. "=func()"
EvalStatement "equals-expression statement"
  = "=" _ expr:PostfixExpression __ { return node("ExpressionStatement", { expression: expr }); }

IfStatement "if statement"
  = "IF"i __ test:Expression __ 
    consequent:(Statement __)*
    "ELSE"i __
    alternate:(Statement __)*
    "ENDIF"i 
    {
      return node("IfStatement", { test, consequent: node("BlockStatement", { body: flatten(consequent.map(s => s[0])) }), alternate: node("BlockStatement", { body: flatten(alternate.map(s => s[0])) }) });
    }
    / "IF"i __ test:Expression __ 
      consequent:(Statement __)* 
      "ENDIF"i 
    {
      return node("IfStatement", { test, consequent: node("BlockStatement", { body: flatten(consequent.map(s => s[0])) }), alternate: null });
    }

// DO FORM FormName | ? [NAME VarName [LINKED]] [WITH cParameterList]
//  [TO VarName] [NOREAD] [NOSHOW]
DoFormStatement "do form statement"
  = "DO FORM"i __ target:(StringLiteral / Identifier / "?") _
    namePart:("NAME"i __ nameIdent:Identifier _ link:("LINKED"i)? )?
    withPart:("WITH"i _ params:ArgumentList)?
    toPart:("TO"i __ v:IdentifierOrString)?
    flags:(_ ("NOREAD"i / "NOSHOW"i))*
    __ {
      return node("DoFormStatement", {
        target,
        name: namePart ? nameIdent : null,
        linked: namePart ? !!(namePart[3]) : false,
        arguments: withPart ? withPart[2] : [],
        to: toPart ? toPart[2] : null,
        noread: flags ? flags.some(f => f[1].toUpperCase() === 'NOREAD') : false,
        noshow: flags ? flags.some(f => f[1].toUpperCase() === 'NOSHOW') : false
      });
    }

DoStatement "do statement"
  = "DO"i __ target:(StringLiteral / (!("FORM"i ![A-Za-z0-9_] / "CASE"i ![A-Za-z0-9_]) Identifier)) _ 
    inPart:(("IN"i _ n:( $([0-9]+) { return parseInt(n,10); } / Identifier / StringLiteral )) _)?
    withPart:("WITH"i _ params:ArgumentList)?
    __ {
      const inSession = inPart ? inPart[2] : null;
      return node("DoStatement", { target, inSession, arguments: withPart ? withPart[2] : [] });
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
  = sc:SelectCore unions:(_ "UNION"i _ all:("ALL"i _)? rhs:SelectCore)* __ {
      const unionParts = unions ? unions.map(u => ({ all: !!u[3], select: u[5] })) : [];
      return node('SelectStatement', { ...sc, unions: unionParts });
    }

SelectCore
  = "SELECT"i _
    quant:("ALL"i / "DISTINCT"i)? _
    top:("TOP"i _ n:Expression _ percent:("PERCENT"i)? { return { count: n, percent: !!percent }; })? _
    list:SelectList _
    from:FromClause? _
    joins:(JoinClause _)* _
    withbuf:WithBufferingClause? _
    where:WhereClause? _
    group:GroupByClause? _
    having:HavingClause? _
    order:OrderByClause? _
    dest:(IntoClause / ToClause)? _
    pref:PreferenceClause? _
    flags:(NoConsoleFlag? _ PlainFlag? _ NowaitFlag?) {
      return {
        quantifier: quant ? (typeof quant === 'string' ? quant.toUpperCase() : quant) : null,
        top: top || null,
        list,
        from: from || null,
        withBuffering: withbuf || null,
        where: where || null,
        groupBy: group || null,
        having: having || null,
        orderBy: order || null,
        destination: dest || null,
        preference: pref || null,
        noconsol: flags && flags[0] ? true : false,
        plain: flags && flags[2] ? true : false,
        nowait: flags && flags[4] ? true : false
      };
    }

SelectList
  = head:SelectItem tail:(_ "," _ SelectItem)* { return [head, ...tail.map(t => t[3])]; }

SelectItem
  = "*" { return node('SelectStar', {}); }
  / tbl:Identifier "." "*" { return node('SelectStar', { table: tbl }); }
  / expr:Expression alias:(__ "AS"i __ a:Identifier { return a; })? {
      return node('SelectItem', { expression: expr, alias: alias ? alias[3] : null });
    }

FromClause
  = "FROM"i _ force:("FORCE"i _)? tables:TableList {
      return { force: !!force, tables };
    }

TableList
  = head:TableRef tail:(_ "," _ TableRef)* { return [head, ...tail.map(t => t[3])]; }

TableRef
  =sub: (
    "(" __ sq:SelectStatement __ ")" _ alias:(("AS"i _ a:Identifier { return a; }) / a:Identifier { return a; })? ) {
      const subquery = sub[2];
      const alias = sub[5] ? sub[5] : null;
      return { subquery, alias };
    }
    / name:QualifiedTable _ alias:( ("AS"i _ a:Identifier { return a; }) / a:Identifier { return a; })? { return { name, alias: alias || null }; }

QualifiedTable
  = db:Identifier "!" tbl:Identifier { return { database: db, table: tbl }; }
  / t:(IdentifierOrString / UnquotedPath) { return { database: null, table: t }; }

JoinClause
  = jt:JoinType? _ "JOIN"i _ tr:TableRef __ "ON"i __ cond:Expression __ {
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
  = "GROUP"i _ "BY"i __ items:ExpressionList { return items; }

HavingClause
  = "HAVING"i __ e:Expression { return e; }

OrderByClause
  = "ORDER"i _ "BY"i __ items:OrderItemList { return items; }

OrderItemList
  = head:OrderItem tail:(_ "," _ OrderItem)* { return [head, ...tail.map(t => t[3])]; }

OrderItem
  = expr:Expression dir:(__ ("ASC"i / "DESC"i))? { return { expression: expr, direction: dir ? dir[1].toUpperCase() : null }; }

IntoClause
  = "INTO"i __ dest:(
      ("CURSOR"i _ a:Identifier { return { kind: 'CURSOR', name: a }; })
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
  = action:("COPY FILE"i / "RENAME"i) _ src:(Expression / UnquotedPath) _ "TO"i _ dst:(Expression / UnquotedPath) __ {
      return node(action === 'COPY FILE' ? 'CopyFileStatement' : 'RenameStatement', { source: src, destination: dst });
    }


CopyToStatement
  = "COPY TO"i
    target:(IdentifierOrString / UnquotedPath) _
    db:DatabaseClause? _
    fields:FieldsClause? _
    forClause:("FOR"i __ fexp:Expression { return fexp; })? _
    whileClause:("WHILE"i __ wexp:Expression { return wexp; })? _
    idx:WithIndexClause? _
    noopt:("NOOPTIMIZE"i)? _
    t:TypeClause? _
    ascp:("AS"i __ cp:Expression { return cp; })?
    __ {
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
IndexOnStatement "index on statement"
  = "INDEX ON"i __ expr:Expression _ 
    totag:(("TO"i _ tgt:(IdentifierOrString / UnquotedPath) { return { kind: 'TO', value: tgt }; })
    / ("TAG"i _ tag:Identifier { return { kind: 'TAG', value: tag }; })) _
    bin:(("BINARY"i) _)?
    coll:("COLLATE"i __ cs:IdentifierOrString)? _
    ofp:("OF"i __ cdx:IdentifierOrString)? _
    forp:("FOR"i __ fexp:Expression)? _
    compact:("COMPACT"i _)?
    dir:("ASCENDING"i / "DESCENDING"i)? _
    uniq:("UNIQUE"i / "CANDIDATE"i)? _
    additive:("ADDITIVE"i)? 
    __ {
      return node('IndexOnStatement', {
        expression: expr,
        to: totag.kind === 'TO' ? totag.value : null,
        tag: totag.kind === 'TAG' ? totag.value : null,
        binary: !!bin,
        collate: coll ? coll[2] : null,
        of: ofp ? ofp[2] : null,
        for: forp ? forp[2] : null,
        compact: !!compact,
        direction: dir ? (typeof dir === 'string' ? dir.toUpperCase() : dir) : null,
        uniqueness: uniq ? (typeof uniq === 'string' ? uniq.toUpperCase() : uniq) : null,
        additive: !!additive
      });
    }

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
    ) 
    __ {
      return node("GoToStatement", {
        command: (typeof cmd === 'string' ? cmd.toUpperCase() : cmd),
        position: part.pos ? (typeof part.pos === 'string' ? part.pos.toUpperCase() : part.pos) : null,
        record: part.rec || null,
        inTarget: part.inTarget
      });
    }

InClause
  = "IN"i _ target:(Identifier / StringLiteral / NumberLiteral / SelectCore) { return target; }

// -----------------------------
// INSERT INTO
// -----------------------------

InsertStatement
  = "INSERT"i __ "INTO"i __ target:IdentifierOrString _
    cols:("(" _ cl:IdentifierList _ ")")? __
    src:(
      "VALUES"i _ "(" _ vals:ExpressionList _ ")" { return { kind: 'values', values: vals }; }
      / "FROM"i __ (
          "ARRAY"i __ arr:Identifier { return { kind: 'from', source: 'ARRAY', name: arr }; }
          / "MEMVAR"i { return { kind: 'from', source: 'MEMVAR', name: null }; }
          / "NAME"i __ obj:Identifier { return { kind: 'from', source: 'NAME', name: obj }; }
        )
      / sel:SelectCore unions:(_ "UNION"i _ all:("ALL"i)? _ rhs:SelectCore)* {
          const parts = unions ? unions.map(u => ({ all: !!u[3], select: u[5] })) : [];
          return { kind: 'select', select: { core: sel, unions: parts } };
        }
    ) __ {
      return node('InsertStatement', {
        target,
        columns: cols ? cols[2] : null,
        source: src
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
    __ {
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
  = "FOR EACH"i _
    varName:ParameterName _
    typePart:("AS"i _ type:Identifier _ ofPart:("OF"i _ clslib:Identifier { return { library: clslib }; })? {
      return { typing: type, of: ofPart || null };
    })?  _
    "IN"i _ group:Expression foxobj:(_ "FOXOBJECT"i)? __
    body:(Statement __)*
    ("ENDFOR"i / "NEXT"i) _ endVar:ParameterName?
    __ {
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
    "ENDDO"i __ {
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
  "ENDCASE"i
  __ {
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

ExitStatement "exit"
  = ("EXIT"i / "QUIT"i) __ { return node("ExitStatement", {}); }

ContinueStatement "continue (LOOP)"
  = "LOOP"i __ { return node("ContinueStatement", {}); }

// -----------------------------
// CREATE TABLE/DBF/CURSOR
// -----------------------------
CreateStatement "create statement"
  = "CREATE"i _ 
    kind:("TABLE"i / "DBF"i / "CURSOR"i) _ 
    name:Identifier _
    nameClause:("NAME"i __ longName:Identifier _ { return longName; })? _
    free:("FREE"i _)?
    codepage:("CODEPAGE"i _ "=" _ cp:(NumberLiteral / Identifier))? _
    def:(
      _ "(" _ items:CreateDefItems _ ")" _ { return { type: 'columns', items: items }; }
      / _ "FROM"i __ "ARRAY"i __ arr:Identifier { return { type: 'fromArray', array: arr }; }
    ) __ {
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

// -----------------------------
// Unknown/catch-all statement
// -----------------------------
// Captures a single logical line (respecting semicolon continuations) that didn't
// match any known statement. Protects block delimiters so structured constructs
// (IF/DO WHILE/FOR/TRY/DEFINE) can still recognize their endings.
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
    / "OTHERWISE"i  ![A-Za-z0-9_]
    / "CATCH"i      ![A-Za-z0-9_]
    / "FINALLY"i    ![A-Za-z0-9_]
    )
    raw:$((!LineTerminator .)+ (LineContinuation (!LineTerminator .)*)*)
    __ {
      return node("UnknownStatement", { raw: raw.trim() });
    }

SetStatement
  = SetOrderToStatement / SetSettingStatement

// SET ORDER TO [nIndexNumber | IDXIndexFileName | [TAG] TagName 
//   [OF CDXFileName] [IN nWorkArea | cTableAlias]
//   [ASCENDING | DESCENDING]]
SetOrderToStatement
  = "SET ORDER TO"i __
    sel:(
      n:NumberLiteral { return { kind: 'NUMBER', value: n }; }
      / f:(IdentifierOrString / UnquotedPath) { return { kind: 'FILE', value: f }; }
      / t:TagSpec { return { kind: 'TAG', tag: t.tag, of: t.of, direction: t.direction }; }
    )
    _ inClause:(_ "IN"i __ target:(Identifier / StringLiteral / NumberLiteral) { return target; })?
    _ dir:("ASCENDING"i / "DESCENDING"i)?
    __ {
      const direction = dir ? (typeof dir === 'string' ? dir.toUpperCase() : dir) : (sel.kind === 'TAG' ? sel.direction : null);
      return node('SetOrder', {
        kind: sel.kind,
        value: sel.kind === 'NUMBER' ? sel.value : (sel.kind === 'FILE' ? sel.value : null),
        tag: sel.kind === 'TAG' ? sel.tag : null,
        of: sel.kind === 'TAG' ? sel.of : null,
        in: inClause ? inClause[1] : null,
        direction: direction
      });
    }

// SET [cSetCommand] [ON | OFF | TO [eSetting]]
SetSettingStatement
  ="SET"i _ inner:(
    ("TO"i __ setting:Expression { return node("SetTo", { setting }); })
    / (cmd:KeywordOrIdentifier toPart:(_ "TO"i __ setting:Expression)? argPart:(_ (StringLiteral / Identifier / NumberLiteral))? additive:(_ "ADDITIVE"i)? state:(_ ("ON"i / "OFF"i))? { const argument = toPart ? toPart[2] : (argPart ? argPart[1] : null); const st = state ? state[1] : null; return node("cSetCommand", { command: cmd, argument: argument, state: st ? st.toUpperCase() : null, additive: !!additive }); })
  ) __ {
      // If TO form, inner is already a SetTo node and we return it directly.
      if (inner && inner.type === 'SetTo') return inner;
      // Otherwise inner is a cSetCommand node; return it as the captured command node.
      return inner;
    }

AppendStatement
  = "APPEND"i _
    blank:("BLANK"i _)?
    inPart:("IN"i _ tableAlias:(Identifier / StringLiteral / NumberLiteral) _)?
    nomenu:("NOMENU"i _)?
    __ {
      return node("AppendStatement", {
        blank: !!blank,
        inTarget: inPart ? inPart[2] : null,
        nomenu: !!nomenu
      });
    }

// REPLACE FieldName1 WITH eExpression1 [ADDITIVE] [, FieldName2 WITH eExpression2 [ADDITIVE]] ... [Scope] [FOR lExpression1] [WHILE lExpression2] [IN nWorkArea | cTableAlias] [NOOPTIMIZE]
ReplaceStatement
  = "REPLACE"i __
    fields:ReplaceFieldList
    forClause:(_ "FOR"i __ condition:Expression)?
    whileClase:(_ "WHILE"i __ condition:Expression)?
    inClause:("IN"i __ target:(Identifier / StringLiteral / NumberLiteral) _)?
    noOptimize:("NOOPTIMIZE"i)?
    __ {
      return node("ReplaceStatement", { 
        fields, 
        forCondition: forClause ? forClause[2] : null,
        whileCondition: whileClase ? whileClase[2] : null,
        inTarget: inClause ? inClause[2] : null,
        noOptimize: !!noOptimize
      });
    }

// LOCATE [FOR lExpression1] [IN nWorkArea | cTableAlias] [WHILE lExpression2] [NOOPTIMIZE]
LocateStatement
  = "LOCATE"i
    forClause:(_ "FOR"i __ condition:Expression)?
    inClause:(_ "IN"i __ target:(Identifier / StringLiteral / NumberLiteral / SelectCore) _)?
    whileClause:(_ "WHILE"i __ condition:Expression)?
    noopt:(_ "NOOPTIMIZE"i)?
    __ {
      return node("LocateStatement", {
        forCondition: forClause ? forClause[2] : null,
        inTarget: inClause ? inClause[2] : null,
        whileCondition: whileClause ? whileClause[2] : null,
        noOptimize: !!(noopt && noopt[1])
      });
    }

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
    ) 
    __ {
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
  = !Keyword name:$([a-zA-Z_][a-zA-Z0-9_]*) { return name; }

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
  / ("CASE"i        ![a-zA-Z0-9_])
  / ("ENDFOR"i      ![a-zA-Z0-9_])
  / ("ENDDO"i       ![a-zA-Z0-9_])
  / ("LOOP"i        ![a-zA-Z0-9_])
  / ("TRY"i        ![a-zA-Z0-9_])
  / ("CATCH"i      ![a-zA-Z0-9_])
  / ("ENDTRY"i     ![a-zA-Z0-9_])
  / ("THROW"i      ![a-zA-Z0-9_])
  / ("FINALLY"i    ![a-zA-Z0-9_])
  / ("OTHERWISE"i   ![a-zA-Z0-9_])
  / ("ENDCASE"i     ![a-zA-Z0-9_])
  / ("SELECT"i     ![a-zA-Z0-9_])
  / ("WITH"i       ![a-zA-Z0-9_])
  / ("WHERE"i      ![a-zA-Z0-9_])
  / ("HAVING"i     ![a-zA-Z0-9_])
  / ("UNION"i      ![a-zA-Z0-9_])
  / ("INTO"i       ![a-zA-Z0-9_])
  / ("INSERT"i     ![a-zA-Z0-9_])
  / ("COPY"i       ![a-zA-Z0-9_])
  / ("CREATE"i      ![a-zA-Z0-9_])
  / ("GO"i         ![a-zA-Z0-9_])
  / ("GOTO"i       ![a-zA-Z0-9_])
  / ("RECORD"i     ![a-zA-Z0-9_])
  / ("CURSOR"i      ![a-zA-Z0-9_])

NumberLiteral "number"
  = value:$("$"? [0-9]+ ("." [0-9]+)? ) {
      const raw = value;
      const isCurrency = raw.charAt(0) === '$';
      const num = parseFloat(isCurrency ? raw.slice(1) : raw);
      return node("NumberLiteral", { value: num, raw, currency: !!isCurrency });
    }

StringLiteral "string"
  = '"' chars:DoubleStringChar* '"' { return node("StringLiteral", { value: chars.join("") }); }
  / "'" chars:SingleStringChar* "'" { return node("StringLiteral", { value: chars.join("") }); }

DoubleStringChar
  = '""' { return '"'; }
  / !'"' . { return text(); }

SingleStringChar
  = "''" { return "'"; }
  / !"'" . { return text(); }

LineTerminator
	= [\n\r\u2028\u2029]

// todo: date literal
DateLiteral "date"
  = "{}"i

// example: `s:\code\mosapi\3_3\aalib\mosapi.h` or `libs\system.app`
UnquotedPath
  = p:$([^ \t\f\v\r\n]+) { return p; }

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

__
  = (Whitespace / LineContinuation / Comment / LineTerminatorSequence)*

_ 
  = (Whitespace / MultiLineCommentNoLineTerminator / LineContinuation)*

// Semicolon at end of physical line continues the logical line onto the next physical line.
LineContinuation "semicolon"
  = ";" [ \t]* LineTerminatorSequence 

Whitespace "whitespace"
  = [ \t\f\v]+ 

EmptyLine "empty line"
	= __ LineTerminator

Comment "comment"
  = SingleLineComment
  / MultiLineComment

//  "&&" or "*"
SingleLineComment "single-line comment"
  = "&&" (!LineTerminator .)*
  / "*"  (!LineTerminator .)*

// Note:. technically A '*' comment is only a comment when it appears as the first non-space char.
// We don't want to caputre tokens like the '*' in a SELECT list (e.g. "SELECT * FROM ...").


MultiLineComment "multi-line comment"
  = "/*" (!"*/" .)* "*/" 

MultiLineCommentNoLineTerminator "/* */ inline comment"
  = "/*" (!"*/" !LineTerminator .)* "*/"

EOF "end of file"
	= !.
