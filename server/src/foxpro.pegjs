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
  = s:( LocalStatement
      / PrivateStatement
      / PublicStatement
      / DeclareStatement
  / CreateStatement
      / DefineClass
      / LParameters
      / PrintStatement
      / UseStatement
      / AppendStatement
      / ReplaceStatement
      / SetStatement
      / PreprocessorStatement
      / Assignment
      / ExpressionStatement
      / DoStatement
      / EvalStatement
      / Procedure
      / ReturnStatement
      / StoreStatement
      / ReplaceStatement
      / IfStatement
      / EmptyLine ) { return s; }

// -----------------------------
// Declarations
// -----------------------------
LocalStatement
  = "LOCAL"i _ vars:IdentifierList _ LineTerminator? {
      return vars.map(v => node("LocalDeclaration", { name: v }));
    }

PrivateStatement
  = "PRIVATE"i _ (
      "ALL"i _ "LIKE"i _ p:(StringLiteral / Pattern) _ LineTerminator? {
        const pat = (typeof p === 'string') ? p : (p && p.value ? p.value : p);
        return node("PrivateAllLike", { pattern: pat });
      }
    / "ALL"i _ LineTerminator? { return node("PrivateAll", {}); }
    / vars:IdentifierList _ LineTerminator? { return vars.map(v => node("PrivateDeclaration", { name: v })); }
    / _ LineTerminator? { return node("PrivateDirective", {}); }
  )

PublicStatement
  = "PUBLIC"i _ vars:IdentifierList _ LineTerminator? {
      return vars.map(v => node("PublicDeclaration", { name: v }));
    }

LParameters
  = ("LPARAMETERS"i / "PARAMETERS"i) _ vars:ParameterList _ LineTerminator? {
      return node("ParametersDeclaration", { names: vars });
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

Assignment
  = id:LValue __ "=" __ expr:Expression LineTerminator? {
      return node("Assignment", { target: id, expression: expr });
    }

// Shorthand print statement: ? <expression> or PRINT <expression>
PrintStatement
  = "?" _ expr:Expression _ LineTerminator? {
      return node("PrintStatement", { argument: expr });
    }
  / "PRINT"i _ expr:Expression _ LineTerminator? {
      return node("PrintStatement", { argument: expr });
    }

// SQL-like USE statement (e.g. USE mytable, USE "path\file.dbf", USE IN app, or USE <path> IN <datasession>)
UseStatement
  = "USE"i _ 
    path:(StringLiteral / Identifier)? _ 
    inSession:("IN"i _ n:( [0-9]+  / Identifier / StringLiteral ) )? _
    LineTerminator? {
      return node("UseStatement", { path: path === undefined ? null : path, inSession: inSession});
    }

// Preprocessor directives
PreprocessorStatement
  = IncludeStatement
  / DefineStatement

IncludeStatement
  = "#" _ "include"i _ path:(StringLiteral / UnquotedPath) _ LineTerminator? {
      return node("IncludeStatement", { path });
    }

DefineStatement
  = "#" _ "define"i _ name:Identifier _ value:$((!LineTerminator .)*) LineTerminator? {
      return node("DefineStatement", { name, value: value.trim() });
    }

// Example: DEFINE CLASS myhandler AS Session
DefineClass
  = "DEFINE CLASS"i _ name:Identifier _ "AS"i _ base:Identifier _ LineTerminator
    statements:(Statement __)*
    "ENDDEFINE"i _ LineTerminator? {
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
  = head:LogicalAnd tail:(_ ("OR"i / ".OR."i) _ LogicalAnd)* {
      return tail.reduce((acc, t) => node("LogicalExpression", { operator: "OR", left: acc, right: t[3] }), head);
    }

LogicalAnd
  = head:Equality tail:(_ ("AND"i / ".AND."i) _ Equality)* {
      return tail.reduce((acc, t) => node("LogicalExpression", { operator: "AND", left: acc, right: t[3] }), head);
    }

Equality
  = head:Relational tail:(_ op:("==" / "=" / "<>" / "!=" / "#") _ Relational)* {
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
  = op:("NOT"i / ".NOT."i / "!" / "-" / "+") _ expr:Unary {
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
  = head:Primary tail:(("." / "->") _ prop:Identifier { return { type: 'member', prop } } / "(" _ args:ArgumentList? _ ")" { return { type: 'call', args: args || [] } })*
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
  = expr:PostfixExpression _ LineTerminator? { return node("ExpressionStatement", { expression: expr }); }

// Leading equals can be used to evaluate/call an expression as a statement, e.g. "=func()"
EvalStatement "equals-expression statement"
  = "=" _ expr:PostfixExpression _ LineTerminator? { return node("ExpressionStatement", { expression: expr }); }

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

DoStatement "do statement"
  = "DO"i __ target:(StringLiteral / Identifier) _ inPart:(("IN"i _ n:( $([0-9]+) { return parseInt(n,10); } / Identifier / StringLiteral )) _)? withPart:("WITH"i _ params:ArgumentList)? _ LineTerminator? {
      const inSession = inPart ? inPart[2] : null;
      return node("DoStatement", { target, inSession, arguments: withPart ? withPart[2] : [] });
    }

// -----------------------------
// CREATE TABLE/DBF/CURSOR
// -----------------------------
CreateStatement
  = "CREATE"i __ kind:("TABLE"i / "DBF"i / "CURSOR"i) __ name:IdentifierOrString _
    nameClause:("NAME"i __ longName:IdentifierOrString _ { return longName; })? _
    free:("FREE"i _)?
    codepage:("CODEPAGE"i _ "=" _ cp:(NumberLiteral / Identifier))? _
    def:(
      _ "(" _ items:CreateDefItems _ ")" _ { return { type: 'columns', items: items }; }
      / _ "FROM"i __ "ARRAY"i __ arr:Identifier { return { type: 'fromArray', array: arr }; }
    ) _ LineTerminator? {
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
  = name:Identifier __ ftype:FieldType _ fsize:FieldSize? _
    nullability:("NULL"i / ("NOT"i __ "NULL"i))? _
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
  = "(" _ w:(NumberLiteral / Identifier) _ "," _ p:(NumberLiteral / Identifier) _ ")" { return { width: w, precision: p }; }
  / "(" _ w:(NumberLiteral / Identifier) _ ")" { return { width: w, precision: null }; }

TableConstraint
  = "PRIMARY"i __ "KEY"i __ expr:Expression __ "TAG"i __ tag:Identifier { return node('TableConstraint', { kind: 'PRIMARY KEY', expression: expr, tag }); }
  / "UNIQUE"i __ expr:Expression __ "TAG"i __ tag:Identifier _ coll:("COLLATE"i __ cs:IdentifierOrString { return cs; })? { return node('TableConstraint', { kind: 'UNIQUE', expression: expr, tag, collate: coll ? coll[2] : null }); }
  / "FOREIGN"i __ "KEY"i __ expr:Expression __ "TAG"i __ tag:Identifier _ nodup:("NODUP"i)? _ coll:("COLLATE"i __ cs:IdentifierOrString { return cs; })? __ "REFERENCES"i __ tbl:IdentifierOrString _ reftag:("TAG"i __ rt:Identifier { return rt; })? { return node('TableConstraint', { kind: 'FOREIGN KEY', expression: expr, tag, nodup: !!nodup, collate: coll ? coll[2] : null, references: { table: tbl, tag: reftag ? reftag[2] : null } }); }
  / "CHECK"i __ expr:Expression _ err:("ERROR"i __ msg:StringLiteral)? { return node('TableConstraint', { kind: 'CHECK', expression: expr, error: err ? err[2] : null }); }

// SET [cSetCommand] [ON | OFF | TO [eSetting]]
SetStatement
  = "SET"i _ inner:(
    ("TO"i __ setting:Expression { return node("SetTo", { setting }); })
    / (cmd:KeywordOrIdentifier toPart:(_ "TO"i __ setting:Expression)? argPart:(_ (StringLiteral / Identifier / NumberLiteral))? additive:(_ "ADDITIVE"i)? state:(_ ("ON"i / "OFF"i))? { const argument = toPart ? toPart[2] : (argPart ? argPart[1] : null); const st = state ? state[1] : null; return node("cSetCommand", { command: cmd, argument: argument, state: st ? st.toUpperCase() : null, additive: !!additive }); })
  ) _ LineTerminator? {
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
    LineTerminator? 
    {
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
    _ LineTerminator? {
      return node("ReplaceStatement", { 
        fields, 
        forCondition: forClause ? forClause[2] : null,
        whileCondition: whileClase ? whileClase[2] : null,
        inTarget: inClause ? inClause[2] : null,
        noOptimize: !!noOptimize
      });
    }

ReplaceFieldList
  = head:ReplaceField tail:(_ "," _ ReplaceField)* {
      return [head, ...tail.map(t => t[3])];
    }

ReplaceField
  = field:Identifier _ "WITH"i _ value:Expression _ additive:("ADDITIVE"i)? {
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
    _ LineTerminator? {
    return node('StoreStatement', { expression: expr, target: toPart });
  }

ExpressionList
  = head:Expression tail:(_ "," _ Expression)* { return [head, ...tail.map(t => t[3])]; }

Procedure "procedure"
  = "TODO"i

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
  / ("FUNCTION"i    ![a-zA-Z0-9_])
  / ("ENDPROC"i     ![a-zA-Z0-9_])
  / ("ENDFUNC"i     ![a-zA-Z0-9_])
  / ("IF"i          ![a-zA-Z0-9_])
  / ("ELSE"i        ![a-zA-Z0-9_])
  / ("ENDIF"i       ![a-zA-Z0-9_])
  / ("AND"i         ![a-zA-Z0-9_])
  / ("OR"i          ![a-zA-Z0-9_])
  / ("NOT"i         ![a-zA-Z0-9_])
  / ("PRINT"i       ![a-zA-Z0-9_])
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
  / ("try"i          ![a-zA-Z0-9_])
  / ("catch"i       ![a-zA-Z0-9_])
  / ("CREATE"i      ![a-zA-Z0-9_])
  / ("TABLE"i       ![a-zA-Z0-9_])
  / ("CURSOR"i      ![a-zA-Z0-9_])
  / ("NAME"i        ![a-zA-Z0-9_])
  / ("FREE"i        ![a-zA-Z0-9_])
  / ("CODEPAGE"i    ![a-zA-Z0-9_])
  / ("UNIQUE"i      ![a-zA-Z0-9_])
  / ("COLLATE"i     ![a-zA-Z0-9_])
  / ("REFERENCES"i  ![a-zA-Z0-9_])
  / ("TAG"i         ![a-zA-Z0-9_])
  / ("CHECK"i       ![a-zA-Z0-9_])
  / ("DEFAULT"i     ![a-zA-Z0-9_])
  / ("AUTOINC"i     ![a-zA-Z0-9_])
  / ("NEXTVALUE"i   ![a-zA-Z0-9_])
  / ("STEP"i        ![a-zA-Z0-9_])
  / ("NOCPTRANS"i   ![a-zA-Z0-9_])
  / ("FOREIGN"i     ![a-zA-Z0-9_])
  / ("FROM"i        ![a-zA-Z0-9_])

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

// example: `s:\code\mosapi\3_3\aalib\mosapi.h`
UnquotedPath
  = p:$((!LineTerminator .)+) { return p.trim(); }

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
  = (Whitespace / Comment / LineTerminatorSequence)*

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
  / "*" (!LineTerminator .)*

MultiLineComment "multi-line comment"
  = "/*" (!"*/" .)* "*/" 

MultiLineCommentNoLineTerminator "/* */ inline comment"
  = "/*" (!"*/" !LineTerminator .)* "*/"

// EOF predicate (for some comment handling if needed later)
EOF 
	= !.

// -----------------------------
// END
// -----------------------------

