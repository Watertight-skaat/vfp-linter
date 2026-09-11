// A discriminated union over every node the Peggy grammar emits, so rules narrow on `type` instead of indexing into untyped bags. `parse()` still returns `any`, so cast its result to Program at the boundary.
// test-files/run-ast-tests.js re-derives the node names and property names from foxpro.pegjs and asserts they match this file exactly, so the two cannot drift apart silently.
// Properties are typed from the grammar where its shape is fixed. A few option bags vary by which clause matched and are typed `unknown` on purpose: that forces a rule to narrow rather than trust a guess.

// Parser positions are 1-based on both axes; LSP ranges are 0-based, so every consumer subtracts.
export interface Position {
  line: number;
  column: number;
}

export interface Loc {
  start?: Position;
  end?: Position;
}

export interface NodeBase {
  location?: Loc;
}

/** IdentifierOrString: the grammar returns a bare string for an identifier, a node for a quoted one. */
export type IdentifierOrString = string | StringLiteral;

/** Anywhere the grammar accepts an arbitrary expression. */
export type Expr =
  | Identifier
  | ImplicitGlobal
  | NumberLiteral
  | StringLiteral
  | BooleanLiteral
  | NullLiteral
  | DateTimeLiteral
  | MacroSubstitute
  | Path
  | BinaryExpression
  | LogicalExpression
  | UnaryExpression
  | InExpression
  | CallExpression
  | MemberExpression
  | WithMemberExpression
  | ArrayIndexExpression
  | CastExpression
  | ExistsExpression
  | CaseExpression
  | ScopeResolution
  | SelectStatement
  | SelectStar;

// ---------------------------------------------------------------------------
// Plain objects the grammar returns alongside nodes. These have no `location`, and where they carry a `type` field (StoreTarget) it is not a node type -- do not feed them to a node-type switch.
// ---------------------------------------------------------------------------

export interface QualifiedTable {
  database: string | null;
  table: Path | IdentifierOrString;
}

export interface TableRef {
  name?: Expr | QualifiedTable;
  subquery?: SelectStatement;
  alias: string | null;
}

export interface JoinClause {
  type: 'LEFT' | 'RIGHT' | 'FULL' | 'INNER' | null;
  target: TableRef;
  condition: Expr;
}

export type FromItem = { kind: 'table'; value: TableRef } | { kind: 'join'; value: JoinClause };

export interface FromClause {
  force: boolean;
  tables: TableRef[];
  joins: JoinClause[];
  items: FromItem[];
}

/** INTO TABLE/CURSOR/ARRAY/DBF, a bare INTO, or TO. */
export type SelectDestination =
  | { kind: 'TABLE'; name: Expr | Path }
  | { kind: 'CURSOR'; name: Expr }
  | { kind: 'ARRAY'; name: string }
  | { kind: 'DBF'; name: IdentifierOrString }
  | { kind: 'DEFAULT'; name: IdentifierOrString }
  | { kind: 'TO'; name: IdentifierOrString };

/** The table named by USE, or `?` for the picker. */
export type UseTargetRef =
  | { kind: 'PROMPT' }
  | { kind: 'TABLE'; name: QualifiedTable }
  | { kind: 'EXPR'; value: Expr };

/** One member of a STORE ... TO list, in its three forms. `type` here is not a node type. */
export type StoreTarget =
  | { type: 'Var'; name: string }
  | { type: 'ArrayIndexed'; array: string; indexes: Expr[] }
  | { type: 'ArrayAssign'; target: string; expression: Expr };

/** One REPLACE pair of the pre-SQL UPDATE ON. */
export interface UpdateOnReplacement {
  field: string;
  expression: Expr;
}

/** One tag named by DELETE TAG, with the compound index file it lives in when that is given. */
export interface DeleteTagItem {
  name: string;
  of: Expr | Path | null;
}

/** SAVE TO and RESTORE FROM name either a file or a memo field. */
export type MemoryStore =
  | { kind: 'MEMO'; name: string }
  | { kind: 'FILE'; name: Expr | Path };

/** The ALL LIKE / ALL EXCEPT skeleton that narrows what SAVE writes out. */
export interface MemvarSkeleton {
  mode: 'LIKE' | 'EXCEPT';
  pattern: string;
}

/** The TO clause of CALCULATE and SUM. */
export type CalcTarget = { kind: 'VARS'; vars: string[] } | { kind: 'ARRAY'; name: string };

/** A scope clause: ALL, REST, NEXT n or RECORD n. */
export type RecordScope = 'ALL' | 'REST' | { type: 'NEXT'; count: NumberLiteral } | { type: 'RECORD'; number: NumberLiteral };

export interface DimensionItem {
  name: string;
  rows: Expr;
  columns: Expr | null;
  asType: IdentifierOrString | null;
}

export interface ProcedureParam {
  name: string;
  type: IdentifierOrString | null;
}

export interface DeclareParameter {
  type: string;
  byRef: boolean;
  name: string;
}

export interface CatchClause {
  to: string | null;
  when: Expr | null;
  body: BlockStatement;
}

export interface ReplaceField {
  field: string;
  value: Expr;
  additive: boolean;
}

export interface FieldSize {
  width: Expr;
  precision: Expr | null;
}

export interface ColumnKey {
  primaryKey: boolean;
  unique: boolean;
  collate: IdentifierOrString | null;
}

export interface TableReference {
  table: IdentifierOrString;
  tag: string | null;
}

export interface ColumnCheck {
  expr: Expr;
  error: StringLiteral | null;
}

export interface ColumnAutoInc {
  nextValue: NumberLiteral | Identifier | null;
  step: NumberLiteral | Identifier | null;
}

/** INSERT INTO ... VALUES / FROM ARRAY|MEMVAR|NAME / SELECT. */
export type InsertSource =
  | { kind: 'values'; values: Expr[] }
  | { kind: 'from'; source: 'ARRAY' | 'MEMVAR' | 'NAME'; name: string | null }
  | { kind: 'select'; select: SelectStatement };

export interface UpdateAssignment {
  field: string;
  expression: Expr;
}

export interface RelationPair {
  expression: Expr;
  into: Expr;
}

export interface TagSpec {
  tag: string;
  of: IdentifierOrString | Path | null;
  direction: 'ASCENDING' | 'DESCENDING' | 'ASC' | 'DESC' | null;
}

/** SET ORDER TO, by index number, file or tag. */
export type OrderSelection =
  | { kind: 'NUMBER'; value: NumberLiteral }
  | { kind: 'FILE'; value: IdentifierOrString | Path }
  | ({ kind: 'TAG' } & TagSpec);

/** FIELDS <list> | FIELDS LIKE <pattern> | FIELDS EXCEPT <pattern>. */
export type FieldsSelection =
  | { kind: 'list'; fields: string[] }
  | { kind: 'like'; pattern: string }
  | { kind: 'except'; pattern: string };

/** The export format of a COPY TO or APPEND FROM TYPE clause. DELIMITED carries its own options. */
export type ExportType =
  | { format: string; sheet?: IdentifierOrString | null }
  | { format: 'DELIMITED'; options: unknown };

export interface CaseWhen {
  when: Expr;
  then: Expr;
}

/** One field of a SORT ... ON list, with its /D and /C flags resolved. */
export interface SortFieldSpec {
  name: string;
  descending: boolean;
  ignoreCase: boolean;
}

export interface DatabaseClause {
  database: IdentifierOrString;
  longName: IdentifierOrString | null;
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

export interface Identifier extends NodeBase {
  type: 'Identifier';
  name: string;
}

/** A name beginning with an underscore, which VFP reserves for its own system variables. */
export interface ImplicitGlobal extends NodeBase {
  type: 'ImplicitGlobal';
  name: string;
}

export interface NumberLiteral extends NodeBase {
  type: 'NumberLiteral';
  value: number;
  raw: string;
  currency: boolean;
}

export interface StringLiteral extends NodeBase {
  type: 'StringLiteral';
  value: string;
}

export interface BooleanLiteral extends NodeBase {
  type: 'BooleanLiteral';
  value: boolean;
}

export interface NullLiteral extends NodeBase {
  type: 'NullLiteral';
}

export interface DateTimeLiteral extends NodeBase {
  type: 'DateTimeLiteral';
  value: string;
}

/** &name, which expands to FoxPro source at run time and so defeats static analysis. */
export interface MacroSubstitute extends NodeBase {
  type: 'MacroSubstitute';
  name: string;
}

/** An unquoted file path, from the clauses that accept one. */
export interface Path extends NodeBase {
  type: 'Path';
  path: string;
}

export interface BinaryExpression extends NodeBase {
  type: 'BinaryExpression';
  operator: string;
  left: Expr;
  right: Expr;
}

export interface LogicalExpression extends NodeBase {
  type: 'LogicalExpression';
  operator: 'AND' | 'OR';
  left: Expr;
  right: Expr;
}

export interface UnaryExpression extends NodeBase {
  type: 'UnaryExpression';
  operator: string;
  argument: Expr;
}

export interface InExpression extends NodeBase {
  type: 'InExpression';
  left: Expr;
  not: boolean;
  right: SelectStatement | Expr[];
}

export interface CallExpression extends NodeBase {
  type: 'CallExpression';
  callee: Expr;
  /** A null entry is an omitted argument, as in `f(1, , 3)`. */
  arguments: (Expr | null)[];
}

export interface MemberExpression extends NodeBase {
  type: 'MemberExpression';
  object: Expr;
  property: Identifier;
}

/** A reference opening with a dot inside WITH ... ENDWITH, so it names a property of the WITH target rather than a memory variable. The leading dot is the only thing that distinguishes the two, which is why it is kept. */
export interface WithMemberExpression extends NodeBase {
  type: 'WithMemberExpression';
  /** The chain after the dot: `Caption`, `grdLines.RecordSource`, `Objects(n).Style`. Its root identifier is a property name, not a variable -- only the arguments and subscripts inside it are real references. */
  expression: Expr;
}

export interface ArrayIndexExpression extends NodeBase {
  type: 'ArrayIndexExpression';
  object: Expr;
  indexes: Expr[];
}

export interface CastExpression extends NodeBase {
  type: 'CastExpression';
  expression: Expr;
  to: { kind: 'typed'; name: string; size: NumberLiteral; scale: NumberLiteral | null } | { kind: 'simple'; name: IdentifierOrString };
}

export interface ExistsExpression extends NodeBase {
  type: 'ExistsExpression';
  argument: SelectStatement;
}

/** SQL CASE [operand] WHEN ... THEN ... [ELSE ...] END. An expression, not a statement. */
export interface CaseExpression extends NodeBase {
  type: 'CaseExpression';
  /** The CASE <operand> WHEN <value> form; null for the CASE WHEN <condition> form. */
  operand: Expr | null;
  whens: CaseWhen[];
  otherwise: Expr | null;
}

/** Base::Method(), which reaches a parent implementation explicitly where DODEFAULT() does it implicitly. */
export interface ScopeResolution extends NodeBase {
  type: 'ScopeResolution';
  object: Expr;
  property: Identifier;
}

// ---------------------------------------------------------------------------
// Structure
// ---------------------------------------------------------------------------

export interface Program extends NodeBase {
  type: 'Program';
  body: Statement[];
}

export interface BlockStatement extends NodeBase {
  type: 'BlockStatement';
  body: Statement[];
}

export interface ProcedureStatement extends NodeBase {
  type: 'ProcedureStatement';
  name: string;
  isFunction: boolean;
  /** Typed objects for the `f(a AS Integer)` form, bare strings for the LPARAMETERS form. */
  parameters: ProcedureParam[] | string[];
  returnType: IdentifierOrString | null;
  body: BlockStatement;
  /** True when the parameters came from an LPARAMETERS/PARAMETERS line rather than a parameter list. */
  lparameters: boolean;
}

export interface DefineClass extends NodeBase {
  type: 'DefineClass';
  name: string;
  base: string | null;
  ofClass: StringLiteral | Path | null;
  olePublic: boolean;
  body: Statement[];
}

export interface DeclareStatement extends NodeBase {
  type: 'DeclareStatement';
  returnType: string | null;
  functionName: string;
  libraryName: Path | IdentifierOrString;
  aliasName: string | null;
  parameters: DeclareParameter[];
}

// ---------------------------------------------------------------------------
// Declarations
// ---------------------------------------------------------------------------

export interface LocalDeclaration extends NodeBase {
  type: 'LocalDeclaration';
  name: string;
  asType: string | null;
  ofClass: string | null;
}

export interface LocalArrayDeclaration extends NodeBase {
  type: 'LocalArrayDeclaration';
  name: string;
  rows: Expr;
  columns: Expr | null;
  asType: string | null;
  ofClass: string | null;
}

export interface PublicDeclaration extends NodeBase {
  type: 'PublicDeclaration';
  name: string;
  isArray: boolean;
}

export interface PrivateDeclaration extends NodeBase {
  type: 'PrivateDeclaration';
  name: string;
  isArray: boolean;
}

/** PRIVATE ALL, which hides every variable of the caller. */
export interface PrivateAll extends NodeBase {
  type: 'PrivateAll';
}

export interface PrivateAllLike extends NodeBase {
  type: 'PrivateAllLike';
  pattern: string;
}

export interface PrivateAllExcept extends NodeBase {
  type: 'PrivateAllExcept';
  pattern: string;
}

/** A bare PRIVATE with no names, which declares nothing. */
export interface PrivateDirective extends NodeBase {
  type: 'PrivateDirective';
}

export interface ParametersDeclaration extends NodeBase {
  type: 'ParametersDeclaration';
  names: string[];
}

export interface DimensionStatement extends NodeBase {
  type: 'DimensionStatement';
  items: DimensionItem[];
}

// ---------------------------------------------------------------------------
// Control flow
// ---------------------------------------------------------------------------

export interface IfStatement extends NodeBase {
  type: 'IfStatement';
  test: Expr;
  consequent: BlockStatement;
  alternate: BlockStatement | null;
}

export interface DoCaseStatement extends NodeBase {
  type: 'DoCaseStatement';
  cases: CaseClause[];
  otherwise: BlockStatement | null;
}

export interface CaseClause extends NodeBase {
  type: 'CaseClause';
  test: Expr;
  consequent: BlockStatement;
}

export interface ForStatement extends NodeBase {
  type: 'ForStatement';
  /** A bare name, so the only location available is the statement's own. */
  variable: string;
  init: Expr;
  final: Expr;
  step: Expr | null;
  endVariable: string | null;
  body: BlockStatement;
}

export interface ForEachStatement extends NodeBase {
  type: 'ForEachStatement';
  variable: string;
  asType: string | null;
  ofClass: { library: string } | null;
  collection: Expr;
  foxObject: boolean;
  endVariable: string | null;
  body: BlockStatement;
}

export interface DoWhileStatement extends NodeBase {
  type: 'DoWhileStatement';
  test: Expr;
  body: BlockStatement;
}

export interface ScanStatement extends NodeBase {
  type: 'ScanStatement';
  noOptimize: boolean;
  scope: RecordScope;
  forCondition: Expr | null;
  whileCondition: Expr | null;
  body: BlockStatement;
}

export interface TryStatement extends NodeBase {
  type: 'TryStatement';
  tryBlock: BlockStatement;
  catchClause: CatchClause | null;
  thrown: Expr | null;
  didExit: boolean;
  finallyBlock: BlockStatement | null;
}

export interface WithStatement extends NodeBase {
  type: 'WithStatement';
  target: Expr;
  asType: string | null;
  ofClass: string | null;
  body: BlockStatement;
}

export interface ReturnStatement extends NodeBase {
  type: 'ReturnStatement';
  argument: Expr | null;
}

export interface ExitStatement extends NodeBase {
  type: 'ExitStatement';
}

/** LOOP, which continues the enclosing loop. */
export interface ContinueStatement extends NodeBase {
  type: 'ContinueStatement';
}

export interface DoStatement extends NodeBase {
  type: 'DoStatement';
  target: Expr | Path | QualifiedTable;
  inSession: number | string | StringLiteral | null;
  arguments: (Expr | null)[];
}

export interface DoFormStatement extends NodeBase {
  type: 'DoFormStatement';
  target: IdentifierOrString | '?';
  name: string | null;
  linked: boolean;
  arguments: (Expr | null)[];
  to: string | null;
  noread: boolean;
  noshow: boolean;
}

// ---------------------------------------------------------------------------
// Assignment and expression statements
// ---------------------------------------------------------------------------

export interface Assignment extends NodeBase {
  type: 'Assignment';
  target: Expr;
  expression: Expr;
}

export interface StoreStatement extends NodeBase {
  type: 'StoreStatement';
  expression: Expr;
  targets: StoreTarget[];
}

export interface ExpressionStatement extends NodeBase {
  type: 'ExpressionStatement';
  expression: Expr;
}

export interface PrintStatement extends NodeBase {
  type: 'PrintStatement';
  arguments: Expr[];
}

export interface WaitWindowStatement extends NodeBase {
  type: 'WaitWindowStatement';
  nowait: boolean;
  noclear: boolean;
  clear: boolean;
  timeout: Expr | null;
  message: Expr | null;
}

// ---------------------------------------------------------------------------
// SQL
// ---------------------------------------------------------------------------

export interface SelectStatement extends NodeBase {
  type: 'SelectStatement';
  quantifier: 'ALL' | 'DISTINCT' | null;
  top: { count: Expr; percent: boolean } | null;
  list: (SelectItem | SelectStar)[];
  from: FromClause | null;
  withBuffering: unknown;
  where: Expr | null;
  groupBy: Expr[] | null;
  having: Expr | null;
  orderBy: unknown;
  destination: SelectDestination | null;
  preference: IdentifierOrString | null;
  noconsol: boolean;
  plain: boolean;
  nowait: boolean;
  unions: { all: boolean; select: SelectStatement }[];
}

export interface SelectItem extends NodeBase {
  type: 'SelectItem';
  expression: Expr;
  alias: string | null;
}

export interface SelectStar extends NodeBase {
  type: 'SelectStar';
  table?: string;
}

export interface InsertStatement extends NodeBase {
  type: 'InsertStatement';
  target: Expr | Path;
  columns: string[] | null;
  source: InsertSource;
}

export interface UpdateStatement extends NodeBase {
  type: 'UpdateStatement';
  target: IdentifierOrString;
  set: UpdateAssignment[] | null;
  from: FromClause | null;
  where: Expr | null;
}

/** Both the SQL form (DELETE FROM ...) and the Xbase form (DELETE FOR ...), so most fields are optional. */
export interface DeleteStatement extends NodeBase {
  type: 'DeleteStatement';
  target: IdentifierOrString | null;
  /** The SQL form only; null for the Xbase form. */
  from: FromClause | null;
  where: Expr | null;
  scope?: IdentifierOrString | null;
  for?: Expr | null;
  while?: Expr | null;
  inTarget?: Expr | null;
  noOptimize?: boolean;
}

export interface CreateStatement extends NodeBase {
  type: 'CreateStatement';
  kind: 'TABLE' | 'DBF' | 'CURSOR';
  name: Expr | string;
  longName: string | null;
  free: boolean;
  codepage: NumberLiteral | string | null;
  columns: ColumnDefinition[];
  constraints: TableConstraint[];
  fromArray: string | null;
}

export interface ColumnDefinition extends NodeBase {
  type: 'ColumnDefinition';
  name: string;
  fieldType: string;
  size: FieldSize | null;
  nullability: 'NULL' | 'NOT NULL' | null;
  check: ColumnCheck | null;
  autoinc: ColumnAutoInc | null;
  default: Expr | null;
  key: ColumnKey | null;
  references: TableReference | null;
  nocptrans: boolean;
}

export interface TableConstraint extends NodeBase {
  type: 'TableConstraint';
  kind: 'PRIMARY KEY' | 'UNIQUE' | 'FOREIGN KEY' | 'CHECK';
  expression: Expr;
  tag?: string;
  collate?: IdentifierOrString | null;
  nodup?: boolean;
  references?: TableReference;
  error?: StringLiteral | null;
}

// ---------------------------------------------------------------------------
// Tables and work areas
// ---------------------------------------------------------------------------

export interface UseStatement extends NodeBase {
  type: 'UseStatement';
  /** Null for a bare USE, which closes the current work area. */
  target: UseTargetRef | null;
  inTarget: Expr | null;
  online: boolean;
  admin: boolean;
  again: boolean;
  norequery: boolean;
  dataSession: Expr | null;
  nodata: boolean;
  index: unknown;
  alias: IdentifierOrString | null;
  exclusive: boolean;
  shared: boolean;
  noUpdate: boolean;
  connection: unknown;
}

export interface LocateStatement extends NodeBase {
  type: 'LocateStatement';
  forCondition: Expr | null;
  scope: RecordScope | null;
  inTarget: Expr | null;
  whileCondition: Expr | null;
  noOptimize: boolean;
}

export interface ReplaceStatement extends NodeBase {
  type: 'ReplaceStatement';
  fields: ReplaceField[];
  scope: RecordScope | null;
  forCondition: Expr | null;
  whileCondition: Expr | null;
  inTarget: Expr | null;
  noOptimize: boolean;
}

/** SCATTER's destination: MEMVAR spreads the record over m.-prefixed variables, ARRAY and NAME create the name they are given. */
export type ScatterDestination = 'MEMVAR' | 'ARRAY' | 'NAME';

export interface ScatterStatement extends NodeBase {
  type: 'ScatterStatement';
  destination: ScatterDestination | null;
  /** The array or object the record is copied into. Null for MEMVAR, which names no single variable. */
  name: string | null;
  fields: FieldsSelection | null;
  memo: boolean;
  blank: boolean;
  additive: boolean;
  autoMem: boolean;
}

export interface GatherStatement extends NodeBase {
  type: 'GatherStatement';
  source: ScatterDestination | null;
  name: string | null;
  fields: FieldsSelection | null;
  memo: boolean;
}

export interface CalculateStatement extends NodeBase {
  type: 'CalculateStatement';
  expressions: Expr[];
  scope: RecordScope | null;
  forCondition: Expr | null;
  whileCondition: Expr | null;
  to: CalcTarget | null;
  noOptimize: boolean;
  inTarget: Expr | null;
}

/** SUM, AVERAGE and COUNT: one command with three names and one option tail. */
export interface AggregateStatement extends NodeBase {
  type: 'AggregateStatement';
  command: 'SUM' | 'AVERAGE' | 'COUNT';
  /** COUNT brings none. */
  expressions: Expr[] | null;
  scope: RecordScope | null;
  forCondition: Expr | null;
  whileCondition: Expr | null;
  to: CalcTarget | null;
  noOptimize: boolean;
  inTarget: Expr | null;
}

export interface FlushStatement extends NodeBase {
  type: 'FlushStatement';
  force: boolean;
}

export interface ReindexStatement extends NodeBase {
  type: 'ReindexStatement';
  compact: boolean;
}

export interface DirectoryStatement extends NodeBase {
  type: 'DirectoryStatement';
  command: 'MKDIR' | 'RMDIR' | 'CHDIR' | 'MD' | 'RD' | 'CD';
  target: Expr | Path;
}

/** CONTINUE resumes the last LOCATE. LOOP is ContinueStatement; this one does not end a block. */
export interface ContinueLocateStatement extends NodeBase {
  type: 'ContinueLocateStatement';
}

export interface NoDefaultStatement extends NodeBase {
  type: 'NoDefaultStatement';
}

export interface PushPopStatement extends NodeBase {
  type: 'PushPopStatement';
  command: 'PUSH' | 'POP';
  what: 'KEY' | 'MENU' | 'POPUP';
  options: string | null;
}

/** EXTERNAL is a compiler directive: it creates nothing, but the names in it are deliberate. */
export interface ExternalStatement extends NodeBase {
  type: 'ExternalStatement';
  kind: string;
  names: string[];
}

export interface ModifyStatement extends NodeBase {
  type: 'ModifyStatement';
  what: string;
  options: string | null;
}

export interface AlterTableStatement extends NodeBase {
  type: 'AlterTableStatement';
  name: IdentifierOrString;
  /** The DDL tail, kept as source: recognising the statement is what stops the false positive. */
  options: string | null;
}

export interface RunStatement extends NodeBase {
  type: 'RunStatement';
  /** The shell command line, which is not FoxPro. */
  command: string;
}

// --- Pre-SQL data commands -------------------------------------------------
// The xbase commands SQL replaced. Each names a table, a field or a variable, so the operands are kept.

export interface TotalStatement extends NodeBase {
  type: 'TotalStatement';
  target: Expr | Path;
  /** The key the records are grouped on. */
  key: Expr;
  fields: FieldsSelection | null;
  scope: RecordScope | null;
  for: Expr | null;
  while: Expr | null;
  noOptimize: boolean;
}

/** The pre-SQL JOIN, which writes its result to a table rather than returning it. */
export interface JoinWithStatement extends NodeBase {
  type: 'JoinWithStatement';
  source: Expr | string;
  target: Expr | Path;
  condition: Expr;
  fields: FieldsSelection | null;
}

/** The pre-SQL UPDATE, which merges another work area into the current table. It shares only the word with SQL UPDATE. */
export interface UpdateOnStatement extends NodeBase {
  type: 'UpdateOnStatement';
  key: string;
  source: Expr | string;
  replacements: UpdateOnReplacement[];
  random: boolean;
}

export interface CopyStructureStatement extends NodeBase {
  type: 'CopyStructureStatement';
  target: Expr | Path;
  /** COPY STRUCTURE EXTENDED writes the field definitions as records instead. */
  extended: boolean;
  fields: FieldsSelection | null;
  index: 'CDX' | 'PRODUCTION' | null;
  database: DatabaseClause | null;
}

export interface DeleteTagStatement extends NodeBase {
  type: 'DeleteTagStatement';
  all: boolean;
  tags: DeleteTagItem[];
  /** The OF clause of the ALL form; in the list form each tag carries its own. */
  of: Expr | Path | null;
}

/** BLANK empties the current record's fields rather than deleting the record. */
export interface BlankStatement extends NodeBase {
  type: 'BlankStatement';
  fields: FieldsSelection | null;
  scope: RecordScope | null;
  for: Expr | null;
  while: Expr | null;
  noOptimize: boolean;
  inTarget: Expr | string | null;
}

// --- Memory variables and debugging ----------------------------------------

export interface SaveToStatement extends NodeBase {
  type: 'SaveToStatement';
  destination: MemoryStore;
  filter: MemvarSkeleton | null;
}

export interface RestoreFromStatement extends NodeBase {
  type: 'RestoreFromStatement';
  source: MemoryStore;
  additive: boolean;
}

export interface AssertStatement extends NodeBase {
  type: 'AssertStatement';
  condition: Expr;
  message: Expr | null;
}

export interface PlayMacroStatement extends NodeBase {
  type: 'PlayMacroStatement';
  /** The key label, or ALL. */
  macro: string;
  times: Expr | null;
}

// --- Screen and menu -------------------------------------------------------
// None of these reaches a table or a variable, so the name is what is kept and the option tail stays source.

export interface DefineScreenStatement extends NodeBase {
  type: 'DefineScreenStatement';
  what: 'WINDOW' | 'MENU' | 'PAD' | 'POPUP' | 'BAR';
  /** A bar is numbered; everything else is named. */
  name: string | NumberLiteral;
  of: string | null;
  options: string | null;
}

export interface ScreenCommandStatement extends NodeBase {
  type: 'ScreenCommandStatement';
  command: 'ACTIVATE' | 'DEACTIVATE' | 'SHOW' | 'HIDE' | 'MOVE' | 'SIZE' | 'ZOOM';
  what: 'WINDOW' | 'MENU' | 'POPUP' | 'SCREEN';
  options: string | null;
}

/** SET SKIP OF greys a menu item out. It is menu furniture rather than a setting. */
export interface SetSkipOfStatement extends NodeBase {
  type: 'SetSkipOfStatement';
  what: 'MENU' | 'PAD' | 'POPUP' | 'BAR';
  target: string | NumberLiteral;
  of: string | null;
  condition: Expr;
}

export interface OnSelectionStatement extends NodeBase {
  type: 'OnSelectionStatement';
  what: 'BAR' | 'MENU' | 'PAD' | 'POPUP';
  target: string | NumberLiteral;
  of: string | null;
  /** The handler, parsed as a statement. Null when the selection is being cleared. */
  command: Statement | null;
}

export interface SkipStatement extends NodeBase {
  type: 'SkipStatement';
  count: Expr | null;
  inTarget: Expr | null;
}

export interface GoToStatement extends NodeBase {
  type: 'GoToStatement';
  command: 'GO' | 'GOTO';
  position: 'TOP' | 'BOTTOM' | null;
  record: Expr | null;
  inTarget: Expr | null;
}

export interface ZapStatement extends NodeBase {
  type: 'ZapStatement';
  inTarget: Expr | null;
}

export interface RecallStatement extends NodeBase {
  type: 'RecallStatement';
  scope: IdentifierOrString | null;
  for: Expr | null;
  while: Expr | null;
  noOptimize: boolean;
  inTarget: Expr | null;
}

export interface UnlockStatement extends NodeBase {
  type: 'UnlockStatement';
  record: Expr | null;
  inTarget: Expr | null;
  all: boolean;
}

export interface BrowseStatement extends NodeBase {
  type: 'BrowseStatement';
  fields: string[] | null;
  for: Expr | null;
  norm: boolean;
  nowait: boolean;
}

export interface AppendStatement extends NodeBase {
  type: 'AppendStatement';
  blank: boolean;
  inTarget: Expr | null;
  nomenu: boolean;
}

export interface AppendFromStatement extends NodeBase {
  type: 'AppendFromStatement';
  source: { kind: 'PROMPT' } | Expr | Path;
  fields: string[] | FieldsSelection | null;
  for: Expr | null;
  /** The TYPE clause, whose shape depends on the export format that matched. */
  exportType: ExportType | null;
  codepage: Expr | null;
}

export interface CopyToStatement extends NodeBase {
  type: 'CopyToStatement';
  target: Expr | Path;
  database: DatabaseClause | null;
  fields: FieldsSelection | null;
  for: Expr | null;
  while: Expr | null;
  index: 'CDX' | 'PRODUCTION' | null;
  noOptimize: boolean;
  /** The TYPE clause, whose shape depends on the export format that matched. */
  exportType: ExportType | null;
  codepage: Expr | null;
}

export interface EraseStatement extends NodeBase {
  type: 'EraseStatement';
  target: Expr | Path | '?';
  recycle: boolean;
}

export interface IndexOnStatement extends NodeBase {
  type: 'IndexOnStatement';
  expression: Expr;
  to: IdentifierOrString | Path | null;
  tag: string | null;
  binary: boolean;
  collate: IdentifierOrString | null;
  of: IdentifierOrString | Path | null;
  for: Expr | null;
  compact: boolean;
  direction: 'ASCENDING' | 'DESCENDING' | null;
  uniqueness: 'UNIQUE' | 'CANDIDATE' | null;
  additive: boolean;
}

// ---------------------------------------------------------------------------
// SET
// ---------------------------------------------------------------------------

/** SET TO <expr>, with no setting name. */
export interface SetTo extends NodeBase {
  type: 'SetTo';
  setting: Expr;
}

export interface SetOrder extends NodeBase {
  type: 'SetOrder';
  selection: OrderSelection | null;
  inTarget: Expr | null;
  direction: 'ASCENDING' | 'DESCENDING' | 'ASC' | 'DESC' | null;
}

export interface SetRelation extends NodeBase {
  type: 'SetRelation';
  pairs: RelationPair[];
  inTarget: Expr | null;
  additive: boolean;
}

/** Any other SET, named after the cSetCommand placeholder in the VFP documentation. */
export interface SetCommand extends NodeBase {
  type: 'SetCommand';
  command: string | Keyword;
  argument: Expr | null;
  /** `SET FILTER TO` with nothing after it, which clears the setting rather than leaving it alone. */
  cleared: boolean;
  state: 'ON' | 'OFF' | null;
  additive: boolean;
}

/** A reserved word, which KeywordOrIdentifier can return in place of a name. */
export type Keyword = string | unknown[];

// ---------------------------------------------------------------------------
// Xbase housekeeping and output
// ---------------------------------------------------------------------------

/** TEXT ... ENDTEXT. The body is raw output text, held verbatim in `content` rather than parsed. */
export interface TextBlockStatement extends NodeBase {
  type: 'TextBlockStatement';
  /** The TO variable, which is where the block is commonly used to build a SQL string. */
  to: string | null;
  additive: boolean;
  textmerge: boolean;
  noshow: boolean;
  flags: Expr | null;
  pretext: Expr | null;
  content: string;
}

export interface ThrowStatement extends NodeBase {
  type: 'ThrowStatement';
  argument: Expr | null;
}

/** @ nRow, nColumn SAY | GET | TO | CLEAR. The per-verb option tail is kept as raw source in `options`. */
export interface AtStatement extends NodeBase {
  type: 'AtStatement';
  row: Expr;
  column: Expr;
  verb: 'SAY' | 'GET' | 'TO' | 'CLEAR';
  /** The SAY expression. */
  expression: Expr | null;
  /** The GET variable. */
  target: Expr | null;
  endRow: Expr | null;
  endColumn: Expr | null;
  /** Everything after the verb, verbatim and untrimmed of meaning: not modelled yet. */
  options: string | null;
}

export interface ClearStatement extends NodeBase {
  type: 'ClearStatement';
  /** ALL, MEMORY, WINDOWS, CLASS, READ and so on; null for a bare CLEAR, which clears the screen. */
  target: string | null;
  /** The class or class library named by CLEAR CLASS / CLASSLIB. */
  name: IdentifierOrString | null;
  all: boolean;
}

export interface CloseStatement extends NodeBase {
  type: 'CloseStatement';
  target: string | null;
  all: boolean;
}

export interface ReleaseStatement extends NodeBase {
  type: 'ReleaseStatement';
  /** ALL, WINDOWS, PROCEDURE and so on; null when the statement names variables directly. */
  scope: string | null;
  extended: boolean;
  /** LIKE or EXCEPT, when RELEASE ALL was given a skeleton. */
  mode: 'LIKE' | 'EXCEPT' | null;
  pattern: StringLiteral | string | null;
  names: string[];
  options: string | null;
}

export interface PackStatement extends NodeBase {
  type: 'PackStatement';
  /** MEMO or DBF, when only one of the two was requested. */
  what: 'MEMO' | 'DBF' | null;
  table: IdentifierOrString | null;
  inTarget: Expr | null;
}

export interface SeekStatement extends NodeBase {
  type: 'SeekStatement';
  expression: Expr;
  order: OrderSelection | null;
  direction: 'ASCENDING' | 'DESCENDING' | null;
  inTarget: Expr | null;
}

export interface SuspendStatement extends NodeBase {
  type: 'SuspendStatement';
}

export interface ResumeStatement extends NodeBase {
  type: 'ResumeStatement';
}

export interface KeyboardStatement extends NodeBase {
  type: 'KeyboardStatement';
  expression: Expr;
  plain: boolean;
  clear: boolean;
}

/** REPORT FORM / LABEL FORM. The option tail is long and order-free, so it is kept as raw source. */
export interface ReportFormStatement extends NodeBase {
  type: 'ReportFormStatement';
  command: 'REPORT' | 'LABEL';
  form: Expr | Path;
  options: string | null;
}

export interface SortStatement extends NodeBase {
  type: 'SortStatement';
  target: Expr | Path;
  fields: SortFieldSpec[];
  options: string | null;
}

/** LIST / DISPLAY. Their option tails depend on the subject, so they are kept as raw source. */
export interface ListStatement extends NodeBase {
  type: 'ListStatement';
  command: 'LIST' | 'DISPLAY';
  /** MEMORY, STATUS, STRUCTURE and so on; null when the command lists records. */
  subject: string | null;
  options: string | null;
}

// ---------------------------------------------------------------------------
// Preprocessor and fallback
// ---------------------------------------------------------------------------

export interface IncludeStatement extends NodeBase {
  type: 'IncludeStatement';
  path: StringLiteral | Path;
}

export interface DefineStatement extends NodeBase {
  type: 'DefineStatement';
  name: string;
  value: string | null;
}

/** #IF / #ELSE / #ENDIF, kept as raw text rather than evaluated. */
export interface PreprocessorIfStatement extends NodeBase {
  type: 'PreprocessorIfStatement';
  raw: string;
}

/** ON ERROR | ESCAPE | SHUTDOWN | READERROR | APLABOUT | PAGE | KEY [LABEL cLabel] [command]. */
export interface OnStatement extends NodeBase {
  type: 'OnStatement';
  event: 'ERROR' | 'ESCAPE' | 'SHUTDOWN' | 'READERROR' | 'APLABOUT' | 'PAGE' | 'KEY' | 'KEY LABEL';
  /** The key name for ON KEY LABEL. */
  label: string | null;
  /** The line number for ON PAGE AT LINE n. */
  atLine: Expr | null;
  /** The handler, parsed as a statement. Null when the event is being cleared. */
  command: Statement | null;
}

/** A statement the grammar does not cover. Advisory by default: see unsupportedSyntaxSeverity. */
export interface UnknownStatement extends NodeBase {
  type: 'UnknownStatement';
  raw: string;
}

// ---------------------------------------------------------------------------
// Unions
// ---------------------------------------------------------------------------

export type Statement =
  | AppendFromStatement
  | AppendStatement
  | AtStatement
  | Assignment
  | BlockStatement
  | BrowseStatement
  | CalculateStatement
  | CaseClause
  | ClearStatement
  | CloseStatement
  | ColumnDefinition
  | ContinueStatement
  | CopyToStatement
  | CopyStructureStatement
  | CreateStatement
  | DeclareStatement
  | DefineClass
  | DefineStatement
  | DeleteStatement
  | DeleteTagStatement
  | DimensionStatement
  | DoCaseStatement
  | DoFormStatement
  | DoStatement
  | DoWhileStatement
  | EraseStatement
  | ExitStatement
  | ExpressionStatement
  | ForEachStatement
  | ForStatement
  | GoToStatement
  | IfStatement
  | IncludeStatement
  | IndexOnStatement
  | KeyboardStatement
  | ListStatement
  | InsertStatement
  | LocalArrayDeclaration
  | LocalDeclaration
  | LocateStatement
  | OnStatement
  | PackStatement
  | ParametersDeclaration
  | PreprocessorIfStatement
  | PrintStatement
  | PrivateAll
  | PrivateAllLike
  | PrivateAllExcept
  | PrivateDeclaration
  | PrivateDirective
  | ProcedureStatement
  | Program
  | PublicDeclaration
  | RecallStatement
  | ReleaseStatement
  | ReportFormStatement
  | ResumeStatement
  | ReplaceStatement
  | ScatterStatement
  | GatherStatement
  | ReturnStatement
  | ScanStatement
  | SelectItem
  | SelectStatement
  | SetCommand
  | SetOrder
  | SetRelation
  | SetTo
  | SeekStatement
  | SkipStatement
  | FlushStatement
  | ReindexStatement
  | DirectoryStatement
  | ContinueLocateStatement
  | NoDefaultStatement
  | PushPopStatement
  | ExternalStatement
  | ModifyStatement
  | AlterTableStatement
  | RunStatement
  | TotalStatement
  | JoinWithStatement
  | BlankStatement
  | SaveToStatement
  | RestoreFromStatement
  | AssertStatement
  | PlayMacroStatement
  | DefineScreenStatement
  | ScreenCommandStatement
  | SetSkipOfStatement
  | OnSelectionStatement
  | SortStatement
  | StoreStatement
  | AggregateStatement
  | SuspendStatement
  | TableConstraint
  | TextBlockStatement
  | ThrowStatement
  | TryStatement
  | UnknownStatement
  | UnlockStatement
  | UpdateStatement
  | UpdateOnStatement
  | UseStatement
  | WaitWindowStatement
  | WithStatement
  | ZapStatement;

/** Every node the grammar emits. */
export type AstNode = Statement | Expr;

/** The node type names, for an exhaustiveness check over the union. */
export type AstNodeType = AstNode['type'];
