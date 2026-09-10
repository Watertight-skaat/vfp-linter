// Minimal shared shapes for the Peggy AST. `parse()` returns `any`; a full discriminated union over the ~90 node types the grammar emits is a separate job.

// Parser positions are 1-based on both axes; LSP ranges are 0-based, so every consumer subtracts.
export interface Position {
  line: number;
  column: number;
}

export interface Loc {
  start?: Position;
  end?: Position;
}

export interface AstNode {
  type: string;
  name?: string;
  location?: Loc;
  [k: string]: unknown;
}

export interface ProgramAst {
  type?: string;
  body?: AstNode[];
  location?: Loc;
}
