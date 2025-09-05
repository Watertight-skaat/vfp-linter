{
  // This is an initializer block. You can define helper functions here.
}

// The "start" rule is where parsing begins. It expects one or more statements.
start = statements:Statement* {
  return { type: "Program", body: statements };
}

// A Statement can currently only be a LocalStatement.
Statement = _ statement:LocalStatement _ { return statement; }

// Defines a LOCAL statement, like "LOCAL myVar".
LocalStatement
  = "LOCAL"i _ name:Identifier {
      return {
        type: "LocalDeclaration",
        name: name,
        location: location()
      };
    }

// Defines an Identifier (a variable name).
Identifier "Identifier"
  = name:([a-zA-Z_][a-zA-Z0-9_]*) { return name.join(""); }

// Defines ignored whitespace.
_ "whitespace"
  = [ \t\r\n]*