/* global console process */
import * as fs from 'fs';
import * as parser from '../server/src/parser.js';

// Usage: node debug-parse.js [path/to/file.prg]
const inputPath = process.argv[2] || './test-files/select.prg';
const src = fs.readFileSync(inputPath, 'utf8');
// Use the input filename as grammarSource so parser error messages reference it
const ast = parser.parse(src, { grammarSource: inputPath });
console.log(JSON.stringify(ast, null, 2));
process.exit(0);