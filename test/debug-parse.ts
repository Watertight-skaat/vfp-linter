import fs from 'fs';
import { parse } from '../server/src/parser.js';

// Usage: bun debug-parse.ts [path/to/file.prg]
const inputPath = process.argv[2] || './test-files/select.prg';
const src = fs.readFileSync(inputPath, 'utf8');
// Use the input filename as grammarSource so parser error messages reference it
const ast = parse(src, { grammarSource: inputPath });
console.log(JSON.stringify(ast, null, 2));
process.exit(0);
