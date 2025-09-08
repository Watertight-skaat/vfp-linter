/* One-off copy of built JavaScript from each src tree to its out tree (mirrors watch script logic) */
/* eslint-env node */
/* global process, console */
import fs from 'fs';
import path from 'path';

const roots = [
  process.cwd(),
  path.join(process.cwd(), 'client'),
  path.join(process.cwd(), 'server')
];

function ensureDir(dir) {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
}

function copyTree(root) {
  const srcRoot = path.join(root, 'src');
  const outRoot = path.join(root, 'out');
  if (!fs.existsSync(srcRoot)) {return;}
  function walk(dir) {
    for (const name of fs.readdirSync(dir)) {
      const full = path.join(dir, name);
      const stat = fs.statSync(full);
      if (stat.isDirectory()) {
        walk(full);
      } else if (full.endsWith('.js') || full.endsWith('.js.map')) {
        const rel = path.relative(srcRoot, full);
        const dest = path.join(outRoot, rel);
        ensureDir(path.dirname(dest));
        fs.copyFileSync(full, dest);
        console.log(`copied ${path.relative(process.cwd(), full)} -> ${path.relative(process.cwd(), dest)}`);
      }
    }
  }
  walk(srcRoot);
}

for (const r of roots) {
  copyTree(r);
}
console.log('copy-src-js done');
