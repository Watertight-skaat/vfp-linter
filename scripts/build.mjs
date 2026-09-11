/**
 * Bundles the extension client and the language server with esbuild.
 *
 * Both entry points are bundled to a single self-contained CommonJS file, so the packaged VSIX carries no node_modules at all. `vscode` is the one module that must stay external -- it is injected by the extension host at runtime and cannot be bundled.
 */
import * as esbuild from 'esbuild';
import * as fs from 'node:fs';
import process from 'node:process';

const watch = process.argv.includes('--watch');
const production = process.argv.includes('--production');
// The VS Code end-to-end harness needs compiled JS to hand to --extensionTestsPath.
// It is built only on demand so it can never end up in the VSIX.
const withTests = process.argv.includes('--tests');

// Stale output from an earlier layout would otherwise be packaged alongside the bundles.
for (const dir of ['client/out', 'server/out']) {
  fs.rmSync(dir, { recursive: true, force: true });
}

/** @type {import('esbuild').BuildOptions} */
const shared = {
  bundle: true,
  platform: 'node',
  // VS Code 1.101 (engines.vscode floor) ships Electron 35 / Node 22.
  target: 'node22',
  format: 'cjs',
  sourcemap: !production,
  minify: production,
  logLevel: 'info'
};

const targets = [
  {
    ...shared,
    entryPoints: ['client/src/extension.ts'],
    outfile: 'client/out/extension.js',
    // Provided by the extension host, never bundled.
    external: ['vscode']
  },
  {
    ...shared,
    entryPoints: ['server/src/server.ts'],
    outfile: 'server/out/server.js',
    external: []
  }
];

if (withTests) {
  // index.js globs for *.test.js at runtime, so every suite needs its own entry point.
  const suites = fs
    .readdirSync('client/src/test')
    .filter(f => f.endsWith('.test.ts'))
    .map(f => `client/src/test/${f}`);

  targets.push({
    ...shared,
    sourcemap: true,
    minify: false,
    entryPoints: ['client/src/test/runTest.ts', 'client/src/test/index.ts', ...suites],
    outdir: 'client/out/test',
    // Resolved from client/node_modules and the root at run time; these never ship.
    external: ['vscode', 'mocha', '@vscode/test-electron']
  });
}

if (watch) {
  const contexts = await Promise.all(targets.map(t => esbuild.context(t)));
  await Promise.all(contexts.map(c => c.watch()));
  console.log('esbuild: watching client and server');
} else {
  await Promise.all(targets.map(t => esbuild.build(t)));
}
