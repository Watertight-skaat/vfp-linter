/**
 * Publishes the extension to the VS Code Marketplace.
 *
 * Checks the release is ready, builds the VSIX, publishes it. Run it with `bun run release`.
 *
 * The Personal Access Token comes from $VSCE_PAT, or from the one `vsce login` stored earlier. Azure DevOps expires those after a year at most, so the token is verified up front -- finding out after a full production build is the slow way to learn it lapsed.
 *
 * Flags: --dry-run packages but does not publish, --skip-tests skips the suite.
 */
import { spawnSync } from 'node:child_process';
import * as fs from 'node:fs';
import * as os from 'node:os';
import * as path from 'node:path';
import process from 'node:process';

const dryRun = process.argv.includes('--dry-run');
const skipTests = process.argv.includes('--skip-tests');

const { version, name, publisher } = JSON.parse(fs.readFileSync('package.json', 'utf8'));
const vsix = path.join(os.tmpdir(), `${name}-${version}.vsix`);
const VSCE = 'node_modules/@vscode/vsce/vsce';

function step(message) { console.log(`\n> ${message}`); }

function fail(message, ...hints) {
  console.error(`\nX ${message}`);
  for (const hint of hints) console.error(`  ${hint}`);
  process.exit(1);
}

/** Runs a command inheriting stdio, and fails the release if it does not exit 0. */
function run(cmd, args, onError) {
  // `bun` is a .cmd shim on Windows and needs a shell; node resolves on its own and must not have one.
  const { status, error } = spawnSync(cmd, args, { stdio: 'inherit', shell: cmd !== process.execPath });
  if (error) fail(error.message);
  if (status !== 0) onError();
}

/** Runs vsce through node, so its shebang bin works on Windows too. */
const runVsce = (args, onError) => run(process.execPath, [VSCE, ...args], onError);

if (!fs.existsSync(VSCE)) fail('@vscode/vsce is not installed.', 'Run `bun install` first.');

step(`Releasing ${publisher}.${name} v${version}`);

// A published version can never be replaced, only superseded, so a forgotten bump is worth catching here rather than as a 409 after the build. A missing changelog entry is the tell.
const headings = fs.readFileSync('CHANGELOG.md', 'utf8').split(/\r?\n/).map(l => l.trim());
if (!headings.includes(`## ${version}`))
  fail(`CHANGELOG.md has no "## ${version}" entry.`, 'Add one, or bump the version in package.json.');

if (spawnSync('git', ['status', '--porcelain'], { encoding: 'utf8' }).stdout?.trim())
  console.warn(`\n! Working tree is dirty -- v${version} will ship with uncommitted changes.`);

step('Verifying the marketplace token');
runVsce(['verify-pat', publisher], () => fail(
  'The Personal Access Token was rejected -- it has expired, or been revoked.',
  'Mint a new one at https://dev.azure.com -> User settings -> Personal Access Tokens,',
  'scoped to All accessible organizations, with Marketplace -> Manage. Then run',
  `\`node_modules/.bin/vsce login ${publisher}\` once, or set $VSCE_PAT, and re-run.`
));

if (!skipTests) {
  step('Running the test suite');
  run('bun', ['run', 'test'], () => fail('Tests failed -- nothing was published.'));
}

step(`Packaging ${vsix}`);
runVsce(['package', '--no-dependencies', '--out', vsix], () => fail('Packaging failed.'));

if (dryRun) {
  console.log(`\nOK  Dry run -- built ${vsix}, nothing published.`);
  process.exit(0);
}

step('Publishing to the marketplace');
runVsce(['publish', '--packagePath', vsix], () => fail('Publishing failed.'));

console.log(`\nOK  Published ${publisher}.${name} v${version}`);
console.log(`    https://marketplace.visualstudio.com/items?itemName=${publisher}.${name}`);
console.log(`\n    Tag it:  git tag v${version} && git push origin v${version}`);
