/*
 Watches for .js and .js.map files under any "src" tree and copies them to the corresponding "out" tree.
 Usage: node ./scripts/copy-src-watch.js
*/
/* eslint-env node */
/* global process, console, setTimeout */
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

function copyFileFromSrc(root, srcFile) {
    const srcRoot = path.join(root, 'src');
    const outRoot = path.join(root, 'out');
    if (!srcFile.startsWith(srcRoot)) {return;}
    const rel = path.relative(srcRoot, srcFile);
    const dest = path.join(outRoot, rel);
    ensureDir(path.dirname(dest));
    try {
        fs.copyFileSync(srcFile, dest);
        console.log(`copied ${path.relative(process.cwd(), srcFile)} -> ${path.relative(process.cwd(), dest)}`);
    } catch (err) {
        console.error('copy error', err.message);
    }
}

function removeOutFile(root, srcFile) {
    const srcRoot = path.join(root, 'src');
    const outRoot = path.join(root, 'out');
    if (!srcFile.startsWith(srcRoot)) {return;}
    const rel = path.relative(srcRoot, srcFile);
    const dest = path.join(outRoot, rel);
    try {
        if (fs.existsSync(dest)) {
            fs.unlinkSync(dest);
            console.log(`removed ${path.relative(process.cwd(), dest)}`);
        }
    } catch (err) {
        console.error('remove error', err.message);
    }
}

function watchRoot(root) {
    const srcRoot = path.join(root, 'src');
    if (!fs.existsSync(srcRoot)) {return;}

    // initial copy of existing files
    function walkAndCopy(dir) {
        for (const name of fs.readdirSync(dir)) {
            const full = path.join(dir, name);
            const stat = fs.statSync(full);
            if (stat.isDirectory()) {
                walkAndCopy(full);
            } else if (full.endsWith('.js') || full.endsWith('.js.map')) {
                copyFileFromSrc(root, full);
            }
        }
    }

    try {
        walkAndCopy(srcRoot);
    } catch (err) {
        console.error('initial copy failed for', root, err.message);
    }

    // watch recursively. fs.watch is not reliably recursive on all platforms for nested folders,
    // but on modern Node on Windows and macOS it works for the top-level. We'll watch directories as discovered.

    const watched = new Set();

    function watchDir(dir) {
        if (watched.has(dir)) {return;}
        watched.add(dir);
        let watcher;
        try {
            watcher = fs.watch(dir, { persistent: true }, (eventType, filename) => {
                if (!filename) {return;}
                const full = path.join(dir, filename);
                // debounce quick successive events per file
                setTimeout(() => {
                    try {
                        if (fs.existsSync(full)) {
                            const stat = fs.statSync(full);
                            if (stat.isDirectory()) {
                                // new directory: watch it and copy its contents
                                watchDir(full);
                                walkAndCopy(full);
                            } else if (full.endsWith('.js') || full.endsWith('.js.map')) {
                                copyFileFromSrc(root, full);
                            }
                        } else {
                            // file removed
                            if (full.endsWith('.js') || full.endsWith('.js.map')) {
                                removeOutFile(root, full);
                            }
                        }
                    } catch (err) {
                        // ignore transient errors
                    }
                }, 50);
            });
            watcher.on('error', (err) => {
                console.error('watch error', dir, err.message);
            });
        } catch (err) {
            // fallback: ignore
        }
        // also watch subdirectories
        try {
            for (const name of fs.readdirSync(dir)) {
                const full = path.join(dir, name);
                try {
                    if (fs.statSync(full).isDirectory()) {
                        watchDir(full);
                    }
                } catch (e) {
                    // ignore
                }
            }
        } catch (e) {
            // ignore
        }
    }

    watchDir(srcRoot);
    console.log(`watching ${srcRoot}`);
}

for (const r of roots) {
    try {
        watchRoot(r);
    } catch (err) {
        console.error('error watching for', r, err.message);
    }
}

console.log('copy-src-watch running');
