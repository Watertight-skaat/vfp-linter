// The disk side of the index: walk the workspace folders, read what is there, and keep the index in step as files change.
// index.ts is pure and knows nothing about files; this is the only place under server/src that touches the filesystem, so the tests can drive it by pointing it at a fixture directory.

import fs from 'fs';
import path from 'path';
import { extract, normalizePath, scanHeader, WorkspaceIndex, type FileRecord } from './index.js';
import { parse } from './parser.js';
import type { Program } from './ast.js';

/** The files the index reads. Anything else in the tree is still recorded as existing, so `SET CLASSLIB TO x` can find an x.vcx. */
export const indexedExtensions = ['.prg', '.mpr', '.spr', '.h'];

export interface CrawlOptions {
  roots: string[];
  /** Globs, matched against the path with forward slashes. */
  exclude?: string[];
  searchPath?: string[];
  /** Called as the crawl goes, for the progress report. */
  onProgress?: (done: number, total: number) => void;
  /** Yielded to every so often so a crawl of a large tree does not hold the event loop. */
  yieldEvery?: number;
}

const defaultExclude = ['**/node_modules/**', '**/.git/**', '**/out/**'];

/** Every file under the roots, excluded ones dropped. Directories are walked breadth-first so the shallow, likely-interesting files are indexed first. */
export async function crawl(options: CrawlOptions): Promise<string[]> {
  const excluded = (options.exclude ?? defaultExclude).map(globToRegExp);
  const seen = new Set<string>();
  const found: string[] = [];
  const queue = options.roots.map(root => path.resolve(root));

  while (queue.length) {
    const dir = queue.shift()!;
    let entries: fs.Dirent[];
    try {
      entries = await fs.promises.readdir(dir, { withFileTypes: true });
    } catch {
      continue; // a folder that vanished or cannot be read is not worth failing the crawl over
    }
    for (const entry of entries) {
      const full = path.join(dir, entry.name);
      const slashed = full.replace(/\\/g, '/');
      if (excluded.some(re => re.test(slashed))) continue;
      if (entry.isDirectory()) queue.push(full);
      else if (entry.isFile() && !seen.has(normalizePath(full))) {
        seen.add(normalizePath(full));
        found.push(full);
      }
    }
  }
  return found;
}

/** Builds an index over the given roots at tier 1. Every file in the tree is recorded as existing; the ones the index reads are scanned. */
export async function buildIndex(options: CrawlOptions): Promise<WorkspaceIndex> {
  const index = new WorkspaceIndex({ roots: options.roots.map(r => path.resolve(r)), searchPath: (options.searchPath ?? []).map(p => path.resolve(p)) });
  const files = await crawl(options);
  const readable = files.filter(isIndexed);
  for (const file of files) index.addKnown(file);

  const every = options.yieldEvery ?? 200;
  for (let i = 0; i < readable.length; i++) {
    const record = readRecord(readable[i], 1);
    if (record) index.upsert(record);
    options.onProgress?.(i + 1, readable.length);
    if (every > 0 && (i + 1) % every === 0) await new Promise(resolve => setImmediate(resolve));
  }
  return index;
}

export function isIndexed(file: string): boolean {
  return indexedExtensions.includes(path.extname(file).toLowerCase());
}

/** Reads one file from disk at the given tier. Null when it cannot be read; a file that cannot be parsed falls back to the header scan rather than vanishing from the index. */
export function readRecord(file: string, tier: 1 | 2): FileRecord | null {
  let text: string;
  let stat: fs.Stats;
  try {
    stat = fs.statSync(file);
    text = fs.readFileSync(file, 'utf-8');
  } catch {
    return null;
  }
  return recordFrom(file, text, { mtime: stat.mtimeMs, size: stat.size }, tier);
}

/** The record for text already in hand -- an open document, or a fixture the tests hold as a string. */
export function recordFrom(file: string, text: string, stat: { mtime: number; size: number }, tier: 1 | 2): FileRecord {
  if (tier === 1) return scanHeader(file, text, stat);
  let ast: Program | null = null;
  try {
    ast = parse(text) as Program;
  } catch {
    // A file being typed in is unparseable most of the time; its headers still are not.
    return scanHeader(file, text, stat);
  }
  return extract(file, ast, text.split(/\r\n|\r|\n/), stat);
}

/** Re-reads one file after the watcher reported it changed, and says which definitions moved as a result. */
export function refresh(index: WorkspaceIndex, file: string, tier: 1 | 2 = 1): Set<string> {
  if (!fs.existsSync(file)) return index.remove(file).changed;
  index.addKnown(file);
  if (!isIndexed(file)) return new Set();
  const record = readRecord(file, tier);
  return record ? index.upsert(record).changed : new Set();
}

/** A glob as a case-insensitive regular expression. Enough for the exclude setting: `**` crosses directories, `*` and `?` do not. The two multi-character wildcards are parked on placeholders first, so rewriting `*` cannot corrupt what `**` already produced. */
export function globToRegExp(glob: string): RegExp {
  const anyDirs = '\u0001';
  const anything = '\u0002';
  const body = glob
    .replace(/[.+^${}()|[\]\\]/g, '\\$&')
    .replace(/\*\*\//g, anyDirs)
    .replace(/\*\*/g, anything)
    .replace(/\*/g, '[^/]*')
    .replace(/\?/g, '[^/]')
    .split(anyDirs).join('(?:.*/)?')
    .split(anything).join('.*');
  return new RegExp(`^${body}$`, 'i');
}
