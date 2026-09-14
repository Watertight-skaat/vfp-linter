// Talking to the Visual FoxPro the developer already has open.
//
// VFP is an automation server, and an interactively started one registers itself, so the extension can attach to the IDE the developer is working in rather than starting a second one beside it. That matters more than it sounds: the session the developer is in has the default directory and the SET PATH their code needs, and a form designer that cannot find a class stops on a modal Locate dialog -- inside VFP, where the editor cannot see it, and the automation call simply never returns.
//
// The conversation goes through resources/vfp-open.vbs, because COM needs a host and Windows Script Host is the one that is always there. Everything VFP is told is built and tested on this side; the script only carries it.

import { execFile } from 'child_process';
import * as fs from 'fs';
import * as path from 'path';

export interface VfpRequest {
	/** `run` sends one command to the designer; `classes` asks what a library holds. */
	mode: 'run' | 'classes';
	/** The VFP command or expression, from server/src/vfp.ts. */
	payload: string;
	/** The VFP to start if none is running. Empty means only attach to one that is. */
	exe: string;
	/** The directory a started VFP begins in. */
	cwd: string;
	/** What to tell a VFP the extension started. A VFP that was already running is left as the developer set it up. */
	startup: string[];
}

export interface VfpResult {
	/** Whether the extension had to start VFP, which is what makes its environment worth mentioning to the user. */
	created: boolean;
	lines: string[];
}

/** One field per line, so a value carrying a quote, a backslash or a bracket needs no escaping. A line break would be read back as the start of another field -- and the field it would look like is a command to run -- so it is the one thing a value cannot hold. */
export function requestText(request: VfpRequest): string {
	const field = (key: string, value: string) => `${key}=${value.replace(/[\r\n]+/g, ' ')}`;
	return [
		field('mode', request.mode),
		field('payload', request.payload),
		field('exe', request.exe),
		field('cwd', request.cwd),
		...request.startup.map(command => field('startup', command))
	].join('\r\n') + '\r\n';
}

/** What a VFP started by the extension is missing: where it is, and where the code is. The developer's own instance is never sent these. */
export function startupCommands(cwd: string, searchPath: string[], extra: string[]): string[] {
	return [
		...(cwd ? [`SET DEFAULT TO ("${cwd}")`] : []),
		...searchPath.map(directory => `SET PATH TO "${directory}" ADDITIVE`),
		...extra
	];
}

/** Where VFP may be, best first. VFP Advanced is preferred over a VFP 9 beside it: it is what the trees this linter is aimed at are built with, and its later builds sort by number rather than by text. */
export function vfpExeCandidates(options: { configured?: string; env?: string; programFiles: string; entries: string[] }): string[] {
	const installed = (match: RegExp, exe: string) => options.entries
		.filter(entry => match.test(entry))
		.sort((a, b) => b.localeCompare(a, undefined, { numeric: true }))
		.map(entry => `${options.programFiles}\\${entry}\\${exe}`);
	return [
		options.configured,
		options.env,
		...installed(/^Microsoft Visual Foxpro Advanced\b/i, 'VFPA.EXE'),
		...installed(/^Microsoft Visual FoxPro \d/i, 'vfp9.exe')
	].filter((candidate): candidate is string => !!candidate);
}

/** The first candidate that is actually there, or null when VFP cannot be found at all. */
export function findVfpExe(configured: string): string | null {
	const programFiles = process.env['ProgramFiles(x86)'] ?? 'C:\\Program Files (x86)';
	let entries: string[] = [];
	// A machine with no 32-bit Program Files has nothing to offer beyond the setting, which is not an error until nothing is found at all.
	try {
		entries = fs.readdirSync(programFiles, { withFileTypes: true }).filter(entry => entry.isDirectory()).map(entry => entry.name);
	} catch { /* nothing installed where we looked */ }
	return vfpExeCandidates({ configured, env: process.env.VFPA_EXE, programFiles, entries }).find(candidate => fs.existsSync(candidate)) ?? null;
}

/** How long VFP is given to answer. A designer that opens a modal dialog never answers at all, so this is what turns a hang into a message. */
const timeout = 90_000;

export function runVfp(request: VfpRequest, options: { script: string; workDir: string }): Promise<VfpResult> {
	// Named for this call rather than reused: asking a library for its classes and then opening one of them are two runs, and a second command started before the first has read its request would otherwise overwrite it.
	const file = path.join(options.workDir, `request-${process.pid}-${Date.now()}.txt`);
	fs.mkdirSync(options.workDir, { recursive: true });
	// UTF-16 with a mark on the front, which is what FileSystemObject reads when it is opened for Unicode.
	fs.writeFileSync(file, '\ufeff' + requestText(request), 'utf16le');

	return new Promise((resolve, reject) => {
		// Read as bytes and decoded here rather than by Node: cscript writes the console code page, and its Unicode switch sends nothing at all down a pipe.
		execFile('cscript', ['//nologo', options.script, file], { timeout, windowsHide: true, encoding: 'buffer' }, (error, stdout, stderr) => {
			fs.rmSync(file, { force: true });
			const out = decode(stdout);
			const message = decode(stderr).replace(/^error:\s*/, '').trim();
			if (error && (error as { killed?: boolean }).killed) return reject(new Error('Visual FoxPro did not answer. It is probably waiting on a dialog of its own -- switch to it and see.'));
			if (error) return reject(new Error(message || `The Visual FoxPro launcher failed: ${error.message}`));
			const lines = out.split(/\r?\n/).map(line => line.trim()).filter(Boolean);
			resolve({ created: lines[0] === 'created', lines: lines.slice(1) });
		});
	});
}

// cscript writes the console code page. Everything the script says is ASCII, and a path quoted back in an error message is close enough in this one.
const decode = (buffer: Buffer) => buffer.toString('latin1');
