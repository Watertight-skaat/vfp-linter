// The files VFP keeps in its own binary formats, and the commands that open one in the designer it belongs to.
// This sits beside the index rather than in the client because it is a fact about VFP, not about the editor: the hover that offers to open a form and the client that opens it have to agree about which files have a designer at all.

export type Designer = 'form' | 'class' | 'report' | 'label' | 'menu' | 'database' | 'project';

/** The file VFP opens, and the verb it opens it with. A .prg is deliberately absent: it is text, and the editor asking is a better place to read it than VFP. */
const designers: Record<string, { designer: Designer; verb: string }> = {
	'.scx': { designer: 'form', verb: 'MODIFY FORM' },
	'.vcx': { designer: 'class', verb: 'MODIFY CLASS' },
	'.frx': { designer: 'report', verb: 'MODIFY REPORT' },
	'.lbx': { designer: 'label', verb: 'MODIFY LABEL' },
	'.mnx': { designer: 'menu', verb: 'MODIFY MENU' },
	'.dbc': { designer: 'database', verb: 'MODIFY DATABASE' },
	'.pjx': { designer: 'project', verb: 'MODIFY PROJECT' }
};

// Each of those carries its memo half in a second file, which sits beside it in the explorer and is the same document to a developer. Right-clicking one opens the form rather than reporting that a .sct has no designer.
const companions: Record<string, string> = { '.sct': '.scx', '.vct': '.vcx', '.frt': '.frx', '.lbt': '.lbx', '.mnt': '.mnx' };

const extensionOf = (file: string) => {
	const dot = file.lastIndexOf('.');
	return dot > file.replace(/\\/g, '/').lastIndexOf('/') ? file.slice(dot) : '';
};

/** The file VFP knows the document by: a companion resolves to the half that names it, keeping the case it was written in. */
export function designerFile(file: string): string {
	const extension = extensionOf(file);
	const primary = companions[extension.toLowerCase()];
	if (!primary) return file;
	return file.slice(0, file.length - extension.length) + (extension === extension.toUpperCase() ? primary.toUpperCase() : primary);
}

/** The designer this file belongs in, or null when the editor can open it itself. */
export function designerFor(file: string): Designer | null {
	return designers[extensionOf(designerFile(file)).toLowerCase()]?.designer ?? null;
}

// Windows forbids a double quote in a path, so one delimiter covers every path that can exist -- and a name holding one came from somewhere other than the filesystem, where it would reach VFP as the end of the command and the start of another.
const literal = (text: string) => (text.includes('"') ? null : `"${text}"`);

/**
 * The command that opens the file in its designer, or null when there is none to send.
 *
 * The path is parenthesised because VFP reads a bare name as a name and stops at the dot in `MODIFY FORM custedit.scx`. NOWAIT is what keeps the call from blocking: without it the automation call does not return until the developer closes the designer.
 */
export function vfpCommandFor(file: string, className?: string): string | null {
	const target = designerFile(file);
	const entry = designers[extensionOf(target).toLowerCase()];
	const path = literal(target);
	if (!entry || !path) return null;
	// A library holds many classes and MODIFY CLASS opens one of them. The name is spliced in bare, so it has to be a name and nothing else.
	if (entry.designer === 'class') {
		if (!className || !/^[A-Za-z_]\w*$/.test(className)) return null;
		return `${entry.verb} ${className} OF (${path}) NOWAIT`;
	}
	return `${entry.verb} (${path}) NOWAIT`;
}

/** The array AVCXCLASSES fills. Named here and read back by resources/vfp-open.vbs, which is the only other place that knows it. */
export const classArray = '__vfplint_classes';

/** What to ask a running VFP for the classes in a library, so the editor can offer them before deciding which one to open. */
export function classListExpression(file: string): string | null {
	const target = designerFile(file);
	const path = literal(target);
	return designerFor(target) === 'class' && path ? `AVCXCLASSES(${classArray}, ${path})` : null;
}
