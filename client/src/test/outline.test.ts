import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate } from './helper';

const docUri = getDocUri('features.prg');

suite('Should get the Outline', () => {
	test('Reports the routines, and the class with its members under it', async () => {
		await activate(docUri);
		const symbols = await vscode.commands.executeCommand<vscode.DocumentSymbol[]>('vscode.executeDocumentSymbolProvider', docUri);

		assert.deepStrictEqual(symbols.map(s => [s.name, s.kind, s.detail]), [
			['Alpha', vscode.SymbolKind.Function, 'PROCEDURE (tcName)'],
			['Widget', vscode.SymbolKind.Class, 'AS Custom']
		]);
		assert.deepStrictEqual(symbols[1].children.map(c => [c.name, c.kind]), [
			['cName', vscode.SymbolKind.Property],
			['Init', vscode.SymbolKind.Method]
		]);
		// The class runs from DEFINE CLASS to ENDDEFINE, and the trailing blank line is not part of it.
		assert.deepStrictEqual([symbols[1].range.start.line, symbols[1].range.end.line], [8, 14]);
	});
});

suite('Should get folding ranges', () => {
	test('Folds every block, keeping its closing line visible', async () => {
		await activate(docUri);
		const ranges = await vscode.commands.executeCommand<vscode.FoldingRange[]>('vscode.executeFoldingRangeProvider', docUri);

		// PROCEDURE Alpha and the IF inside it; DEFINE CLASS Widget and its Init.
		assert.deepStrictEqual(ranges.map(r => [r.start, r.end]).sort((a, b) => a[0] - b[0]), [[1, 5], [3, 4], [8, 13], [11, 12]]);
	});
});
