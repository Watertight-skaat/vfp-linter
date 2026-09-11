/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
import * as path from 'path';

import { runTests } from '@vscode/test-electron';

async function main() {
	try {
		// The folder containing the Extension Manifest package.json, passed to `--extensionDevelopmentPath`
		const extensionDevelopmentPath = path.resolve(__dirname, '../../../');

		// The path to test runner, passed to --extensionTestsPath
		const extensionTestsPath = path.resolve(__dirname, './index');

		// Run against the oldest VS Code the extension claims to support, so the suite actually exercises the floor declared in `engines.vscode` rather than whatever `stable` happens to be today.
		await runTests({ version: '1.101.0', extensionDevelopmentPath, extensionTestsPath });
	} catch (err) {
		console.error('Failed to run tests', err);
		process.exit(1);
	}
}

main();
