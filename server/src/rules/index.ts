// The registry. Order matters only for the settings schema and the README table; diagnostics are sorted by position.

import type { Rule } from '../rule.js';
import { havingWithoutGroupBy, selectWithoutInto, unlinkedTables } from './sql.js';
import { danglingTerminator, duplicateCase, emptyBranch, privateAll, tryWithoutCatch, unclosedTransaction, unreachableCode, unsupportedSyntax, unterminatedBlock, unterminatedString } from './structure.js';
import { implicitPrivate, missingMemvarPrefix, unusedLocal } from './symbols.js';
import { duplicateRoutine, missingFile, tooManyArguments } from './workspace.js';

export const rules: Rule[] = [
  danglingTerminator,
  unterminatedBlock,
  unterminatedString,
  unsupportedSyntax,
  implicitPrivate,
  unusedLocal,
  missingMemvarPrefix,
  tooManyArguments,
  duplicateRoutine,
  missingFile,
  unreachableCode,
  duplicateCase,
  privateAll,
  unclosedTransaction,
  unlinkedTables,
  selectWithoutInto,
  havingWithoutGroupBy,
  tryWithoutCatch,
  emptyBranch
];
