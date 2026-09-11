// The registry. Order matters only for the settings schema and the README table; diagnostics are sorted by position.

import type { Rule } from '../rule.js';
import { havingWithoutGroupBy, selectWithoutInto, unlinkedTables } from './sql.js';
import { danglingTerminator, duplicateCase, emptyBranch, privateAll, tryWithoutCatch, unreachableCode, unsupportedSyntax, unterminatedBlock } from './structure.js';
import { implicitPrivate, missingMemvarPrefix, unusedLocal } from './symbols.js';

export const rules: Rule[] = [
  danglingTerminator,
  unterminatedBlock,
  unsupportedSyntax,
  implicitPrivate,
  unusedLocal,
  missingMemvarPrefix,
  unreachableCode,
  duplicateCase,
  privateAll,
  unlinkedTables,
  selectWithoutInto,
  havingWithoutGroupBy,
  tryWithoutCatch,
  emptyBranch
];
