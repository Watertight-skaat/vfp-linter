* The entry point, calling into the framework folder beside it. FoxPro resolves a file by name over SET PATH and never by folder, and MAINSET.prg is what sets that path -- so by the time any of this runs, every code folder in the tree is on it.
* The linter searches the calling file's own directory, then the workspace roots, then the configured searchPath, and nothing else. None of the names below is in any of those, so all four are reported missing even though every one of them is in this fixture. That is the whole of the 493 missing-file findings on the Watertight tree: MAINSET.prg, APPMAIN.prg, MOSAPI.h, QBINT.vcx and DATASRCH.scx all exist and are all reported as absent.
* The setting exists to fix this by hand, but a tree laid out by what a file is rather than by who calls it cannot list every folder in it, and a user meeting 493 warnings on first open turns the rule off rather than configuring it.
#INCLUDE EMAILLIB.h

SET PROCEDURE TO mainset ADDITIVE
SET CLASSLIB TO QbInt ADDITIVE

DO FORM datasrch

RETURN .t.
