* Fixture for the end-to-end diagnostics test.
* The first statements are valid FoxPro; the last is not a statement at all,
* so the linter must report exactly one problem, on line 6 (0-based line 5).
LOCAL lcName
lcName = "ok"
~~~ not foxpro ~~~
