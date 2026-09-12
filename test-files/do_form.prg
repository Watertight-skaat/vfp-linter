DO FORM testform WITH param1, m.param2 TO varname

* A form is a file, so the name holds what an identifier cannot. Read as an identifier it stopped at the hyphen and at the dot, which left the statement naming the wrong form and the remainder reported as a statement of its own.
DO FORM start-up_code_mod
DO FORM myform.scx
DO FORM "c:\forms\myform.scx"

* And the documented way to name one at runtime.
DO FORM (m.cFormName)
