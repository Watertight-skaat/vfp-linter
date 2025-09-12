a = "Fox"
? "&a.Pro"

* test in logical expression
b = ".and. .t."
if .t. &b
	? "works"
endif

* test in command like CALC
c = STRTRAN(.t., "part1", "part2")
d = ""
CALCULATE cnt() for .t. = .t. ;
    &d ;
	AND (((.t.).and. test="test").or. &c) ;
	AND NOT &c ;
	AND tablename.pid = 0 ;
TO m.TotalCount

* in group
GroupCondition = ",6"
SELECT * FROM tablename GROUP BY 1,2,3,4,5 &GroupCondition having count(*) > 0

* in subquery
select * from ( ;
    SELECT * FROM tableA group by 1,2 &GroupCondition ;
) subq

* as a field name
my_macro = "test"
SELECT &my_macro as fieldA from tableA

* index on
my_macro = ".t."
INDEX on fielda + fieldb FOR &my_macro OR &my_macro

* in select list
my_macro = ".t."
SELECT fieldA as aliasA &my_macro , fieldB FROM tableB ;

* multiple macros
macroA = "fieldA"
macroB = "fieldB"
SELECT &macroA as aliasA, &macroB as aliasB FROM enumFieldsTable ;

* before select list
my_macro = "* from tableA union all select"
SELECT &my_macro fieldA FROM tableA
SELECT &my_macro fieldA as aliasA FROM tableA 

* before where list
SELECT fieldA from tableA WHERE &my_macro my_func(fieldA) into cursor SalesCurTemp

* inside IIF
SELECT fieldA, IIF(&ReceiptFilt2 fieldA="test", fieldA, 0) as aliasA FROM tableA
