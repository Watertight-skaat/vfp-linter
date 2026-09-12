replace testvar WITH 70

REPLACE ALL TableA.PropertyA WITH .t. 

replace ALL fieldA WITH customfunction("parameter") FOR EMPTY(fieldA)

replace ALL fieldA WITH objectA.propertyA FOR ALLTRIM(fieldA)==ALLTRIM(objectA.propertyA) .and. EMPTY(fieldA) 

REPLACE coacctnum	WITH "xxdevxx", updateuser	WITH "xxdevxx"

REPLACE ALL FieldName WITH customfunc("test") FOR fieldA="U" IN TableName

* The scope belongs after the field list as well as before it, and NEXT there used to close the enclosing loop.
REPLACE invbal WITH 0 RECORD 5
REPLACE invbal WITH 0 NEXT 3 IN invinfo
REPLACE invbal WITH 0 REST FOR invbal > 0 NOOPTIMIZE
