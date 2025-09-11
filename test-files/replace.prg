replace testvar WITH 70

REPLACE ALL TableA.PropertyA WITH .t. 

replace ALL fieldA WITH customfunction("parameter") FOR EMPTY(fieldA)

replace ALL fieldA WITH objectA.propertyA FOR ALLTRIM(fieldA)==ALLTRIM(objectA.propertyA) .and. EMPTY(fieldA) 

REPLACE coacctnum	WITH "xxdevxx", updateuser	WITH "xxdevxx"

REPLACE ALL FieldName WITH customfunc("test") FOR fieldA="U" IN TableName