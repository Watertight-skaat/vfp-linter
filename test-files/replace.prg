replace ALL fieldA WITH customfunction("parameter") FOR EMPTY(fieldA)
replace ALL fieldA WITH objectA.propertyA FOR ALLTRIM(fieldA)==ALLTRIM(objectA.propertyA) .and. EMPTY(fieldA) 
REPLACE ALL TableA.PropertyA WITH .t. 