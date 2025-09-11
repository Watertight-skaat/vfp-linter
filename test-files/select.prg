select * from tablename

select ;
	* ;
from ;
	tablename

SELECT ;
	field1, ;
	sum(field2) AS Total, ;
	count(*) AS Count ;
FROM metrics ;
WHERE datetime > DATE() - 7 ;
GROUP BY 1 ;
ORDER BY 1 ;
INTO CURSOR Cursorname

SELECT *
FROM tableA ;
    JOIN tableB ON prop1 = prop2
    ,NewTran ;
    ,Prodmast ;

SELECT * FROM tablename ;
    *comment
WHERE .t.

SELECT 1 FROM tableA JOIN tableB ON fieldA + "constant" = "constant"
SELECT * FROM Trancursor, delivcd LEFT OUTER JOIN codes ON stnum + "test" = "test" WHERE .t.
SELECT * FROM Trancursor t, delivcd d JOIN codes c ON c.stnum + "test" = "test" WHERE .t.

select CAST(fieldname as m(4)) as AliasName from tablename

SELECT * FROM tablename GROUP BY tablename.fieldA HAVING .t. INTO CURSOR cname

SELECT * FROM tablename WHERE field in (select field from tablename2)

SELECT * FROM tablename WHERE field not in (select field from tablename2)

SELECT prop1, prop2 ;
FROM tableA ;
INTO CURSOR cursA ;
GROUP BY 1,2,3 