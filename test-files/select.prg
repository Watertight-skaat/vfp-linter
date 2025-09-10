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