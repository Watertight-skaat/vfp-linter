* SQL CASE is an expression, so it works in a SELECT list and in an ordinary assignment.
SELECT cust_id, CASE WHEN total > 100 THEN "big" ELSE "small" END AS band ;
  FROM orders INTO CURSOR curBands

lcLabel = CASE lnType WHEN 1 THEN "retail" WHEN 2 THEN "trade" ELSE "unknown" END

* Class::Method reaches a parent implementation explicitly.
MyBase::Init()
lnValue = MyBase::GetValue()
DODEFAULT()
