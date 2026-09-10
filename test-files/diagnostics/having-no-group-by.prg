* HAVING with no GROUP BY is only a post-filter on the result set.
SELECT cust_id, total FROM orders HAVING total > 100 INTO CURSOR curBig

* With a GROUP BY it is a real aggregate filter and must stay clean.
SELECT cust_id, SUM(total) AS total FROM orders GROUP BY cust_id HAVING SUM(total) > 100 INTO CURSOR curOk
