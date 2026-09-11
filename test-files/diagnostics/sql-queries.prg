* A query with nowhere to put its result browses it at run time, which in a .prg is almost never meant.
SELECT cust_id, total FROM orders WHERE total > 100

* Every way of naming a destination keeps the rule quiet.
SELECT * FROM customer INTO CURSOR curAll
SELECT * FROM customer INTO TABLE backup
SELECT * FROM customer INTO ARRAY laRows
SELECT * FROM customer TO FILE dump.txt
SELECT a FROM t1 UNION SELECT b FROM t2 INTO CURSOR curBoth

* A subquery and an INSERT ... SELECT have nowhere to put a result either, and should not be asked to.
SELECT * FROM orders WHERE cust_id IN (SELECT cust_id FROM customer) INTO CURSOR curIn
INSERT INTO archive SELECT * FROM orders

* Nothing relates these two, so VFP builds the Cartesian product.
SELECT * FROM customer, orders INTO CURSOR curCross

* Two of the three are related; the third is not.
SELECT * FROM customer c, orders o, products p ;
	WHERE c.cust_id = o.cust_id INTO CURSOR curPartial

* Properly related, by a WHERE term and by a JOIN, so both stay quiet.
SELECT * FROM customer c, orders o WHERE c.cust_id = o.cust_id INTO CURSOR curJoined
SELECT * FROM customer INNER JOIN orders ON customer.cust_id = orders.cust_id INTO CURSOR curInner

* Unqualified names could be the relating term, so the rule does not guess.
SELECT * FROM customer, orders WHERE cust_id = o_cust_id INTO CURSOR curUnknown

* Both tables keyed off the same variable are related through it, which is the usual parent/child fetch.
SELECT * FROM customer c, orders o WHERE c.cust_id = m.lnId AND o.cust_id = m.lnId INTO CURSOR curKeyed
