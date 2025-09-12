insert into TableA (FieldA, FieldB) values (1, 2)
insert into TableA (FieldA, FieldB)
	values (1, 2)
INSERT INTO TableA (FieldA) VALUES (m.obj1.propproc(m.cnt, 1))

* multi-line SELECT-based insert
INSERT INTO TableA FROM NAME m.varname
    SELECT * ;
        FROM TableB ;
        ORDER BY FieldA ;
        INTO CURSOR TableB READWRITE nofilter

INSERT INTO ("testname" + m.test) FROM NAME m.PeriodObj