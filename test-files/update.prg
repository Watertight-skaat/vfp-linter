Update TableA set FieldA = .null. where FieldA = 0
Update TableA set FieldA = .null. from TableB where TableA.id = TableB.id and TableB.FieldA = 0

* set after FROM
UPDATE WorkPoints ;
	FROM NewPoints ;
	SET clicked = NewPoints.clicked ;
		,completed = NewPoints.completed ;
	WHERE NewPoints.mosnum#SPACE(8) ;
			.and. NewPoints.id=NewPoints.mosnum+SPACE(8) ;