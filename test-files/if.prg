* comment in-between conditions
if .t. ;
	.and. .t.
	? "test"
endif
if .t. ; && New Logic since 2/1/17: (if NOT received within the prior 60 days or next 30 days)
    .and. .t.
    ? "test"
endif