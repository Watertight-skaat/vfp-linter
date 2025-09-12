Use Customers && open table
APPEND FROM Customers FOR Country = 'Finland'
APPEND BLANK
APPEND FROM (DBF("tablename"))
APPEND FROM (DBF(m.CursorName))
APPEND FROM mytxt.txt DELIMITED WITH _ WITH CHARACTER *
