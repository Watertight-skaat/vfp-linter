* The database container: the commands that make one and open it, the ones that put a table in or take it out,
* and the transaction frame around the writes. All of these used to reach the catch-all and report as unsupported.
CREATE DATABASE mydata
OPEN DATABASE mydata EXCLUSIVE
OPEN DATABASE (m.cContainer) SHARED NOUPDATE
OPEN DATABASE ?
CREATE CONNECTION myconn DATASOURCE 'dsn'
CREATE CONNECTION myconn CONNSTRING lcConn
FREE TABLE customer
REMOVE TABLE customer DELETE RECYCLE
DELETE DATABASE mydata DELETETABLES
DELETE VIEW myview
DELETE CONNECTION myconn
CREATE TRIGGER ON customer FOR INSERT AS NewCustomer()
DELETE TRIGGER ON customer FOR INSERT
VALIDATE DATABASE RECOVER
CREATE SQL VIEW myview REMOTE CONNECTION myconn AS SELECT custid FROM customer

BEGIN TRANSACTION
REPLACE balance WITH balance - m.nAmount
END TRANSACTION
ROLLBACK
