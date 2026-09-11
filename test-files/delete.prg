DELETE IN workareaname

USE customer  && Opens Customer table
DELETE FOR country = 'USA'  && Mark for deletion
DELETE RECORD RECNO("customer") IN customer
DELETE NEXT 5
DELETE FROM cities
DELETE FROM cities where .t.
DELETE FROM cities where city in (select city from citiesCur)