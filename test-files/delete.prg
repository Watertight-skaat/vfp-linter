DELETE IN workareaname

USE customer  && Opens Customer table
DELETE FOR country = 'USA'  && Mark for deletion
DELETE FROM cities
DELETE FROM cities where .t.
DELETE FROM cities where city in (select city from citiesCur)