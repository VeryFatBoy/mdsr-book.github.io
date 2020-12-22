## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/sql2-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ", warning=FALSE)

## ----knitr-mysql, echo=FALSE, eval=TRUE----------------------------------
opts_chunk$set(engine.opts = "--defaults-group-suffix=scidb -t airlines")

## SHOW DATABASES;

## CREATE DATABASE airlines;

## USE airlines;

## SHOW TABLES;

## DESCRIBE airports;

## SHOW CREATE TABLE airports;

## ALTER TABLE airports CHANGE tz tz smallint(2) DEFAULT 0;

## SHOW KEYS FROM carriers;

## ----include=FALSE-------------------------------------------------------
id_1 <- c(5, 18, 2, 3, 4, 2, 1)
id_2 <- c(5, 6, 3, 18, 4, 7, 1, 2)

## ----echo=FALSE----------------------------------------------------------
id_1
id_2

## ----echo=FALSE----------------------------------------------------------
sort(id_1)
sort(id_2)

## SHOW INDEXES FROM flights;

## EXPLAIN SELECT * FROM flights WHERE distance > 3000;

## EXPLAIN SELECT * FROM flights WHERE year = 2013;

## EXPLAIN SELECT * FROM flights WHERE year = 2013 AND month = 6;

## EXPLAIN SELECT * FROM flights WHERE month = 6;

## ----sys-time, eval=FALSE, include=FALSE---------------------------------
## system.time(dbGetQuery(db$con,
##   "SELECT * FROM flights WHERE Distance > 3000"))
## system.time(dbGetQuery(db$con, "SELECT * FROM flights WHERE Year = 2013"))

## EXPLAIN

## SELECT faa, name FROM airports WHERE faa = 'DCA';

## UPDATE airports

## INSERT INTO airports (faa, name)

## ------------------------------------------------------------------------
library(babynames)

## ----warning=FALSE-------------------------------------------------------
babynames %>%
  filter(year > 1975) %>%
  write.csv(file = "babynames.csv", row.names = FALSE)
births %>%
  write.csv(file = "births.csv", row.names = FALSE)
list.files(".", pattern = ".csv")  

## USE babynamedb;

## DROP TABLE IF EXISTS babynames;

## ------------------------------------------------------------------------
glimpse(babynames)

## CREATE TABLE `babynames` (

## ------------------------------------------------------------------------
glimpse(births)

## CREATE TABLE `births` (

## LOAD DATA LOCAL INFILE './babynames.csv' INTO TABLE `babynames`

## ----echo=FALSE----------------------------------------------------------
sql <- "
USE babynamedata;

DROP TABLE IF EXISTS babynames;
DROP TABLE IF EXISTS births;

CREATE TABLE `babynames` (
  `year` smallint(4) NOT NULL DEFAULT 0,
  `sex` char(1) NOT NULL DEFAULT 'F',
  `name` varchar(255) NOT NULL DEFAULT '',
  `n` mediumint(7) NOT NULL DEFAULT 0,
  `prop` decimal(21,20) NOT NULL DEFAULT 0,
  PRIMARY KEY (`year`, `sex`, `name`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

CREATE TABLE `births` (
  `year` smallint(4) NOT NULL DEFAULT 0,
  `births` mediumint(8) NOT NULL DEFAULT 0,
  PRIMARY KEY (`year`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

LOAD DATA LOCAL INFILE './babynames.csv' INTO TABLE `babynames` 
  FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"' IGNORE 1 LINES;
LOAD DATA LOCAL INFILE './births.csv' INTO TABLE `births`
  FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"' IGNORE 1 LINES;

SELECT year, count(distinct name) as numNames
  , sum(n) as numBirths
  FROM babynames 
  GROUP BY year
  ORDER BY numBirths desc
  LIMIT 0,10;
"
cat(sql, file = "/tmp/babynamedata.mysql")
cat(sql)

## mysql -e "SHOW DATABASES;"

## mysql -e "CREATE DATABASE babynamedb;"

## mysql --local-infile --show-warnings -v babynamedb

## ERROR 1148 (42000): The used command is not allowed with this MySQL version

## ERROR 29 (HY000): File './babynames.csv' not found (Errcode: 13)

## ----eval=FALSE----------------------------------------------------------
## db <- src_mysql(dbname = "babynamedb", default.file = "~/.my.cnf",
##                 user = NULL, password = NULL)
## babynames <- tbl(db, "babynames")
## babynames %>% filter(name == "Benjamin")

