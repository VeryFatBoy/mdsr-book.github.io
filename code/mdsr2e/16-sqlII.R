## SHOW DATABASES;


## CREATE DATABASE airlines;


## USE airlines;


## SHOW TABLES;


## DESCRIBE airports;


## ---- echo=FALSE, include=FALSE-----------------------------------------------
create_airports <- db %>%
  dbGetQuery("SHOW CREATE TABLE airports") %>%
  pull(2) %>%
  cat()


## SHOW CREATE TABLE airports;

## CREATE TABLE `airports` (

##   `faa` varchar(3) NOT NULL DEFAULT '',

##   `name` varchar(255) DEFAULT NULL,

##   `lat` decimal(10,7) DEFAULT NULL,

##   `lon` decimal(10,7) DEFAULT NULL,

##   `alt` int(11) DEFAULT NULL,

##   `tz` smallint(4) DEFAULT NULL,

##   `dst` char(1) DEFAULT NULL,

##   `city` varchar(255) DEFAULT NULL,

##   `country` varchar(255) DEFAULT NULL,

##   PRIMARY KEY (`faa`)


## ALTER TABLE airports CHANGE tz tz smallint(2) DEFAULT 0;


## ---- include=FALSE-----------------------------------------------------------
knitr::opts_knit$set(sql.print = mdsr::mdsr_sql_keys_table)


## SHOW KEYS FROM carriers;


## ----include=FALSE------------------------------------------------------------
id_1 <- c(5, 18, 2, 3, 4, 2, 1)
id_2 <- c(5, 6, 3, 18, 4, 7, 1, 2)


## ----echo=FALSE---------------------------------------------------------------
id_1
id_2


## ----echo=FALSE---------------------------------------------------------------
sort(id_1)
sort(id_2)


## SHOW INDEXES FROM flights;


## ----show-indexes, echo=FALSE-------------------------------------------------
flights_indices %>%
  mdsr::mdsr_sql_keys_table(caption = "Indices in the flights table.")


## ---- include=FALSE-----------------------------------------------------------
knitr::opts_knit$set(sql.print = mdsr::mdsr_sql_explain_table)


## EXPLAIN SELECT * FROM flights WHERE distance > 3000;


## EXPLAIN SELECT * FROM flights WHERE year = 2013;


## EXPLAIN SELECT * FROM flights WHERE year = 2013 AND month = 6;


## EXPLAIN SELECT * FROM flights WHERE month = 6;


## EXPLAIN

##   SELECT * FROM planes p

##   LEFT JOIN flights o ON p.tailnum = o.TailNum

##   WHERE manufacturer = 'BOEING';


## EXPLAIN

##   SELECT * FROM planes p

##   LEFT JOIN flights o ON p.Year = o.Year

##   WHERE manufacturer = 'BOEING';


## ---- include=FALSE-----------------------------------------------------------
knitr::opts_knit$set(sql.print = mdsr::mdsr_table)


## SELECT faa, name FROM airports WHERE faa = 'DCA';


## UPDATE airports

##   SET name = 'Washington National'

##   WHERE faa = 'DCA';


## INSERT INTO airports (faa, name)

##   VALUES ('SJU', 'Luis Munoz Marin International Airport');


## -----------------------------------------------------------------------------
library(babynames)


## ----warning=FALSE------------------------------------------------------------
babynames %>%
  filter(year > 1975) %>%
  write_csv("babynames.csv")
births %>%
  write_csv("births.csv")
list.files(".", pattern = ".csv")  


## USE babynamedb;


## DROP TABLE IF EXISTS babynames;

## DROP TABLE IF EXISTS births;


## -----------------------------------------------------------------------------
glimpse(babynames)


## CREATE TABLE `babynames` (

##   `year` smallint(4) NOT NULL DEFAULT 0,

##   `sex` char(1) NOT NULL DEFAULT 'F',

##   `name` varchar(255) NOT NULL DEFAULT '',

##   `n` mediumint(7) NOT NULL DEFAULT 0,

##   `prop` decimal(21,20) NOT NULL DEFAULT 0,

##   PRIMARY KEY (`year`, `sex`, `name`)

## ) ENGINE=MyISAM DEFAULT CHARSET=latin1;


## -----------------------------------------------------------------------------
glimpse(births)


## CREATE TABLE `births` (

##   `year` smallint(4) NOT NULL DEFAULT 0,

##   `births` mediumint(8) NOT NULL DEFAULT 0,

##   PRIMARY KEY (`year`)

## ) ENGINE=MyISAM DEFAULT CHARSET=latin1;


## LOAD DATA LOCAL INFILE './babynames.csv' INTO TABLE `babynames`

##   FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' IGNORE 1 LINES;

## SHOW WARNINGS;

## LOAD DATA LOCAL INFILE './births.csv' INTO TABLE `births`

##   FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' IGNORE 1 LINES;

## SHOW WARNINGS;


## USE babynamedb;

## 
## DROP TABLE IF EXISTS babynames;

## DROP TABLE IF EXISTS births;

## 
## CREATE TABLE `babynames` (

##   `year` smallint(4) NOT NULL DEFAULT 0,

##   `sex` char(1) NOT NULL DEFAULT 'F',

##   `name` varchar(255) NOT NULL DEFAULT '',

##   `n` mediumint(7) NOT NULL DEFAULT 0,

##   `prop` decimal(21,20) NOT NULL DEFAULT 0,

##   PRIMARY KEY (`year`, `sex`, `name`)

## ) ENGINE=MyISAM DEFAULT CHARSET=latin1;

## 
## CREATE TABLE `births` (

##   `year` smallint(4) NOT NULL DEFAULT 0,

##   `births` mediumint(8) NOT NULL DEFAULT 0,

##   PRIMARY KEY (`year`)

## ) ENGINE=MyISAM DEFAULT CHARSET=latin1;

## 
## LOAD DATA LOCAL INFILE './babynames.csv' INTO TABLE `babynames`

##   FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"' IGNORE 1 LINES;

## LOAD DATA LOCAL INFILE './births.csv' INTO TABLE `births`

##   FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"' IGNORE 1 LINES;

## 
## SELECT year

##   , COUNT(DISTINCT name) AS numNames

##   , SUM(n) AS numBirths

##   FROM babynames

##   GROUP BY year

##   ORDER BY numBirths DESC

##   LIMIT 0,10;


## NA

## mysql -e "SHOW DATABASES;"


## mysql -e "CREATE DATABASE babynamedb;"


## mysql --local-infile --show-warnings -v babynamedb

##   < babynamedb.mysql


## ERROR 1148 (42000): The used command is not allowed with this MySQL version


## ERROR 29 (HY000): File './babynames.csv' not found (Errcode: 13)


## ----eval=FALSE---------------------------------------------------------------
## db <- dbConnect(RMySQL::MySQL(), dbname = "babynamedb")
## babynames <- tbl(db, "babynames")
## babynames %>%
##   filter(name == "Benjamin")

