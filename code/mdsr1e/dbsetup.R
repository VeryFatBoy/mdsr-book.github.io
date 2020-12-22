## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/dbsetup-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ps aux | grep "mysql"

## export PATH=$PATH:/usr/local/mysql/bin

## mysql -u root

## mysql -u root -p

## find / -name "mysql"

## ----eval=FALSE, prompt=FALSE--------------------------------------------
## bbaumer@bbaumer-Precision-Tower-7810:~$ mysql -u root -p
## Enter password:
## Welcome to the MySQL monitor.  Commands end with ; or \g.
## Your MySQL connection id is 47
## Server version: 5.5.44-0ubuntu0.14.04.1 (Ubuntu)
## 
## Copyright (c) 2000, 2015, Oracle and/or its affiliates. All rights reserved.
## 
## Oracle is a registered trademark of Oracle Corporation and/or its
## affiliates. Other names may be trademarks of their respective
## owners.
## 
## Type 'help;' or '\h' for help. Type '\c' to clear the current input
## statement.
## 
## mysql>

## ----opts, echo=FALSE,eval=TRUE------------------------------------------
opts_chunk$set(engine.opts = "--defaults-group-suffix=scidb")

## SELECT User, Host, Password  FROM mysql.user;

## UPDATE mysql.user SET Password = PASSWORD('mypass') WHERE User = 'root';

## CREATE USER 'r-user'@'localhost' IDENTIFIED BY 'mypass';

## GRANT ALL PRIVILEGES ON *.* TO 'r-user'@'localhost' WITH GRANT OPTION;

## FLUSH PRIVILEGES;

## mysql -u yourusername -p

## [client]

## mysql -u yourusername -p

## mysql -u yourusername -p dbname < myscript.sql

## sudo -u postgres psql

## \du

## ALTER USER postgres PASSWORD 'some_pass';

## CREATE USER yourusername SUPERUSER CREATEDB PASSWORD 'some_pass';

## CREATE DATABASE airlines;

## \q

## psql -U yourusername -W

## mysql -u username -p -h localhost dbname

## ----echo=FALSE----------------------------------------------------------
opts_chunk$set(engine = "R")

## ----message=FALSE-------------------------------------------------------
library(RMySQL)

## ----message=FALSE, eval=FALSE-------------------------------------------
## library(dplyr)
## db <- src_mysql(dbname = "airlines", host = "localhost",
##                 user = "r-user", password = "mypass")

## ----message=FALSE, eval=FALSE-------------------------------------------
## db <- src_mysql(dbname = "airlines", host = "localhost",
##                 default.file = "~/.my.cnf",
##                 user = NULL, password = NULL)

## ----echo=FALSE----------------------------------------------------------
db <- src_scidb(dbname = "airlines")

## ------------------------------------------------------------------------
res <- tbl(db, sql("SELECT faa, name FROM airports"))
res

## ------------------------------------------------------------------------
class(res)

## ------------------------------------------------------------------------
collect(res)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## library(DBI)
## con <- dbConnect(MySQL(), dbname = "airlines", host = "localhost",
##                 user = "r-user", password = "mypass")

## ----echo=FALSE----------------------------------------------------------
library(DBI)
con <- dbConnect(MySQL(), dbname="airlines", host="scidb.smith.edu",
  user = "mth292", password="RememberPi")

## ------------------------------------------------------------------------
res <- dbGetQuery(con, "SELECT faa, name FROM airports")
head(res, 10)

## ------------------------------------------------------------------------
class(res)

## ------------------------------------------------------------------------
dbGetQuery(con, "EXPLAIN SELECT faa, name FROM airports")
dbGetQuery(con, "DESCRIBE airports")
dbGetQuery(con, "SHOW DATABASES")

## ------------------------------------------------------------------------
db
class(db)

## ------------------------------------------------------------------------
con
class(con)

## ------------------------------------------------------------------------
class(db$con)

## ------------------------------------------------------------------------
dbGetQuery(db$con, "SHOW TABLES")

## sqlite3

## ----eval=FALSE----------------------------------------------------------
## .open babynamesdata.sqlite3

## ----eval=FALSE----------------------------------------------------------
## .mode csv
## .import babynames.csv babynames
## .import births.csv births
## .exit

## ----eval=TRUE-----------------------------------------------------------
db <- src_sqlite(path = "babynamesdata.sqlite3")
babynames <- tbl(db, "babynames")
babynames %>% filter(name == "Benjamin")

