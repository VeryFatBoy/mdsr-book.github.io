## ps aux | grep "mysql"


## export PATH=$PATH:/usr/local/mysql/bin

## echo $PATH


## which mysql


## mysql -u root


## mysql -u root -p


## bbaumer@bbaumer-Precision-Tower-7810:~$ mysql -u root -p

## Enter password:

## Welcome to the MySQL monitor.  Commands end with ; or \g.

## Your MySQL connection id is 47

## Server version: 5.7.31-0ubuntu0.18.04.1 (Ubuntu)

## 
## Copyright (c) 2000, 2020, Oracle and/or its affiliates. All rights reserved.

## 
## Oracle is a registered trademark of Oracle Corporation and/or its

## affiliates. Other names may be trademarks of their respective

## owners.

## 
## Type 'help;' or '\h' for help. Type '\c' to clear the current input

## statement.

## 
## mysql>


## SELECT User, Host, Password  FROM mysql.user;


## UPDATE mysql.user SET Password = PASSWORD('mypass') WHERE User = 'root';

## FLUSH PRIVILEGES;


## CREATE USER 'r-user'@'localhost' IDENTIFIED BY 'mypass';


## GRANT ALL PRIVILEGES ON *.* TO 'r-user'@'localhost' WITH GRANT OPTION;


## FLUSH PRIVILEGES;


## mysql -u r-user -p


## [client]

## user=r-user

## password="mypass"


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


## ----sql-r, message=FALSE, echo=FALSE, fig.cap="Schematic of SQL-related (ref:R) packages and their dependencies."----
library(tidygraph)
V <- tibble(
  id = 1:7, 
  label = c("dplyr", "dbplyr", "DBI", "RMySQL", "RPostgreSQL", "RSQLite", "odbc"),
  shape = "rectangle",
  size = 20
)
E <- tribble(
  ~from, ~to,
  1, 2, 
  2, 3, 
  3, 4,
  3, 5,
  3, 6, 
  3, 7,
)
g <- tbl_graph(edges = E, nodes = V)

library(ggraph)
ggraph(g, layout = 'dendrogram') + 
  geom_edge_fan(
    arrow = arrow(type = "closed", length = unit(0.2, "inches")),
    end_cap = circle(0.5, 'cm')
  ) + 
  geom_node_label(aes(label = label), color = "dodgerblue") + 
  coord_cartesian(clip = "off") + 
  theme_graph()


## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(engine = "R")


## ----message=FALSE------------------------------------------------------------
library(RMySQL)


## ----message=FALSE, eval=FALSE------------------------------------------------
## library(dplyr)
## db <- dbConnect(
##   RMySQL::MySQL(),
##   dbname = "airlines", host = "localhost",
##   user = "r-user", password = "mypass"
## )


## ----message=FALSE, eval=FALSE------------------------------------------------
## db <- dbConnect(
##   RMySQL::MySQL(),
##   dbname = "airlines", host = "localhost",
##   default.file = "~/.my.cnf"
## )

## ----echo=FALSE---------------------------------------------------------------
db <- dbConnect_scidb(dbname = "airlines")


## -----------------------------------------------------------------------------
res <- tbl(db, sql("SELECT faa, name FROM airports"))
res


## -----------------------------------------------------------------------------
class(res)


## -----------------------------------------------------------------------------
collect(res)


## -----------------------------------------------------------------------------
dbGetQuery(db, "SELECT faa, name FROM airports LIMIT 0,5")


## -----------------------------------------------------------------------------
dbGetQuery(db, "EXPLAIN SELECT faa, name FROM airports")
dbGetQuery(db, "DESCRIBE airports")
dbGetQuery(db, "SHOW DATABASES")


## sqlite3


## .open babynamesdata.sqlite3


## .mode csv

## .import babynames.csv babynames

## .import births.csv births

## .exit


## ----eval=FALSE---------------------------------------------------------------
## db <- dbConnect(RSQLite::SQLite(), "babynamesdata.sqlite3")
## babynames <- tbl(db, "babynames")
## babynames %>%
##   filter(name == "Benjamin")

