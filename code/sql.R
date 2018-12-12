## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/sql-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ", warning=FALSE)

## ----my.cnf, include=FALSE, eval=FALSE-----------------------------------
## # ~/.my.cnf file should look like this:
## [client]
## user=bbaumer
## password=your_password
## 
## [clientscidb]
## host=scidb.smith.edu
## user=mth292
## password=RememberPi
## 
## # Then use --defaults-group-suffix to access different DBs:
## mysql --defaults-group-suffix=scidb

## ----connect, echo=FALSE-------------------------------------------------
library(methods)
db <- src_scidb("airlines")
flights <- tbl(db, "flights")
carriers <- tbl(db, "carriers")

## ----dplyr-flights, warning=FALSE----------------------------------------
q <- flights %>%
  filter(year == 1996 & month == 9) %>%
  filter(dest == "JFK") %>%
  inner_join(carriers, by = c("carrier" = "carrier")) %>%
  group_by(name) %>%
  summarize(N = n(), 
            pct_ontime = sum(arr_delay <= 15) / n()) %>%
  filter(N >= 100) %>%
  arrange(desc(pct_ontime))
head(q, 4)

## ----connect-show, eval=FALSE--------------------------------------------
## db <- src_scidb("airlines")
## flights <- tbl(db, "flights")
## carriers <- tbl(db, "carriers")

## ------------------------------------------------------------------------
class(flights)

## ----show-query----------------------------------------------------------
show_query(q)

## SELECT

## ----translate-sql-------------------------------------------------------
translate_sql(mean(arr_delay))

## ------------------------------------------------------------------------
translate_sql(paste0("this", "is", "a", "string"))

## ------------------------------------------------------------------------
carriers %>%
  mutate(name_code = paste0(name, "(", carrier, ")"))
class(carriers)

## ------------------------------------------------------------------------
carriers %>%
  mutate(name_code = CONCAT(name, "(", carrier, ")"))

## ----warning=FALSE-------------------------------------------------------
carriers %>%
  collect() %>%
  mutate(name_code = paste0(name, "(", carrier, ")"))

## ------------------------------------------------------------------------
print(object.size(carriers), units = "Kb")
print(object.size(collect(carriers)), units = "Kb")

## ----big-calc, cache=TRUE, eval=TRUE-------------------------------------
n <- 100 * 1000000
x <- matrix(runif(n), ncol = 100)
dim(x)
print(object.size(x), units = "Mb")

## ----knitr-mysql, echo=FALSE, eval=TRUE----------------------------------
opts_chunk$set(engine.opts = "--defaults-group-suffix=scidb -t airlines")

## SHOW TABLES;

## DESCRIBE airports;

## SELECT * FROM flights;

## SELECT * FROM flights LIMIT 0,10;

## SELECT year, month, day, dep_time, sched_dep_time, dep_delay, orig

## ----include=FALSE, eval=FALSE-------------------------------------------
## # ?merge

## SELECT code, name FROM airports;

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## ----db-src,echo=FALSE, message=FALSE------------------------------------
library(mdsr)
library(DBI)
db <- src_scidb("airlines")

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## DESCRIBE airports;

## SELECT

## SELECT * FROM airports WHERE faa = 'SJU';

## (SELECT

## SELECT faa, name, tz, city

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## SELECT

## ----ft8-res-query,warning=FALSE-----------------------------------------
query <- 
  "SELECT o.carrier, c.name, 
    sum(1) as numFlights,
    sum(if(arr_delay > 15 AND arr_delay <= 119, 1, 0)) as shortDelay,
    sum(if(arr_delay >= 120 OR 
           cancelled = 1 OR diverted = 1, 1, 0)) as longDelay
  FROM
    flights o
  LEFT JOIN 
    carriers c ON o.carrier = c.carrier
  WHERE year = 2014
  GROUP BY carrier
  ORDER BY shortDelay desc"

## ----ft8-res,warning=FALSE-----------------------------------------------
res <- DBI::dbGetQuery(db$con, query)
res

## ----ft8-wrangle,warning=FALSE-------------------------------------------
res <- res %>%
  mutate(name = gsub("Air(lines|ways| Lines)", "", name),
         name = gsub("(Inc\\.|Co\\.|Corporation)", "", name),
         name = gsub("\\(.*\\)", "", name),
         name = gsub(" *$", "", name))
res

## ----ft8-carriers--------------------------------------------------------
carriers2014 <- res %>%
  mutate(groupName = ifelse(name %in% 
    c("Envoy Air", "American Eagle"), "American", name)) %>%
  mutate(groupName = 
    ifelse(groupName == "AirTran", "Southwest", groupName)) %>%
  group_by(groupName) %>%
  summarize(numFlights = sum(numFlights), 
            wShortDelay = sum(shortDelay), 
            wLongDelay = sum(longDelay)) %>%
  mutate(wShortDelayPct = wShortDelay / numFlights,
         wLongDelayPct = wLongDelay / numFlights,
         delayed = wShortDelayPct + wLongDelayPct,
         ontime = 1 - delayed)
carriers2014

## ------------------------------------------------------------------------
carriers_tidy <- carriers2014 %>%
  select(groupName, wShortDelayPct, wLongDelayPct, delayed) %>%
  tidyr::gather(key = "delay_type", value = "pct", -groupName, -delayed)
delay_chart <- ggplot(data = carriers_tidy, 
                      aes(x = reorder(groupName, pct, max), y = pct)) + 
  geom_bar(stat = "identity", aes(fill = delay_type)) +
  scale_fill_manual(name = NULL, values = c("red", "gold"), 
        labels = c("Flights Delayed 120+ Minutes, Canceled or Diverted", 
                               "Flights Delayed 15-119 Minutes")) + 
  scale_y_continuous(limits = c(0, 1)) + 
  coord_flip() + 
  ggtitle("Southwest's Delays Are Short; United's Are Long") + 
  ylab(NULL) + xlab(NULL) + 
  ggthemes::theme_fivethirtyeight()

## ----ft8-plot,fig.cap="Re-creation of the FiveThirtyEight plot on flight delays."----
delay_chart +
  geom_text(data = filter(carriers_tidy, delay_type == "wShortDelayPct"), 
    aes(label = paste0(round(pct * 100, 1), "% ")), hjust = "right") + 
  geom_text(data = filter(carriers_tidy, delay_type == "wLongDelayPct"), 
    aes(y = delayed - pct, label = paste0(round(pct * 100, 1), "% ")),
            hjust = "left", nudge_y = 0.01)

## ----ft8-two-queries,warning=FALSE---------------------------------------
queryDest <- "SELECT
            dest, 
            sum(1) as numFlights,
            avg(arr_delay) as avgArrivalDelay
          FROM
            flights o
          WHERE year = 2014
          GROUP BY dest
          ORDER BY numFlights desc
          LIMIT 0, 30"
dests <- DBI::dbGetQuery(db$con, queryDest)
queryArr <- "SELECT
            origin, 
            sum(1) as numFlights,
            avg(arr_delay) as avgDepartDelay
          FROM
            flights o
          WHERE year = 2014
          GROUP BY origin
          ORDER BY numFlights desc
          LIMIT 0, 30"
origins <- DBI::dbGetQuery(db$con, queryArr)
dests %>%
  left_join(origins, by = c("dest" = "origin")) %>%
  select(dest, avgDepartDelay, avgArrivalDelay) %>%
  arrange(desc(avgDepartDelay))

## ----warning=FALSE-------------------------------------------------------
query <- "SELECT 
            origin, dest, 
            sum(1) as numFlights,
            avg(arr_delay) as avgDelay
          FROM
            flights o
          WHERE year = 2014
          GROUP BY origin, dest"
routes <- dbGetQuery(db$con, query)
head(routes)

## ----warning=FALSE-------------------------------------------------------
query <- "SELECT 
            origin, dest, 
            o.carrier, c.name, 
            sum(1) as numFlights,
            avg(arr_delay) as avgDelay
          FROM
            flights o
          LEFT JOIN 
            carriers c ON o.carrier = c.carrier
          WHERE year = 2014
          GROUP BY origin, dest, o.carrier"
routes_carriers <- dbGetQuery(db$con, query)

## ------------------------------------------------------------------------
routes_aug <- left_join(routes_carriers, routes, 
  by = c("origin" = "origin", "dest" = "dest"))
head(routes_aug)

## ------------------------------------------------------------------------
routes_aug %>%
  group_by(carrier) %>%
  # use gsub to remove parentheses
  summarise(carrier_name = gsub("\\(.*\\)", "", first(name)), 
            numRoutes = n(), numFlights = sum(numFlights.x), 
            wAvgDelay = sum(numFlights.x * (avgDelay.x - avgDelay.y), 
              na.rm = TRUE) / sum(numFlights.x)) %>%
  arrange(wAvgDelay)

