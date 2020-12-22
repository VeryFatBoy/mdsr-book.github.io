## ----connect, echo=FALSE------------------------------------------------------
db <- dbConnect_scidb("airlines")
flights <- tbl(db, "flights")
carriers <- tbl(db, "carriers")


## ----dplyr-flights, warning=FALSE---------------------------------------------
q <- flights %>%
  filter(
    year == 2016 & month == 9, 
    dest == "JFK"
  ) %>%
  inner_join(carriers, by = c("carrier" = "carrier")) %>%
  group_by(name) %>%
  summarize(
    N = n(), 
    pct_ontime = sum(arr_delay <= 15) / n()
  ) %>%
  filter(N >= 100) %>%
  arrange(desc(pct_ontime))
head(q, 4)


## ----connect-show, eval=FALSE-------------------------------------------------
## library(tidyverse)
## library(mdsr)
## db <- dbConnect_scidb("airlines")
## flights <- tbl(db, "flights")
## carriers <- tbl(db, "carriers")


## -----------------------------------------------------------------------------
class(flights)


## ----show-query, warning=FALSE, message=FALSE---------------------------------
show_query(q)


## SELECT

##   c.name,

##   SUM(1) AS N,

##   SUM(arr_delay <= 15) / SUM(1) AS pct_ontime

## FROM flights AS f

## JOIN carriers AS c ON f.carrier = c.carrier

## WHERE year = 2016 AND month = 9

##   AND dest = 'JFK'

## GROUP BY name

## HAVING N >= 100

## ORDER BY pct_ontime DESC

## LIMIT 0,4;


## ----translate-sql, message=FALSE---------------------------------------------
library(dbplyr)
translate_sql(mean(arr_delay, na.rm = TRUE))


## -----------------------------------------------------------------------------
my_paste <- paste0

translate_sql(my_paste("this", "is", "a", "string"))


## ---- error=TRUE--------------------------------------------------------------
carriers %>%
  mutate(name_code = my_paste(name, "(", carrier, ")"))
class(carriers)


## -----------------------------------------------------------------------------
carriers %>%
  mutate(name_code = CONCAT(name, "(", carrier, ")"))


## ----warning=FALSE------------------------------------------------------------
carriers %>%
  collect() %>%
  mutate(name_code = my_paste(name, "(", carrier, ")"))


## -----------------------------------------------------------------------------
carriers %>%
  object.size() %>%
  print(units = "Kb")
carriers %>%
  collect() %>%
  object.size() %>%
  print(units = "Kb")


## ----big-calc, cache=TRUE, eval=TRUE------------------------------------------
n <- 100 * 1e6
x <- matrix(runif(n), ncol = 100)
dim(x)
print(object.size(x), units = "Mb")


## SHOW TABLES;


## DESCRIBE airports;


## SELECT * FROM flights;


## SELECT * FROM flights LIMIT 0,10;


## SELECT year, month, day, dep_time, sched_dep_time, dep_delay, origin

## FROM flights

## LIMIT 0, 10;

## ----select-limit2, echo=FALSE------------------------------------------------
select_limit %>%
  mdsr_table(caption = "Specifying a subset of variables.") %>%
  kableExtra::column_spec(1:3, width = "3em") %>%
  kableExtra::column_spec(5, width = "10em")


## ----sql-r, echo=FALSE--------------------------------------------------------
tribble(
  ~Concept, ~SQL, ~R, 
  "Filter by rows & columns", "(ref:filter-sql)", "(ref:filter-r)",
  "Aggregate by rows", "(ref:aggregate-sql)", "(ref:aggregate-r)", 
  "Combine two tables", "(ref:join-sql)", "(ref:join-r)"
) %>%
  mdsr_table(caption = "Equivalent commands in SQL and R, where $a$ and $b$ are SQL tables and R dataframes.")


## SELECT faa, name FROM airports;


## SELECT

##   name,

##   CONCAT('(', lat, ', ', lon, ')')

## FROM airports

## LIMIT 0, 6;


## SELECT

##   name,

##   CONCAT('(', lat, ', ', lon, ')') AS coords

## FROM airports

## LIMIT 0, 6;


## SELECT

##   name AS airport_name,

##   CONCAT('(', lat, ', ', lon, ')') AS coords

## FROM airports

## LIMIT 0, 6;


## SELECT

##   year, month, day, origin, dest,

##   flight, carrier

## FROM flights

## WHERE year = 2013 AND month = 6 AND day = 26

## AND origin = 'BDL'

## LIMIT 0, 6;


## SELECT

##   STR_TO_DATE(CONCAT(year, '-', month, '-', day), '%Y-%m-%d') AS theDate,

##   origin,

##   flight, carrier

## FROM flights

## WHERE year = 2013 AND month = 6 AND day = 26

##   AND origin = 'BDL'

## LIMIT 0, 6;


## SELECT

##   STR_TO_DATE(CONCAT(year, '-', month, '-', day), '%Y-%m-%d') AS theDate,

##   origin, flight, carrier

## FROM flights

## WHERE theDate = '2013-06-26'

##   AND origin = 'BDL'

## LIMIT 0, 6;


## SELECT

##   STR_TO_DATE(CONCAT(year, '-', month, '-', day), '%Y-%m-%d') AS theDate,

##   origin, flight, carrier

## FROM flights

## WHERE STR_TO_DATE(CONCAT(year, '-', month, '-', day), '%Y-%m-%d') =

##   '2013-06-26'

##   AND origin = 'BDL'

## LIMIT 0, 6;


## SELECT

##   DISTINCT STR_TO_DATE(CONCAT(year, '-', month, '-', day), '%Y-%m-%d')

##     AS theDate

## FROM flights

## WHERE year = 2013 AND month = 6 AND day BETWEEN 26 and 30

##   AND origin = 'BDL'

## LIMIT 0, 6;


## SELECT

##   DISTINCT STR_TO_DATE(CONCAT(year, '-', month, '-', day), '%Y-%m-%d')

##     AS theDate

## FROM flights

## WHERE year = 2013 AND month = 6 AND day IN (26, 30)

##   AND origin = 'BDL'

## LIMIT 0, 6;


## ----db-src, echo=FALSE, message=FALSE----------------------------------------
library(mdsr)
library(DBI)
db <- dbConnect_scidb("airlines")


## /* returns 557,874 records  */

## SELECT

##   COUNT(*) AS N

## FROM flights

## WHERE year = 2013 AND month = 6 OR day = 26

##   AND origin = 'BDL';


## /* returns 2,542 records  */

## SELECT

##   COUNT(*) AS N

## FROM flights

## WHERE year = 2013 AND (month = 6 OR day = 26)

##   AND origin = 'BDL';


## SELECT

##   carrier,

##   COUNT(*) AS numFlights,

##   SUM(1) AS numFlightsAlso

## FROM flights

## WHERE year = 2013 AND month = 6 AND day = 26

##   AND origin = 'BDL'

## GROUP BY carrier;


## SELECT

##   carrier,

##   COUNT(*) AS numFlights,

##   MIN(dep_time)

## FROM flights

## WHERE year = 2013 AND month = 6 AND day = 26

##   AND origin = 'BDL'

## GROUP BY carrier;


## SELECT

##   carrier,

##   COUNT(*) AS numFlights,

##   MAKETIME(

##     IF(LENGTH(MIN(dep_time)) = 3,

##       LEFT(MIN(dep_time), 1),

##       LEFT(MIN(dep_time), 2)

##     ),

##     RIGHT(MIN(dep_time), 2),

##     0

##     ) AS firstDepartureTime

## FROM flights

## WHERE year = 2013 AND month = 6 AND day = 26

##   AND origin = 'BDL'

## GROUP BY carrier

## LIMIT 0, 6;


## SELECT

##   carrier, dest,

##   COUNT(*) AS numFlights,

##   MAKETIME(

##     IF(LENGTH(MIN(dep_time)) = 3,

##       LEFT(MIN(dep_time), 1),

##       LEFT(MIN(dep_time), 2)

##     ),

##     RIGHT(MIN(dep_time), 2),

##     0

##     ) AS firstDepartureTime

## FROM flights

## WHERE year = 2013 AND month = 6 AND day = 26

##   AND origin = 'BDL'

## GROUP BY carrier, dest

## LIMIT 0, 6;


## SELECT

##   dest, SUM(1) AS numFlights

## FROM flights

## WHERE year = 2013

##   AND origin = 'BDL'

## GROUP BY dest

## ORDER BY numFlights DESC

## LIMIT 0, 6;


## SELECT

##   dest, SUM(1) AS numFlights,

##   AVG(arr_delay) AS avg_arr_delay

## FROM flights

## WHERE year = 2013

##   AND origin = 'BDL'

## GROUP BY dest

## ORDER BY avg_arr_delay ASC

## LIMIT 0, 6;


## SELECT

##   dest, SUM(1) AS numFlights,

##   AVG(arr_delay) AS avg_arr_delay

## FROM flights

## WHERE year = 2013

##   AND origin = 'BDL'

## GROUP BY dest

## HAVING numFlights > 365 * 2

## ORDER BY avg_arr_delay ASC

## LIMIT 0, 6;


## SELECT

##   dest, SUM(1) AS numFlights,

##   AVG(arr_delay) AS avg_arr_delay

## FROM flights

## WHERE year = 2013

##   AND origin = 'BDL'

##   AND numFlights > 365 * 2

## GROUP BY dest

## ORDER BY avg_arr_delay ASC

## LIMIT 0, 6;


## SELECT

##   origin, dest, SUM(1) AS numFlights,

##   AVG(arr_delay) AS avg_arr_delay

## FROM flights

## WHERE year = 2013

## GROUP BY origin, dest

## HAVING numFlights > 365 * 2

##   AND origin = 'BDL'

## ORDER BY avg_arr_delay ASC

## LIMIT 0, 6;


## SELECT

##   dest, SUM(1) AS numFlights,

##   AVG(arr_delay) AS avg_arr_delay

## FROM flights

## WHERE year = 2013

##   AND origin = 'BDL'

## GROUP BY dest

## HAVING numFlights > 365*2

## ORDER BY avg_arr_delay ASC

## LIMIT 0, 6;


## SELECT

##   dest, SUM(1) AS numFlights,

##   AVG(arr_delay) AS avg_arr_delay

## FROM flights

## WHERE year = 2013

##   AND origin = 'BDL'

## GROUP BY dest

## HAVING numFlights > 365*2

## ORDER BY avg_arr_delay ASC

## LIMIT 3,4;


## SELECT

##   origin, dest,

##   flight, carrier

## FROM flights

## WHERE year = 2013 AND month = 6 AND day = 26

##   AND origin = 'BDL'

## LIMIT 0, 6;


## SELECT

##   origin, dest,

##   airports.name AS dest_name,

##   flight, carrier

## FROM flights

## JOIN airports ON flights.dest = airports.faa

## WHERE year = 2013 AND month = 6 AND day = 26

##   AND origin = 'BDL'

## LIMIT 0, 6;

## ----join, echo=FALSE---------------------------------------------------------
join_res %>%
  mdsr_table(caption = "Using JOIN to retrieve airport names.") %>%
  kableExtra::column_spec(3, width = "15em")


## SELECT

##   origin, dest,

##   a.name AS dest_name,

##   flight, carrier

## FROM flights AS o

## JOIN airports AS a ON o.dest = a.faa

## WHERE year = 2013 AND month = 6 AND day = 26

##   AND origin = 'BDL'

## LIMIT 0, 6;

## ----join-alias, echo=FALSE---------------------------------------------------
join_alias %>%
  mdsr_table(caption = "Using JOIN with table aliases.") %>%
  kableExtra::column_spec(3, width = "15em")


## SELECT

##   dest, a.name AS dest_name,

##   o.carrier, c.name AS carrier_name

## FROM flights AS o

## JOIN airports AS a ON o.dest = a.faa

## JOIN carriers AS c ON o.carrier = c.carrier

## WHERE year = 2013 AND month = 6 AND day = 26

##   AND origin = 'BDL'

## LIMIT 0, 6;

## ----join-multiple, echo=FALSE------------------------------------------------
join_multiple %>%
  mdsr_table(caption = "Using JOIN with multiple tables.") %>%
  kableExtra::column_spec(c(2, 4), width = "15em")


## SELECT

##   flight,

##   a2.name AS orig_name,

##   a1.name AS dest_name,

##   c.name AS carrier_name

## FROM flights AS o

## JOIN airports AS a1 ON o.dest = a1.faa

## JOIN airports AS a2 ON o.origin = a2.faa

## JOIN carriers AS c ON o.carrier = c.carrier

## WHERE year = 2013 AND month = 6 AND day = 26

##   AND origin = 'BDL'

## LIMIT 0, 6;

## ----join-multiple-times, echo=FALSE------------------------------------------
join_multiple_times %>%
  mdsr_table(caption = "Using JOIN on the same table more than once.") %>%
  kableExtra::column_spec(2, width = "6em") %>%
  kableExtra::column_spec(3, width = "15em") %>%
  kableExtra::column_spec(4, width = "12em")


## DESCRIBE airports;


## SELECT

##   year, month, day, origin, dest,

##   a.name AS dest_name,

##   flight, carrier

## FROM flights AS o

## LEFT JOIN airports AS a ON o.dest = a.faa

## WHERE year = 2013 AND month = 6 AND day = 26

##   AND a.name is null

## LIMIT 0, 6;


## (SELECT

##   year, month, day, origin, dest,

##   flight, carrier

## FROM flights

## WHERE year = 2013 AND month = 6 AND day = 26

##   AND origin = 'BDL' AND dest = 'MSP')

## UNION

## (SELECT

##   year, month, day, origin, dest,

##   flight, carrier

## FROM flights

## WHERE year = 2013 AND month = 6 AND day = 26

## AND origin = 'JFK' AND dest = 'ORD')

## LIMIT 0,10;


## SELECT faa, name, tz, city

## FROM airports AS a

## WHERE tz < -8

## LIMIT 0, 6;

## ----outside, echo=FALSE------------------------------------------------------
outside %>%
  mdsr_table(caption = "First set of six airports outside the lower 48 states.") %>%
  kableExtra::column_spec(2, width = "15em")


## SELECT

##   dest, a.name AS dest_name,

##   SUM(1) AS N, COUNT(distinct carrier) AS numCarriers

## FROM flights AS o

## LEFT JOIN airports AS a ON o.dest = a.faa

## WHERE year = 2013

##   AND origin = 'BDL'

##   AND dest IN

##     (SELECT faa

##       FROM airports

##       WHERE tz < -8)

## GROUP BY dest;


## SELECT

##   dest, a.name AS dest_name,

##   SUM(1) AS N, COUNT(distinct carrier) AS numCarriers

## FROM flights AS o

## LEFT JOIN airports AS a ON o.origin = a.faa

## WHERE year = 2013

##   AND dest = 'BDL'

##   AND origin IN

##   (SELECT faa

##     FROM airports

##     WHERE tz < -7)

## GROUP BY origin;


## SELECT

##   dest, a.name AS dest_name,

##   SUM(1) AS N, COUNT(distinct carrier) AS numCarriers

## FROM flights AS o

## JOIN (SELECT *

##         FROM airports

##         WHERE tz < -7) AS a

##   ON o.origin = a.faa

## WHERE year = 2013 AND dest = 'BDL'

## GROUP BY origin;


## SELECT

##   dest, a.name AS dest_name,

##   SUM(1) AS N, COUNT(distinct carrier) AS numCarriers

## FROM flights AS o

## LEFT JOIN airports AS a ON o.origin = a.faa

## WHERE year = 2013

##   AND dest = 'BDL'

##   AND tz < -7

## GROUP BY origin;


## SELECT

##   SUM(1) AS numFlights,

##   SUM(IF(arr_delay < 15, 1, 0)) / SUM(1) AS ontimePct,

##   SUM(IF(arr_delay < 0, 1, 0)) / SUM(1) AS earlyPct,

##   SUM(arr_delay) / 1e6 AS netMinLate,

##   SUM(IF(arr_delay > 0, arr_delay, 0)) / 1e6 AS minLate,

##   SUM(IF(arr_delay < 0, arr_delay, 0)) / 1e6 AS minEarly

## FROM flights AS o

## WHERE year = 2014

## LIMIT 0, 6;


## SELECT

##   SUM(1) AS numFlights,

##   SUM(IF(arr_delay < 15, 1, 0)) / SUM(1) AS ontimePct,

##   SUM(IF(arr_delay < 0, 1, 0)) / SUM(1) AS earlyPct,

##   SUM(IF(cancelled = 1, 270, arr_delay)) / 1e6 AS netMinLate,

##   SUM(

##     IF(cancelled = 1, 270, IF(arr_delay > 0, arr_delay, 0))

##   ) / 1e6 AS minLate,

##   SUM(IF(arr_delay < 0, arr_delay, 0)) / 1e6 AS minEarly

## FROM flights AS o

## WHERE year = 2014

## LIMIT 0, 6;


## SELECT o.carrier, c.name,

##   SUM(1) AS numFlights,

##   SUM(IF(arr_delay > 15 AND arr_delay <= 119, 1, 0)) AS shortDelay,

##   SUM(

##     IF(arr_delay >= 120 OR cancelled = 1 OR diverted = 1, 1, 0)

##   ) AS longDelay

## FROM

##   flights AS o

## LEFT JOIN

##   carriers c ON o.carrier = c.carrier

## WHERE year = 2014

## GROUP BY carrier

## ORDER BY shortDelay DESC


## ----ft8-wrangle, warning=FALSE-----------------------------------------------
res <- res %>%
  as_tibble() %>%
  mutate(
    name = str_remove_all(name, "Air(lines|ways| Lines)"),
    name = str_remove_all(name, "(Inc\\.|Co\\.|Corporation)"),
    name = str_remove_all(name, "\\(.*\\)"),
    name = str_remove_all(name, " *$")
  )
res %>%
  pull(name)


## ----ft8-carriers-------------------------------------------------------------
carriers_2014 <- res %>%
  mutate(
    groupName = case_when(
      name %in% c("Envoy Air", "American Eagle") ~ "American",
      name == "AirTran" ~ "Southwest", 
      TRUE ~ name
    )
  ) %>%
  group_by(groupName) %>%
  summarize(
    numFlights = sum(numFlights), 
    wShortDelay = sum(shortDelay), 
    wLongDelay = sum(longDelay)
  ) %>%
  mutate(
    wShortDelayPct = wShortDelay / numFlights,
    wLongDelayPct = wLongDelay / numFlights,
    delayed = wShortDelayPct + wLongDelayPct,
    ontime = 1 - delayed
  )
carriers_2014


## -----------------------------------------------------------------------------
carriers_tidy <- carriers_2014 %>%
  select(groupName, wShortDelayPct, wLongDelayPct, delayed) %>%
  pivot_longer(
    -c(groupName, delayed), 
    names_to = "delay_type", 
    values_to = "pct"
  )
delay_chart <- ggplot(
  data = carriers_tidy, 
  aes(x = reorder(groupName, pct, max), y = pct)
) + 
  geom_col(aes(fill = delay_type)) +
  scale_fill_manual(
    name = NULL, 
    values = c("red", "gold"), 
    labels = c(
      "Flights Delayed 120+ Minutes\ncancelled or Diverted", 
      "Flights Delayed 15-119 Minutes"
    )
  ) + 
  scale_y_continuous(limits = c(0, 1)) + 
  coord_flip() + 
  labs(
    title = "Southwest's Delays Are Short; United's Are Long", 
    subtitle = "As share of scheduled flights, 2014"
  ) + 
  ylab(NULL) + 
  xlab(NULL) + 
  ggthemes::theme_fivethirtyeight() + 
  theme(
    plot.title = element_text(hjust = 1),
    plot.subtitle = element_text(hjust = -0.2)
  )


## ----ft8-plot, fig.cap="Recreation of the FiveThirtyEight plot on flight delays."----
delay_chart +
  geom_text(
    data = filter(carriers_tidy, delay_type == "wShortDelayPct"), 
    aes(label = paste0(round(pct * 100, 1), "% ")), 
    hjust = "right",
    size = 2
  ) + 
  geom_text(
    data = filter(carriers_tidy, delay_type == "wLongDelayPct"), 
    aes(y = delayed - pct, label = paste0(round(pct * 100, 1), "% ")),
    hjust = "left", 
    nudge_y = 0.01,
    size = 2
  )


## SELECT

##   dest,

##   SUM(1) AS numFlights,

##   AVG(arr_delay) AS avgArrivalDelay

## FROM

##   flights AS o

## WHERE year = 2014

## GROUP BY dest

## ORDER BY numFlights DESC

## LIMIT 0, 30


## SELECT

##   origin,

##   SUM(1) AS numFlights,

##   AVG(arr_delay) AS avgDepartDelay

## FROM

##   flights AS o

## WHERE year = 2014

## GROUP BY origin

## ORDER BY numFlights DESC

## LIMIT 0, 30


## -----------------------------------------------------------------------------
dests %>%
  left_join(origins, by = c("dest" = "origin")) %>%
  select(dest, avgDepartDelay, avgArrivalDelay) %>%
  arrange(desc(avgDepartDelay))  %>%
  as_tibble()


## SELECT

##   origin, dest,

##   SUM(1) AS numFlights,

##   AVG(arr_delay) AS avgDelay

## FROM

##   flights AS o

## WHERE year = 2014

## GROUP BY origin, dest


## -----------------------------------------------------------------------------
head(routes)


## SELECT

##   origin, dest,

##   o.carrier, c.name,

##   SUM(1) AS numFlights,

##   AVG(arr_delay) AS avgDelay

## FROM

##   flights AS o

## LEFT JOIN

##   carriers c ON o.carrier = c.carrier

## WHERE year = 2014

## GROUP BY origin, dest, o.carrier


## -----------------------------------------------------------------------------
routes_aug <- routes_carriers %>%
  left_join(routes, by = c("origin" = "origin", "dest" = "dest")) %>%
  as_tibble()
head(routes_aug)


## -----------------------------------------------------------------------------
routes_aug %>%
  group_by(carrier) %>%
  # use str_remove_all() to remove parentheses
  summarize(
    carrier_name = str_remove_all(first(name), "\\(.*\\)"), 
    numRoutes = n(), 
    numFlights = sum(numFlights.x), 
    wAvgDelay = sum(
      numFlights.x * (avgDelay.x - avgDelay.y), 
      na.rm = TRUE
    ) / sum(numFlights.x)
  ) %>%
  arrange(wAvgDelay)

