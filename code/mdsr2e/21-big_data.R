## ----message=FALSE------------------------------------------------------------
library(tidyverse)
library(mdsr)
library(biglm)
library(bench)
n <- 20000
p <- 500
d <- rnorm(n * (p + 1)) %>%
  matrix(ncol = (p + 1)) %>%
  as_tibble(.name_repair = "unique")
expl_vars <- names(d) %>%
  tail(-1) %>%
  paste(collapse = " + ")
my_formula <- as.formula(paste("...1 ~ ", expl_vars))

system_time(lm(my_formula, data = d))
system_time(biglm(my_formula, data = d))


## -----------------------------------------------------------------------------
n <- 1e6
k <- 20
d <- tibble(y = rnorm(n*k), x = rnorm(n*k), set = rep(1:k, each = n))

fit_lm <- function(data, set_id) {
  data %>%
    filter(set == set_id) %>%
    lm(y ~ x, data = .)
}


## -----------------------------------------------------------------------------
system_time(map(1:1, fit_lm, data = d))
system_time(map(1:k, fit_lm, data = d)) / k


## ----eval = TRUE--------------------------------------------------------------
library(parallel)
my_cores <- detectCores()
my_cores


## ----future_map, message=FALSE, warning=FALSE, eval=FALSE---------------------
## library(furrr)
## plan(multiprocess, workers = my_cores)
## 
## system_time(
##   future_map(1:k, fit_lm, data = d)
## ) / k


## ----plan, eval=FALSE---------------------------------------------------------
## plan(sequential)


## import MapReduce

## import sys

## 
## mr = MapReduce.MapReduce()

## 
## def mapper(record):

##     key = record[0]

##     value = record[1]

##     words = value.split()

##     for w in words:

##       mr.emit_intermediate(w, 1)

## 
## def reducer(key, list_of_values):

##     total = 0

##     for v in list_of_values:

##       total += v

##     mr.emit((key, total))

## 
## if __name__ == '__main__':

##   inputdata = open(sys.argv[1])

##   mr.execute(inputdata, mapper, reducer)


## ----eval=FALSE---------------------------------------------------------------
## library(jsonlite)
## url <- "https://api.github.com/repos/tidyverse/ggplot2/issues"
## gg_issues <- url %>%
##   fromJSON() %>%
##   select(url, body) %>%
##   group_split(url) %>%
##   map_chr(~toJSON(as.character(.x))) %>%
##   write(file = "code/map-reduce/issues.json")


## ----eval=TRUE, echo=TRUE-----------------------------------------------------
readLines("code/map-reduce/issues.json") %>%
  head(1) %>%
  str_wrap(width = 70) %>%
  cat()


## ----message=FALSE, warning=FALSE---------------------------------------------
library(mdsr)
cmd <- "python code/map-reduce/wordcount.py code/map-reduce/issues.json"
res <- system(cmd, intern = TRUE)
freq_df <- res %>%
  purrr::map(jsonlite::fromJSON) %>%
  purrr::map(set_names, c("word", "count")) %>%
  bind_rows() %>%
  mutate(count = parse_number(count))
glimpse(freq_df)
freq_df %>%
  filter(str_detect(pattern = "[a-z]", word)) %>%
  arrange(desc(count)) %>%
  head(10)


## ----eval=FALSE---------------------------------------------------------------
## library(sparklyr)
## spark_install(version = "3.0") # only once!


## ----include=FALSE------------------------------------------------------------
library(sparklyr)


## ----sparkR, message=FALSE----------------------------------------------------
# sudo apt-get install openjdk-8-jdk
sc <- spark_connect(master = "local", version = "3.0")
class(sc)


## -----------------------------------------------------------------------------
src_tbls(sc)


## ----sparkR-DataFrame, warning=FALSE------------------------------------------
babynames_tbl <- sc %>% 
  copy_to(babynames::babynames, "babynames")
src_tbls(sc)
class(babynames_tbl)


## ---- warning=FALSE-----------------------------------------------------------
babynames_tbl %>%
  filter(name == "Benjamin") %>%
  group_by(year) %>%
  summarize(N = n(), total_births = sum(n)) %>%
  arrange(desc(total_births)) %>%
  head()


## ----spark-DBI----------------------------------------------------------------
library(DBI)
dbGetQuery(sc, "SELECT year, sum(1) as N, sum(n) as total_births
                FROM babynames WHERE name == 'Benjamin' 
                GROUP BY year
                ORDER BY total_births desc
                LIMIT 6")


## ----message=FALSE------------------------------------------------------------
library(macleish)
weather_tbl <- copy_to(sc, whately_2015)
weather_tbl %>%
  ml_linear_regression(rainfall ~ temperature + pressure + rel_humidity) %>%
  summary()


## ----include=FALSE------------------------------------------------------------
# project_id <- "mdsr-1216"   # BB
project_id <- "taxicabs-980" # NH

## ----eval=FALSE,warning=FALSE-------------------------------------------------
## library(bigrquery)
## project_id <- "my-google-id"
## 
## sql <- "
## SELECT word
## , count(distinct corpus) AS numPlays
## , sum(word_count) AS N
## FROM [publicdata:samples.shakespeare]
## GROUP BY word
## ORDER BY N desc
## LIMIT 10
## "
## bq_project_query(sql, project = project_id)

