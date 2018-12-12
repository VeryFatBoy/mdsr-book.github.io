## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/bigdata-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----message=FALSE-------------------------------------------------------
library(mdsr)
library(biglm)
n <- 20000
p <- 500
d <- as.data.frame(matrix(rnorm(n * (p + 1)), ncol = (p + 1)))
expl_vars <- paste(paste0("V", 2:(p+1)), collapse = " + ")
my_formula <- as.formula(paste("V1 ~ ", expl_vars))
system.time(lm(my_formula, data = d))
system.time(biglm(my_formula, data = d))

## ------------------------------------------------------------------------
n <- 1e5
k <- 20
d <- data.frame(y = rnorm(n*k), x = rnorm(n*k), set = rep(1:k, each = n))

fit_lm <- function(data, set_id) {
  data %>%
    filter(set == set_id) %>%
    lm(y ~ x, data = .)
}

## ------------------------------------------------------------------------
system.time(fit_lm(d, 1))
system.time(lapply(1:k, fit_lm, data = d))

## ------------------------------------------------------------------------
library(parallel)
my_cores <- detectCores()
my_cores

## ------------------------------------------------------------------------
system.time(mclapply(1:k, fit_lm, data = d, mc.cores = my_cores - 1))

## ----eval=FALSE----------------------------------------------------------
## library(gputools)

## ----eval=FALSE, include=FALSE-------------------------------------------
## cat(readChar("code/map-reduce/wordcount.py", 1e5))

## import MapReduce

## ----eval=FALSE----------------------------------------------------------
## library(jsonlite)
## gg_issues <- fromJSON("https://api.github.com/repos/hadley/ggplot2/issues")
## gg_issues %>%
##   select(url, body) %>%
##   apply(MARGIN = 1, FUN = toJSON) %>%
##   write(file = "code/map-reduce/issues.json")

## ----eval=TRUE, echo=TRUE------------------------------------------------
readLines("code/map-reduce/issues.json")[1] %>%
  stringr::str_wrap(width = 70) %>%
  cat()

## ----message=FALSE, warning=FALSE----------------------------------------
library(mdsr)
cmd <- "python code/map-reduce/wordcount.py code/map-reduce/issues.json"
res <- system(cmd, intern = TRUE)
freq_df <- res %>%
  lapply(jsonlite::fromJSON) %>%
  lapply(FUN = function(x) { data.frame(word = x[1], 
                                        count = as.numeric(x[2]))}) %>%
  bind_rows()
glimpse(freq_df)
freq_df %>%
  filter(grepl(pattern = "[a-z]", word)) %>%
  arrange(desc(count)) %>%
  head(10)

## ----eval=FALSE----------------------------------------------------------
## devtools::install_github("rstudio/sparklyr")

## ----eval=FALSE----------------------------------------------------------
## library(sparklyr)
## spark_install() # only once!

## ----include=FALSE-------------------------------------------------------
library(sparklyr)

## ----sparkR, message=FALSE-----------------------------------------------
sc <- spark_connect(master = "local")
class(sc)

## ------------------------------------------------------------------------
src_tbls(sc)

## ----sparkR-DataFrame, warning=FALSE-------------------------------------
babynames_tbl <- sc %>% copy_to(babynames::babynames, "babynames")
src_tbls(sc)
class(babynames_tbl)

## ------------------------------------------------------------------------
babynames_tbl %>%
  filter(name == "Benjamin") %>%
  group_by(year) %>%
  summarize(N = n(), total_births = sum(n)) %>%
  arrange(desc(total_births)) %>%
  head()

## ------------------------------------------------------------------------
library(DBI)
dbGetQuery(sc, "SELECT year, sum(1) as N, sum(n) as total_births
                FROM babynames WHERE name == 'Benjamin' 
                GROUP BY year
                ORDER BY total_births desc
                LIMIT 6")

## ----message=FALSE-------------------------------------------------------
library(macleish)
weather_tbl <- copy_to(sc, whately_2015)
weather_tbl %>%
  ml_linear_regression(rainfall ~ temperature + pressure + rel_humidity) %>%
  summary()

## ----include=FALSE-------------------------------------------------------
# project_id <- "mdsr-1216"   # BB
project_id <- "taxicabs-980" # NH

## ----eval=FALSE,warning=FALSE--------------------------------------------
## library(bigrquery)
## project_id <- "my-google-id"
## 
## sql <- "
## SELECT word, count(distinct corpus) as numPlays
## , sum(word_count) as N
## FROM [publicdata:samples.shakespeare]
## GROUP BY word
## ORDER BY N desc
## LIMIT 10
## "
## query_exec(sql, project = project_id)

