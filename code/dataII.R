## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/dataII-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----message=FALSE, warning=FALSE----------------------------------------
library(mdsr)
library(googlesheets)
hiv_key <- "1kWH_xdJDM4SMfT_Kzpkk-1yuxWChfurZuWYjfmv51EA"
hiv <- gs_key(hiv_key, lookup = FALSE) %>%
  gs_read(ws = "Data", range = cell_limits(c(1, 1), c(276, 34)))
names(hiv)[1] <- "Country"
hiv %>% 
  filter(Country %in% c("United States", "France", "South Africa")) %>%
  select(Country, `1979`, `1989`, `1999`, `2009`)

## ----message=FALSE-------------------------------------------------------
library(tidyr)
hiv_long <- hiv %>% gather(key = Year, value = hiv_rate, -Country)
hiv_long %>%
  filter(Country %in% c("United States", "France", "South Africa")) %>%
  filter(Year %in% c(1979, 1989, 1999, 2009))

## ----tab:names-short1, results="asis", echo=FALSE, message=FALSE,warning=FALSE----
library(babynames)
library(xtable)
set.seed(325)
babynames %>%
  sample_n(7) %>%
  select(-prop) %>%
  xtable(caption = "A data table showing how many babies were given each name in each year in the U.S., for a few names.", label = "tab:names-short1", digits = 0) %>%
  print(include.rownames = FALSE)

## ----echo=TRUE, eval=TRUE------------------------------------------------
popular_names <- babynames %>% 
  group_by(sex, name) %>%
  summarize(total_births = sum(n)) %>% 
  arrange(desc(total_births))

## ----echo=FALSE, results='asis'------------------------------------------
library(xtable)
popular_names %>%
  head(10) %>%
  xtable(label = "tab:names-popular1", caption = "The most popular baby names across all years.") %>%
  print()

## ----echo=FALSE, message=FALSE-------------------------------------------
neat <- Elections %>%
  mutate(Ward = as.character(Ward)) %>%
  mutate(Precinct = factor(Precinct, 
                        levels = c("1","1C","2","2D","3","3A","4","4D",
                                 "5","5A","6","6C","7","8","9","10"))) %>%
  select(1,2,6,7,8,10)
names(neat) <- c("ward", "precinct","registered","voters","absentee","total_turnout")

## ----echo=FALSE, results='asis'------------------------------------------
neat %>%
  slice(round(seq(from = 1, to = 25, by = 3))) %>%
  xtable(label = "tab:vote-summary", 
    caption = "A selection from the Minneapolis election data in tidy form.") %>%
  print(include.rownames = FALSE)

## ----ward-turnouts,echo=FALSE,fig.cap="A graphical depiction of voter turnout in the different wards."----
ggplot(data = neat, aes(x = precinct, y = 100 * total_turnout)) + 
  geom_point() + ylim(0,55) + 
  ylab("Voter Turnout (%)") + xlab("Precinct")

## ----tab:indiv-ballots, echo=FALSE---------------------------------------
mdsr::Minneapolis2013[c(6,2,10,5,27), ]

## ----include=FALSE-------------------------------------------------------
data(Cherry)
runners <- Cherry

## ----tab:race-excerpt, results="asis", echo=FALSE, message=FALSE,warning=FALSE----
xtable(runners[15996:16010, c(1,5,2,6,4)], caption = "An excerpt of runners' performance over time in a 10-mile race.\\label{tab:race-excerpt}")

## ----eval=FALSE----------------------------------------------------------
## help(HELPrct)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
BP_full <- readr::read_delim("data/BP_narrow.csv", delim = "|")
library(tidyr)
BP_narrow <- BP_full %>%
  distinct(subject, when, .keep_all = TRUE) %>%
  select(subject, when, sbp) %>%
  arrange(desc(when), subject)
BP_wide <- BP_narrow %>%
  spread(when, sbp) %>%
  select(subject, before, after)

## ----echo=FALSE, results='asis'------------------------------------------
library(xtable)
xtable(BP_wide, caption = "\\data{BP\\_wide}: a data table in a wide format", label = "tab:wide-example") %>%
  print(include.rownames = FALSE)

## ----echo=FALSE, results='asis'------------------------------------------
xtable(BP_narrow, caption = "\\data{BP\\_narrow}: a tidy data table in a narrow format.", label = "tab:narrow-example") %>%
  print(include.rownames = FALSE)

## ----eval=FALSE----------------------------------------------------------
## BP_wide %>% mutate(change = after - before)

## ----echo=FALSE, warning=FALSE, results='asis'---------------------------
xtable(BP_full, caption = "A data table extending the information in Tables~\\ref{tab:narrow-example} and~\\ref{tab:wide-example} to include additional variables and repeated measurements. The narrow format facilitates including new cases or variables.", label = "tab:narrow-augmented") %>%
  print(include.rownames = FALSE, NA.string = "NA")

## ----eval=FALSE----------------------------------------------------------
## BP_narrow %>% spread(key = when, value = sbp)

## ----eval=FALSE----------------------------------------------------------
## BP_wide %>% gather(key = when, value = sbp, before, after)

## ----fig.cap="99.6\\% of Sues are female."-------------------------------
babynames %>% 
  filter(name == "Sue") %>%
  group_by(name, sex) %>% 
  summarise(total = sum(n))

## ----fig.cap="15\\% of people named Robin are male."---------------------
babynames %>% 
  filter(name == "Robin") %>%
  group_by(name, sex) %>% 
  summarise(total = sum(n))

## ----tab:balance-wide, echo=TRUE, fig.cap="A wide format facilitates examining gender balance in \\data{babynames}.", warning=FALSE----
babynames %>%
  filter(name %in% c("Sue", "Robin", "Leslie")) %>%
  group_by(name, sex) %>%
  summarise(total = sum(n)) %>%
  spread(key = sex, value = total, fill=0)

## ----warning=FALSE-------------------------------------------------------
BabyWide <- babynames %>%
  group_by(sex, name) %>%
  summarize(total = sum(n)) %>%
  spread(key = sex, value = total, fill = 0)
head(BabyWide, 3)

## ----tab:most-balanced, fig.cap="The most gender-balanced common names."----
BabyWide %>% 
  filter(M > 50000, F > 50000) %>%
  mutate(ratio = pmin(M / F, F / M) ) %>% 
  arrange(desc(ratio)) %>% 
  head(3)

## ----message=FALSE-------------------------------------------------------
library(Lahman)
names(Teams)

## ----eval=FALSE----------------------------------------------------------
## str(Teams)
## glimpse(Teams)

## ----message=FALSE-------------------------------------------------------
averages <- NULL
for (i in 15:40) {
  averages[i - 14] <- mean(Teams[, i], na.rm = TRUE)
}
names(averages) <- names(Teams)[15:40]
averages

## ------------------------------------------------------------------------
a <- "a string"
class(a)
length(a)

## ------------------------------------------------------------------------
Teams %>% 
  select(15:40) %>% 
  apply(MARGIN = 2, FUN = mean, na.rm = TRUE)

## ----eval=FALSE----------------------------------------------------------
## Teams %>%
##   select(15:40) %>%
##   apply(MARGIN = 1, FUN = mean, na.rm = TRUE)

## ------------------------------------------------------------------------
Teams %>% 
  select(teamID) %>% 
  apply(MARGIN = 2, FUN = mean, na.rm = TRUE)

## ------------------------------------------------------------------------
angels <- Teams %>% 
  filter(franchID == "ANA") %>% 
  group_by(teamID, name) %>%
  summarise(began = first(yearID), ended = last(yearID)) %>% 
  arrange(began)
angels

## ------------------------------------------------------------------------
angels_names <- angels$name
nchar(angels_names[1])
nchar(angels_names[2])
nchar(angels_names[3])
nchar(angels_names[4])

## ------------------------------------------------------------------------
sapply(angels_names, FUN = nchar)
lapply(angels_names, FUN = nchar)

## ------------------------------------------------------------------------
top5 <- function(x, teamname) {
  x %>%
    filter(name == teamname) %>%
    select(teamID, yearID, W, L, name) %>%
    arrange(desc(W)) %>%
    head(n = 5)
}

## ------------------------------------------------------------------------
angels_list <- lapply(angels_names, FUN = top5, x = Teams)
angels_list

## ------------------------------------------------------------------------
angels_list %>% bind_rows() %>%
  group_by(teamID, name) %>%
  summarize(N = n(), mean_wins = mean(W)) %>%
  arrange(desc(mean_wins))

## ----pythag, fig.keep="last", fig.cap="Fit for the Pythagorean Winning Percentage model for all teams since 1954."----
exp_wpct <- function (x) { 
  return(1/(1 + (1/x)^2))
}
TeamRuns <- Teams %>% 
  filter(yearID >= 1954) %>%
  rename(RS = R) %>% 
  mutate(WPct = W / (W + L), run_ratio = RS/RA) %>%
  select(yearID, teamID, lgID, WPct, run_ratio) 
ggplot(data = TeamRuns, aes(x = run_ratio, y = WPct)) +
  geom_vline(xintercept = 1, color= "darkgray", linetype = 2) +
  geom_hline(yintercept = 0.5, color= "darkgray", linetype = 2) +
  geom_point(alpha = 0.3) + 
  stat_function(fun = exp_wpct, size = 2, color = "blue") + 
  xlab("Ratio of Runs Scored to Runs Allowed") + ylab("Winning Percentage")

## ------------------------------------------------------------------------
exWpct <- fitModel(WPct ~ 1/(1 + (1/run_ratio)^k), data = TeamRuns)
coef(exWpct)

## ------------------------------------------------------------------------
fit_k <- function(x) {
  mod <- fitModel(formula = WPct ~ 1/(1 + (1/run_ratio)^k), data = x)
  return(data.frame(k = coef(mod)))
}

## ------------------------------------------------------------------------
fit_k(TeamRuns)

## ------------------------------------------------------------------------
TeamRuns %>% 
  mutate(decade = yearID %/% 10 * 10) %>%
  group_by(decade) %>% 
  do(fit_k(x = .))

## ----message=FALSE-------------------------------------------------------
hr_leader <- function (x) {
# x is a subset of Teams for a single year and league
  x %>% 
    select(yearID, lgID, teamID, HR) %>% 
    arrange(desc(HR)) %>% 
    head(n = 1)
}

## ------------------------------------------------------------------------
Teams %>% 
  filter(yearID == 1961 & lgID == "AL") %>% 
  hr_leader()

## ------------------------------------------------------------------------
hr_leaders <- Teams %>% 
  group_by(yearID, lgID) %>% 
  do(hr_leader(.))
head(hr_leaders, 4)

## ------------------------------------------------------------------------
mean(HR ~ lgID, data = hr_leaders)
mean(HR ~ lgID, data = filter(hr_leaders, yearID >= 1916))

## ----dh, message=FALSE, fig.cap="Number of home runs hit by the team with the most home runs, 1916--2014. Note how the AL has consistently bested the NL since the introduction of the designated hitter (DH) in 1973."----
hr_leaders %>% 
  filter(yearID >= 1916) %>%
  ggplot(aes(x = yearID, y = HR, color = lgID)) + geom_line() + 
    geom_point() + geom_smooth(se = 0) + geom_vline(xintercept = 1973) + 
    annotate("text", x=1974, y=25, label="AL adopts DH", hjust="left")

## ----teamdens------------------------------------------------------------
k_actual <- TeamRuns %>% 
  group_by(yearID) %>% 
  do(fit_k(.)) 
favstats(~ k, data = k_actual)

## ----teamdens2, fig.cap="Distribution of best-fitting exponent across single seasons from 1961--2014."----
ggplot(data = k_actual, aes(x = k)) + geom_density() + 
  xlab("Best fit exponent for a single season")

## ----bdensplot0,message=FALSE--------------------------------------------
bstrap <- do(1000) * mean(~ k, data = resample(k_actual))
head(bstrap, 3)
civals <- qdata(~ mean, c(0.025, .975), data = bstrap)
civals

## ----bdensplot,message=FALSE,fig.keep='last', fig.cap="Bootstrap distribution of mean optimal exponent."----
ggplot(data = bstrap, aes(x = mean)) + geom_density() + 
  xlab("Distribution of resampled means") + 
  geom_vline(data = civals, aes(xintercept = quantile), color = "red", 
    linetype = 3)

## ----eval=FALSE----------------------------------------------------------
## save(hr_leaders, file = "hr_leaders.rda", compress = "xz")

## ----eval=FALSE----------------------------------------------------------
## load(file = "hr_leaders.rda")

## ----echo=FALSE----------------------------------------------------------
library(babynames)
write.csv(head(babynames), row.names = FALSE)

## ----message=FALSE-------------------------------------------------------
myURL <- "http://tiny.cc/dcf/houses-for-sale.csv"
Houses <- readr::read_csv(myURL)
head(Houses, 3)

## ----eval=FALSE, include=FALSE-------------------------------------------
## webshot::webshot(
##   url = "http://en.wikipedia.org/wiki/Mile_run_world_record_progression",
##   file = "gfx/wiki_running.png", cliprect = c(2300, 0, 992, 644))

## ----message=FALSE-------------------------------------------------------
library(rvest)
library(methods)
url <- "http://en.wikipedia.org/wiki/Mile_run_world_record_progression"
tables <- url %>% 
  read_html() %>% 
  html_nodes("table")

## ------------------------------------------------------------------------
length(tables)

## ------------------------------------------------------------------------
Table3 <- html_table(tables[[3]])

## ----echo=FALSE, results='asis'------------------------------------------
head(Table3) %>%
  xtable(label = "tab:wikipedia-table-three", caption = "The third table embedded in the Wikipedia page on running records.") %>%
  print(include.rownames = FALSE)

## ------------------------------------------------------------------------
Table4 <- html_table(tables[[4]])
Table4 <- select(Table4, -Auto)  # remove unwanted column

## ----echo=FALSE, results='asis'------------------------------------------
head(Table4) %>%
  xtable(label = "tab:wikipedia-table-four", caption = "The fourth table embedded in the Wikipedia page on running records.") %>%
  print(include.rownames = FALSE)

## ----eval=FALSE,echo=FALSE-----------------------------------------------
## save(tables, file = "RunnersTables.rda")
## # Save these so that you can keep the example even after the Wikipedia page changes
## # note that "eval=" is set to FALSE.  We're working with the saved data.

## ----echo=FALSE, results='asis'------------------------------------------
Houses %>% 
  select(fuel, heat, sewer, construction) %>% 
  head(5) %>%
  xtable(label = "tab:house-systems", caption = "Four of the  variables from the \\texttt{houses-for-sale.csv} file giving features of the Saratoga houses stored as integer codes. Each case is a different house.") %>%
  print(include.rownames = FALSE)

## ----message=FALSE-------------------------------------------------------
Translations <- readr::read_csv("http://tiny.cc/dcf/house_codes.csv")
Translations %>% head(5)

## ------------------------------------------------------------------------
CodeVals <- Translations %>%
  spread(key = system_type, value = meaning, fill = "invalid")

## ----echo=FALSE, results='asis'------------------------------------------
CodeVals %>%
  xtable(label = "tab:code-vals", caption = "The \\data{Translations} data table rendered in a wide format.") %>%
  print(include.rownames = FALSE)

## ------------------------------------------------------------------------
Houses <- Houses %>%
  left_join(CodeVals %>% 
  select(code, fuel_type), by = c(fuel="code")) %>%
  left_join(CodeVals %>% select(code, heat_type), by = c(heat="code")) %>%
  left_join(CodeVals %>% select(code, sewer_type), by = c(sewer="code"))

## ----echo=FALSE, results='asis'------------------------------------------
Houses %>% 
  select(fuel_type, heat_type, sewer_type) %>% 
  head() %>%
  xtable(label = "tab:recode-houses", caption = "The \\data{Houses} data with re-coded categorical variables.")

## ------------------------------------------------------------------------
OrdwayBirds %>% 
  select(Timestamp, Year, Month, Day) %>% 
  glimpse()

## ----message=FALSE-------------------------------------------------------
library(readr)
OrdwayBirds <- OrdwayBirds %>%
  mutate(Month = parse_number(Month), Year = parse_number(Year),
         Day = parse_number(Day))
OrdwayBirds %>% 
  select(Timestamp, Year, Month, Day) %>% 
  glimpse()

## ----message=FALSE-------------------------------------------------------
library(lubridate)
WhenAndWho <- OrdwayBirds %>% 
  mutate(When = mdy_hms(Timestamp)) %>% 
  select(Timestamp, Year, Month, Day, When, DataEntryPerson) %>% 
  glimpse()

## ----when-and-who2, warning=FALSE, fig.cap="The transcribers of \\data{OrdwayBirds} from lab notebooks worked during different time intervals."----
WhenAndWho %>% ggplot(aes(x = When, y = DataEntryPerson)) + 
  geom_point(alpha = 0.1, position = "jitter") 

## ----eval=FALSE----------------------------------------------------------
## WhenAndWho %>%
##   group_by(DataEntryPerson) %>%
##   summarize(start = first(When), finish = last(When)) %>%
##   mutate(duration = interval(start, finish) / ddays(1))

## ----echo=FALSE,results='asis'-------------------------------------------
temp <- WhenAndWho %>% 
  group_by(DataEntryPerson) %>% 
  summarize(start = first(When),
            finish = last(When)) %>%
  mutate(duration = interval(start, finish) / ddays(1)) %>%
  na.omit()
temp <- temp %>%
  mutate(start = as.character(start), finish = as.character(finish))
temp %>%
  xtable(label = "tab:transcriber-dates", caption = "Starting and ending dates for each transcriber involved in the \\data{OrdwayBirds} project.") %>%
  print(include.rownames = FALSE)

## ------------------------------------------------------------------------
now()
class(now())
class(as.POSIXlt(now()))

## ------------------------------------------------------------------------
as.Date(now())

## ------------------------------------------------------------------------
library(lubridate)
example <- c("2017-04-29 06:00:00", "2017-12-31 12:00:00")
str(example)

## ------------------------------------------------------------------------
converted <- ymd_hms(example)
str(converted)

## ------------------------------------------------------------------------
converted
converted[2] - converted[1]

## ----message=FALSE-------------------------------------------------------
my_html <- 
  read_html("http://en.wikipedia.org/wiki/List_of_nuclear_reactors")
tables <- my_html %>% html_nodes(css = "table")
relevant_tables <- tables[grep("Fukushima Daiichi", tables)]
reactors <- html_table(relevant_tables[[1]], fill = TRUE)
names(reactors)[c(3,4,6,7)] <- c("Reactor Type", 
  "Reactor Model", "Capacity Net", "Capacity Gross")
reactors <- reactors[-1,]

## ----warning=FALSE-------------------------------------------------------
library(readr)
reactors <- reactors %>% 
  rename(capacity_net=`Capacity Net`, capacity_gross=`Capacity Gross`) %>%
  mutate(plantstatus = ifelse(grepl("Shut down", reactors$Status), 
      "Shut down", "Not formally shut down"), 
    capacity_net = parse_number(capacity_net),
    construct_date = dmy(`Construction Start Date`), 
    operation_date = dmy(`Commercial Operation Date`), 
    closure_date = dmy(Closure))

## ----japannukes, warning=FALSE, message=FALSE, fig.cap="Distribution of capacity of Japanese nuclear power plants over time."----
ggplot(data = reactors, 
  aes(x = construct_date, y = capacity_net, color = plantstatus)) +
  geom_point() + geom_smooth() + 
  xlab("Date of Plant Construction") + ylab("Net Plant Capacity (MW)")

