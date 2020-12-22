## ----message=FALSE, warning=FALSE---------------------------------------------
library(tidyverse)
library(mdsr)
library(googlesheets4)
hiv_key <- "1kWH_xdJDM4SMfT_Kzpkk-1yuxWChfurZuWYjfmv51EA"
hiv <- read_sheet(hiv_key) %>%
  rename(Country = 1) %>%
  filter(
    Country %in% c("United States", "France", "South Africa")
  ) %>%
  select(Country, `1979`, `1989`, `1999`, `2009`) %>%
  unnest(cols = c(`2009`)) %>%
  mutate(across(matches("[0-9]"), as.double))
hiv


## ----message=FALSE------------------------------------------------------------
hiv %>% 
  pivot_longer(-Country, names_to = "Year", values_to = "hiv_rate")


## ----names-short1, echo=FALSE, message=FALSE, warning=FALSE-------------------
library(babynames)
set.seed(325)
names_short <- babynames %>%
  slice_sample(n = 7) %>%
  select(-prop)
names_short %>%
  mdsr_table(
    caption = "A data table showing how many babies were given each name in each year in the United States, for a few names.", 
    digits = 0
  ) %>%
  kableExtra::kable_styling(full_width = FALSE)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
popular_names <- babynames %>% 
  group_by(sex, name) %>%
  summarize(total_births = sum(n)) %>% 
  arrange(desc(total_births))


## ----names-popular1, echo=FALSE-----------------------------------------------
popular_names %>%
  head(10) %>%
  mdsr_table(caption = "The most popular baby names across all years.")


## ----minn-vote-1, echo=FALSE, fig.cap = "Ward and precinct votes cast in the 2013 Minneapolis mayoral election."----
knitr::include_graphics("gfx/MinneapolisVoting.png")


## ----echo=FALSE, message=FALSE------------------------------------------------
neat <- Elections %>%
  mutate(Ward = factor(Ward)) %>%
  mutate(Precinct = factor(Precinct, 
                        levels = c("1","1C","2","2D","3","3A","4","4D",
                                 "5","5A","6","6C","7","8","9","10"))) %>%
  select(1,2,6,7,8,10)
names(neat) <- c("ward", "precinct","registered","voters","absentee","total_turnout")


## ----vote-summary, echo=FALSE-------------------------------------------------
neat %>%
  slice(round(seq(from = 1, to = 25, by = 3))) %>%
  mdsr_table(caption = "A selection from the Minneapolis election data in tidy form.")


## ----ward-turnouts,echo=FALSE, fig.cap="A graphical depiction of voter turnout by precinct in the different wards."----
ggplot(data = neat, aes(x = ward, y = 100 * total_turnout)) + 
  geom_jitter(width = 0.05, alpha = 0.5) + ylim(0,55) + 
  ylab("Voter Turnout (%)") + xlab("Ward")


## ----indiv-ballots, echo=FALSE------------------------------------------------
mdsr::Minneapolis2013[c(6,2,10,5,27), ] %>%
  as_tibble() %>%
  mdsr_table(caption = "Individual ballots in the Minneapolis election. Each voter votes in one precinct within one ward. The ballot marks the voter's first three choices for mayor.") %>%
  kableExtra::column_spec(2:4, width = "9em")


## ----include=FALSE------------------------------------------------------------
data(Cherry)
runners <- Cherry


## ----race-excerpt, echo=FALSE, message=FALSE,warning=FALSE--------------------
mdsr_table(runners[15996:16010, c(1,5,2,6,4)], caption = "An excerpt of runners' performance over time in a 10-mile race.") %>%
  kableExtra::column_spec(1, width = "10em") %>%
  kableExtra::column_spec(2, width = "3em")


## ----eval=FALSE---------------------------------------------------------------
## help(HELPrct)


## ---- eval=FALSE, include=FALSE-----------------------------------------------
## webshot::webshot(
##   "https://www.rdocumentation.org/packages/mosaicData/versions/0.17.0/topics/HELPrct",
##   file = "gfx/HELPrct.png",
##   cliprect = c(200, 0, 992, 1100)
## )


## ----babynames-codebook, echo=FALSE, fig.cap="(ref:babynames-cookbook-cap)"----
knitr::include_graphics("gfx/HELPrct.png")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
BP_full <- readr::read_delim("data/BP_narrow.csv", delim = "|")
BP_narrow <- BP_full %>%
  distinct(subject, when, .keep_all = TRUE) %>%
  select(subject, when, sbp) %>%
  arrange(desc(when), subject)
BP_wide <- BP_narrow %>%
  pivot_wider(names_from = when, values_from = sbp) %>%
  select(subject, before, after)


## ----wide-example, echo=FALSE-------------------------------------------------
mdsr_table(BP_wide, caption = "A blood pressure data table in a wide format.")


## ----narrow-example, echo=FALSE-----------------------------------------------
mdsr_table(BP_narrow, caption = "A tidy blood pressure data table in a narrow format.")


## ----eval=TRUE----------------------------------------------------------------
BP_wide %>% 
  mutate(change = after - before)


## ----narrow-augmented, echo=FALSE, warning=FALSE------------------------------
mdsr_table(BP_full, caption = "A data table extending the information in the previous two to include additional variables and repeated measurements. The narrow format facilitates including new cases or variables.")


## ----eval=TRUE----------------------------------------------------------------
BP_narrow %>% 
  pivot_wider(names_from = when, values_from = sbp)


## -----------------------------------------------------------------------------
BP_wide %>% 
  pivot_longer(-subject, names_to = "when", values_to = "sbp")


## -----------------------------------------------------------------------------
BP_full %>%
  group_by(subject, when) %>%
  summarize(mean_sbp = mean(sbp, na.rm = TRUE))


## -----------------------------------------------------------------------------
BP_summary <- BP_full %>%
  group_by(subject, when) %>%
  summarize(
    sbps = paste(sbp, collapse = ", "),
    dbps = paste(dbp, collapse = ", ")
  )


## -----------------------------------------------------------------------------
BP_summary %>%
  mutate(mean_sbp = mean(parse_number(sbps)))


## -----------------------------------------------------------------------------
BP_nested <- BP_full %>%
  group_by(subject, when) %>%
  nest()
BP_nested


## ---- error=TRUE--------------------------------------------------------------
BP_nested %>%
  mutate(sbp_list = pull(data, sbp))


## -----------------------------------------------------------------------------
BP_nested <- BP_nested %>%
  mutate(sbp_list = map(data, pull, sbp))
BP_nested


## -----------------------------------------------------------------------------
BP_nested %>% 
  pluck("sbp_list")


## -----------------------------------------------------------------------------
BP_nested <- BP_nested %>%
  mutate(sbp_mean = map(sbp_list, mean, na.rm = TRUE))
BP_nested


## -----------------------------------------------------------------------------
BP_nested %>%
  unnest(cols = c(sbp_mean))


## -----------------------------------------------------------------------------
BP_nested %>%
  unnest(cols = c(sbp_list))


## ----fig.cap="99.6% of Sues are female."--------------------------------------
babynames %>% 
  filter(name == "Sue") %>%
  group_by(name, sex) %>% 
  summarize(total = sum(n))


## ----fig.cap="15% of people named Robin are male."----------------------------
babynames %>% 
  filter(name == "Robin") %>%
  group_by(name, sex) %>% 
  summarize(total = sum(n))


## ----tab:balance-wide, echo=TRUE, fig.cap="A wide format facilitates examining gender balance in `r mdsr_data('babynames')`.", warning=FALSE----
babynames %>%
  filter(name %in% c("Sue", "Robin", "Leslie")) %>%
  group_by(name, sex) %>%
  summarize(total = sum(n)) %>%
  pivot_wider(
    names_from = sex, 
    values_from = total
  )


## ----warning=FALSE------------------------------------------------------------
baby_wide <- babynames %>%
  group_by(sex, name) %>%
  summarize(total = sum(n)) %>%
  pivot_wider(
    names_from = sex, 
    values_from = total, 
    values_fill = 0
  )
head(baby_wide, 3)


## ----tab:most-balanced, fig.cap="The most gender-balanced common names."------
baby_wide %>% 
  filter(M > 50000, F > 50000) %>%
  mutate(ratio = pmin(M / F, F / M) ) %>% 
  arrange(desc(ratio)) %>% 
  head(3)


## ----eval=FALSE---------------------------------------------------------------
## saveRDS(mtcars, file = "mtcars.rda", compress = TRUE)


## ----eval=FALSE---------------------------------------------------------------
## mtcars <- readRDS("mtcars.rda")


## ----echo=FALSE---------------------------------------------------------------
library(babynames)
write.csv(head(babynames), row.names = FALSE)


## ----message=FALSE------------------------------------------------------------
mdsr_url <- "https://raw.githubusercontent.com/beanumber/mdsr/master/data-raw/"
houses <- mdsr_url %>%
  paste0("houses-for-sale.csv") %>%
  read_csv()
head(houses, 3)


## ----eval=FALSE, include=FALSE------------------------------------------------
## webshot::webshot(
##   url = "http://en.wikipedia.org/wiki/Mile_run_world_record_progression",
##   file = "gfx/wiki_running.png", cliprect = c(2300, 0, 992, 644))


## ----wiki-running, echo=FALSE, fig.cap="Part of a page on mile-run world records from Wikipedia. Two separate data tables are visible. You can't tell from this small part of the page, but there are many tables on the page.  These two tables are the third and fourth in the page."----
knitr::include_graphics("gfx/wiki_running.png")


## ----message=FALSE------------------------------------------------------------
library(rvest)
url <- "http://en.wikipedia.org/wiki/Mile_run_world_record_progression"
tables <- url %>% 
  read_html() %>% 
  html_nodes("table")


## -----------------------------------------------------------------------------
length(tables)


## -----------------------------------------------------------------------------
amateur <- tables %>%
  purrr::pluck(3) %>%
  html_table()


## ----wikipedia-table-three, echo=FALSE----------------------------------------
head(amateur) %>%
  mdsr_table(caption = "The third table embedded in the Wikipedia page on running records.") %>%
  kableExtra::column_spec(1, width = "3em") %>%
  kableExtra::column_spec(2, width = "10em") %>%
  kableExtra::column_spec(3:4, width = "8em")


## -----------------------------------------------------------------------------
records <- tables %>%
  purrr::pluck(4) %>%
  html_table() %>%
  select(-Auto)  # remove unwanted column


## ----wikipedia-table-four, echo=FALSE-----------------------------------------
head(records) %>%
  mdsr_table(caption = "The fourth table embedded in the Wikipedia page on running records.") %>%
  kableExtra::column_spec(1, width = "3em") %>%
  kableExtra::column_spec(c(2, 4), width = "9em")


## ----house-systems, echo=FALSE------------------------------------------------
houses %>% 
  select(fuel, heat, sewer, construction) %>% 
  head(5) %>%
  mdsr_table(caption = "Four of the variables from the tables giving features of the Saratoga houses stored as integer codes. Each case is a different house.")


## ----message=FALSE------------------------------------------------------------
translations <- mdsr_url %>%
  paste0("house_codes.csv") %>%
  read_csv()
translations %>% head(5)


## -----------------------------------------------------------------------------
codes <- translations %>%
  pivot_wider(
    names_from = system_type, 
    values_from = meaning, 
    values_fill = "invalid"
  )


## ----code-vals, echo=FALSE----------------------------------------------------
codes %>%
  mdsr_table(caption = "The Translations data table rendered in a wide format.")


## -----------------------------------------------------------------------------
houses <- houses %>%
  left_join(
    codes %>% select(code, fuel_type), 
    by = c(fuel = "code")
  ) %>%
  left_join(
    codes %>% select(code, heat_type), 
    by = c(heat = "code")
  ) %>%
  left_join(
    codes %>% select(code, sewer_type), 
    by = c(sewer = "code")
  )


## ----recode-houses, echo=FALSE------------------------------------------------
houses %>% 
  select(fuel_type, heat_type, sewer_type) %>% 
  head() %>%
  mdsr_table(caption = "The Saratoga houses data with re-coded categorical variables.")


## -----------------------------------------------------------------------------
ordway_birds %>% 
  select(Timestamp, Year, Month, Day) %>% 
  glimpse()


## ----message=FALSE------------------------------------------------------------
library(readr)
ordway_birds <- ordway_birds %>%
  mutate(
    Month = parse_number(Month), 
    Year = parse_number(Year),
    Day = parse_number(Day)
  )
ordway_birds %>% 
  select(Timestamp, Year, Month, Day) %>% 
  glimpse()


## ----message=FALSE------------------------------------------------------------
library(lubridate)
birds <- ordway_birds %>% 
  mutate(When = mdy_hms(Timestamp)) %>% 
  select(Timestamp, Year, Month, Day, When, DataEntryPerson)
birds %>% 
  glimpse()


## ----when-and-who2, warning=FALSE, fig.cap="(ref:ordway-birds-cap)"-----------
birds %>% 
  ggplot(aes(x = When, y = DataEntryPerson)) + 
  geom_point(alpha = 0.1, position = "jitter") 


## -----------------------------------------------------------------------------
bird_summary <- birds %>% 
  group_by(DataEntryPerson) %>% 
  summarize(
    start = first(When), 
    finish = last(When)
  ) %>%
  mutate(duration = interval(start, finish) / ddays(1))


## ----transcriber-dates, echo=FALSE--------------------------------------------
bird_summary %>%
  na.omit() %>%
  mdsr_table(caption = paste("Starting and ending dates for each transcriber involved in the Ordway Birds project.")) %>%
  kableExtra::column_spec(1, width = "10em")


## -----------------------------------------------------------------------------
now()
class(now())
class(as.POSIXlt(now()))


## -----------------------------------------------------------------------------
as.Date(now())


## -----------------------------------------------------------------------------
library(lubridate)
example <- c("2021-04-29 06:00:00", "2021-12-31 12:00:00")
str(example)


## -----------------------------------------------------------------------------
converted <- ymd_hms(example)
str(converted)


## -----------------------------------------------------------------------------
converted
converted[2] - converted[1]


## ----wikijapan, echo=FALSE, fig.cap="Screenshot of Wikipedia's list of Japanese nuclear reactors."----
knitr::include_graphics("gfx/japanreactor.png")


## ----message=FALSE------------------------------------------------------------
tables <- "http://en.wikipedia.org/wiki/List_of_nuclear_reactors" %>%
  read_html() %>% 
  html_nodes(css = "table")

idx <- tables %>%
  html_text() %>%
  str_detect("Fukushima Daiichi") %>%
  which()

reactors <- tables %>%
  purrr::pluck(idx) %>%
  html_table(fill = TRUE) %>%
  janitor::clean_names() %>%
  rename(
    reactor_type = reactor,
    reactor_model = reactor_2,
    capacity_net = capacity_in_mw,
    capacity_gross = capacity_in_mw_2
  ) %>%
  tail(-1)

glimpse(reactors)


## ----warning=FALSE------------------------------------------------------------
reactors <- reactors %>% 
  mutate(
    plant_status = ifelse(
      str_detect(status, "Shut down"), 
      "Shut down", "Not formally shut down"
    ), 
    capacity_net = parse_number(capacity_net),
    construct_date = dmy(construction_start), 
    operation_date = dmy(commercial_operation), 
    closure_date = dmy(closure)
  )
glimpse(reactors)


## ----japannukes, warning=FALSE, message=FALSE, fig.cap="Distribution of capacity of Japanese nuclear power plants over time."----
ggplot(
  data = reactors, 
  aes(x = construct_date, y = capacity_net, color = plant_status
  )
) +
  geom_point() + 
  geom_smooth() + 
  xlab("Date of Plant Construction") + 
  ylab("Net Plant Capacity (MW)")

