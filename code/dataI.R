## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/dataI-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----message=FALSE-------------------------------------------------------
library(mdsr)
presidential

## ------------------------------------------------------------------------
select(presidential, name, party)

## ------------------------------------------------------------------------
filter(presidential, party == "Republican")

## ------------------------------------------------------------------------
select(filter(presidential, start > 1973 & party == "Democratic"), name)

## ------------------------------------------------------------------------
presidential %>%
  filter(start > 1973 & party == "Democratic") %>% 
  select(name)

## ----eval=FALSE----------------------------------------------------------
## dataframe %>% filter(condition)

## ----message=FALSE, warning=FALSE----------------------------------------
library(lubridate)
mypresidents <- presidential %>%
  mutate(term.length = interval(start, end) / eyears(1))
mypresidents

## ------------------------------------------------------------------------
mypresidents <- mypresidents %>% mutate(elected = year(start) - 1)
mypresidents

## ------------------------------------------------------------------------
mypresidents <- mypresidents %>%
  mutate(elected = ifelse((elected %in% c(1962, 1973)), NA, elected))
mypresidents

## ------------------------------------------------------------------------
mypresidents <- mypresidents %>% rename(term_length = term.length)
mypresidents

## ------------------------------------------------------------------------
mypresidents %>% arrange(desc(term_length))

## ------------------------------------------------------------------------
mypresidents %>% arrange(desc(term_length), party, elected)

## ------------------------------------------------------------------------
mypresidents %>%
  summarize(
    N = n(), first_year = min(year(start)), last_year = max(year(end)), 
    num_dems = sum(party == "Democratic"), 
    years = sum(term_length), 
    avg_term_length = mean(term_length))

## ------------------------------------------------------------------------
mypresidents %>% 
  group_by(party) %>% 
  summarize(
    N = n(), first_year = min(year(start)), last_year = max(year(end)), 
    num_dems = sum(party == "Democratic"), 
    years = sum(term_length), 
    avg_term_length = mean(term_length))

## ----message=FALSE-------------------------------------------------------
library(Lahman)
dim(Teams)

## ------------------------------------------------------------------------
mets <- Teams %>% filter(teamID == "NYN")
myMets <- mets %>% filter(yearID %in% 2004:2012)
myMets %>% select(yearID, teamID, W, L)

## ------------------------------------------------------------------------
nrow(mets)

## ------------------------------------------------------------------------
select(filter(mets, teamID == "NYN" & yearID %in% 2004:2012),
  yearID, teamID, W, L)

## ----eval=FALSE----------------------------------------------------------
## Teams %>%
##   select(yearID, teamID, W, L) %>%
##   filter(teamID == "NYN" & yearID %in% 2004:2012)

## ------------------------------------------------------------------------
metsBen <- Teams %>% select(yearID, teamID, W, L, R, RA) %>%
  filter(teamID == "NYN" & yearID %in% 2004:2012)
metsBen

## ------------------------------------------------------------------------
metsBen <- metsBen %>% rename(RS = R)    # new name = old name
metsBen

## ------------------------------------------------------------------------
metsBen <- metsBen %>% mutate(WPct = W / (W + L))
metsBen

## ------------------------------------------------------------------------
metsBen <- metsBen %>% mutate(WPct_hat = 1 / (1 + (RA/RS)^2))
metsBen

## ------------------------------------------------------------------------
metsBen <- metsBen %>% mutate(W_hat = WPct_hat * (W + L))
metsBen

## ------------------------------------------------------------------------
filter(metsBen, W >= W_hat)
filter(metsBen, W < W_hat)

## ------------------------------------------------------------------------
arrange(metsBen, desc(WPct))

## ------------------------------------------------------------------------
metsBen %>% 
  mutate(Diff = W - W_hat) %>% 
  arrange(desc(Diff))

## ------------------------------------------------------------------------
favstats(~ W, data = metsBen)

## ------------------------------------------------------------------------
metsBen %>% 
  summarize(
    num_years = n(), total_W = sum(W), total_L = sum(L), 
    total_WPct = sum(W) / sum(W + L), sum_resid = sum(W - W_hat))

## ------------------------------------------------------------------------
metsBen <- metsBen %>% 
  mutate(
    gm = ifelse(yearID == 2004, "Duquette", 
         ifelse(yearID >= 2011, "Alderson", "Minaya")))

## ------------------------------------------------------------------------
metsBen %>% 
  group_by(gm) %>% 
  summarize(
    num_years = n(), total_W = sum(W), total_L = sum(L), 
    total_WPct = sum(W) / sum(W + L), sum_resid = sum(W - W_hat)) %>%
  arrange(desc(sum_resid))

## ------------------------------------------------------------------------
Teams %>%
  select(yearID, teamID, W, L, R, RA) %>%
  filter(teamID == "NYN" & yearID %in% 2004:2012) %>%
  rename(RS = R) %>% 
  mutate(
    WPct = W / (W + L), WPct_hat = 1 / (1 + (RA/RS)^2), 
    W_hat = WPct_hat * (W + L), 
    gm = ifelse(yearID == 2004, "Duquette", 
         ifelse(yearID >= 2011, "Alderson", "Minaya"))) %>%
  group_by(gm) %>%
  summarize(
    num_years = n(), total_W = sum(W), total_L = sum(L),
    total_WPct = sum(W) / sum(W + L), sum_resid = sum(W - W_hat)) %>%
  arrange(desc(sum_resid))

## ------------------------------------------------------------------------
Teams %>% select(yearID, teamID, franchID, W, L, R, RA) %>%
  filter(yearID %in% 2004:2012) %>% 
  rename(RS = R) %>% 
  mutate(
    WPct = W / (W + L), WPctHat = 1 / (1 + (RA/RS)^2), 
    WHat = WPctHat * (W + L)) %>%
  group_by(franchID) %>%
  summarize(
    numYears = n(), totalW = sum(W), totalL = sum(L), 
    totalWPct = sum(W) / sum(W + L), sumResid = sum(W - WHat)) %>%
  arrange(sumResid) %>% 
  print(n = 6)

## ----message=FALSE, include=FALSE----------------------------------------
library(nycflights13)

## ------------------------------------------------------------------------
library(nycflights13)
head(flights, 3)

## ------------------------------------------------------------------------
head(airlines, 3)

## ----warning=FALSE-------------------------------------------------------
flightsJoined <- flights %>% 
  inner_join(airlines, by = c("carrier" = "carrier"))
glimpse(flightsJoined)

## ------------------------------------------------------------------------
flightsJoined %>% 
  select(carrier, name, flight, origin, dest) %>% 
  head(3)

## ------------------------------------------------------------------------
nrow(flights)
nrow(flightsJoined)

## ------------------------------------------------------------------------
airportsPT <- filter(airports, tz == -8)
nrow(airportsPT)

## ------------------------------------------------------------------------
nycDestsPT <- flights %>% inner_join(airportsPT, by = c("dest" = "faa"))
nrow(nycDestsPT)

## ------------------------------------------------------------------------
nycDests <- flights %>% left_join(airportsPT, by = c("dest" = "faa"))
nrow(nycDests)
sum(is.na(nycDests$name))

## ------------------------------------------------------------------------
manny <- filter(Batting, playerID == "ramirma02")
nrow(manny)

## ------------------------------------------------------------------------
manny %>% summarize(
  span = paste(min(yearID), max(yearID), sep = "-"), 
  numYears = n_distinct(yearID), numTeams = n_distinct(teamID), 
  BA = sum(H)/sum(AB), tH = sum(H), tHR = sum(HR), tRBI = sum(RBI))

## ------------------------------------------------------------------------
manny %>% 
  group_by(teamID) %>%
  summarize(
    span = paste(min(yearID), max(yearID), sep = "-"), 
    numYears = n_distinct(yearID), numTeams = n_distinct(teamID), 
    BA = sum(H)/sum(AB), tH = sum(H), tHR = sum(HR), tRBI = sum(RBI)) %>%
  arrange(span)

## ------------------------------------------------------------------------
manny %>% 
  group_by(lgID) %>%
  summarize(
    span = paste(min(yearID), max(yearID), sep = "-"), 
    numYears = n_distinct(yearID), numTeams = n_distinct(teamID), 
    BA = sum(H)/sum(AB), tH = sum(H), tHR = sum(HR), tRBI = sum(RBI)) %>%
  arrange(span)

## ------------------------------------------------------------------------
manny %>% 
  filter(HR >= 30) %>% 
  nrow()

## ------------------------------------------------------------------------
manny %>% 
  group_by(yearID) %>% 
  summarize(tHR = sum(HR)) %>%
  filter(tHR >= 30) %>% 
  nrow()

## ------------------------------------------------------------------------
Master %>% filter(nameLast == "Ramirez" & nameFirst == "Manny")

## ------------------------------------------------------------------------
Batting %>% 
  filter(playerID == "ramirma02") %>%
  inner_join(Master, by = c("playerID" = "playerID")) %>%
  group_by(yearID) %>%
  summarize(
    Age = max(yearID - birthYear), numTeams = n_distinct(teamID), 
    BA = sum(H)/sum(AB), tH = sum(H), tHR = sum(HR), tRBI = sum(RBI)) %>%
  arrange(yearID)

## ------------------------------------------------------------------------
mannyBySeason <- Batting %>%
  filter(playerID == "ramirma02") %>%
  inner_join(Master, by = c("playerID" = "playerID"))  %>%
  group_by(yearID) %>%
  summarize(
    Age = max(yearID - birthYear), numTeams = n_distinct(teamID), 
    BA = sum(H)/sum(AB), tH = sum(H), tHR = sum(HR), tRBI = sum(RBI), 
    OBP = sum(H + BB + HBP) / sum(AB + BB + SF + HBP),
    SLG = sum(H + X2B + 2*X3B + 3*HR) / sum(AB)) %>%
  mutate(OPS = OBP + SLG) %>% 
  arrange(desc(OPS))
mannyBySeason

## ------------------------------------------------------------------------
mlb <- Batting %>%
  filter(yearID %in% 1993:2011) %>%
  group_by(yearID) %>%
  summarize(lgOPS = 
    sum(H + BB + HBP, na.rm = TRUE) / sum(AB + BB + SF + HBP, na.rm = TRUE) +
    sum(H + X2B + 2*X3B + 3*HR, na.rm = TRUE) / sum(AB, na.rm = TRUE))

## ------------------------------------------------------------------------
mannyRatio <- mannyBySeason %>%
  inner_join(mlb, by = c("yearID" = "yearID")) %>%
  mutate(OPSplus = OPS / lgOPS) %>%
  select(yearID, Age, OPS, lgOPS, OPSplus) %>%
  arrange(desc(OPSplus))
mannyRatio

## ------------------------------------------------------------------------
ripken <- Batting %>% filter(playerID == "ripkeca01")
nrow(inner_join(ripken, mlb, by = c("yearID" = "yearID")))
nrow(inner_join(mlb, ripken, by = c("yearID" = "yearID"))) #same

## ------------------------------------------------------------------------
ripken %>% 
  left_join(mlb, by = c("yearID" = "yearID")) %>%
  select(yearID, playerID, lgOPS) %>% 
  head(3)

## ----eval=FALSE----------------------------------------------------------
## mlb %>%
##   left_join(ripken, by = c("yearID" = "yearID")) %>%
##   select(yearID, playerID, lgOPS)

