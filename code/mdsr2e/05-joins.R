## -----------------------------------------------------------------------------
library(tidyverse)
library(mdsr)
library(nycflights13)
glimpse(flights)


## -----------------------------------------------------------------------------
head(airlines, 3)


## ----warning=FALSE------------------------------------------------------------
flights_joined <- flights %>% 
  inner_join(airlines, by = c("carrier" = "carrier"))
glimpse(flights_joined)


## -----------------------------------------------------------------------------
flights_joined %>% 
  select(carrier, name, flight, origin, dest) %>% 
  head(3)


## -----------------------------------------------------------------------------
nrow(flights)
nrow(flights_joined)


## -----------------------------------------------------------------------------
airports_pt <- airports %>%
  filter(tz == -8)
nrow(airports_pt)


## -----------------------------------------------------------------------------
nyc_dests_pt <- flights %>% 
  inner_join(airports_pt, by = c("dest" = "faa"))
nrow(nyc_dests_pt)


## -----------------------------------------------------------------------------
nyc_dests <- flights %>% 
  left_join(airports_pt, by = c("dest" = "faa"))

nyc_dests %>%
  summarize(
    num_flights = n(),
    num_flights_pt = sum(!is.na(name)),
    num_flights_not_pt = sum(is.na(name))
  )


## -----------------------------------------------------------------------------
library(Lahman)
manny <- Batting %>%
  filter(playerID == "ramirma02")
nrow(manny)


## -----------------------------------------------------------------------------
manny %>% 
  summarize(
    span = paste(min(yearID), max(yearID), sep = "-"), 
    num_years = n_distinct(yearID), 
    num_teams = n_distinct(teamID), 
    BA = sum(H)/sum(AB), 
    tH = sum(H), 
    tHR = sum(HR), 
    tRBI = sum(RBI)
  )


## -----------------------------------------------------------------------------
manny %>% 
  group_by(teamID) %>%
  summarize(
    span = paste(min(yearID), max(yearID), sep = "-"), 
    num_years = n_distinct(yearID), 
    num_teams = n_distinct(teamID), 
    BA = sum(H)/sum(AB), 
    tH = sum(H), 
    tHR = sum(HR), 
    tRBI = sum(RBI)
  ) %>%
  arrange(span)


## -----------------------------------------------------------------------------
manny %>% 
  group_by(lgID) %>%
  summarize(
    span = paste(min(yearID), max(yearID), sep = "-"), 
    num_years = n_distinct(yearID), 
    num_teams = n_distinct(teamID), 
    BA = sum(H)/sum(AB), 
    tH = sum(H), 
    tHR = sum(HR), 
    tRBI = sum(RBI)
  ) %>%
  arrange(span)


## -----------------------------------------------------------------------------
manny %>% 
  filter(HR >= 30) %>% 
  nrow()


## -----------------------------------------------------------------------------
manny %>% 
  group_by(yearID) %>% 
  summarize(tHR = sum(HR)) %>%
  filter(tHR >= 30) %>% 
  nrow()


## -----------------------------------------------------------------------------
Master %>% 
  filter(nameLast == "Ramirez" & nameFirst == "Manny")


## -----------------------------------------------------------------------------
Batting %>% 
  filter(playerID == "ramirma02") %>%
  inner_join(Master, by = c("playerID" = "playerID")) %>%
  group_by(yearID) %>%
  summarize(
    Age = max(yearID - birthYear), 
    num_teams = n_distinct(teamID), 
    BA = sum(H)/sum(AB), 
    tH = sum(H), 
    tHR = sum(HR), 
    tRBI = sum(RBI)
  ) %>%
  arrange(yearID)


## -----------------------------------------------------------------------------
manny_by_season <- Batting %>%
  filter(playerID == "ramirma02") %>%
  inner_join(Master, by = c("playerID" = "playerID"))  %>%
  group_by(yearID) %>%
  summarize(
    Age = max(yearID - birthYear), 
    num_teams = n_distinct(teamID), 
    BA = sum(H)/sum(AB), 
    tH = sum(H), 
    tHR = sum(HR), 
    tRBI = sum(RBI), 
    OBP = sum(H + BB + HBP) / sum(AB + BB + SF + HBP),
    SLG = sum(H + X2B + 2 * X3B + 3 * HR) / sum(AB)
  ) %>%
  mutate(OPS = OBP + SLG) %>% 
  arrange(desc(OPS))
manny_by_season


## -----------------------------------------------------------------------------
mlb <- Batting %>%
  filter(yearID %in% 1993:2011) %>%
  group_by(yearID) %>%
  summarize(
    lg_OBP = sum(H + BB + HBP, na.rm = TRUE) / 
      sum(AB + BB + SF + HBP, na.rm = TRUE), 
    lg_SLG = sum(H + X2B + 2*X3B + 3*HR, na.rm = TRUE) /
      sum(AB, na.rm = TRUE)
  ) %>%
  mutate(lg_OPS = lg_OBP + lg_SLG)


## -----------------------------------------------------------------------------
manny_ratio <- manny_by_season %>%
  inner_join(mlb, by = c("yearID" = "yearID")) %>%
  mutate(OPS_plus = OPS / lg_OPS) %>%
  select(yearID, Age, OPS, lg_OPS, OPS_plus) %>%
  arrange(desc(OPS_plus))
manny_ratio


## -----------------------------------------------------------------------------
ripken <- Batting %>% 
  filter(playerID == "ripkeca01")
ripken %>%
  inner_join(mlb, by = c("yearID" = "yearID")) %>%
  nrow()
# same
mlb %>%
  inner_join(ripken, by = c("yearID" = "yearID")) %>%
  nrow()


## -----------------------------------------------------------------------------
ripken %>% 
  left_join(mlb, by = c("yearID" = "yearID")) %>%
  select(yearID, playerID, lg_OPS) %>% 
  head(3)


## ----eval=FALSE---------------------------------------------------------------
## mlb %>%
##   left_join(ripken, by = c("yearID" = "yearID")) %>%
##   select(yearID, playerID, lg_OPS)

