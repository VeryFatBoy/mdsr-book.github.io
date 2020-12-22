## ----message=FALSE------------------------------------------------------------
library(tidyverse)
library(mdsr)
library(Lahman)
names(Teams)


## ----eval=FALSE---------------------------------------------------------------
## str(Teams)
## glimpse(Teams)


## ----message=FALSE------------------------------------------------------------
averages <- NULL
for (i in 15:40) {
  averages[i - 14] <- mean(Teams[, i], na.rm = TRUE)
}
names(averages) <- names(Teams)[15:40]
averages


## -----------------------------------------------------------------------------
a <- "a string"
class(a)
is.vector(a)
length(a)


## -----------------------------------------------------------------------------
exp(1:3)


## -----------------------------------------------------------------------------
mean(1:3)


## -----------------------------------------------------------------------------
if (c(TRUE, FALSE)) {
  cat("This is a great book!")
}


## ----bench-mark-exp-----------------------------------------------------------
x <- 1:1e5
bench::mark(
  exp(x),
  map_dbl(x, exp)
)


## -----------------------------------------------------------------------------
Teams %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))


## -----------------------------------------------------------------------------
Teams %>%
  summarize(across(c(yearID, R:SF, BPF), mean, na.rm = TRUE))


## -----------------------------------------------------------------------------
Teams %>% 
  select(15:40) %>%
  map_dbl(mean, na.rm = TRUE)


## ----map-bad------------------------------------------------------------------
Teams %>% 
  select(teamID) %>% 
  map_dbl(mean, na.rm = TRUE)


## -----------------------------------------------------------------------------
angels <- Teams %>% 
  filter(franchID == "ANA") %>% 
  group_by(teamID, name) %>%
  summarize(began = first(yearID), ended = last(yearID)) %>% 
  arrange(began)
angels


## -----------------------------------------------------------------------------
angels_names <- angels %>%
  pull(name)
nchar(angels_names[1])
nchar(angels_names[2])
nchar(angels_names[3])
nchar(angels_names[4])


## -----------------------------------------------------------------------------
map_int(angels_names, nchar)


## -----------------------------------------------------------------------------
nchar(angels_names)


## -----------------------------------------------------------------------------
top5 <- function(data, team_name) {
  data %>%
    filter(name == team_name) %>%
    select(teamID, yearID, W, L, name) %>%
    arrange(desc(W)) %>%
    head(n = 5)
}


## -----------------------------------------------------------------------------
angels_names %>%
  map(top5, data = Teams)


## -----------------------------------------------------------------------------
angels_names %>% 
  map_dfr(top5, data = Teams) %>%
  group_by(teamID, name) %>%
  summarize(N = n(), mean_wins = mean(W)) %>%
  arrange(desc(mean_wins))


## ----pythag, fig.keep="last", fig.cap="Fit for the Pythagorean Winning Percentage model for all teams since 1954."----
exp_wpct <- function(x) { 
  return(1/(1 + (1/x)^2))
}

TeamRuns <- Teams %>% 
  filter(yearID >= 1954) %>%
  rename(RS = R) %>% 
  mutate(WPct = W / (W + L), run_ratio = RS/RA) %>%
  select(yearID, teamID, lgID, WPct, run_ratio)

ggplot(data = TeamRuns, aes(x = run_ratio, y = WPct)) +
  geom_vline(xintercept = 1, color = "darkgray", linetype = 2) +
  geom_hline(yintercept = 0.5, color = "darkgray", linetype = 2) +
  geom_point(alpha = 0.2) + 
  stat_function(fun = exp_wpct, size = 2, color = "blue") + 
  xlab("Ratio of Runs Scored to Runs Allowed") + 
  ylab("Winning Percentage")


## ----message = FALSE----------------------------------------------------------
TeamRuns %>%
  nls(
    formula = WPct ~ 1/(1 + (1/run_ratio)^k), 
    start = list(k = 2)
  ) %>%
  coef()


## ----fitModel, message=FALSE--------------------------------------------------
fit_k <- function(x) {
  mod <- nls(
    formula = WPct ~ 1/(1 + (1/run_ratio)^k), 
    data = x,
    start = list(k = 2)
  )
  return(tibble(k = coef(mod), n = nrow(x)))
}


## -----------------------------------------------------------------------------
fit_k(TeamRuns)


## -----------------------------------------------------------------------------
TeamRuns %>% 
  mutate(decade = yearID %/% 10 * 10) %>%
  group_by(decade) %>% 
  group_modify(~fit_k(.x))


## ----message=FALSE------------------------------------------------------------
hr_leader <- function(x) {
# x is a subset of Teams for a single year and league
  x %>% 
    select(teamID, HR) %>% 
    arrange(desc(HR)) %>% 
    head(1)
}


## -----------------------------------------------------------------------------
Teams %>% 
  filter(yearID == 1961 & lgID == "AL") %>% 
  hr_leader()


## -----------------------------------------------------------------------------
hr_leaders <- Teams %>% 
  group_by(yearID, lgID) %>% 
  group_modify(~hr_leader(.x), .keep = TRUE)

tail(hr_leaders, 4)


## -----------------------------------------------------------------------------
hr_leaders %>%
  group_by(lgID) %>%
  summarize(mean_hr = mean(HR))


## -----------------------------------------------------------------------------
hr_leaders %>%
  filter(yearID >= 1916) %>%
  group_by(lgID) %>%
  summarize(mean_hr = mean(HR))


## ----dh, message=FALSE, fig.cap="Number of home runs hit by the team with the most home runs, 1916--2019. Note how the AL has consistently bested the NL since the introduction of the designated hitter (DH) in 1973."----
hr_leaders %>% 
  filter(yearID >= 1916) %>%
  ggplot(aes(x = yearID, y = HR, color = lgID)) + 
  geom_line() + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  geom_vline(xintercept = 1973) + 
  annotate(
    "text", x = 1974, y = 25, 
    label = "AL adopts DH", hjust = "left"
  ) +
  labs(x = "Year", y = "Home runs", color = "League")


## ----teamdens, skimr_include_summary=FALSE, render=knitr::normal_print--------
k_actual <- TeamRuns %>% 
  group_by(yearID) %>% 
  group_modify(~fit_k(.x))
k_actual %>%
  ungroup() %>%
  skim(k)


## ----teamdens2, fig.asp = 0.45, fig.cap="Distribution of best-fitting exponent across single seasons from 1954--2019."----
ggplot(data = k_actual, aes(x = k)) + 
  geom_density() + 
  xlab("Best fit exponent for a single season")


## ----bdensplot0, message=FALSE------------------------------------------------
n <- 10000

bstrap <- 1:n %>%
  map_dbl(
    ~k_actual %>%
      pull(k) %>%
      sample(replace = TRUE) %>% 
      mean()
  )

civals <- bstrap %>%
  quantile(probs = c(0.025, .975))
civals


## ----bdensplot, fig.asp = 0.45, message=FALSE, fig.cap="Bootstrap distribution of mean optimal Pythagorean exponent."----
ggplot(data = enframe(bstrap, value = "k"), aes(x = k)) + 
  geom_density() + 
  xlab("Distribution of resampled means") + 
  geom_vline(
    data = enframe(civals), aes(xintercept = value), 
    color = "red", linetype = 3
  )


## -----------------------------------------------------------------------------
library(NHANES)


## ----nhanes-age, fig.cap="(ref:nhanes-age-cap)", warning=FALSE, message=FALSE----
ggplot(NHANES, aes(x = Age, y = BMI)) +
  geom_point() + 
  geom_smooth()


## -----------------------------------------------------------------------------
bmi_plot <- function(.data, x_var) {
  ggplot(.data, aes(y = BMI)) +
    aes_string(x = x_var) + 
    geom_jitter(alpha = 0.3) + 
    geom_smooth() + 
    labs(
      title = paste("BMI by", x_var),
      subtitle = "NHANES",
      caption = "US National Center for Health Statistics (NCHS)"
    )
}


## ---- eval=FALSE--------------------------------------------------------------
## bmi_plot(NHANES, "Age")


## ----patchwork, fig.cap="(ref:nhanes-patchwork)", fig.asp=1.4, message=FALSE, warning=FALSE----
c("Age", "HHIncomeMid", "PhysActiveDays", 
  "TVHrsDay", "AlcoholDay", "Pulse") %>%
  map(bmi_plot, .data = NHANES) %>%
  patchwork::wrap_plots(ncol = 2)

