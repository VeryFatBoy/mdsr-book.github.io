## ----echo = TRUE, message=FALSE-----------------------------------------------
library(tidyverse)
library(mdsr)
NCI60 <- etl_NCI60()
spreads <- NCI60 %>%
  pivot_longer(
    -Probe, values_to = "expression", 
    names_to = "cellLine"
  ) %>%  
  group_by(Probe) %>%
  summarize(N = n(), spread = sd(expression)) %>%
  arrange(desc(spread)) %>%
  mutate(order = row_number())


## ----message = FALSE----------------------------------------------------------
sim_spreads <- NCI60 %>%
  pivot_longer(
    -Probe, values_to = "expression", 
    names_to = "cellLine"
  ) %>%
  mutate(Probe = mosaic::shuffle(Probe)) %>%
  group_by(Probe) %>%
  summarize(N = n(), spread = sd(expression)) %>%
  arrange(desc(spread)) %>%
  mutate(order = row_number())


## ----nci60sim, fig.keep="last", message=FALSE, fig.cap="Comparing the variation in expression for individual probes across cell lines data (blue) and a simulation of a null hypothesis (red)."----
spreads %>%
  filter(order <= 500) %>%
  ggplot(aes(x = order, y = spread)) +
  geom_line(color = "blue", size = 2) +
  geom_line(
    data = filter(sim_spreads, order <= 500), 
    color = "red", 
    size = 2
  ) +
  geom_text(
    label = "simulated", x = 275, y = 4.4, 
    size = 3, color = "red"
  ) +
  geom_text(
    label = "observed", x = 75, y = 5.5, 
    size = 3, color = "blue"
  )


## -----------------------------------------------------------------------------
runif(5)


## -----------------------------------------------------------------------------
select_one <- function(vec) {
  n <- length(vec)
  ind <- which.max(runif(n))
  vec[ind]
}
select_one(letters) # letters are a, b, c, ..., z
select_one(letters)


## ----echo=FALSE---------------------------------------------------------------
set.seed(1977)


## -----------------------------------------------------------------------------
n <- 100000
sim_meet <- tibble(
  sally = runif(n, min = 0, max = 60),
  joan = runif(n, min = 0, max = 60),
  result = ifelse(
    abs(sally - joan) <= 10, "They meet", "They do not"
  )
)
mosaic::tally(~ result, format = "percent", data = sim_meet)
mosaic::binom.test(~result, n, success = "They meet", data = sim_meet)


## ----sally1, message=FALSE, eval=TRUE, fig.keep="last", fig.cap="Distribution of Sally and Joan arrival times (shaded area indicates where they meet)."----
ggplot(data = sim_meet, aes(x = joan, y = sally, color = result)) + 
  geom_point(alpha = 0.3) + 
  geom_abline(intercept = 10, slope = 1) + 
  geom_abline(intercept = -10, slope = 1) + 
  scale_color_brewer(palette = "Set2")


## ----echo=FALSE---------------------------------------------------------------
set.seed(1977)


## -----------------------------------------------------------------------------
jobs_true <- 150
jobs_se <- 65  # in thousands of jobs
gen_samp <- function(true_mean, true_sd, 
                     num_months = 12, delta = 0, id = 1) {
  samp_year <- rep(true_mean, num_months) + 
    rnorm(num_months, mean = delta * (1:num_months), sd = true_sd)
  return(
    tibble(
      jobs_number = samp_year, 
      month = as.factor(1:num_months), 
      id = id
    )
  )
}


## -----------------------------------------------------------------------------
n_sims <- 3
params <- tibble(
  sd = c(0, rep(jobs_se, n_sims)), 
  id = c("Truth", paste("Sample", 1:n_sims))
)
params


## -----------------------------------------------------------------------------
df <- params %>%
  pmap_dfr(~gen_samp(true_mean = jobs_true, true_sd = ..1, id = ..2))


## ----nytimes, fig.keep="last", warning=FALSE, fig.cap="True number of new jobs from simulation as well as three realizations from a simulation."----
ggplot(data = df, aes(x = month, y = jobs_number)) + 
  geom_hline(yintercept = jobs_true, linetype = 2) + 
  geom_col() + 
  facet_wrap(~ id) + 
  ylab("Number of new jobs (in thousands)")


## -----------------------------------------------------------------------------
minval <- 7
maxval <- 19
violation_scores <- Violations %>%
  filter(lubridate::year(inspection_date) == 2015) %>%
  filter(score >= minval & score <= maxval) %>%
  select(dba, score)


## ----rest, fig.keep="last", fig.cap="Distribution of NYC restaurant health violation scores."----
ggplot(data = violation_scores, aes(x = score)) + 
  geom_histogram(binwidth = 0.5) + 
  geom_vline(xintercept = 13, linetype = 2) + 
  scale_x_continuous(breaks = minval:maxval) + 
  annotate(
    "text", x = 10, y = 15000, 
    label = "'A' grade: score of 13 or less"
  )


## -----------------------------------------------------------------------------
scores <- mosaic::tally(~score, data = violation_scores)
scores
mean(scores[c("13", "14")])
random_flip <- 1:1000 %>%
  map_dbl(~mosaic::nflip(scores["13"] + scores["14"])) %>%
  enframe(name = "sim", value = "heads")
head(random_flip, 3)


## ----rest2, warning=FALSE, fig.keep="last", fig.cap="Distribution of health violation scores under a randomization procedure (permutation test)."----
ggplot(data = random_flip, aes(x = heads)) + 
  geom_histogram(binwidth = 10) + 
  geom_vline(xintercept = scores["14"], col = "red") + 
  annotate(
    "text", x = 2200, y = 75, 
    label = "observed", hjust = "left"
  ) + 
  xlab("Number of restaurants with scores of 14 (if equal probability)") 


## ----echo=FALSE---------------------------------------------------------------
set.seed(1492)

## ----render=knitr::normal_print-----------------------------------------------
campus_sim <- function(sims = 1000, wait = 10) {
  sally <- runif(sims, min = 0, max = 60)
  joan <- runif(sims, min = 0, max = 60)
  return(
    tibble(
      num_sims = sims, 
      meet = sum(abs(sally - joan) <= wait),
      meet_pct = meet / num_sims,
    )
  )
}

reps <- 5000
sim_results <- 1:reps %>%
  map_dfr(~map_dfr(c(100, 400, 1600), campus_sim))

sim_results %>%
  group_by(num_sims) %>%
  skim(meet_pct)


## ----converge, fig.cap="Convergence of the estimate of the proportion of times that Sally and Joan meet."----
sim_results %>%
  ggplot(aes(x = meet_pct, color = factor(num_sims))) + 
  geom_density(size = 2) + 
  geom_vline(aes(xintercept = 11/36), linetype = 3) +
  scale_x_continuous("Proportion of times that Sally and Joan meet") + 
  scale_color_brewer("Number\nof sims", palette = "Set2")




## -----------------------------------------------------------------------------
set.seed(1974)
campus_sim()
campus_sim()
set.seed(1974)
campus_sim()

