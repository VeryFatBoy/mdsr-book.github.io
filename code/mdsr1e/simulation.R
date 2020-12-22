## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/simulation-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----echo = TRUE, message=FALSE------------------------------------------
library(mdsr)
library(tidyr)
NCI60 <- etl_NCI60()
Spreads <- NCI60 %>%
  gather(value = expression, key = cellLine, -Probe) %>%
  group_by(Probe) %>%
  summarize(N = n(), spread = sd(expression)) %>%
  arrange(desc(spread)) %>%
  mutate(order = row_number())

## ------------------------------------------------------------------------
Sim_spreads <- NCI60 %>%
  gather(value = expression, key = cellLine, -Probe) %>%
  mutate(Probe = shuffle(Probe)) %>%
  group_by(Probe) %>%
  summarize(N = n(), spread = sd(expression)) %>%
  arrange(desc(spread)) %>%
  mutate(order = row_number())

## ----nci60sim,fig.keep="last", message=FALSE-----------------------------
Spreads %>%
  filter(order <= 500) %>%
  ggplot(aes(x = order, y = spread)) +
  geom_line(color = "blue", size = 2) +
  geom_line(data = filter(Sim_spreads, order <= 500), color = "red", size = 2)

## ------------------------------------------------------------------------
runif(5)

## ------------------------------------------------------------------------
select_one <- function(vec) {
  n <- length(vec)
  ind <- which.max(runif(n))
  vec[ind]
}
select_one(letters) # letters are a, b, c, ..., z
select_one(letters)

## ----echo=FALSE----------------------------------------------------------
set.seed(1977)

## ------------------------------------------------------------------------
n <- 100000
sim_meet <- data.frame(
  sally <- runif(n, min = 0, max = 60),
  joan <- runif(n, min = 0, max = 60)) %>%
  mutate(result = ifelse(abs(sally - joan) <= 10, 
    "They meet", "They do not"))
tally(~ result, format = "percent", data = sim_meet)
binom.test(~result, n, success = "They meet", data = sim_meet)

## ----sally1,message=FALSE,eval=TRUE,fig.keep="last"----------------------
ggplot(data = sim_meet, aes(x = joan, y = sally, color = result)) + 
  geom_point(alpha = 0.3) + 
  geom_abline(intercept = 10, slope = 1) + 
  geom_abline(intercept = -10, slope = 1)

## ----echo=FALSE----------------------------------------------------------
set.seed(1977)

## ------------------------------------------------------------------------
jobs_true <- 150
jobs_se <- 65  # in thousands of jobs
gen_samp <- function(true_mean, true_sd, 
                     num_months = 12, delta = 0, id = 1) {
  samp_year <- rep(true_mean, num_months) + 
    rnorm(num_months, mean = delta * (1:num_months), sd = true_sd)
  return(data.frame(jobs_number = samp_year, 
                    month = as.factor(1:num_months), id = id))
}

## ------------------------------------------------------------------------
n_sims <- 3
params <- data.frame(sd = c(0, rep(jobs_se, n_sims)), 
                     id = c("Truth", paste("Sample", 1:n_sims)))
params

## ------------------------------------------------------------------------
df <- params %>%
  group_by(id) %>%
  dplyr::do(gen_samp(true_mean = jobs_true, true_sd = .$sd, id = .$id))

## ----jobs1,fig.keep="last", warning=FALSE--------------------------------
ggplot(data = df, aes(x = month, y = jobs_number)) + 
  geom_hline(yintercept = jobs_true, linetype = 2) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ id) + ylab("Number of new jobs (in thousands)")

## ------------------------------------------------------------------------
minval <- 7
maxval <- 19
JustScores <- Violations %>%
  filter(score >= minval & score <= maxval) %>%
  select(dba, score) %>%
  unique()

## ----restaurant1,fig.keep="last"-----------------------------------------
ggplot(data = JustScores, aes(x = score)) + 
  geom_histogram(binwidth = 0.5) + geom_vline(xintercept = 13, linetype = 2) + 
  scale_x_continuous(breaks = minval:maxval) + 
  annotate("text", x = 10.5, y = 10300,
           label = "A grade: score of 13 or less")

## ------------------------------------------------------------------------
scores <- tally(~score, data = JustScores)
scores
obs_diff <- scores["13"] - scores["14"]
mean(scores[c("13", "14")])
RandomFlip <- do(1000) * rflip(scores["13"] + scores["14"])
head(RandomFlip, 3)

## ----restaurant2,fig.keep="last"-----------------------------------------
ggplot(data = RandomFlip, aes(x = heads)) + 
  geom_histogram(binwidth = 5) + xlim(c(2100, NA)) + 
  geom_vline(xintercept = scores["14"], col = "red") + 
  annotate("text", x = 2137, y = 45, label = "observed", hjust = "left") + 
  xlab("Number of restaurants with scores of 14 (if equal probability)") 

## ------------------------------------------------------------------------
any_active <- function(df) {
  # return TRUE if someone has not finished
  return(max(df$endtime) == Inf)
}

next_customer <- function(df) {
  # returns the next customer in line
  res <- filter(df, endtime == Inf) %>%
    arrange(arrival)
  return(head(res, 1))
}

update_customer <- function(df, cust_num, end_time) {
  # sets the end time of a specific customer
  return(mutate(df, endtime = 
    ifelse(custnum == cust_num, end_time, endtime)))
}

## ------------------------------------------------------------------------
run_sim <- function(n = 1/2, m = 3/2, hours = 6) {
# simulation of bank where there is just one teller
# n: expected number of customers per minute
# m: expected length of transaction is m minutes
# hours: bank open for this many hours
  customers <- rpois(hours * 60, lambda = n)
  arrival <- numeric(sum(customers))
  position <- 1
  for (i in 1:length(customers)) {
    numcust <- customers[i]
    if (numcust != 0) {
      arrival[position:(position + numcust - 1)] <- rep(i, numcust)
      position <- position + numcust
    }
  }
  duration <- rexp(length(arrival), rate = 1/m)   # E[X]=m
  df <- data.frame(arrival, duration, custnum = 1:length(duration), 
                 endtime = Inf, stringsAsFactors = FALSE)

  endtime <- 0 # set up beginning of simulation
  while (any_active(df)) { # anyone left to serve?
    next_one <- next_customer(df)
    now <- ifelse(next_one$arrival >= endtime, next_one$arrival, endtime)
    endtime <- now + next_one$duration
    df <- update_customer(df, next_one$custnum, endtime)
  }
  df <- mutate(df, totaltime = endtime - arrival)
  return(favstats(~ totaltime, data = df))
}

## ----echo=FALSE----------------------------------------------------------
set.seed(1964)

## ------------------------------------------------------------------------
sim_results <- do(3) * run_sim()
sim_results

## ----echo=FALSE----------------------------------------------------------
set.seed(1492)

## ------------------------------------------------------------------------
campus_sim <- function(num_sim = 1000, wait = 10) {
  sally <- runif(num_sim, min = 0, max = 60)
  joan <- runif(num_sim, min = 0, max = 60)
  return(sum(abs(sally - joan) <= wait) / num_sim)
}
reps <- 5000
params <- data.frame(num_sims = c(100, 400, 1600))
sim_results <- params %>%
  group_by(num_sims) %>%
  dplyr::do(mosaic::do(reps) * campus_sim(.$num_sims))
favstats(campus_sim ~ num_sims, data = sim_results)

## ----dplot1--------------------------------------------------------------
ggplot(data = sim_results, aes(x = campus_sim, color = factor(num_sims))) + 
  geom_density(size = 2) + 
  scale_x_continuous("Proportion of times that Sally and Joan meet")

## ----bigsim,cache=TRUE---------------------------------------------------
sim_results <- do(reps) * campus_sim(num_sim = 20000)
favstats(~ campus_sim, data = sim_results)

## ------------------------------------------------------------------------
set.seed(1974)
campus_sim()
campus_sim()
set.seed(1974)
campus_sim()

