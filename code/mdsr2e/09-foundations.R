## ----echo=FALSE,eval=TRUE-----------------------------------------------------
set.seed(1999)


## ----message=FALSE------------------------------------------------------------
library(tidyverse)
library(mdsr)
library(nycflights13)
SF <- flights %>%
  filter(dest == "SFO", !is.na(arr_delay))


## -----------------------------------------------------------------------------
set.seed(101)
sf_25 <- SF %>%
  slice_sample(n = 25)


## ---- skimr_include_summary=FALSE, render=knitr::normal_print-----------------
sf_25 %>%
  skim(arr_delay)


## ---- skimr_include_summary=FALSE, render=knitr::normal_print-----------------
SF %>%
  skim(arr_delay)


## -----------------------------------------------------------------------------
sf_25 %>%
  summarize(q98 = quantile(arr_delay, p = 0.98))


## -----------------------------------------------------------------------------
SF %>%
  group_by(arr_delay < 90) %>%
  count() %>%
  mutate(pct = n / nrow(SF))


## -----------------------------------------------------------------------------
SF %>%
  summarize(q98 = quantile(arr_delay, p = 0.98))


## ----echo = FALSE-------------------------------------------------------------
set.seed(112)


## -----------------------------------------------------------------------------
n <- 25
SF %>%
  slice_sample(n = n) %>%
  summarize(mean_arr_delay = mean(arr_delay))

SF %>%
  slice_sample(n = n) %>%
  summarize(mean_arr_delay = mean(arr_delay))


## -----------------------------------------------------------------------------
num_trials <- 500
sf_25_means <- 1:num_trials %>%
  map_dfr(
    ~ SF %>%
      slice_sample(n = n) %>%
      summarize(mean_arr_delay = mean(arr_delay))
  ) %>%
  mutate(n = n)

head(sf_25_means)


## ---- skimr_include_summary=FALSE, render=knitr::normal_print-----------------
sf_25_means %>%
  skim(mean_arr_delay)


## -----------------------------------------------------------------------------
sf_25_means %>%
  summarize(
    x_bar = mean(mean_arr_delay),
    se = sd(mean_arr_delay)
  ) %>%
  mutate(
    ci_lower = x_bar - 2 * se, # approximately 95% of observations 
    ci_upper = x_bar + 2 * se  # are within two standard errors
  )


## ----message = FALSE----------------------------------------------------------
sf_25_means %>%
  pull(mean_arr_delay) %>%
  t.test()


## ----echo=FALSE---------------------------------------------------------------
set.seed(111)

## ----echo = TRUE--------------------------------------------------------------
n <- 100
sf_100_means <- 1:500 %>%
  map_dfr(
    ~ SF %>%
      slice_sample(n = n) %>%
      summarize(mean_arr_delay = mean(arr_delay))
  ) %>%
  mutate(n = n)


## ----sampdist25, echo=TRUE, eval=TRUE, fig.cap="The sampling distribution of the mean arrival delay with a sample size of $n=25$ (left) and also for a larger sample size of $n = 100$ (right). Note that the sampling distribution is less variable for a larger sample size."----
sf_25_means %>%
  bind_rows(sf_100_means) %>%
  ggplot(aes(x = mean_arr_delay)) + 
  geom_histogram(bins = 30) + 
  facet_grid( ~ n) + 
  xlab("Sample mean")


## ----echo = FALSE-------------------------------------------------------------
set.seed(200)


## -----------------------------------------------------------------------------
three_flights <- SF %>%
  slice_sample(n = 3, replace = FALSE) %>%
  select(year, month, day, dep_time)
three_flights


## -----------------------------------------------------------------------------
three_flights %>% slice_sample(n = 3, replace = TRUE)


## -----------------------------------------------------------------------------
three_flights %>% slice_sample(n = 3, replace = TRUE)


## -----------------------------------------------------------------------------
n <- 200
orig_sample <- SF %>% 
  slice_sample(n = n, replace = FALSE)


## -----------------------------------------------------------------------------
orig_sample %>%
  slice_sample(n = n, replace = TRUE) %>%
  summarize(mean_arr_delay = mean(arr_delay))


## ---- skimr_include_summary=FALSE, render=knitr::normal_print-----------------
sf_200_bs <- 1:num_trials %>%
  map_dfr(
    ~orig_sample %>%
      slice_sample(n = n, replace = TRUE) %>%
      summarize(mean_arr_delay = mean(arr_delay))
  ) %>%
  mutate(n = n)

sf_200_bs %>%
  skim(mean_arr_delay)


## ---- skimr_include_summary=FALSE, render=knitr::normal_print-----------------
sf_200_pop <- 1:num_trials %>%
  map_dfr(
    ~SF %>%
      slice_sample(n = n, replace = TRUE) %>%
      summarize(mean_arr_delay = mean(arr_delay))
  ) %>%
  mutate(n = n)

sf_200_pop %>%
  skim(mean_arr_delay)


## -----------------------------------------------------------------------------
orig_sample %>%
  summarize(q98 = quantile(arr_delay, p = 0.98))


## ----echo = FALSE-------------------------------------------------------------
set.seed(101)


## ---- skimr_include_summary=FALSE, render=knitr::normal_print-----------------
n <- nrow(orig_sample)
sf_200_bs <- 1:num_trials %>%
  map_dfr(
    ~orig_sample %>%
      slice_sample(n = n, replace = TRUE) %>%
      summarize(q98 = quantile(arr_delay, p = 0.98))
  )

sf_200_bs %>%
  skim(q98)


## ----echo = TRUE, skimr_include_summary=FALSE, render=knitr::normal_print-----
set.seed(1001)
n_large <- 10000
sf_10000_bs <- SF %>% 
  slice_sample(n = n_large, replace = FALSE)

sf_200_bs <- 1:num_trials %>%
  map_dfr(~sf_10000_bs %>%
        slice_sample(n = n_large, replace = TRUE) %>%
        summarize(q98 = quantile(arr_delay, p = 0.98))
  )

sf_200_bs %>%
  skim(q98)


## -----------------------------------------------------------------------------
SF %>%
  filter(arr_delay >= 420) %>% 
  select(month, day, dep_delay, arr_delay, carrier)


## ----allflights2, echo=TRUE, eval=TRUE, fig.cap="Distribution of flight arrival delays in 2013 for flights to San Francisco from NYC airports that were delayed less than 7 hours.  The distribution features a long right tail (even after pruning the outliers)."----
SF %>% 
  filter(arr_delay < 420) %>%
  ggplot(aes(arr_delay)) + 
  geom_histogram(binwidth = 15) + 
  labs(x = "Arrival delay (in minutes)")


## -----------------------------------------------------------------------------
SF %>% 
  mutate(long_delay = arr_delay > 60) %>%
  group_by(month, long_delay) %>%
  count() %>%
  pivot_wider(names_from = month, values_from = n) %>%
  data.frame()


## -----------------------------------------------------------------------------
SF %>% 
  mutate(long_delay = arr_delay > 60) %>%
  group_by(carrier, long_delay) %>%
  count() %>%
  pivot_wider(names_from = carrier, values_from = n) %>%
  data.frame()


## -----------------------------------------------------------------------------
SF %>%
  group_by(hour) %>%
  count() %>%
  pivot_wider(names_from = hour, values_from = n) %>%
  data.frame()


## ----schedhour,echo=TRUE, message = FALSE, eval=TRUE, fig.cap="Association of flight arrival delays with scheduled departure time for flights to San Francisco from New York airports in 2013."----
SF %>%
  ggplot(aes(x = hour, y = arr_delay)) +
  geom_boxplot(alpha = 0.1, aes(group = hour)) +
  geom_smooth(method = "lm") + 
  xlab("Scheduled hour of departure") + 
  ylab("Arrival delay (minutes)") + 
  coord_cartesian(ylim = c(-30, 120)) 


## -----------------------------------------------------------------------------
mod1 <- lm(arr_delay ~ hour, data = SF)
broom::tidy(mod1)


## ----message=FALSE------------------------------------------------------------
library(lubridate)
SF <- SF %>% 
  mutate(
    day = as.Date(time_hour), 
    dow = as.character(wday(day, label = TRUE)),
    season = ifelse(month %in% 6:7, "summer", "other month")
  )


## -----------------------------------------------------------------------------
mod2 <- lm(arr_delay ~ hour + origin + carrier + season + dow, data = SF)
broom::tidy(mod2)


## ----sat1, message = FALSE, fig.keep="last", fig.cap="Scatterplot of average SAT scores versus average teacher salaries (in thousands of dollars) for the 50 United States in 2010."----
SAT_2010 <- SAT_2010 %>%
  mutate(Salary = salary/1000)
SAT_plot <- ggplot(data = SAT_2010, aes(x = Salary, y = total)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ylab("Average total score on the SAT") + 
  xlab("Average teacher salary (thousands of USD)")
SAT_plot


## -----------------------------------------------------------------------------
SAT_mod1 <- lm(total ~ Salary, data = SAT_2010)
broom::tidy(SAT_mod1)


## ---- skimr_include_summary=FALSE, render=knitr::normal_print-----------------
SAT_2010 %>%
  skim(sat_pct)
SAT_2010 <- SAT_2010 %>%
  mutate(SAT_grp = ifelse(sat_pct <= 27, "Low", "High"))
SAT_2010 %>%
  group_by(SAT_grp) %>%
  count()


## ----sat2, fig.keep="last", message = FALSE, fig.cap="Scatterplot of average SAT scores versus average teacher salaries (in thousands of dollars) for the 50 United States in 2010, stratified by the percentage of students taking the SAT in each state."----
SAT_plot %+% SAT_2010 + 
  aes(color = SAT_grp) + 
  scale_color_brewer("% taking\nthe SAT", palette = "Set2")


## -----------------------------------------------------------------------------
SAT_2010 %>%
  group_by(SAT_grp) %>%
  group_modify(~broom::tidy(lm(total ~ Salary, data = .x)))


## -----------------------------------------------------------------------------
SAT_mod2 <- lm(total ~ Salary + sat_pct, data = SAT_2010)
broom::tidy(SAT_mod2)


## -----------------------------------------------------------------------------
1 - (1 - 0.05)^5


## -----------------------------------------------------------------------------
1 - (1 - 0.01)^5

