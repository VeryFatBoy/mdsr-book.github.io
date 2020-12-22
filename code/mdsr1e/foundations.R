## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/foundations-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")
set.seed(1999)

## ----message=FALSE-------------------------------------------------------
library(mdsr)
library(nycflights13)
SF <- flights %>%
  filter(dest == "SFO", !is.na(arr_delay))

## ------------------------------------------------------------------------
set.seed(101)
Sample25 <- SF %>%
  sample_n(size = 25)

## ------------------------------------------------------------------------
favstats( ~ arr_delay, data = Sample25)

## ------------------------------------------------------------------------
favstats( ~ arr_delay, data = SF)

## ------------------------------------------------------------------------
qdata( ~ arr_delay, p = 0.98, data = Sample25)

## ------------------------------------------------------------------------
tally( ~ arr_delay < 90, data = SF, format = "proportion")

## ------------------------------------------------------------------------
qdata( ~ arr_delay, p = 0.98, data = SF)

## ----echo = FALSE--------------------------------------------------------
set.seed(112)

## ------------------------------------------------------------------------
n <- 25
mean( ~ arr_delay, data = sample_n(SF, size = n, replace = FALSE))
mean( ~ arr_delay, data = sample_n(SF, size = n, replace = FALSE))

## ------------------------------------------------------------------------
Trials <- do(500) *
  mean( ~ arr_delay, data = sample_n(SF, size = n, replace = FALSE))
head(Trials)

## ------------------------------------------------------------------------
favstats( ~ mean, data = Trials)

## ------------------------------------------------------------------------
mean(~ mean, data = Trials) + 2 * sd(~ mean, data = Trials) * c(-1, 1)

## ----echo=FALSE----------------------------------------------------------
set.seed(111)

## ----echo = TRUE---------------------------------------------------------
Trials_100 <- do(500) *
  mean( ~ arr_delay, data = SF %>% sample_n(size = 100, replace = FALSE))

## ----sampdist25,echo=TRUE, eval=TRUE-------------------------------------
rbind(Trials %>% mutate(n = 25), Trials_100 %>% mutate(n = 100)) %>%
  ggplot(aes(x = mean)) + geom_histogram(bins = 30) + 
  facet_grid( ~ n) + xlab("Sample mean")

## ----echo = FALSE--------------------------------------------------------
set.seed(200)

## ----result = "hide"-----------------------------------------------------
Small <- sample_n(SF, size = 3, replace = FALSE)

## ----echo = FALSE--------------------------------------------------------
Small[,1:7]

## ----eval = FALSE--------------------------------------------------------
## Small %>% sample_n(size = 3, replace = TRUE)

## ----echo = FALSE--------------------------------------------------------
Small[, 1:7] %>% sample_n(size = 3, replace = TRUE)

## ----eval = FALSE--------------------------------------------------------
## Small %>% sample_n(size = 3, replace = TRUE)

## ----echo = FALSE--------------------------------------------------------
set.seed(512)
Small[, 1:7] %>% sample_n(size = 3, replace = TRUE)

## ------------------------------------------------------------------------
n <- 200
Orig_sample <- SF %>% sample_n(size = n, replace = FALSE)

## ------------------------------------------------------------------------
mean( ~ arr_delay, 
      data = sample_n(Orig_sample, size = n, replace = TRUE))

## ------------------------------------------------------------------------
Bootstrap_trials <- do(500) * mean( ~ arr_delay, 
  data = sample_n(Orig_sample, size = n, replace = TRUE))
favstats( ~ mean, data = Bootstrap_trials)

## ------------------------------------------------------------------------
Trials_200 <- do(500) *
  mean( ~ arr_delay, data = sample_n(SF, size = n, replace = FALSE))
favstats( ~ mean, data = Trials_200)

## ------------------------------------------------------------------------
qdata( ~ arr_delay, p = 0.98, data = Orig_sample)

## ----echo = FALSE--------------------------------------------------------
set.seed(101)

## ------------------------------------------------------------------------
Bootstrap_trials <- do(500) *
  qdata( ~ arr_delay, p = 0.98, 
         data = sample_n(Orig_sample, size = n, replace = TRUE))
favstats( ~ quantile, data = Bootstrap_trials)

## ----echo = TRUE---------------------------------------------------------
set.seed(1001)
Bigger_sample <- SF %>% 
  sample_n(size = 10000, replace = FALSE)
Bootstrap_trials <- do(500) *
  qdata( ~ arr_delay, p = 0.90, 
         data = sample_n(Bigger_sample, size = 10000, replace = TRUE))
favstats( ~ quantile, data = Bootstrap_trials)

## ------------------------------------------------------------------------
SF %>%
  filter(arr_delay >= 420) %>% 
  select(month, day, dep_delay, arr_delay, carrier)

## ----allairport2,echo=TRUE, eval=TRUE------------------------------------
SF %>% filter(arr_delay < 420) %>%
  ggplot(aes(arr_delay)) + geom_histogram(binwidth = 15)

## ------------------------------------------------------------------------
SF %>% 
  mutate(long_delay = arr_delay > 60) %>%
  tally(~ long_delay | month, data = .)

## ------------------------------------------------------------------------
SF %>% 
  mutate(long_delay = arr_delay > 60) %>%
    tally(~ long_delay | carrier, data = .)

## ------------------------------------------------------------------------
tally( ~ hour, data = SF)

## ----schedhour,echo=TRUE, eval=TRUE--------------------------------------
SF %>%
  ggplot(aes(x = hour, y = arr_delay)) +
  geom_boxplot(alpha = 0.1, aes(group = hour)) + geom_smooth(method = "lm") + 
  xlab("Scheduled hour of departure") + ylab("Arrival delay (minutes)") + 
  coord_cartesian(ylim = c(-30, 120)) 

## ------------------------------------------------------------------------
mod1 <- lm(arr_delay ~ hour, data = SF)
msummary(mod1)

## ----message=FALSE-------------------------------------------------------
library(lubridate)
SF <- SF %>% 
  mutate(day = ymd(paste0(year, "-", month, "-", day)), 
         dow = as.character(wday(day, label = TRUE)),
         season = ifelse(month %in% 6:7, "summer", "other month"))

## ------------------------------------------------------------------------
mod2 <- lm(arr_delay ~ hour + origin + carrier + season + dow, data = SF)
msummary(mod2)

## ----sat1,fig.keep="last"------------------------------------------------
library(mdsr)
SAT_2010 <- mutate(SAT_2010, Salary = salary/1000)
SAT_plot <- ggplot(data = SAT_2010, aes(x = Salary, y = total)) + 
  geom_point() + geom_smooth(method = "lm") + 
  ylab("Average total score on the SAT") + 
  xlab("Average teacher salary (thousands of USD)")
SAT_plot

## ------------------------------------------------------------------------
SAT_mod1 <- lm(total ~ Salary, data = SAT_2010)
msummary(SAT_mod1)

## ------------------------------------------------------------------------
favstats(~ sat_pct, data = SAT_2010)
SAT_2010 <- SAT_2010 %>%
  mutate(SAT_grp = ifelse(sat_pct <= 27, "Low", "High"))
tally(~ SAT_grp, data = SAT_2010)

## ----sat2,fig.keep="last"------------------------------------------------
SAT_plot %+% SAT_2010 + aes(color = SAT_grp)

## ------------------------------------------------------------------------
coef(lm(total ~ Salary, data = filter(SAT_2010, SAT_grp == "Low")))
coef(lm(total ~ Salary, data = filter(SAT_2010, SAT_grp == "High")))

## ------------------------------------------------------------------------
SAT_mod2 <- lm(total ~ Salary + sat_pct, data = SAT_2010)
msummary(SAT_mod2)

## ------------------------------------------------------------------------
1 - (1-0.05)^5

## ------------------------------------------------------------------------
1 - (1-.01)^5

