## ----eval=FALSE---------------------------------------------------------------
## library(tidyverse)
## library(mdsr)
## new_function <- function(argument1, argument2) {
##   R expression
##   another R expression
## }


## ----cdistoutput, message=FALSE, fig.show='hide'------------------------------
library(mosaic)
mosaic::cdist(dist = "t", p = 0.95, df = 50)


## ----xqt, echo=TRUE, fig.cap="Illustration of the location of the critical value for a 95\\% confidence interval for a mean. The critical value of 2.01 corresponds to the location in the t-distribution with 50 degrees of freedom, for which 2.5\\% of the distribution lies above it."----
mosaic::xqt(p = c(0.025, 0.975), df = 50)


## -----------------------------------------------------------------------------
# calculate a t confidence interval for a mean
ci_calc <- function(x, alpha = 0.95) {
  samp_size <- length(x)
  t_star <- qt(1 - ((1 - alpha)/2), df = samp_size - 1)
  my_mean <- mean(x)
  my_sd <- sd(x)
  se <- my_sd/sqrt(samp_size)
  me <- t_star * se
  return(
    list(
      ci_vals = c(my_mean - me, my_mean + me), 
      alpha = alpha
    )
  )
}


## ----echo=FALSE---------------------------------------------------------------
set.seed(1999)


## -----------------------------------------------------------------------------
x1 <- rnorm(100, mean = 0, sd = 1)
ci_calc(x1)


## ----eval=FALSE---------------------------------------------------------------
## ?ci_calc # won't work because we haven't written any documentation
## args(ci_calc)
## ci_calc


## ----eval=FALSE---------------------------------------------------------------
## ci_calc(rnorm(100), 0.9)


## -----------------------------------------------------------------------------
ci_calc(x1, alpha = 0.90)


## -----------------------------------------------------------------------------
ci_calc(alpha = 0.90, x = x1)


## -----------------------------------------------------------------------------
rnorm(100, mean = 0, sd = 1) %>%
  ci_calc(alpha = 0.9)


## -----------------------------------------------------------------------------
runave <- function(n, gendist, ...) {
  x <- gendist(n, ...)
  avex <- numeric(n)
  for (k in 1:n) {
    avex[k] <- mean(x[1:k])
  }
  return(tibble(x, avex, n = 1:length(avex)))
}


## ----cauchyt, echo=TRUE, fig.keep="last", eval=TRUE, fig.cap="Cauchy distribution (solid line) and t-distribution with 4 degrees of freedom (dashed line)."----
mosaic::plotDist(
  "t", 
  params = list(df = 4), 
  xlim = c(-5, 5), 
  lty = 2, 
  lwd = 3
)
mosaic::plotDist("cauchy", xlim = c(-10, 10), lwd = 3, add = TRUE)


## -----------------------------------------------------------------------------
nvals <- 1000
set.seed(1984)
sims <- bind_rows(
  runave(nvals, rt, 4), 
  runave(nvals, rcauchy)
) %>% 
  mutate(dist = rep(c("t4", "cauchy"), each = nvals))


## ----t4, echo=TRUE, fig.keep="last", eval=TRUE, warning=FALSE, fig.cap="Running average for t-distribution with 4 degrees of freedom and a Cauchy random variable (equivalent to a t-distribution with 1 degree of freedom). Note that while the former converges, the latter does not."----
ggplot(
  data = sims, 
  aes(x = n, y = avex, color = dist)) + 
  geom_hline(yintercept = 0, color = "black", linetype = 2) + 
  geom_line() + 
  geom_point() + 
  labs(color = "Distribution", y = "running mean", x = "sample size") +
  xlim(c(0, 600)
)


## -----------------------------------------------------------------------------
library(NHANES)
nhanes_small <- NHANES %>%
  select(ID, SurveyYr, Gender, Age, AgeMonths, Race1, Poverty)
glimpse(nhanes_small)


## ---- error=TRUE--------------------------------------------------------------
"ID"   # string variable
ID     # generates an error
nhanes_small %>%
  pull(ID) %>%    # access within a data frame
  summary()


## ----eval=TRUE----------------------------------------------------------------
nhanes_small %>%
  mutate(SurveyYr = as.character(SurveyYr)) %>%
  select(ID, SurveyYr) %>%
  glimpse()


## ----warning=FALSE------------------------------------------------------------
varname <- "SurveyYr"
nhanes_small %>%
  mutate(varname = as.character(varname)) %>%
  select(ID, SurveyYr, varname) %>%
  glimpse()


## -----------------------------------------------------------------------------
nhanes_small %>%
  mutate(across(where(is.factor), as.character))


## -----------------------------------------------------------------------------
var_to_char_base <- function(data, varname) {
   data[[varname]] <- as.character(data[[varname]])
   data
}
var_to_char_base(nhanes_small, "SurveyYr")


## ---- error=TRUE, message=FALSE-----------------------------------------------
library(assertthat)
# calculate a t confidence interval for a mean
ci_calc <- function(x, alpha = 0.95) {
   if (length(x) < 2) { 
     stop("Need to provide a vector of length at least 2.\n") 
   }
   if (alpha < 0 | alpha > 1) { 
     stop("alpha must be between 0 and 1.\n") 
   }
   assert_that(is.numeric(x))
   samp_size <- length(x)
   t_star <- qt(1 - ((1 - alpha)/2), df = samp_size - 1)
   my_mean <- mean(x)
   my_sd <- sd(x)
   se <- my_sd / sqrt(samp_size)
   me <- t_star * se
   return(list(ci_vals = c(my_mean - me, my_mean + me),
               alpha = alpha))
}
ci_calc(1)   # will generate error
ci_calc(1:3, alpha = -1)   # will generate error
ci_calc(c("hello", "goodbye"))   # will generate error

