## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/algo-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----eval=FALSE----------------------------------------------------------
## new_function <- function(argument1, argument2) {
##   R expression
##   another R expression
## }

## ----cdistoutput---------------------------------------------------------
cdist("t", 0.95, df = 50)

## ----xqt, echo=TRUE, fig.cap="Illustration of the location of the critical value for a 95\\% confidence interval for a mean. The critical value of 2.01 corresponds to the location in the t-distribution with 50 degrees of freedom, for which 2.5\\% of the distribution lies above it."----
xqt(c(0.025, 0.975), df = 50)

## ------------------------------------------------------------------------
# calculate a t confidence interval for a mean
ci_calc <- function(x, alpha = 0.95) {
   samp_size <- length(x)
   t_star <- qt(1 - ((1 - alpha)/2), df = samp_size - 1)
   my_mean <- mean(x)
   my_sd <- sd(x)
   se <- my_sd/sqrt(samp_size)
   me <- t_star * se
   return(list(ci_vals = c(my_mean - me, my_mean + me), 
               alpha = alpha))
}

## ----echo=FALSE----------------------------------------------------------
set.seed(1999)

## ------------------------------------------------------------------------
x1 <- rnorm(100, mean = 0, sd = 1)
ci_calc(x1)

## ----eval=FALSE----------------------------------------------------------
## ?ci_calc # won't work because we haven't written any documentation
## args(ci_calc)
## ci_calc

## ----eval=FALSE----------------------------------------------------------
## ci_calc(rnorm(100), 0.9)

## ------------------------------------------------------------------------
ci_calc(x1, alpha = 0.90)

## ------------------------------------------------------------------------
ci_calc(alpha = 0.90, x = x1)

## ------------------------------------------------------------------------
rnorm(100, mean = 0, sd = 1) %>%
  ci_calc(alpha = 0.9)

## ------------------------------------------------------------------------
runave <- function(n, gendist, ...) {
   x <- gendist(n, ...)
   avex <- numeric(n)
   for (k in 1:n) {
      avex[k] <- mean(x[1:k])
   }
   return(data.frame(x, avex, n = 1:length(avex)))
}

## ----cauchyt,echo=TRUE, fig.keep="last", eval=TRUE-----------------------
plotDist("t", params = list(df = 4), xlim = c(-5, 5), lty = 2, lwd = 3)
plotDist("cauchy", xlim = c(-10, 10), lwd = 3, add = TRUE)

## ------------------------------------------------------------------------
nvals <- 1000
set.seed(1984)
sims <- bind_rows(
  runave(nvals, rt, 4), 
  runave(nvals, rcauchy)) %>% 
  mutate(dist = rep(c("t4", "cauchy"), each = nvals))

## ----runavg1,echo=TRUE, fig.keep="last", eval=TRUE, warning=FALSE--------
ggplot(data = sims, aes(x = n, y = avex, color = dist)) + 
  geom_hline(yintercept = 0, color = "black", linetype = 2) + 
  geom_line() + geom_point() +
  ylab("mean") + xlab("sample size") + xlim(c(0, 600))

## ------------------------------------------------------------------------
library(NHANES)
NHANESsubset <- NHANES %>%
  select(ID, SurveyYr, Gender, Age, AgeMonths, Race1, Poverty)
NHANESsubset

## ------------------------------------------------------------------------
"ID"   # string variable
ID     # generates an error
NHANESsubset$ID %>% summary()    # access within a data frame

## ----eval=TRUE-----------------------------------------------------------
NHANESsubset %>% mutate(SurveyYr = as.character(SurveyYr)) %>%
  select(ID, SurveyYr) %>%
  glimpse()

## ----warning=FALSE-------------------------------------------------------
varname <- "SurveyYr"
mutate(NHANESsubset, varname = as.character(varname)) %>%
  select(ID, SurveyYr, varname) %>%
  glimpse()

## ------------------------------------------------------------------------
select(NHANESsubset, ID) %>% summary()
select_(NHANESsubset, ~ID) %>% summary()

## ------------------------------------------------------------------------
factor_to_char <- function(data, varname) {
   data %>%
    mutate(var_nse_wrong_values = varname,
           var_nse_hard_coded = as.character(SurveyYr)) %>%
    mutate_(var_se_wrong_type = varname,
            var_se_correct = ~as.character(var_se_wrong_type))
}
factor_to_char(NHANESsubset, "SurveyYr") %>%
  select(SurveyYr, contains("var")) %>%
  glimpse()

## ------------------------------------------------------------------------
is_factor <- sapply(NHANESsubset, class) == "factor"
sum(is_factor)

## ------------------------------------------------------------------------
var_names <- names(NHANESsubset[is_factor])

## ------------------------------------------------------------------------
NHANESsubset %>%
  mutate_at(.funs = as.character, .cols = var_names)

## ------------------------------------------------------------------------
convert_types <- function(data, type, convert_fun) {
  col_idx <- sapply(data, class) == type
  mutate_at(data, .funs = convert_fun, .cols = names(data[col_idx]))
}
convert_types(NHANESsubset, type = "integer", convert_fun = function(x) x^2)

## ------------------------------------------------------------------------
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

