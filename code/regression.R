## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/regression-'
options(digits=4)

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----message=FALSE-------------------------------------------------------
glimpse(RailTrail)

## ------------------------------------------------------------------------
mod <- lm(volume ~ hightemp, data = RailTrail)
coef(mod)

## ----railtrail-----------------------------------------------------------
plotModel(mod, system = "ggplot2")

## ----regplot1, message=FALSE, echo=FALSE, fig.cap="At left, the model based on the overall average high temperature. At right, the simple linear regression model."----
library(broom)
mod_avg <- RailTrail %>%
  lm(volume ~ 1, data = .) %>%
  augment(RailTrail)
mod_temp <- RailTrail %>%
  lm(volume ~ hightemp, data = .) %>%
  augment(RailTrail)
mod_data <- bind_rows(mod_avg, mod_temp) %>%
  mutate(model = rep(c("null", "slr"), each = nrow(RailTrail)))
ggplot(data = mod_data, aes(x = hightemp, y = volume)) + 
  geom_smooth(data = filter(mod_data, model == "null"), 
              method = "lm", se = 0, formula = y ~ 1, 
              color = "dodgerblue", size = 0.5) + 
  geom_smooth(data = filter(mod_data, model == "slr"),
              method = "lm", se = 0, formula = y ~ x, 
              color = "dodgerblue", size = 0.5) + 
  geom_segment(aes(xend = hightemp, yend = .fitted), 
               arrow = arrow(length = unit(0.1,"cm")), 
               size = 0.5, color = "darkgray") + 
  geom_point(color = "dodgerblue") + 
  facet_wrap(~model)

## ----povplot2, fig.show='hide'-------------------------------------------
n <- nrow(RailTrail)
SST <- var(~volume, data = RailTrail) * (n - 1)
SSE <- var(residuals(mod)) * (n - 1)
1 - SSE / SST
rsquared(mod)

## ----include=FALSE, eval=FALSE,message=FALSE-----------------------------
## rsquared(lm(volume ~ 1, data = RailTrail))

## ----xycoef,echo=TRUE----------------------------------------------------
coef(lm(volume ~ weekday, data = RailTrail))

## ------------------------------------------------------------------------
mean(volume ~ weekday, data = RailTrail)
diff(mean(volume ~ weekday, data = RailTrail))

## ----echo=TRUE,message=FALSE---------------------------------------------
RailTrail <- RailTrail %>%
  mutate(day = ifelse(weekday == 1, "weekday", "weekend/holiday"))

## ----xycoef2,echo=TRUE---------------------------------------------------
coef(lm(volume ~ day, data = RailTrail))

## ------------------------------------------------------------------------
mod_parallel <- lm(volume ~ hightemp + weekday, data = RailTrail)
coef(mod_parallel)
rsquared(mod_parallel)

## ----parallel-slopes, fig.cap="Visualization of parallel slopes model for the rail trail data."----
plotModel(mod_parallel, system = "ggplot2")

## ----message=FALSE, eval=TRUE--------------------------------------------
mod_planes <- lm(volume ~ hightemp + precip, data = RailTrail)
coef(mod_planes)

## ------------------------------------------------------------------------
mod_p_planes <- lm(volume ~ hightemp + precip + weekday, data = RailTrail)
coef(mod_p_planes)

## ------------------------------------------------------------------------
mod_interact <- lm(volume ~ hightemp + weekday + hightemp * weekday, 
  data = RailTrail)
coef(mod_interact)
rsquared(mod_interact)

## ----interact, fig.cap="Visualization of interaction model for the rail trail data. "----
plotModel(mod_interact, system = "ggplot2")

## ----echo=FALSE----------------------------------------------------------
set.seed(1066)

## ----ageheightmod, warning=FALSE,fig.cap="Scatterplot of height as a function of age with superimposed linear model (blue) and smoother (green). "----
library(NHANES)
NHANES %>%
  sample(300) %>%
  filter(Gender == "female") %>%
ggplot(aes(x = Age, y = Height)) + 
  geom_point() + 
  stat_smooth(method = lm, se = 0) + 
  stat_smooth(method = loess, se = 0, color = "green") + 
  xlab("Age (in years)") + ylab("Height (in cm)")

## ----railtrailsmooth, warning=FALSE,fig.cap="Scatterplot of volume as a function of high temperature  with superimposed linear and smooth models for the rail trail data."----
ggplot(data = RailTrail, aes(x = hightemp, y = volume)) + 
  geom_point() +
  stat_smooth(method = lm) + stat_smooth(method = loess, color = "green") + 
  ylab("Number of trail crossings") + xlab("High temperature (F)") 

## ------------------------------------------------------------------------
msummary(mod_p_planes)

## ------------------------------------------------------------------------
confint(mod_p_planes)

## ----plotmod3a, message=FALSE, results='hide', fig.cap="Assessing linearity using a scatterplot of residuals versus fitted (predicted) values."----
mplot(mod_p_planes, which = 1, system = "ggplot2")

## ----plotmod3b, message=FALSE, results='hide', fig.cap="Assessing normality assumption using a Q--Q plot."----
mplot(mod_p_planes, which = 2, system = "ggplot2")

## ----plotmod3c, message=FALSE, results='hide', fig.cap="Assessing equal variance using a scale--location plot."----
mplot(mod_p_planes, which = 3, system = "ggplot2")

## ----plotmod3d, message=FALSE, results='hide', fig.cap="Cook's distance for rail trail model."----
mplot(mod_p_planes, which = 4, system = "ggplot2")

## ------------------------------------------------------------------------
library(broom)
augment(mod_p_planes) %>%
  filter(.cooksd > 0.4)

## ----echo=FALSE----------------------------------------------------------
data(NHANES)

## ------------------------------------------------------------------------
NHANES <- NHANES %>%
  mutate(has_diabetes = as.numeric(Diabetes == "Yes"))
log_plot <- ggplot(data = NHANES, aes(x = Age, y = has_diabetes)) + 
  geom_jitter(alpha = 0.1, height = 0.05) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ylab("Diabetes status") 

## ----diabeteslogreg, warning=FALSE---------------------------------------
log_plot + xlab("Age (in years)")

## ----diabeteslogreg2, warning=FALSE--------------------------------------
log_plot + aes(x = BMI) + xlab("BMI (body mass index)")

## ------------------------------------------------------------------------
logreg <- glm(has_diabetes ~ BMI + Age, family = "binomial", data = NHANES)
msummary(logreg)

## ------------------------------------------------------------------------
ages <- range(~Age, data = NHANES)
bmis <- range(~BMI, data = NHANES, na.rm = TRUE)
res <- 100
fake_grid <- expand.grid(
  Age = seq(from = ages[1], to = ages[2], length.out = res),
  BMI = seq(from = bmis[1], to = bmis[2], length.out = res)
)
y_hats <- fake_grid %>%
  mutate(y_hat = predict(logreg, newdata = ., type = "response"))

## ----plotFundiabetes, warning=FALSE, fig.height=10, fig.cap="Predicted probabilities for diabetes as a function of BMI and age."----
ggplot(data = NHANES, aes(x = Age, y = BMI)) +
  geom_tile(data = y_hats, aes(fill = y_hat), color = NA) +
  geom_count(aes(color = as.factor(has_diabetes)), alpha = 0.4) + 
  scale_fill_gradient(low = "white", high = "dodgerblue") +
  scale_color_manual("Diabetes", values = c("gray", "gold")) +
  scale_size(range = c(0, 2)) 

