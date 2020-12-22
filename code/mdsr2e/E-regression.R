## ----message=FALSE------------------------------------------------------------
library(tidyverse)
library(mdsr)
library(mosaic)
glimpse(RailTrail)


## -----------------------------------------------------------------------------
mod <- lm(volume ~ hightemp, data = RailTrail)
library(broom)
tidy(mod)


## ----railtrail, message = FALSE, fig.cap="Scatterplot of number of trail crossings as a function of highest daily temperature (in degrees Fahrenheit)."----
ggplot(RailTrail, aes(x = hightemp, y = volume)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


## -----------------------------------------------------------------------------
library(broom)
mod_avg <- RailTrail %>%
  lm(volume ~ 1, data = .) %>%
  augment(RailTrail)
mod_temp <- RailTrail %>%
  lm(volume ~ hightemp, data = .) %>%
  augment(RailTrail)
mod_data <- bind_rows(mod_avg, mod_temp) %>%
  mutate(model = rep(c("null", "slr"), each = nrow(RailTrail)))


## ----regplot1, message=FALSE, echo=TRUE, fig.cap="At left, the model based on the overall average high temperature. At right, the simple linear regression model."----
ggplot(data = mod_data, aes(x = hightemp, y = volume)) + 
  geom_smooth(
    data = filter(mod_data, model == "null"), 
    method = "lm", se = FALSE, formula = y ~ 1, 
    color = "dodgerblue", size = 0.5
  ) + 
  geom_smooth(
    data = filter(mod_data, model == "slr"),
    method = "lm", se = FALSE, formula = y ~ x, 
    color = "dodgerblue", size = 0.5
  ) + 
  geom_segment(
    aes(xend = hightemp, yend = .fitted), 
    arrow = arrow(length = unit(0.1, "cm")), 
    size = 0.5, color = "darkgray"
  ) + 
  geom_point(color = "dodgerblue") + 
  facet_wrap(~model)


## ----povplot2, fig.show='hide'------------------------------------------------
n <- nrow(RailTrail)
SST <- var(pull(RailTrail, volume)) * (n - 1)
SSE <- var(residuals(mod)) * (n - 1)
1 - SSE / SST
glance(mod)


## ----include=FALSE, eval=FALSE,message=FALSE----------------------------------
## glance(lm(volume ~ 1, data = RailTrail))


## ----xycoef,echo=TRUE---------------------------------------------------------
coef(lm(volume ~ weekday, data = RailTrail))


## -----------------------------------------------------------------------------
RailTrail %>%
  group_by(weekday) %>%
  summarize(mean_volume = mean(volume))


## ----echo=TRUE,message=FALSE--------------------------------------------------
RailTrail <- RailTrail %>%
  mutate(day = ifelse(weekday == 1, "weekday", "weekend/holiday"))


## ----xycoef2,echo=TRUE--------------------------------------------------------
coef(lm(volume ~ day, data = RailTrail))


## -----------------------------------------------------------------------------
mod_parallel <- lm(volume ~ hightemp + weekday, data = RailTrail)
tidy(mod_parallel)
glance(mod_parallel)


## ----parallel-slopes, fig.cap="Visualization of parallel slopes model for the rail trail data."----
mod_parallel %>%
  augment() %>%
  ggplot(aes(x = hightemp, y = volume, color = weekday)) +
  geom_point() + 
  geom_line(aes(y = .fitted)) + 
  labs(color = "Is it a\nweekday?")


## ----message=FALSE, eval=TRUE-------------------------------------------------
mod_plane <- lm(volume ~ hightemp + precip, data = RailTrail)
tidy(mod_plane)


## -----------------------------------------------------------------------------
mod_p_planes <- lm(volume ~ hightemp + precip + weekday, data = RailTrail)
tidy(mod_p_planes)


## -----------------------------------------------------------------------------
mod_interact <- lm(volume ~ hightemp + weekday + hightemp * weekday, 
  data = RailTrail)
tidy(mod_interact)
glance(mod_interact)


## ----interact, fig.cap="Visualization of interaction model for the rail trail data. "----
mod_interact %>%
  augment() %>%
  ggplot(aes(x = hightemp, y = volume, color = weekday)) +
  geom_point() + 
  geom_line(aes(y = .fitted)) + 
  labs(color = "Is it a\nweekday?")


## ----echo=FALSE---------------------------------------------------------------
set.seed(1066)


## ----ageheightmod, warning=FALSE,fig.cap="Scatterplot of height as a function of age with superimposed linear model (blue) and smoother (green). ", message=FALSE----
library(NHANES)
NHANES %>%
  sample(300) %>%
  filter(Gender == "female") %>%
ggplot(aes(x = Age, y = Height)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) + 
  geom_smooth(method = loess, se = FALSE, color = "green") + 
  xlab("Age (in years)") + 
  ylab("Height (in cm)")


## ----railtrailsmooth, warning=FALSE, fig.cap="Scatterplot of volume as a function of high temperature  with superimposed linear and smooth models for the rail trail data.", message=FALSE----
ggplot(data = RailTrail, aes(x = hightemp, y = volume)) + 
  geom_point() +
  geom_smooth(method = lm) +
  geom_smooth(method = loess, color = "green") + 
  ylab("Number of trail crossings") + 
  xlab("High temperature (F)") 


## -----------------------------------------------------------------------------
tidy(mod_p_planes)
glance(mod_p_planes)


## -----------------------------------------------------------------------------
confint(mod_p_planes)


## ----plotmod3a, message=FALSE, results='hide', fig.cap="Assessing linearity using a scatterplot of residuals versus fitted (predicted) values."----
mplot(mod_p_planes, which = 1, system = "ggplot2")


## ----plotmod3b, message=FALSE, results='hide', fig.cap="Assessing normality assumption using a Q--Q plot."----
mplot(mod_p_planes, which = 2, system = "ggplot2")


## ----plotmod3c, message=FALSE, results='hide', fig.cap="Assessing equal variance using a scale--location plot."----
mplot(mod_p_planes, which = 3, system = "ggplot2")


## ----plotmod3d, message=FALSE, results='hide', fig.cap="Cook's distance for rail trail model."----
mplot(mod_p_planes, which = 4, system = "ggplot2")


## -----------------------------------------------------------------------------
library(broom)
augment(mod_p_planes) %>%
  mutate(row_num = row_number()) %>%
  select(-.std.resid, -.sigma) %>%
  filter(.cooksd > 0.4)


## ----echo=FALSE---------------------------------------------------------------
data(NHANES)


## ----message = FALSE----------------------------------------------------------
NHANES <- NHANES %>%
  mutate(has_diabetes = as.numeric(Diabetes == "Yes"))


## ----diabeteslogreg, message = FALSE, warning=FALSE, fig.cap="Scatterplot of diabetes as a function of age with superimposed smoother."----
log_plot <- ggplot(data = NHANES, aes(x = Age, y = has_diabetes)) + 
  geom_jitter(alpha = 0.1, height = 0.05) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ylab("Diabetes status") +
  xlab("Age (in years)")
log_plot


## ----diabeteslogreg2, warning=FALSE, fig.cap="Scatterplot of diabetes as a function of BMI with superimposed smoother.", message = FALSE----
log_plot + aes(x = BMI) + xlab("BMI (body mass index)")


## -----------------------------------------------------------------------------
logreg <- glm(has_diabetes ~ BMI + Age, family = "binomial", data = NHANES)
tidy(logreg)


## ----message = FALSE----------------------------------------------------------
library(modelr)
fake_grid <- data_grid(
  NHANES, 
  Age = seq_range(Age, 100),
  BMI = seq_range(BMI, 100)
)
y_hats <- fake_grid %>%
  mutate(y_hat = predict(logreg, newdata = ., type = "response"))
head(y_hats, 1)


## -----------------------------------------------------------------------------
linear_component <- c(1, 12.9, 0) %*% coef(logreg)
exp(linear_component) / (1 + exp(linear_component))


## -----------------------------------------------------------------------------
linear_component <- c(1, 25, 60) %*% coef(logreg)
exp(linear_component) / (1 + exp(linear_component))


## ----plotFundiabetes, warning=FALSE, fig.asp=1, fig.cap="Predicted probabilities for diabetes as a function of BMI and age."----
ggplot(data = NHANES, aes(x = Age, y = BMI)) +
  geom_tile(data = y_hats, aes(fill = y_hat), color = NA) +
  geom_count(aes(color = factor(has_diabetes)), alpha = 0.8) + 
  scale_fill_gradient(low = "white", high = "red") +
  scale_color_manual("Diabetes", values = c("gold", "black")) +
  scale_size(range = c(0, 2)) +
  labs(fill = "Predicted\nprobability")

