## ----eval=FALSE---------------------------------------------------------------
## diabetic ~ age + sex + weight + height


## ----message=FALSE------------------------------------------------------------
library(tidyverse)
library(mdsr)
url <-
"http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
census <- read_csv(
  url,
  col_names = c(
    "age", "workclass", "fnlwgt", "education", 
    "education_1", "marital_status", "occupation", "relationship", 
    "race", "sex", "capital_gain", "capital_loss", "hours_per_week", 
    "native_country", "income"
  )
) %>%
  mutate(income = factor(income))
glimpse(census)


## ----split, message=FALSE-----------------------------------------------------
library(tidymodels)
set.seed(364)
n <- nrow(census)
census_parts <- census %>%
  initial_split(prop = 0.8)

train <- census_parts %>%
  training()

test <- census_parts %>%
  testing()

list(train, test) %>%
  map_int(nrow)


## ----pi_bar-------------------------------------------------------------------
pi_bar <- train %>%
  count(income) %>%
  mutate(pct = n / sum(n)) %>%
  filter(income == ">50K") %>%
  pull(pct)
pi_bar


## ----tally-pi_bar,message=FALSE-----------------------------------------------
train %>%
  count(income) %>%
  mutate(pct = n / sum(n))


## -----------------------------------------------------------------------------
mod_null <- logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(income ~ 1, data = train)


## -----------------------------------------------------------------------------
library(yardstick)
pred <- train %>%
  select(income, capital_gain) %>%
  bind_cols(
    predict(mod_null, new_data = train, type = "class")
  ) %>%
  rename(income_null = .pred_class)
accuracy(pred, income, income_null)


## ----message=FALSE------------------------------------------------------------
confusion_null <- pred %>%
  conf_mat(truth = income, estimate = income_null)
confusion_null


## ---- warning=FALSE-----------------------------------------------------------
mod_log_1 <- logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(income ~ capital_gain, data = train)


## ----log-cap-gains, message = FALSE, warning=FALSE, fig.cap="Simple logistic regression model for high-earner status based on capital gains tax paid. "----
train_plus <- train %>%
  mutate(high_earner = as.integer(income == ">50K"))

ggplot(train_plus, aes(x = capital_gain, y = high_earner)) + 
  geom_count(
    position = position_jitter(width = 0, height = 0.05), 
    alpha = 0.5
  ) + 
  geom_smooth(
    method = "glm", method.args = list(family = "binomial"), 
    color = "dodgerblue", lty = 2, se = FALSE
  ) + 
  geom_hline(aes(yintercept = 0.5), linetype = 3) + 
  scale_x_log10(labels = scales::dollar)


## -----------------------------------------------------------------------------
pred <- pred %>%
  bind_cols(
    predict(mod_log_1, new_data = train, type = "class")
  ) %>%
  rename(income_log_1 = .pred_class)

confusion_log_1 <- pred %>%
  conf_mat(truth = income, estimate = income_log_1)

confusion_log_1
accuracy(pred, income, income_log_1)


## ----autoplot-null-confusion, fig.show="hold", out.width="50%", fig.cap="Visual summary of the predictive accuracy of the null model (left) versus the logistic regression model with one explanatory variable (right). The null model never predicts a positive."----
autoplot(confusion_null) +
  geom_label(
    aes(
      x = (xmax + xmin) / 2, 
      y = (ymax + ymin) / 2, 
      label = c("TN", "FP", "FN", "TP")
    )
  )
autoplot(confusion_log_1) +
  geom_label(
    aes(
      x = (xmax + xmin) / 2, 
      y = (ymax + ymin) / 2, 
      label = c("TN", "FP", "FN", "TP")
    )
  )


## -----------------------------------------------------------------------------
broom::tidy(mod_log_1)


## -----------------------------------------------------------------------------
income_probs <- pred %>%
  select(income, income_log_1, capital_gain) %>%
  bind_cols(
    predict(mod_log_1, new_data = train, type = "prob")
  )

income_probs %>%
  rename(rich_prob = `.pred_>50K`) %>%
  distinct() %>%
  filter(abs(rich_prob - 0.5) < 0.02) %>%
  arrange(desc(rich_prob))


## ---- warning=FALSE-----------------------------------------------------------
mod_log_all <- logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(
    income ~ age + workclass + education + marital_status + 
      occupation + relationship + race + sex + 
      capital_gain + capital_loss + hours_per_week, 
    data = train
  )

pred <- pred %>%
  bind_cols(
    predict(mod_log_all, new_data = train, type = "class")
  ) %>%
  rename(income_log_all = .pred_class)

pred %>%
  conf_mat(truth = income, estimate = income_log_all)
accuracy(pred, income, income_log_all)


## ----income_probs, warning=FALSE----------------------------------------------
head(income_probs)


## -----------------------------------------------------------------------------
income_probs %>%
  group_by(rich = `.pred_>50K` > 0.5) %>%
  count() %>%
  mutate(pct = n / nrow(income_probs))


## -----------------------------------------------------------------------------
income_probs %>%
  group_by(rich = `.pred_>50K` > pi_bar) %>%
  count() %>%
  mutate(pct = n / nrow(income_probs))


## ----roc-compute, message=FALSE, warning=FALSE, eval=TRUE---------------------
roc <- pred %>%
  mutate(estimate = pull(income_probs, `.pred_>50K`)) %>%
  roc_curve(truth = income, estimate, event_level = "second") %>%
  autoplot()


## ----roc-log, fig.cap="ROC curve for the simple logistic regression model.", message=FALSE, eval=TRUE----
metrics <- pred %>%
  conf_mat(income, income_log_1) %>%
  summary(event_level = "second")
metrics

roc_mod <- metrics %>%
  filter(.metric %in% c("sens", "spec")) %>%
  pivot_wider(-.estimator, names_from = .metric, values_from = .estimate)

roc + 
  geom_point(
    data = roc_mod, size = 3, 
    aes(x = 1 - spec, y = sens)
  )


## ---- skimr_include_summary=FALSE, render=knitr::normal_print-----------------
train %>%
  skim(capital_gain)
test %>%
  skim(capital_gain)


## ----mod_null-----------------------------------------------------------------
mods <- tibble(
  type = c("null", "log_1", "log_all"),
  mod = list(mod_null, mod_log_1, mod_log_all)
)


## ----make-predictons, warning=FALSE-------------------------------------------
mods <- mods %>%
  mutate(
    y_train = list(pull(train, income)),
    y_test = list(pull(test, income)),
    y_hat_train = map(
      mod, 
      ~pull(predict(.x, new_data = train, type = "class"), .pred_class)
    ),
    y_hat_test = map(
      mod, 
      ~pull(predict(.x, new_data = test, type = "class"), .pred_class)
    )
  )
mods


## ----predictions_summary------------------------------------------------------
mods <- mods %>%
  mutate(
    accuracy_train = map2_dbl(y_train, y_hat_train, accuracy_vec),
    accuracy_test = map2_dbl(y_test, y_hat_test, accuracy_vec),
    sens_test = 
      map2_dbl(y_test, y_hat_test, sens_vec, event_level = "second"),
    spec_test = 
      map2_dbl(y_test, y_hat_test, spec_vec, event_level = "second")
  )


## ----accuracytest, echo=FALSE, message=FALSE----------------------------------
mods %>%
  select(-mod, -matches("^y")) %>%
  arrange(desc(accuracy_test)) %>%
  mdsr_table(caption = "Model accuracy measures for the income model.")


## ----roc2, warning=FALSE------------------------------------------------------
mods <- mods %>%
  mutate(
    y_hat_prob_test = map(
      mod, 
      ~pull(predict(.x, new_data = test, type = "prob"), `.pred_>50K`)
    ),
    type = fct_reorder(type, sens_test, .desc = TRUE)
  )


## ----roc-log-compare, fig.cap="Comparison of ROC curves across three logistic regression models on the Census testing data. The null model has a true positive rate of zero and lies along the diagonal. The full model is the best overall performer, as its curve lies furthest from the diagonal."----
mods %>%
  select(type, y_test, y_hat_prob_test) %>%
  unnest(cols = c(y_test, y_hat_prob_test)) %>%
  group_by(type) %>%
  roc_curve(truth = y_test, y_hat_prob_test, event_level = "second") %>%
  autoplot() + 
  geom_point(
    data = mods, 
    aes(x = 1 - spec_test, y = sens_test, color = type), 
    size = 3
  ) + 
  scale_color_brewer("Model", palette = "Set2")


## ----nhanes-people------------------------------------------------------------
library(NHANES)
people <- NHANES %>%
  select(Age, Gender, Diabetes, BMI, HHIncome, PhysActive) %>% 
  drop_na()
glimpse(people)
people %>%
  group_by(Diabetes) %>%
  count() %>%
  mutate(pct = n / nrow(people))


## ----fake-grid, message = FALSE-----------------------------------------------
library(modelr)
num_points <- 100
fake_grid <- data_grid(
  people, 
  Age = seq_range(Age, num_points),
  BMI = seq_range(BMI, num_points)
)


## ----pred-all, warning=FALSE--------------------------------------------------
dmod_null <- logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(Diabetes ~ 1, data = people)
dmod_log_1 <- logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(Diabetes ~ Age, data = people)
dmod_log_2 <- logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(Diabetes ~ BMI, data = people)
dmod_log_12 <- logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(Diabetes ~ Age + BMI, data = people)
bmi_mods <- tibble(
  type = factor(
    c("Null", "Logistic (Age)", "Logistic (BMI)", "Logistic (Age, BMI)")
  ),
  mod = list(dmod_null, dmod_log_1, dmod_log_2, dmod_log_12),
  y_hat = map(mod, predict, new_data = fake_grid, type = "prob")
)


## -----------------------------------------------------------------------------
bmi_mods <- bmi_mods %>%
  mutate(
    X = list(fake_grid),
    yX = map2(y_hat, X, bind_cols)
  )


## ----res-for-images,warning=FALSE---------------------------------------------
res <- bmi_mods %>%
  select(type, yX) %>%
  unnest(cols = yX)
res


## ----mod-log-compare, fig.asp = 1, fig.cap="Comparison of logistic regression models in the data space. Note the greater flexibility as more variables are introduced."----
ggplot(data = res, aes(x = Age, y = BMI)) +
  geom_tile(aes(fill = .pred_Yes), color = NA) + 
  geom_count(
    data = people, 
    aes(color = Diabetes), alpha = 0.4
  ) + 
  scale_fill_gradient("Prob of\nDiabetes", low = "white", high = "red") + 
  scale_color_manual(values = c("gold", "black")) + 
  scale_size(range = c(0, 2)) + 
  scale_x_continuous(expand = c(0.02, 0)) + 
  scale_y_continuous(expand = c(0.02, 0)) + 
  facet_wrap(~fct_rev(type))

