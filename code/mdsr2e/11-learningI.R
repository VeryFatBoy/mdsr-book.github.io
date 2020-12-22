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

library(tidymodels)
set.seed(364)
n <- nrow(census)
census_parts <- census %>%
  initial_split(prop = 0.8)
train <- census_parts %>% training()
test <- census_parts %>% testing()
pi_bar <- train %>%
  count(income) %>%
  mutate(pct = n / sum(n)) %>%
  filter(income == ">50K") %>%
  pull(pct)


## ----rpart1-------------------------------------------------------------------
mod_dtree <- decision_tree(mode = "classification") %>%
  set_engine("rpart") %>%
  fit(income ~ capital_gain, data = train)

split_val <- mod_dtree$fit$splits %>%
  as_tibble() %>%
  pull(index)


## ----include=FALSE, warning=FALSE---------------------------------------------
dtree_frame <- mod_dtree$fit$frame %>%
  select(var, n, dev, yval) %>%
  mutate(pct = n / nrow(train), right = (n - dev) / n)


## -----------------------------------------------------------------------------
mod_dtree


## ----census-rpart, fig.cap="(ref:census-rpart-cap)", warning=FALSE------------
train_plus <- train %>% 
  mutate(hi_cap_gains = capital_gain >= split_val)

ggplot(data = train_plus, aes(x = capital_gain, y = income)) + 
  geom_count(
    aes(color = hi_cap_gains), 
    position = position_jitter(width = 0, height = 0.1), 
    alpha = 0.5
  ) + 
  geom_vline(xintercept = split_val, color = "dodgerblue", lty = 2) + 
  scale_x_log10(labels = scales::dollar)


## ----formula------------------------------------------------------------------
form <- as.formula(
  "income ~ age + workclass + education + marital_status + 
  occupation + relationship + race + sex + 
  capital_gain + capital_loss + hours_per_week"
)

## -----------------------------------------------------------------------------
mod_tree <- decision_tree(mode = "classification") %>%
  set_engine("rpart") %>%
  fit(form, data = train)
mod_tree


## ----as-party-hack1, include=FALSE--------------------------------------------
# https://github.com/tidymodels/parsnip/issues/257
mod_tree$fit$call <- rlang::call2("rpart::rpart", data = expr(train))


## ----maptree, fig.asp = 1, fig.width=10, out.extra='angle=90', fig.cap="(ref:maptree-cap)", message=FALSE----
library(rpart)
library(partykit)
plot(as.party(mod_tree$fit))


## ----include=FALSE------------------------------------------------------------
options(digits = 5)
mod_tree_frame <- mod_tree$fit$frame %>%
  select(var, n, dev, yval) %>%
  mutate(pct = n / nrow(train), right = (n - dev) / n)


## ----census-rpart2, fig.cap="Graphical depiction of the full recursive partitioning decision tree classifier. On the left, those whose relationship status is neither 'Husband' nor 'Wife' are classified based on their capital gains paid. On the right, not only is the capital gains threshold different, but the decision is also predicated on whether the person has a college degree.", warning=FALSE----
train_plus <- train_plus %>%
  mutate(
    husband_or_wife = relationship %in% c("Husband", "Wife"), 
    college_degree = husband_or_wife & education %in% 
      c("Bachelors", "Doctorate", "Masters", "Prof-school")
  ) %>%
  bind_cols(
    predict(mod_tree, new_data = train, type = "class")
  ) %>%
  rename(income_dtree = .pred_class)


cg_splits <- tribble(
  ~husband_or_wife, ~vals,
  TRUE, 5095.5, 
  FALSE, 7073.5
)

ggplot(data = train_plus, aes(x = capital_gain, y = income)) + 
  geom_count(
    aes(color = income_dtree, shape = college_degree), 
    position = position_jitter(width = 0, height = 0.1), 
    alpha = 0.5
  ) + 
  facet_wrap(~ husband_or_wife) + 
  geom_vline(
    data = cg_splits, aes(xintercept = vals), 
    color = "dodgerblue", lty = 2
  ) + 
  scale_x_log10()


## -----------------------------------------------------------------------------
printcp(mod_tree$fit)


## ----message=FALSE------------------------------------------------------------
library(yardstick)
pred <- train %>%
  select(income) %>%
  bind_cols(
    predict(mod_tree, new_data = train, type = "class")
  ) %>%
  rename(income_dtree = .pred_class)

confusion <- pred %>%
  conf_mat(truth = income, estimate = income_dtree)
confusion
accuracy(pred, income, income_dtree)


## ----autoplot-confusion, fig.cap="Visual summary of the predictive accuracy of our decision tree model. The largest rectangle represents the cases that are true negatives."----
autoplot(confusion) +
  geom_label(
    aes(
      x = (xmax + xmin) / 2, 
      y = (ymax + ymin) / 2, 
      label = c("TN", "FP", "FN", "TP")
    )
  )


## -----------------------------------------------------------------------------
mod_tree2 <- decision_tree(mode = "classification") %>%
  set_engine("rpart", control = rpart.control(cp = 0.002)) %>%
  fit(form, data = train)


## ----randomforest-hack, include=FALSE-----------------------------------------
predict._randomForest <- function(object, new_data, ...) {
  x <- object$fit %>%
    predict(newdata = new_data, ...) %>%
    as_tibble()
  dots <- list(...)
  if (dots[["type"]] == "class") {
    names(x) <- ".pred_class"
  } else {
    names(x) <- paste0(".pred_", names(x))
    x <- x %>%
      mutate(across(.cols = everything(), as.double))
  }
  x
}

#predict(mods$mod[[3]], new_data = test, type = "class")
#predict(mods$mod[[3]], new_data = test, type = "prob")


## ----randomforest, message=FALSE----------------------------------------------
mod_forest <- rand_forest(
  mode = "classification", 
  mtry = 3, 
  trees = 201
) %>%
  set_engine("randomForest") %>%
  fit(form, data = train)

pred <- pred %>%
  bind_cols(
    predict(mod_forest, new_data = train, type = "class")
  ) %>%
  rename(income_rf = .pred_class)
  

pred %>%
  conf_mat(income, income_rf)

pred %>%
  accuracy(income, income_rf)


## ----importance, warning=FALSE------------------------------------------------
randomForest::importance(mod_forest$fit) %>% 
  as_tibble(rownames = "variable") %>%
  arrange(desc(MeanDecreaseGini))


## ----knn-compute, message=FALSE-----------------------------------------------
library(kknn)
# distance metric only works with quantitative variables
train_q <- train %>%
  select(income, where(is.numeric), -fnlwgt)

mod_knn <- nearest_neighbor(neighbors = 5, mode = "classification") %>%
  set_engine("kknn", scale = TRUE) %>%
  fit(income ~ ., data = train_q)

pred <- pred %>%
  bind_cols(
    predict(mod_knn, new_data = train, type = "class")
  ) %>%
  rename(income_knn = .pred_class)

pred %>%
  conf_mat(income, income_knn)

pred %>%
  accuracy(income, income_knn)


## ----knn-error-func-----------------------------------------------------------
knn_fit <- function(.data, k) {
  nearest_neighbor(neighbors = k, mode = "classification") %>%
    set_engine("kknn", scale = TRUE) %>%
    fit(income ~ ., data = .data)
}

knn_accuracy <- function(mod, .new_data) {
  mod %>%
    predict(new_data = .new_data) %>%
    mutate(income = .new_data$income) %>%
    accuracy(income, .pred_class) %>%
    pull(.estimate)
}


## ----knn-compute-short, echo=FALSE--------------------------------------------
ks <- c(1, 5, 10, 20, 40)




## ----knn-error-compute--------------------------------------------------------
knn_tune <- tibble(
  k = ks,
  mod = map(k, knn_fit, .data = train_q),
  train_accuracy = map_dbl(mod, knn_accuracy, .new_data = train_q)
)
knn_tune


## ----cval, fig.cap="Performance of nearest-neighbor classifier for different choices of $k$ on census training data.", eval=TRUE, warning=FALSE, message = FALSE----
ggplot(data = knn_tune, aes(x = k, y = train_accuracy)) + 
  geom_point() + 
  geom_line() + 
  ylab("Accuracy rate")


## ----head_train---------------------------------------------------------------
train %>%
  as.data.frame() %>%
  head(1)


## ----naive-bayes, message=FALSE, warning=FALSE--------------------------------
library(discrim)
mod_nb <- naive_Bayes(mode = "classification") %>%
  set_engine("klaR") %>%
  fit(form, data = train)

pred <- pred %>%  
  bind_cols(
    predict(mod_nb, new_data = train, type = "class")
  ) %>%
  rename(income_nb = .pred_class)

accuracy(pred, income, income_nb)


## ----nnet, message=FALSE------------------------------------------------------
mod_nn <- mlp(mode = "classification", hidden_units = 5) %>%
  set_engine("nnet") %>%
  fit(form, data = train)


## ----plot-nnet, echo=FALSE, fig.asp=1.2, fig.cap="Visualization of an artificial neural network. The 57 input variables are shown on the left, with the five hidden nodes in the middle, and the single output variable on the right."----
NeuralNetTools::plotnet(mod_nn$fit, circle_cex = 3, cex_val = 0.4, alpha_val = 0.3)


## ----income_nn, message=FALSE-------------------------------------------------
pred <- pred %>%
  bind_cols(
    predict(mod_nn, new_data = train, type = "class")
  ) %>%
  rename(income_nn = .pred_class)

accuracy(pred, income, income_nn)


## ----ensemble, message=FALSE, error=TRUE--------------------------------------
pred <- pred %>%
  rowwise() %>%
  mutate(
    rich_votes = sum(c_across(contains("income_")) == ">50K"),
    income_ensemble = factor(ifelse(rich_votes >= 3, ">50K", "<=50K"))
  ) %>%
  ungroup()

pred %>%
  select(-rich_votes) %>%
  pivot_longer(
    cols = -income, 
    names_to = "model", 
    values_to = "prediction"
  ) %>%
  group_by(model) %>%
  summarize(accuracy = accuracy_vec(income, prediction)) %>%
  arrange(desc(accuracy))


## ----knn-bias-var, message=FALSE, fig.cap="Performance of nearest-neighbor classifier for different choices of $k$ on census training and testing data.", eval=TRUE, warning=FALSE----
test_q <- test %>%
  select(income, where(is.numeric), -fnlwgt)

knn_tune <- knn_tune %>%
  mutate(test_accuracy = map_dbl(mod, knn_accuracy, .new_data = test_q))

knn_tune %>%
  select(-mod) %>%
  pivot_longer(-k, names_to = "type", values_to = "accuracy") %>%
ggplot(aes(x = k, y = accuracy, color = factor(type))) + 
  geom_point() + 
  geom_line() + 
  ylab("Accuracy") + 
  scale_color_discrete("Set")


## ----mod_null, warning=FALSE--------------------------------------------------
mod_null <- logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(income ~ 1, data = train)

mod_log_all <- logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(form, data = train)

mods <- tibble(
  type = c(
    "null", "log_all", "tree", "forest", 
    "knn", "neural_net", "naive_bayes"
  ),
  mod = list(
    mod_null, mod_log_all, mod_tree, mod_forest, 
    mod_knn, mod_nn, mod_nb
  )
)


## ----lapply-mods--------------------------------------------------------------
map(mods$mod, class)


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


## -----------------------------------------------------------------------------
predict_ensemble <- function(x) {
  majority <- ceiling(length(x) / 2)
  x %>%
    data.frame() %>%
    rowwise() %>%
    mutate(
      rich_votes = sum(c_across() == ">50K"),
      .pred_class = factor(ifelse(rich_votes >= majority , ">50K", "<=50K"))
    ) %>%
    pull(.pred_class) %>%
    fct_relevel("<=50K")
}


## -----------------------------------------------------------------------------
ensemble <- tibble(
  type = "ensemble",
  mod = NA,
  y_train = list(predict_ensemble(pull(mods, y_train))),
  y_test = list(predict_ensemble(pull(mods, y_test))),
  y_hat_train = list(predict_ensemble(pull(mods, y_hat_train))),
  y_hat_test = list(predict_ensemble(pull(mods, y_hat_test))),
)

mods <- mods %>%
  bind_rows(ensemble)


## ----predictions_summary------------------------------------------------------
mods <- mods %>%
  mutate(
    accuracy_train = map2_dbl(y_train, y_hat_train, accuracy_vec),
    accuracy_test = map2_dbl(y_test, y_hat_test, accuracy_vec),
    sens_test = map2_dbl(
      y_test,
      y_hat_test,
      sens_vec,
      event_level = "second"
    ),
    spec_test = map2_dbl(y_test,
      y_hat_test,
      spec_vec,
      event_level = "second"
    )
  )


## -----------------------------------------------------------------------------
mods %>%
  select(-mod, -matches("^y")) %>%
  arrange(desc(accuracy_test))


## ----roc2, warning=FALSE------------------------------------------------------
mods <- mods %>%
  filter(type != "ensemble") %>%
  mutate(
    y_hat_prob_test = map(
      mod, 
      ~pull(predict(.x, new_data = test, type = "prob"), `.pred_>50K`)
    ),
    type = fct_reorder(type, sens_test, .desc = TRUE)
  )


## ----roc-compare, fig.cap="Comparison of ROC curves across five models on the Census testing data. The null model has a true positive rate of zero and lies along the diagonal. The naïve Bayes model has a lower true positive rate than the other models. The random forest may be the best overall performer, as its curve lies furthest from the diagonal."----
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
  )


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


## -----------------------------------------------------------------------------
mod_diabetes <- decision_tree(mode = "classification") %>%
  set_engine(
    "rpart", 
    control = rpart.control(cp = 0.005, minbucket = 30)
  ) %>%
  fit(Diabetes ~ Age + BMI + Gender + PhysActive, data = people)
mod_diabetes


## ----as-party-hack2, include=FALSE--------------------------------------------
mod_diabetes$fit$call <- rlang::call2("rpar::rpart", data = expr(people))


## ----diabetes-rpart, fig.asp=1, fig.width=8, out.extra='angle=90', fig.cap="Illustration of decision tree for diabetes. "----
plot(as.party(mod_diabetes$fit))


## ----diabetes, fig.cap="Scatterplot of age against BMI for individuals in the NHANES data set. The black dots represent a collection of people with diabetes, while the gold dots represent those without diabetes."----
segments <- tribble(
  ~Age, ~xend, ~BMI, ~yend,
  52.5, 100, 39.985, 39.985, 
  67.5, 67.5, 39.985, Inf, 
  60.5, 60.5, 39.985, Inf
)

ggplot(data = people, aes(x = Age, y = BMI)) + 
  geom_count(aes(color = Diabetes), alpha = 0.5) + 
  geom_vline(xintercept = 52.5) + 
  geom_segment(
    data = segments, 
    aes(xend = xend, yend = yend)
  ) +
  scale_fill_gradient(low = "white", high = "red") + 
  scale_color_manual(values = c("gold", "black")) +
  annotate(
    "rect", fill = "blue", alpha = 0.1,
    xmin = 60.5, xmax = 67.5, ymin = 39.985, ymax = Inf
  )


## ----fake-grid, message = FALSE-----------------------------------------------
library(modelr)
fake_grid <- data_grid(
  people, 
  Age = seq_range(Age, 100),
  BMI = seq_range(BMI, 100)
)


## ----pred-all, warning=FALSE--------------------------------------------------
form <- as.formula("Diabetes ~ Age + BMI")

dmod_null <- logistic_reg(mode = "classification") %>%
  set_engine("glm")

dmod_tree <- decision_tree(mode = "classification") %>%
  set_engine("rpart", control = rpart.control(cp = 0.005, minbucket = 30))

dmod_forest <- rand_forest(
  mode = "classification", 
  trees = 201, 
  mtry = 2
) %>%
  set_engine("randomForest")

dmod_knn <- nearest_neighbor(mode = "classification", neighbors = 5) %>%
  set_engine("kknn", scale = TRUE)

dmod_nnet <- mlp(mode = "classification", hidden_units = 6) %>%
  set_engine("nnet")

dmod_nb <- naive_Bayes() %>%
  set_engine("klaR")

bmi_mods <- tibble(
  type = c(
    "Logistic Regression", "Decision Tree", "Random Forest", 
    "k-Nearest-Neighbor", "Neural Network", "Naive Bayes"
  ),
  spec = list(
    dmod_null, dmod_tree, dmod_forest, dmod_knn, dmod_nnet, dmod_nb
  ),
  mod = map(spec, fit, form, data = people),
  y_hat = map(mod, predict, new_data = fake_grid, type = "prob")
)

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


## ----mod-compare, fig.asp=1, fig.cap="Comparison of predictive models in the data space. Note the rigidity of the decision tree, the flexibility of $k$-NN and the random forest, and the bold predictions of $k$-NN."----
ggplot(data = res, aes(x = Age, y = BMI)) +
  geom_tile(aes(fill = .pred_Yes), color = NA) + 
  geom_count(
    data = people, 
    aes(color = Diabetes), alpha = 0.4
  ) + 
  scale_fill_gradient("Prob of\nDiabetes", low = "white", high = "red") + 
  scale_color_manual(values = c("gold", "black")) +
  scale_size(range = c(0, 2)) + 
  scale_x_continuous(expand = c(0.02,0)) + 
  scale_y_continuous(expand = c(0.02,0)) + 
  facet_wrap(~type, ncol = 2)


## ----cover, include=FALSE, eval=FALSE-----------------------------------------
## cover <- res %>%
##   filter(!grepl("Bayes|Neural", type)) %>%
## ggplot(aes(x = Age, y = BMI)) +
##   geom_tile(aes(fill = .pred_Yes), color = NA) +
##   geom_count(aes(color = Diabetes), alpha = 0.6, data = people) +
##   scale_fill_gradient("Prob of\nDiabetes", low = "white", high = "#AB3241") +
##   scale_color_manual(values = c("gold", "black")) +
##   scale_size(range = c(0, 4)) +
##   scale_x_continuous(expand = c(0.02,0)) +
##   scale_y_continuous(expand = c(0.02,0)) +
##   facet_wrap(~type, ncol = 2)
## cover
## ggsave("gfx/cover.pdf", width = 10, height = 10)
## cover +
##   guides(color = FALSE, fill = FALSE, size = FALSE) +
##   scale_x_continuous(NULL, expand = c(0.00,0)) +
##   scale_y_continuous(NULL, expand = c(0.00,0)) +
##   theme(
##     axis.text.x = element_blank(),
##     axis.ticks.x = element_blank(),
##     axis.text.y = element_blank(),
##     axis.ticks.y = element_blank()
##   )
## ggsave("gfx/cover_bare.pdf", width = 10, height = 10)


## -----------------------------------------------------------------------------
library(nycflights13)
California <- flights %>%
  filter(
    dest %in% c("LAX", "SFO", "OAK", "SJC"), 
         !is.na(arr_delay)
  ) %>%
  mutate(
    day = as.Date(time_hour), 
    dow = as.character(lubridate::wday(day, label = TRUE)),
    month = as.factor(month),
    hour = as.factor(hour)
  )
dim(California)


## -----------------------------------------------------------------------------
library(broom)
set.seed(386)
California_split <- initial_split(California, prop = 0.7)
California_train <- training(California_split)
California_test <- testing(California_split)


## -----------------------------------------------------------------------------
flight_model <- formula(
  "arr_delay ~ origin + dest + hour + carrier + month + dow")
mod_reg <- linear_reg() %>%
  set_engine("lm") %>%
  fit(flight_model, data = California_train)
tidy(mod_reg) %>%
  head(4)


## -----------------------------------------------------------------------------
California_test %>%
  select(arr_delay) %>%
  bind_cols(predict(mod_reg, new_data = California_test)) %>%
  metrics(truth = arr_delay, estimate = .pred)


## ----message = FALSE----------------------------------------------------------
mod_lasso <- linear_reg(penalty = 0.01, mixture = 1) %>%
  set_engine("glmnet") %>%
  fit(flight_model, data = California_train)
tidy(mod_lasso) %>% 
  head(4)


## -----------------------------------------------------------------------------
California_test %>%
  select(arr_delay) %>%
  bind_cols(predict(mod_lasso, new_data = California_test)) %>%
  metrics(truth = arr_delay, estimate = .pred)

