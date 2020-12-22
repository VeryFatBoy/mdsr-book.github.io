## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/learning-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----eval=FALSE----------------------------------------------------------
## diabetic ~ age + sex + weight + height

## ----message=FALSE-------------------------------------------------------
library(mdsr)
census <- read.csv(
"http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", 
  header = FALSE)
names(census) <- c("age", "workclass", "fnlwgt", "education", 
  "education.num", "marital.status", "occupation", "relationship", 
  "race", "sex", "capital.gain", "capital.loss", "hours.per.week", 
  "native.country", "income")
glimpse(census)

## ------------------------------------------------------------------------
set.seed(364)
n <- nrow(census)
test_idx <- sample.int(n, size = round(0.2 * n))
train <- census[-test_idx, ]
nrow(train)
test <- census[test_idx, ]
nrow(test)

## ----pi_bar,include=FALSE------------------------------------------------
pi_bar <- tally(~income, data = train, format = "percent")[2]

## ----tally-pi_bar,message=FALSE------------------------------------------
tally(~income, data = train, format = "percent")

## ----include=FALSE,warning=FALSE-----------------------------------------
library(rpart)
dtree <- rpart(income ~ capital.gain, data = train)
split_val <- as.data.frame(dtree$splits)$index
dtree_frame <- dtree$frame %>%
  select(var, n, dev, yval) %>%
  mutate(pct = n / nrow(train), right = (n - dev) / n)

## ----rpart1--------------------------------------------------------------
library(rpart)
rpart(income ~ capital.gain, data = train)

## ----census-rpart, fig.cap="A single partition of the \\data{census} data set using the \\var{capital.gain} variable to determine the split. Color, and the vertical line at \\$5,095.50 in capital gains tax indicate the split. If one paid more than this amount, one almost certainly made more than \\$50,000 in income. On the other hand, if one paid less than this amount in capital gains, one almost certainly made less than \\$50,000.", warning=FALSE----
split <- 5095.5
train <- train %>% mutate(hi_cap_gains = capital.gain >= split)

ggplot(data = train, aes(x = capital.gain, y = income)) + 
  geom_count(aes(color = hi_cap_gains), 
    position = position_jitter(width = 0, height = 0.1), alpha = 0.5) + 
  geom_vline(xintercept = split, color = "dodgerblue", lty = 2) + 
  scale_x_log10(labels = scales::dollar)

## ----formula-------------------------------------------------------------
form <- as.formula("income ~ age + workclass + education + marital.status + 
  occupation + relationship + race + sex + capital.gain + capital.loss + 
  hours.per.week")
mod_tree <- rpart(form, data = train)
mod_tree

## ----eval=FALSE----------------------------------------------------------
## plot(mod_tree)
## text(mod_tree, use.n = TRUE, all = TRUE, cex = 0.7)

## ----maptree, fig.height=10, fig.cap="Decision tree for income using the \\data{census} data.", message=FALSE----
library(partykit)
plot(as.party(mod_tree))

## ----include=FALSE-------------------------------------------------------
options(digits = 5)
mod_tree_frame <- mod_tree$frame %>%
  select(var, n, dev, yval) %>%
  mutate(pct = n / nrow(train), right = (n - dev) / n)

## ----census-rpart2, fig.cap="Graphical depiction of the full recursive partitioning decision tree classifier. On the left, those whose relationship status is neither ``Husband'' nor ``Wife'' are classified based on their capital gains paid. On the right, not only is the capital gains threshold different, but the decision is also predicated on whether the person has a college degree.", warning=FALSE----
train <- train %>%
  mutate(husband_or_wife = relationship %in% c(" Husband", " Wife"), 
         college_degree = husband_or_wife & education %in% 
           c(" Bachelors", " Doctorate", " Masters", " Prof-school"), 
         income_dtree = predict(mod_tree, type = "class"))

cg_splits <- data.frame(husband_or_wife = c(TRUE, FALSE), 
                        vals = c(5095.5, 7073.5))

ggplot(data = train, aes(x = capital.gain, y = income)) + 
  geom_count(aes(color = income_dtree, shape = college_degree), 
             position = position_jitter(width = 0, height = 0.1), 
             alpha = 0.5) + 
  facet_wrap(~ husband_or_wife) + 
  geom_vline(data = cg_splits, aes(xintercept = vals), 
             color = "dodgerblue", lty = 2) + 
  scale_x_log10()

## ------------------------------------------------------------------------
printcp(mod_tree)
# plotcp(mod_tree)

## ----message=FALSE-------------------------------------------------------
train <- train %>%
  mutate(income_dtree = predict(mod_tree, type = "class"))
confusion <- tally(income_dtree ~ income, data = train, format = "count")
confusion
sum(diag(confusion)) / nrow(train)

## ------------------------------------------------------------------------
mod_tree2 <- rpart(form, data = train, control = rpart.control(cp = 0.002))

## ----randomforest, message=FALSE-----------------------------------------
library(randomForest)
mod_forest <- randomForest(form, data = train, ntree = 201, mtry = 3)
mod_forest
sum(diag(mod_forest$confusion)) / nrow(train)

## ----importance, warning=FALSE-------------------------------------------
library(tibble)
importance(mod_forest) %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(desc(MeanDecreaseGini))

## ----knn-compute,message=FALSE-------------------------------------------
library(class)
# distance metric only works with quantitative variables
train_q <- train %>%
  select(age, education.num, capital.gain, capital.loss, hours.per.week)
income_knn <- knn(train_q, test = train_q, cl = train$income, k = 10)

confusion <- tally(income_knn ~ income, data = train, format = "count")
confusion
sum(diag(confusion)) / nrow(train)

## ----cval, fig.cap="Performance of nearest neighbor classifier for different choices of $k$ on census training data.", eval=TRUE----
knn_error_rate <- function(x, y, numNeighbors, z = x) {
  y_hat <- knn(train = x, test = z, cl = y, k = numNeighbors)
  return(sum(y_hat != y) / nrow(x))
}
ks <- c(1:15, 20, 30, 40, 50)
train_rates <- sapply(ks, FUN = knn_error_rate, x = train_q, y = train$income)
knn_error_rates <- data.frame(k = ks, train_rate = train_rates)
ggplot(data = knn_error_rates, aes(x = k, y = train_rate)) + 
  geom_point() + geom_line() + ylab("Misclassification Rate")

## ----head_train----------------------------------------------------------
head(train, 1)

## ----naive-bayes,message=FALSE-------------------------------------------
library(e1071)
mod_nb <- naiveBayes(form, data = train) 
income_nb <- predict(mod_nb, newdata = train)
confusion <- tally(income_nb ~ income, data = train, format = "count")
confusion
sum(diag(confusion)) / nrow(train)

## ----message=FALSE-------------------------------------------------------
library(nnet)
mod_nn <- nnet(form, data = train, size = 5)

## ----plot-nnet, echo=FALSE, fig.width=10, fig.height=6.6, out.width="8.5in", fig.cap="Visualization of an artificial neural network. The input 57 input variables are shown on the bottom, with the five hidden nodes in the middle, and the single output variable at the top.", out.extra='angle=90'----
NeuralNetTools::plotnet(mod_nn, circle_cex = 3, cex_val = 0.4, alpha_val = 0.3)

## ----message=FALSE-------------------------------------------------------
income_nn <- predict(mod_nn, newdata = train, type = "class")
confusion <- tally(income_nn ~ income, data = train, format = "count")
confusion
sum(diag(confusion)) / nrow(train)

## ----message=FALSE-------------------------------------------------------
income_ensemble <- ifelse((income_knn == " >50K") + 
                           (income_nb == " >50K") + 
                           (income_nn == " >50K") >= 2, " >50K", " <=50K")
confusion <- tally(income_ensemble ~ income, data = train, format = "count")
confusion
sum(diag(confusion)) / nrow(train)

## ------------------------------------------------------------------------
income_probs <- mod_nb %>% 
  predict(newdata = train, type = "raw") %>% 
  as.data.frame()
head(income_probs, 3)

## ------------------------------------------------------------------------
names(income_probs)
tally(~` >50K` > 0.5, data = income_probs, format = "percent")

## ------------------------------------------------------------------------
tally(~` >50K` > 0.24, data = income_probs, format = "percent")

## ----roc-compute, message=FALSE, warning=FALSE, eval=TRUE----------------
pred <- ROCR::prediction(income_probs[,2], train$income)
perf <- ROCR::performance(pred, 'tpr', 'fpr')
class(perf) # can also plot(perf)

## ------------------------------------------------------------------------
perf_df <- data.frame(perf@x.values, perf@y.values)
names(perf_df) <- c("fpr", "tpr")
roc <- ggplot(data = perf_df, aes(x = fpr, y = tpr)) + 
  geom_line(color="blue") + geom_abline(intercept=0, slope=1, lty=3) + 
  ylab(perf@y.name) + xlab(perf@x.name)

## ----roc, fig.cap="ROC curve for naive Bayes model.", message=FALSE, eval=TRUE----
confusion <- tally(income_nb ~ income, data = train, format = "count")
confusion
sum(diag(confusion)) / nrow(train)
tpr <- confusion[" >50K", " >50K"] / sum(confusion[, " >50K"])
fpr <- confusion[" >50K", " <=50K"] / sum(confusion[, " <=50K"])
roc + geom_point(x = fpr, y = tpr, size = 3)

## ----knn-bias-var, message=FALSE, fig.cap="Performance of nearest neighbor classifier for different choices of $k$ on census training and testing data.", eval=TRUE, warning=FALSE----
test_q <- test %>%
  select(age, education.num, capital.gain, capital.loss, hours.per.week)
test_rates <- sapply(ks, FUN = knn_error_rate, x = train_q, 
                     y = train$income, z = test_q)
knn_error_rates <- knn_error_rates %>% mutate(test_rate = test_rates)
library(tidyr)
knn_error_rates_tidy <- knn_error_rates %>%
  gather(key = "type", value = "error_rate", -k)
ggplot(data = knn_error_rates_tidy, aes(x = k, y = error_rate)) + 
  geom_point(aes(color = type)) + geom_line(aes(color = type)) + 
  ylab("Misclassification Rate")

## ------------------------------------------------------------------------
favstats(~ capital.gain, data = train)
favstats(~ capital.gain, data = test)

## ----mod_null------------------------------------------------------------
mod_null <- glm(income ~ 1, data = train, family = binomial)
mods <- list(mod_null, mod_tree, mod_forest, mod_nn, mod_nb)

## ----lapply-mods---------------------------------------------------------
lapply(mods, class)
predict_methods <- methods("predict")
predict_methods[grepl(pattern = "(glm|rpart|randomForest|nnet|naive)", 
  predict_methods)]

## ----make-predictons,warning=FALSE---------------------------------------
predictions_train <- data.frame(
  y = as.character(train$income),
  type = "train", 
  mod_null = predict(mod_null, type = "response"), 
  mod_tree = predict(mod_tree, type = "class"), 
  mod_forest = predict(mod_forest, type = "class"), 
  mod_nn = predict(mod_nn, type = "class"), 
  mod_nb = predict(mod_nb, newdata = train, type = "class"))
predictions_test <- data.frame(
  y = as.character(test$income),
  type = "test", 
  mod_null = predict(mod_null, newdata = test, type = "response"), 
  mod_tree = predict(mod_tree, newdata = test, type = "class"), 
  mod_forest = predict(mod_forest, newdata = test, type = "class"), 
  mod_nn = predict(mod_nn, newdata = test, type = "class"), 
  mod_nb = predict(mod_nb, newdata = test, type = "class"))
predictions <- bind_rows(predictions_train, predictions_test)
glimpse(predictions)

## ----predictions_tidy,warning=FALSE--------------------------------------
predictions_tidy <- predictions %>%
  mutate(mod_null = ifelse(mod_null < 0.5, " <=50K", " >50K")) %>%
  gather(key = "model", value = "y_hat", -type, -y)
glimpse(predictions_tidy)

## ----predictions_summary-------------------------------------------------
predictions_summary <- predictions_tidy %>%
  group_by(model, type) %>%
  summarize(N = n(), correct = sum(y == y_hat, 0), 
            positives = sum(y == " >50K"), 
            true_pos = sum(y_hat == " >50K" & y == y_hat),
            false_pos = sum(y_hat == " >50K" & y != y_hat)) %>%
  mutate(accuracy = correct / N, 
         tpr = true_pos / positives,
         fpr = false_pos / (N - positives)) %>%
  ungroup() %>% 
  gather(val_type, val, -model, -type) %>%
  unite(temp1, type, val_type, sep = "_") %>%   # glue variables 
  spread(temp1, val) %>% 
  arrange(desc(test_accuracy)) %>%
  select(model, train_accuracy, test_accuracy, test_tpr, test_fpr)
predictions_summary

## ----roc2----------------------------------------------------------------
outputs <- c("response", "prob", "prob", "raw", "raw")
roc_test <- mapply(predict, mods, type = outputs, 
    MoreArgs = list(newdata = test)) %>%
  as.data.frame() %>% 
  select(1,3,5,6,8)
names(roc_test) <- 
  c("mod_null", "mod_tree", "mod_forest", "mod_nn", "mod_nb")
glimpse(roc_test)

get_roc <- function(x, y) {
  pred <- ROCR::prediction(x$y_hat, y)
  perf <- ROCR::performance(pred, 'tpr', 'fpr')
  perf_df <- data.frame(perf@x.values, perf@y.values)
  names(perf_df) <- c("fpr", "tpr")
  return(perf_df)
}

roc_tidy <- roc_test %>%
  gather(key = "model", value = "y_hat") %>%
  group_by(model) %>% 
  dplyr::do(get_roc(., y = test$income))

## ----roc-compare, fig.cap="Comparison of ROC curves across five models on the Census testing data. The null model has a true positive rate of zero and lies along the diagonal. The Bayes has a lower true positive rate than the other models. The random forest may be the best overall performer, as its curve lies furthest from the diagonal."----
ggplot(data = roc_tidy, aes(x = fpr, y = tpr)) + 
  geom_line(aes(color = model)) + 
  geom_abline(intercept = 0, slope = 1, lty = 3) + 
  ylab(perf@y.name) + xlab(perf@x.name) +
  geom_point(data = predictions_summary, size = 3,
    aes(x = test_fpr, y = test_tpr, color = model))

## ----nhanes-people-------------------------------------------------------
library(NHANES)
people <- NHANES %>%
  select(Age, Gender, Diabetes, BMI, HHIncome, PhysActive) %>% 
  na.omit()
glimpse(people)
tally(~ Diabetes, data = people, format = "percent")

## ----diabetes-rpart, fig.cap="Illustration of decision tree for diabetes. "----
whoIsDiabetic <- rpart(Diabetes ~ Age + BMI + Gender + PhysActive, 
  data = people, control = rpart.control(cp = 0.005, minbucket = 30))
whoIsDiabetic
plot(as.party(whoIsDiabetic))

## ----diabetes, fig.cap="Scatterplot of age against BMI for individuals in the NHANES data set. The green dots represent a collection of people with diabetes, while the pink dots represent those without diabetes."----
ggplot(data = people, aes(x = Age, y = BMI)) + 
  geom_count(aes(color = Diabetes), alpha = 0.5) + 
  geom_vline(xintercept = 52.5) + 
  geom_segment(x = 52.5, xend = 100, y = 39.985, yend = 39.985) +
  geom_segment(x = 67.5, xend = 67.5, y = 39.985, yend = Inf) +
  geom_segment(x = 60.5, xend = 60.5, y = 39.985, yend = Inf) +
  annotate("rect", xmin = 60.5, xmax = 67.5, ymin = 39.985, 
    ymax = Inf, fill = "blue", alpha = 0.1)

## ----fake-grid-----------------------------------------------------------
ages <- range(~ Age, data = people)
bmis <- range(~ BMI, data = people)
res <- 100
fake_grid <- expand.grid(
  Age = seq(from = ages[1], to = ages[2], length.out = res),
  BMI = seq(from = bmis[1], to = bmis[2], length.out = res))

## ----pred-all,warning=FALSE----------------------------------------------
form <- as.formula("Diabetes ~ Age + BMI")
dmod_tree <- rpart(form, data = people, 
                       control = rpart.control(cp = 0.005, minbucket = 30))
dmod_forest <- randomForest(form, data = people, ntree = 201, mtry = 3)
dmod_nnet <- nnet(form, data = people, size = 6)
dmod_nb <- naiveBayes(form, data = people)

pred_tree <- predict(dmod_tree, newdata = fake_grid)[, "Yes"]
pred_forest <- predict(dmod_forest, newdata = fake_grid, 
  type = "prob")[, "Yes"]
pred_knn <- people %>%
  select(Age, BMI) %>%
  knn(test = select(fake_grid, Age, BMI), cl = people$Diabetes, k = 5) %>%
  as.numeric() - 1
pred_nnet <- predict(dmod_nnet, newdata = fake_grid, type = "raw") %>%
  as.numeric()
pred_nb <- predict(dmod_nb, newdata = fake_grid, type = "raw")[, "Yes"]

## ----diabetes-p----------------------------------------------------------
p <- tally(~ Diabetes, data = people, format = "proportion")["Yes"]

## ----res-for-images,warning=FALSE----------------------------------------
res <- fake_grid %>% 
  mutate(
    "Null" = rep(p, nrow(fake_grid)), "Decision Tree" = pred_tree,
    "Random Forest" = pred_forest, "k-Nearest Neighbor" = pred_knn,
    "Neural Network" = pred_nnet, "Naive Bayes" = pred_nb) %>%
  gather(key = "model", value = "y_hat", -Age, -BMI)

## ----mod-compare, fig.width=10, fig.height=6.7, out.width="8.5in", fig.cap="Comparison of predictive models in the data space. Note the rigidity of the decision tree, the flexibility of $k$-NN and the random forest, and the bold predictions of $k$-NN.", out.extra='angle=90'----
ggplot(data = res, aes(x = Age, y = BMI)) +
  geom_tile(aes(fill = y_hat), color = NA) + 
  geom_count(aes(color = Diabetes), alpha = 0.4, data = people) + 
  scale_fill_gradient(low = "white", high = "dodgerblue") + 
  scale_color_manual(values = c("gray", "gold")) + 
  scale_size(range = c(0, 2)) + 
  scale_x_continuous(expand = c(0.02,0)) + 
  scale_y_continuous(expand = c(0.02,0)) + 
  facet_wrap(~model)

## ----cover, include=FALSE, eval=FALSE------------------------------------
## cover <- res %>%
##   filter(model == "Random Forest") %>%
## ggplot(aes(x = Age, y = BMI)) +
##   geom_tile(aes(fill = y_hat), color = NA) +
##   geom_count(aes(color = Diabetes), alpha = 0.6, data = people) +
##   scale_fill_gradient(low = "white", high = "dodgerblue") +
##   scale_color_manual(values = c("gray", "gold")) +
##   scale_size(range = c(0, 4)) +
##   scale_x_continuous(expand = c(0.02,0)) +
##   scale_y_continuous(expand = c(0.02,0))
## cover
## ggsave("gfx/cover.pdf", width = 10, height = 10)
## cover +
##   guides(colour = FALSE, fill = FALSE, size = FALSE) +
##   scale_x_continuous(NULL, expand = c(0.00,0)) +
##   scale_y_continuous(NULL, expand = c(0.00,0)) +
##   theme(axis.text.x = element_blank(),
##         axis.ticks.x = element_blank(),
##         axis.text.y = element_blank(),
##         axis.ticks.y = element_blank())
## ggsave("gfx/cover_bare.pdf", width = 10, height = 10)

