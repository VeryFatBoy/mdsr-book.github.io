### R code from vignette source 'learningI.Rnw'

###################################################
### code chunk number 1: learningI.Rnw:1-3
###################################################
source('hooks.R', echo=TRUE)
fig.path='figures/learning-'


###################################################
### code chunk number 2: learningI.Rnw:8-9
###################################################
options(continue="  ")


###################################################
### code chunk number 3: learningI.Rnw:75-76 (eval = FALSE)
###################################################
## diabetic ~ age + sex + weight + height


###################################################
### code chunk number 4: learningI.Rnw:177-186
###################################################
library(mdsr)
census <- read.csv(
"http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", 
  header = FALSE)
names(census) <- c("age", "workclass", "fnlwgt", "education", 
  "education.num", "marital.status", "occupation", "relationship", 
  "race", "sex", "capital.gain", "capital.loss", "hours.per.week", 
  "native.country", "income")
glimpse(census)


###################################################
### code chunk number 5: learningI.Rnw:202-209
###################################################
set.seed(364)
n <- nrow(census)
test_idx <- sample.int(n, size = round(0.2 * n))
train <- census[-test_idx, ]
nrow(train)
test <- census[test_idx, ]
nrow(test)


###################################################
### code chunk number 6: pi_bar
###################################################
pi_bar <- tally(~income, data = train, format = "percent")[2]


###################################################
### code chunk number 7: tally-pi_bar
###################################################
tally(~income, data = train, format = "percent")


###################################################
### code chunk number 8: learningI.Rnw:233-239
###################################################
library(rpart)
dtree <- rpart(income ~ capital.gain, data = train)
split_val <- as.data.frame(dtree$splits)$index
dtree_frame <- dtree$frame %>%
  select(var, n, dev, yval) %>%
  mutate(pct = n / nrow(train), right = (n - dev) / n)


###################################################
### code chunk number 9: rpart1
###################################################
library(rpart)
rpart(income ~ capital.gain, data = train)


