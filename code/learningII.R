## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/learningII-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----message=FALSE, eval=FALSE-------------------------------------------
## download.file("https://www.fueleconomy.gov/feg/epadata/16data.zip",
##               destfile = "data/fueleconomy.zip")
## unzip("data/fueleconomy.zip", exdir = "data/fueleconomy/")

## ----message=FALSE, warning=FALSE----------------------------------------
library(mdsr)
library(readxl)
filename <- list.files("data/fueleconomy", pattern = "public\\.xlsx")[1]
cars <- read_excel(paste0("data/fueleconomy/", filename)) %>% data.frame()
cars <- cars %>%
  rename(make = Mfr.Name, model = Carline, displacement = Eng.Displ,
    cylinders = X..Cyl, city_mpg = City.FE..Guide....Conventional.Fuel,
    hwy_mpg = Hwy.FE..Guide....Conventional.Fuel, gears = X..Gears) %>%
  select(make, model, displacement, cylinders, gears, city_mpg, hwy_mpg) %>%
  distinct(model, .keep_all = TRUE) %>% 
  filter(make == "Toyota")
rownames(cars) <- cars$model
glimpse(cars)

## ----warning=FALSE-------------------------------------------------------
car_diffs <- dist(cars)
str(car_diffs)
car_mat <- car_diffs %>% as.matrix()
car_mat[1:6, 1:6] %>% round(digits = 2)

## ----cars-tree, fig.height=14, fig.cap="A dendrogram constructed by hierarchical clustering from car-to-car distances implied by the Toyota fuel economy data."----
library(ape)
car_diffs %>% 
  hclust() %>% 
  as.phylo() %>% 
  plot(cex = 0.9, label.offset = 1)

## ----message=FALSE-------------------------------------------------------
BigCities <- WorldCities %>% 
  arrange(desc(population)) %>%
  head(4000) %>% 
  select(longitude, latitude)
glimpse(BigCities)

## ----cluster-cities, fig.cap="The world's 4,000 largest cities, clustered by the 6-means clustering algorithm.", message=FALSE----
set.seed(15)
library(mclust)
city_clusts <- BigCities %>% 
  kmeans(centers = 6) %>%
  fitted("classes") %>% 
  as.character()
BigCities <- BigCities %>% mutate(cluster = city_clusts)
BigCities %>% ggplot(aes(x = longitude, y = latitude)) +
  geom_point(aes(color = cluster), alpha = 0.5)

## ----echo=FALSE, results='asis'------------------------------------------
library(xtable)
Votes_wide <- Votes %>%
  tidyr::spread(key = bill, value = vote)
Votes_wide %>%
  select(1:5) %>%
  head(10) %>%
  xtable(caption = "Sample voting records data from the Scottish Parliament.", 
         label = "tab:scot-votes-small") %>%
  print(include.rownames = FALSE)

## ----ballot-grid, fig.cap="Visualization of the Scottish Parliament votes."----
Votes %>% 
  mutate(Vote = factor(vote, labels = c("Nay","Abstain","Aye"))) %>%
  ggplot(aes(x = bill, y = name, fill = Vote)) +
    geom_tile() + xlab("Ballot") + ylab("Member of Parliament") +
    scale_fill_manual(values = c("darkgray", "white", "goldenrod")) + 
    scale_x_discrete(breaks = NULL, labels = NULL) + 
    scale_y_discrete(breaks = NULL, labels = NULL)

## ----two-ballots, fig.cap="Scottish Parliament votes for two ballots."----
Votes %>% filter(bill %in% c("S1M-240.2", "S1M-639.1")) %>%
  tidyr::spread(key = bill, value = vote) %>%
  ggplot(aes(x = `S1M-240.2`, y = `S1M-639.1`)) +
    geom_point(alpha = 0.7, 
               position = position_jitter(width = 0.1, height = 0.1)) +
    geom_point(alpha = 0.01, size = 10, color = "red" )

## ----many-ballots, fig.cap="Scatterplot showing the correlation between Scottish Parliament votes in two arbitrary collections of ballots."----
Votes %>%
  mutate(set_num = as.numeric(factor(bill)), 
    set = 
      ifelse(set_num < max(set_num) / 2, "First_Half", "Second_Half")) %>%
  group_by(name, set) %>% 
  summarize(Ayes = sum(vote)) %>%
  tidyr::spread(key = set, value = Ayes) %>%
  ggplot(aes(x = First_Half, y = Second_Half)) + 
  geom_point(alpha = 0.7, size = 5)

## ----ballot-PCA, fig.cap="Clustering members of Scottish Parliament based on SVD along the members."----
Votes_wide <- Votes %>%
  tidyr::spread(key = bill, value = vote)
vote_svd <- Votes_wide %>% 
  select(-name) %>% 
  svd()
voters <- vote_svd$u[ , 1:5] %>% as.data.frame()
clusts <- voters %>% kmeans(centers = 6)
voters <- voters %>% mutate(cluster = as.factor(clusts$cluster))
ggplot(data = voters, aes(x = V1, y = V2)) +
  geom_point(aes(x = 0, y = 0), color = "red", shape = 1, size = 7) + 
  geom_point(size = 5, alpha = 0.6, aes(color = cluster)) +
  xlab("Best Vector from SVD") + ylab("Second Best Vector from SVD") +
  ggtitle("Political Positions of Members of Parliament")

## ------------------------------------------------------------------------
voters <- voters %>% 
  mutate(name = Votes_wide$name) %>%
  left_join(Parties, by = c("name" = "name"))
tally(party ~ cluster, data = voters)

## ------------------------------------------------------------------------
ballots <- vote_svd$v[ , 1:5] %>% as.data.frame()
clust_ballots <- kmeans(ballots, centers = 16)
ballots <- ballots %>% mutate(cluster = as.factor(clust_ballots$cluster),
  bill = names(Votes_wide)[-1])

## ----issue-clusters, fig.cap="Clustering of Scottish Parliament ballots based on SVD along the ballots."----
ggplot(data = ballots, aes(x = V1, y = V2)) +
  geom_point(aes(x = 0, y = 0), color = "red", shape = 1, size = 7) + 
  geom_point(size = 5, alpha = 0.6, aes(color = cluster)) +
  xlab("Best Vector from SVD") + ylab("Second Best Vector from SVD") +
  ggtitle("Influential Ballots")

## ----SVD-ballots, fig.cap="Illustration of the Scottish Parliament votes when ordered by the primary vector of the SVD.", warning=FALSE----
Votes_svd <- Votes %>%
  mutate(Vote = factor(vote, labels = c("Nay", "Abstain", "Aye"))) %>%
  inner_join(ballots, by = "bill") %>%
  inner_join(voters, by = "name")
ggplot(data = Votes_svd, 
  aes(x = reorder(bill, V1.x), y = reorder(name, V1.y), fill = Vote)) +
  geom_tile() + xlab("Ballot") + ylab("Member of Parliament") +
  scale_fill_manual(values = c("darkgray", "white", "goldenrod")) + 
  scale_x_discrete(breaks = NULL, labels = NULL) + 
  scale_y_discrete(breaks = NULL, labels = NULL)

## ------------------------------------------------------------------------
Votes_svd %>% 
  arrange(V1.y) %>% 
  head(1)

## ------------------------------------------------------------------------
Votes_svd %>% 
  arrange(V1.y) %>% 
  tail(1)

