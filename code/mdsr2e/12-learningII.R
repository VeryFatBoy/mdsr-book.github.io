## ----evolution-mammals, echo=FALSE, fig.cap="(ref:mammals-cap)"---------------
knitr::include_graphics("gfx/evolutionary-tree.jpeg")


## ----message=FALSE, eval=FALSE------------------------------------------------
## src <- "https://www.fueleconomy.gov/feg/epadata/16data.zip"
## lcl <- usethis::use_zip(src)


## ----message=FALSE, warning=FALSE---------------------------------------------
library(tidyverse)
library(mdsr)
library(readxl)
filename <- fs::dir_ls("data", regexp = "public\\.xlsx") %>%
  head(1)
cars <- read_excel(filename) %>% 
  janitor::clean_names() %>%
  select(
    make = mfr_name, 
    model = carline, 
    displacement = eng_displ,
    number_cyl,
    number_gears,
    city_mpg = city_fe_guide_conventional_fuel,
    hwy_mpg = hwy_fe_guide_conventional_fuel
  ) %>%
  distinct(model, .keep_all = TRUE) %>% 
  filter(make == "Toyota")
glimpse(cars)


## ----warning=FALSE------------------------------------------------------------
car_diffs <- cars %>%
  column_to_rownames(var = "model") %>%
  dist()
str(car_diffs)
car_mat <- car_diffs %>% 
  as.matrix()
car_mat[1:6, 1:6] %>% 
  round(digits = 2)


## ----city-distances, echo=FALSE, fig.cap="Distances between some U.S. cities."----
knitr::include_graphics("gfx/City_Distances.png")


## ----cars-tree, fig.asp=16/9, out.width="60%", fig.cap="A dendrogram constructed by hierarchical clustering from car-to-car distances implied by the Toyota fuel economy data."----
library(ape)
car_diffs %>% 
  hclust() %>% 
  as.phylo() %>% 
  plot(cex = 0.8, label.offset = 1)


## ----message=FALSE------------------------------------------------------------
big_cities <- world_cities %>% 
  arrange(desc(population)) %>%
  head(4000) %>% 
  select(longitude, latitude)
glimpse(big_cities)


## ----cluster-cities, fig.cap="The world's 4,000 largest cities, clustered by the 6-means clustering algorithm.", message=FALSE----
set.seed(15)
library(mclust)
city_clusts <- big_cities %>% 
  kmeans(centers = 6) %>%
  fitted("classes") %>% 
  as.character()
big_cities <- big_cities %>% 
  mutate(cluster = city_clusts)
big_cities %>% 
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(aes(color = cluster), alpha = 0.5)  + 
  scale_color_brewer(palette = "Set2")


## ----scot-votes-small, echo=FALSE---------------------------------------------
Votes_wide <- Votes %>%
  pivot_wider(names_from = bill, values_from = vote)
Votes_wide %>%
  select(1:5) %>%
  head(10) %>%
  mdsr_table(caption = "Sample voting records data from the Scottish Parliament.") %>%
  kableExtra::column_spec(1, width = "15em")


## ----ballot-grid, fig.cap="Visualization of the Scottish Parliament votes."----
Votes %>% 
  mutate(Vote = factor(vote, labels = c("Nay", "Abstain", "Aye"))) %>%
  ggplot(aes(x = bill, y = name, fill = Vote)) +
    geom_tile() + 
    xlab("Ballot") + 
    ylab("Member of Parliament") +
    scale_fill_manual(values = c("darkgray", "white", "goldenrod")) + 
    scale_x_discrete(breaks = NULL, labels = NULL) + 
    scale_y_discrete(breaks = NULL, labels = NULL)


## ----two-ballots, fig.cap="Scottish Parliament votes for two ballots."--------
Votes %>% 
  filter(bill %in% c("S1M-240.2", "S1M-639.1")) %>%
  pivot_wider(names_from = bill, values_from = vote) %>%
  ggplot(aes(x = `S1M-240.2`, y = `S1M-639.1`)) +
    geom_point(
      alpha = 0.7, 
      position = position_jitter(width = 0.1, height = 0.1)
    ) +
    geom_point(alpha = 0.01, size = 10, color = "red" )


## ----many-ballots, fig.cap="Scatterplot showing the correlation between Scottish Parliament votes in two arbitrary collections of ballots."----
Votes %>%
  mutate(
    set_num = as.numeric(factor(bill)), 
    set = ifelse(
      set_num < max(set_num) / 2, "First_Half", "Second_Half"
    )
  ) %>%
  group_by(name, set) %>% 
  summarize(Ayes = sum(vote)) %>%
  pivot_wider(names_from = set, values_from = Ayes) %>%
  ggplot(aes(x = First_Half, y = Second_Half)) + 
  geom_point(alpha = 0.7, size = 5)


## ----ballot-PCA, warning=FALSE, fig.cap="Clustering members of Scottish Parliament based on SVD along the members.", message = FALSE----
Votes_wide <- Votes %>%
  pivot_wider(names_from = bill, values_from = vote)
vote_svd <- Votes_wide %>% 
  select(-name) %>% 
  svd()

num_clusters <- 5   # desired number of clusters
library(broom)
vote_svd_tidy <- vote_svd %>%
  tidy(matrix = "u") %>%
  filter(PC < num_clusters) %>%
  mutate(PC = paste0("pc_", PC)) %>%
  pivot_wider(names_from = PC, values_from = value) %>%
  select(-row)

clusts <- vote_svd_tidy %>% 
  kmeans(centers = num_clusters)

tidy(clusts)

voters <- clusts %>%
  augment(vote_svd_tidy)

ggplot(data = voters, aes(x = pc_1, y = pc_2)) +
  geom_point(aes(x = 0, y = 0), color = "red", shape = 1, size = 7) + 
  geom_point(size = 5, alpha = 0.6, aes(color = .cluster)) +
  xlab("Best Vector from SVD") + 
  ylab("Second Best Vector from SVD") +
  ggtitle("Political Positions of Members of Parliament") + 
  scale_color_brewer(palette = "Set2")


## ----message = FALSE----------------------------------------------------------
voters <- voters %>% 
  mutate(name = Votes_wide$name) %>%
  left_join(Parties, by = c("name" = "name"))
mosaic::tally(party ~ .cluster, data = voters)


## ----message = FALSE----------------------------------------------------------
ballots <- vote_svd %>%
  tidy(matrix = "v") %>%
  filter(PC < num_clusters) %>%
  mutate(PC = paste0("pc_", PC)) %>%
  pivot_wider(names_from = PC, values_from = value) %>%
  select(-column)
clust_ballots <- kmeans(ballots, centers = num_clusters)
ballots <- clust_ballots %>% 
  augment(ballots) %>%
  mutate(bill = names(select(Votes_wide, -name)))


## ----issue-clusters, fig.cap="Clustering of Scottish Parliament ballots based on SVD along the ballots."----
ggplot(data = ballots, aes(x = pc_1, y = pc_2)) +
  geom_point(aes(x = 0, y = 0), color = "red", shape = 1, size = 7) + 
  geom_point(size = 5, alpha = 0.6, aes(color = .cluster)) +
  xlab("Best Vector from SVD") + 
  ylab("Second Best Vector from SVD") +
  ggtitle("Influential Ballots") + 
  scale_color_brewer(palette = "Set2")


## ----SVD-ballots, fig.cap="Illustration of the Scottish Parliament votes when ordered by the primary vector of the SVD.", warning=FALSE----
Votes_svd <- Votes %>%
  mutate(Vote = factor(vote, labels = c("Nay", "Abstain", "Aye"))) %>%
  inner_join(ballots, by = "bill") %>%
  inner_join(voters, by = "name")

ggplot(data = Votes_svd, 
  aes(x = reorder(bill, pc_1.x), y = reorder(name, pc_1.y), fill = Vote)) +
  geom_tile() + 
  xlab("Ballot") + 
  ylab("Member of Parliament") +
  scale_fill_manual(values = c("darkgray", "white", "goldenrod")) + 
  scale_x_discrete(breaks = NULL, labels = NULL) + 
  scale_y_discrete(breaks = NULL, labels = NULL)


## -----------------------------------------------------------------------------
Votes_svd %>% 
  arrange(pc_1.y) %>% 
  head(1)


## -----------------------------------------------------------------------------
Votes_svd %>% 
  arrange(pc_1.y) %>% 
  tail(1)

