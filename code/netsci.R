## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/netsci-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----er-graphs, message=FALSE, fig.cap="Two Erd{\\H{o}}s--R{\\'e}nyi random graphs on 100 vertices with different values of $p$. The graph at left is not connected, but the graph at right is. The value of $p$ hasn't changed by much.", fig.subcap=c("A graph that is not connected.", "A connected graph."), out.width="0.49\\textwidth"----
library(mdsr)
library(igraph)
n <- 100
p_star <- log(n)/n

plot_er <- function(n, p, ...) {
  g <- erdos.renyi.game(n, p)
  plot(g, main = paste("p =", round(p, 4)), vertex.frame.color = "white", 
    vertex.size = 3, vertex.label = NA, ...)
}
plot_er(n, p = 0.8 * p_star)
plot_er(n, p = 1.2 * p_star)

## ----connectedness-threshold, fig.cap="Simulation of connectedness of ER random graphs on 1,000 vertices.", message=FALSE----
n <- 1000
p_star <- log(n)/n
ps <- rep(seq(from = 0, to = 2 * p_star, by = 0.001), each = 100)
er_connected <- function(n, p, ...) {
  c(n = n, p = p, connected = is.connected(erdos.renyi.game(n, p)))
}
sims <- as.data.frame(t(sapply(ps, er_connected, n = n)))
ggplot(data = sims, aes(x = p, y = connected)) + 
  geom_vline(xintercept = p_star, color = "darkgray") + 
  geom_text(x = p_star, y = 0.9, label = "Threshold value", hjust="right") + 
  labs(x = "Probability of edge existing", 
       y = "Probability that random graph is connected") +
  geom_count() + geom_smooth()

## ----eval=FALSE----------------------------------------------------------
## g <- watts.strogatz.game(n)

## ----degree-dist, warning=FALSE, fig.cap="Degree distribution for two random graphs."----
g1 <- erdos.renyi.game(n, p = log(n)/n)
g2 <- barabasi.game(n, m = 3, directed = FALSE)
summary(g1)
summary(g2)
d <- data.frame(type = rep(c("Erdos-Renyi", "Barabasi-Albert"), each = n),
                degree = c(degree(g1), degree(g2)))
ggplot(data = d, aes(x = degree, color = type)) + 
  geom_density(size = 2) + 
  scale_x_continuous(limits = c(0, 25))

## ----eval=TRUE,message=FALSE---------------------------------------------
library(mdsr)
db <- src_scidb("imdb")

## ----message=FALSE-------------------------------------------------------
sql <- 
  "SELECT a.person_id as src, b.person_id as dest, 
    a.movie_id, 
    a.nr_order * b.nr_order as weight, 
    t.title, idx.info as ratings
  FROM imdb.cast_info a 
    CROSS JOIN imdb.cast_info b USING (movie_id)
    LEFT JOIN imdb.title t ON a.movie_id = t.id
    LEFT JOIN imdb.movie_info_idx idx ON idx.movie_id = a.movie_id
  WHERE t.production_year = 2012 AND t.kind_id = 1
    AND info_type_id = 100 AND idx.info > 125000
    AND a.nr_order <= 20 AND b.nr_order <= 20
    AND a.role_id IN (1,2) AND b.role_id IN (1,2)
    AND a.person_id < b.person_id
  GROUP BY src, dest, movie_id"
E <- DBI::dbGetQuery(db$con, sql) %>%
  mutate(ratings = as.numeric(ratings))
glimpse(E)

## ------------------------------------------------------------------------
nrow(E)
length(unique(E$title))

## ------------------------------------------------------------------------
E %>%
  group_by(movie_id) %>%
  summarize(title = max(title), N = n(), numRatings = max(ratings)) %>%
  arrange(desc(numRatings))

## ------------------------------------------------------------------------
actor_ids <- unique(c(E$src, E$dest))
V <- db %>%
  tbl("name") %>%
  filter(id %in% actor_ids) %>%
  select(id, name) %>%
  rename(actor_name = name) %>%
  collect() %>%
  arrange(id)
glimpse(V)

## ----message=FALSE,warning=FALSE-----------------------------------------
library(igraph)
g <- graph_from_data_frame(E, directed = FALSE, vertices = V)
summary(g)

## ------------------------------------------------------------------------
g <- set_vertex_attr(g, "imdbId", value = V(g)$name)

## ----hollywood, fig.cap="Visualization of Hollywood network for popular 2012 movies.", eval=TRUE, fig.width=10, fig.height=10----
plot(g, edge.color = "lightgray", vertex.size = 2, vertex.label = NA)

## ----most-connected------------------------------------------------------
g <- set_vertex_attr(g, "degree", value = degree(g))
as_data_frame(g, what = "vertices") %>%
  arrange(desc(degree)) %>%
  head()

## ----getMovies-----------------------------------------------------------
getMovies <- function(imdbId, E) {
  E %>%
    filter(src == imdbId | dest == imdbId) %>%
    tally(~ title, data = .)
}
getMovies(439008, E)

## ----crans, message=FALSE, warning=FALSE---------------------------------
ggplot(data = data.frame(degree = degree(g)), aes(x = degree)) + 
  geom_density(size = 2)

## ----hollywood-ggnetwork, warning=FALSE, message=FALSE-------------------
library(ggnetwork)
g_df <- ggnetwork(g)
hollywood <- ggplot(g_df, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(size = weight), color = "lightgray") +
  geom_nodes(aes(color = degree), alpha = 0.6) +
  scale_size_continuous(range = c(0.001, 0.2)) + 
  theme_blank()

## ----ggplot-network, fig.height = 10, fig.cap="The Hollywood network for popular 2012 movies, in \\pkg{ggplot2}"----
hollywood + 
  geom_nodetext(aes(label = gsub(", ", ",\n", actor_name)), 
                data = subset(g_df, degree > 40))

## ----btw, warning=FALSE--------------------------------------------------
g <- g %>%
  set_vertex_attr("btw", value = igraph::betweenness(g, normalized = TRUE))
get.data.frame(g, what = "vertices") %>%
  arrange(desc(btw)) %>%
  head()
getMovies(3443577, E)

## ----jgl-----------------------------------------------------------------
ks <- V(g)[actor_name == "Stewart, Kristen"]
ct <- V(g)[actor_name == "Theron, Charlize"]
p <- shortest_paths(g, from = ks, to = ct, weights = NA, output = "epath")
edge_attr(g, "title", index = p$epath[[1]])

## ----ks------------------------------------------------------------------
jgl <- V(g)[actor_name == "Gordon-Levitt, Joseph"]
p <- shortest_paths(g, from = jgl, to = ks, weights = NA, output = "both")
vertex_attr(g, "actor_name", index = p$vpath[[1]])
edge_attr(g, "title", index = p$epath[[1]])

## ------------------------------------------------------------------------
length(all_shortest_paths(g, from = ks, to = jgl, weights = NA)$res)

## ----diameter------------------------------------------------------------
diameter(g, weights = NA)
eccentricity(g, vids = ks)

## ----eval=FALSE----------------------------------------------------------
## prefix <- "https://www.kaggle.com/c/march-machine-learning-mania-2015"
## url_teams <- paste(prefix, "download/teams.csv", sep = "/")
## url_games <- paste(prefix,
##                    "download/regular_season_compact_results.csv", sep = "/")
## download.file(url_teams, destfile = "data/teams.csv")
## download.file(url_games, destfile = "data/games.csv")

## ----message=FALSE-------------------------------------------------------
library(mdsr)
teams <- readr::read_csv("data/teams.csv")
games <- readr::read_csv("data/games.csv") %>%
  filter(season == 1996)
dim(games)

## ------------------------------------------------------------------------
E <- games %>%
  mutate(score_ratio = wscore/lscore) %>%
  select(lteam, wteam, score_ratio)
V <- teams %>%
  filter(team_id %in% unique(c(E$lteam, E$wteam)))
library(igraph)
g <- graph_from_data_frame(E, directed = TRUE, vertices = V)
summary(g)

## ------------------------------------------------------------------------
g <- set_vertex_attr(g, "pagerank", value = page_rank(g)$vector)
as_data_frame(g, what = "vertices") %>%
  arrange(desc(pagerank)) %>%
  head(20)

## ------------------------------------------------------------------------
wins <- E %>%
  group_by(wteam) %>%
  summarise(N = n())
losses <- E %>%
  group_by(lteam) %>%
  summarise(N = n())
wins %>%
  full_join(losses, by = c("wteam" = "lteam")) %>%
  left_join(teams, by = c("wteam" = "team_id")) %>%
  rename(wins = N.x, losses = N.y) %>%
  mutate(win_pct = wins / (wins + losses)) %>%
  arrange(desc(win_pct)) %>%
  head(20)

## ------------------------------------------------------------------------
E %>%
  filter(wteam == 1269 & lteam == 1246)

## ------------------------------------------------------------------------
E %>%
  filter(lteam %in% c(1203, 1269) & wteam %in% c(1203, 1269))

## ------------------------------------------------------------------------
A_10 <- c("Massachusetts", "Temple", "G Washington", "Rhode Island", 
          "St Bonaventure", "St Joseph's PA", "Virginia Tech", "Xavier", 
          "Dayton", "Duquesne", "La Salle", "Fordham")

## ------------------------------------------------------------------------
a10 <- V(g)[ team_name %in% A_10 ]
a <- induced_subgraph(g, vids = a10)
a <- set_vertex_attr(a, "pagerank", value = page_rank(a)$vector)
summary(a)

## ----a10, fig.height=10, warning=FALSE, fig.cap="Atlantic 10 Conference network, NCAA men's basketball, 1995--1996."----
library(ggnetwork)
a_df <- ggnetwork(a)
ggplot(a_df, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(alpha = score_ratio), color = "lightgray", 
             arrow = arrow(length = unit(0.2, "cm")), 
             curvature = 0.2) +
  geom_nodes(aes(size = pagerank, color = pagerank), alpha = 0.6) +
  geom_nodetext(aes(label = team_name)) + 
  scale_alpha_continuous(range = c(0.4, 1)) + 
  scale_size_continuous(range = c(1, 10)) + 
  guides(color = guide_legend("PageRank"), size=guide_legend("PageRank")) + 
  theme_blank()

## ------------------------------------------------------------------------
P <- t(as_adjacency_matrix(a, sparse = FALSE, attr = "score_ratio"))

## ------------------------------------------------------------------------
P <- scale(P, center = FALSE, scale = colSums(P))
round(P, 2)

## ------------------------------------------------------------------------
v0 <- rep(1, vcount(a)) / vcount(a)
v0

## ------------------------------------------------------------------------
v <- v0
for (i in 1:20) {
  v <- P %*% v
}
as.vector(v)

## ------------------------------------------------------------------------
page_rank(a)$vector

## ------------------------------------------------------------------------
page_rank(a, damping = 1)$vector

## ------------------------------------------------------------------------
w <- v0
d <- 0.85
for (i in 1:20) {
  w <- d * P %*% w + (1 - d) * v0
}
as.vector(w)
page_rank(a, damping = 0.85)$vector

