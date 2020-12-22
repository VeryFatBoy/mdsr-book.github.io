## ----er-graphs, warning = FALSE, message=FALSE, fig.show='hold', fig.cap="Two Erdős--Rényi random graphs on 100 vertices with different values of $p$. The graph at left is not connected, but the graph at right is. The value of $p$ hasn't changed by much.", fig.subcap=c("A graph that is not connected.", "A connected graph."), out.width='49%'----
library(tidyverse)
library(mdsr)
library(tidygraph)
library(ggraph)
set.seed(21)
n <- 100
p_star <- log(n)/n

plot_er <- function(n, p) {
  g <- play_erdos_renyi(n, p, directed = FALSE)
  ggraph(g) + 
    geom_edge_fan(width = 0.1) + 
    geom_node_point(size = 3, color = "dodgerblue") + 
    labs(
      title = "Erdős--Rényi random graph", 
      subtitle = paste0("n = ", n, ", p = ", round(p, 4))
    ) + 
    theme_void()
}
plot_er(n, p = 0.8 * p_star)
plot_er(n, p = 1.2 * p_star)


## ----connectedness-threshold, fig.cap="Simulation of connectedness of ER random graphs on 1,000 vertices.", message=FALSE----
n <- 1000
p_star <- log(n)/n
p <- rep(seq(from = 0, to = 2 * p_star, by = 0.001), each = 100)

sims <- tibble(n, p) %>%
  mutate(
    g = map2(n, p, play_erdos_renyi, directed = FALSE), 
    is_connected = map_int(g, ~with_graph(., graph_is_connected()))
  )

ggplot(data = sims, aes(x = p, y = is_connected)) + 
  geom_vline(xintercept = p_star, color = "darkgray") + 
  geom_text(
    x = p_star, y = 0.9, label = "Threshold value", hjust = "right"
  ) + 
  labs(
    x = "Probability of edge existing", 
    y = "Probability that random graph is connected"
  ) +
  geom_count() + 
  geom_smooth()


## ----eval=FALSE---------------------------------------------------------------
## g <- play_smallworld(n_dim = 2, dim_size = 10, order = 5, p_rewire = 0.05)


## ----degree-dist, warning=FALSE, fig.cap="Degree distribution for two random graphs."----
g1 <- play_erdos_renyi(n, p = log(n)/n, directed = FALSE)
g2 <- play_barabasi_albert(n, power = 1, growth = 3, directed = FALSE)
summary(g1)
summary(g2)

d <- tibble(
  type = c("Erdos-Renyi", "Barabasi-Albert"),
  graph = list(g1, g2)
) %>%
  mutate(node_degree = map(graph, ~with_graph(., centrality_degree()))) %>%
  unnest(node_degree)

ggplot(data = d, aes(x = node_degree, color = type)) + 
  geom_density(size = 2) + 
  scale_x_continuous(limits = c(0, 25))


## ----eval=TRUE, message=FALSE-------------------------------------------------
library(mdsr)
db <- dbConnect_scidb("imdb")


## SELECT a.person_id AS src, b.person_id AS dest,

##     a.movie_id,

##     a.nr_order * b.nr_order AS weight,

##     t.title, idx.info AS ratings

##   FROM imdb.cast_info AS a

##     CROSS JOIN imdb.cast_info AS b USING (movie_id)

##     LEFT JOIN imdb.title AS t ON a.movie_id = t.id

##     LEFT JOIN imdb.movie_info_idx AS idx ON idx.movie_id = a.movie_id

##   WHERE t.production_year = 2012 AND t.kind_id = 1

##     AND info_type_id = 100 AND idx.info > 150000

##     AND a.nr_order <= 20 AND b.nr_order <= 20

##     AND a.role_id IN (1,2) AND b.role_id IN (1,2)

##     AND a.person_id < b.person_id

##   GROUP BY src, dest, movie_id


## -----------------------------------------------------------------------------
E <- E %>%
  mutate(ratings = parse_number(ratings))
glimpse(E)


## -----------------------------------------------------------------------------
E %>%
  summarize(
    num_rows = n(), 
    num_titles = n_distinct(title)
  )


## -----------------------------------------------------------------------------
movies <- E %>%
  group_by(movie_id) %>%
  summarize(title = max(title), N = n(), numRatings = max(ratings)) %>%
  arrange(desc(numRatings))
movies


## -----------------------------------------------------------------------------
actor_ids <- unique(c(E$src, E$dest))
V <- db %>%
  tbl("name") %>%
  filter(id %in% actor_ids) %>%
  select(actor_id = id, actor_name = name) %>%
  collect() %>%
  arrange(actor_id) %>%
  mutate(id = row_number())
glimpse(V)


## ----message=FALSE,warning=FALSE----------------------------------------------
edges <- E %>%
  left_join(select(V, from = id, actor_id), by = c("src" = "actor_id")) %>%
  left_join(select(V, to = id, actor_id), by = c("dest" = "actor_id"))

g <- tbl_graph(nodes = V, directed = FALSE, edges = edges)
summary(g)


## ----hollywood, message=FALSE, fig.cap="Visualization of Hollywood network for popular 2012 movies.", eval=TRUE, fig.asp=1----
ggraph(g, 'drl') +
  geom_edge_fan(width = 0.1) + 
  geom_node_point(color = "dodgerblue") + 
  theme_void()


## ----most-connected-----------------------------------------------------------
g <- g %>%
  mutate(degree = centrality_degree())
g %>%
  as_tibble() %>%
  arrange(desc(degree)) %>%
  head()


## ---- include=FALSE-----------------------------------------------------------
cranston <- g %>%
  as_tibble() %>%
  arrange(desc(degree)) %>%
  head(1)


## ----getMovies----------------------------------------------------------------
show_movies <- function(g, id) {
  g %>%
    activate(edges) %>%
    as_tibble() %>%
    filter(src == id | dest == id) %>%
    group_by(movie_id) %>%
    summarize(title = first(title), num_connections = n())
}
show_movies(g, 502126)


## ----crans, message=FALSE, warning=FALSE, fig.cap="Distribution of degrees for actors in the Hollywood network of popular 2012 movies."----
ggplot(data = enframe(igraph::degree(g)), aes(x = value)) + 
  geom_density(size = 2)


## ----hollywood-ggnetwork, warning=FALSE, message=FALSE------------------------
hollywood <- ggraph(g, layout = 'drl') +
  geom_edge_fan(aes(alpha = weight), color = "lightgray") +
  geom_node_point(aes(color = degree), alpha = 0.6) +
  scale_edge_alpha_continuous(range = c(0, 1)) + 
  scale_color_viridis_c() + 
  theme_void()


## ----ggplot-network, fig.asp=1, fig.cap="The Hollywood network for popular 2012 movies. Color is mapped to degree centrality. "----
hollywood + 
  geom_node_label(
    aes(
      filter = degree > 40, 
      label = str_replace_all(actor_name, ", ", ",\n")
    ), 
    repel = TRUE
  )


## ----btw, warning=FALSE-------------------------------------------------------
g <- g %>%
  mutate(btw = centrality_betweenness(weights = weight, normalized = TRUE))
g %>%
  as_tibble() %>%
  arrange(desc(btw)) %>%
  head(10)
show_movies(g, 3945132)


## ----jgl----------------------------------------------------------------------
ks <- V %>%
  filter(actor_name == "Stewart, Kristen")
ct <- V %>%
  filter(actor_name == "Theron, Charlize")

g %>%
  convert(to_shortest_path, from = ks$id, to = ct$id)


## ----ks, warning=FALSE, fig.cap="Subgraph showing a shortest path through the Hollywood network from Joseph Gordon-Levitt to Kristen Stewart."----
set.seed(47)
jgl <- V %>%
  filter(actor_name == "Gordon-Levitt, Joseph")

h <- g %>%
  convert(to_shortest_path, from = jgl$id, to = ks$id, weights = NA)

h %>%
  ggraph('gem') +
  geom_node_point() + 
  geom_node_label(aes(label = actor_name)) + 
  geom_edge_fan2(aes(label = title)) + 
  coord_cartesian(clip = "off") + 
  theme(plot.margin = margin(6, 36, 6, 36))


## -----------------------------------------------------------------------------
igraph::all_shortest_paths(g, from = ks$id, to = jgl$id, weights = NA) %>%
  pluck("res") %>%
  length()


## ----diameter-----------------------------------------------------------------
igraph::diameter(g, weights = NA)
g %>%
  mutate(eccentricity = node_eccentricity()) %>%
  filter(actor_name == "Stewart, Kristen")


## ----eval=FALSE---------------------------------------------------------------
## prefix <- "https://www.kaggle.com/c/march-machine-learning-mania-2015"
## url_teams <- paste(prefix, "download/teams.csv", sep = "/")
## url_games <- paste(
##   prefix,
##   "download/regular_season_compact_results.csv", sep = "/"
## )
## download.file(url_teams, destfile = "data/teams.csv")
## download.file(url_games, destfile = "data/games.csv")


## ----message=FALSE------------------------------------------------------------
library(mdsr)
teams <- readr::read_csv("data/teams.csv")
games <- readr::read_csv("data/games.csv") %>%
  filter(season == 1996)
dim(games)


## ---- message=FALSE-----------------------------------------------------------
E <- games %>%
  mutate(score_ratio = wscore/lscore) %>%
  select(lteam, wteam, score_ratio)
V <- teams %>%
  filter(team_id %in% unique(c(E$lteam, E$wteam)))

g <- igraph::graph_from_data_frame(E, directed = TRUE, vertices = V) %>%
  as_tbl_graph() %>%
  mutate(team_id = parse_number(name))
summary(g)


## -----------------------------------------------------------------------------
g <- g %>%
  mutate(pagerank = centrality_pagerank())
g %>%
  as_tibble() %>%
  arrange(desc(pagerank)) %>%
  head(20)


## -----------------------------------------------------------------------------
wins <- E %>%
  group_by(wteam) %>%
  summarize(W = n())
losses <- E %>%
  group_by(lteam) %>%
  summarize(L = n())

g <- g %>%
  left_join(wins, by = c("team_id" = "wteam")) %>%
  left_join(losses, by = c("team_id" = "lteam")) %>%
  mutate(win_pct = W / (W + L))
g %>%
  as_tibble() %>%
  arrange(desc(win_pct)) %>%
  head(20)

g %>%
  as_tibble() %>%
  summarize(pr_wpct_cor = cor(pagerank, win_pct, use = "complete.obs"))


## -----------------------------------------------------------------------------
E %>%
  filter(wteam == 1269 & lteam == 1246)


## -----------------------------------------------------------------------------
E %>%
  filter(lteam %in% c(1203, 1269) & wteam %in% c(1203, 1269))


## -----------------------------------------------------------------------------
A_10 <- c("Massachusetts", "Temple", "G Washington", "Rhode Island", 
          "St Bonaventure", "St Joseph's PA", "Virginia Tech", "Xavier", 
          "Dayton", "Duquesne", "La Salle", "Fordham")


## -----------------------------------------------------------------------------
a10 <- g %>%
  filter(team_name %in% A_10) %>%
  mutate(pagerank = centrality_pagerank())
summary(a10)


## ----a10, fig.asp=1, warning=FALSE, fig.cap="Atlantic 10 Conference network, NCAA men's basketball, 1995--1996."----
library(ggraph)
ggraph(a10, layout = 'kk') +
  geom_edge_arc(
    aes(alpha = score_ratio), color = "lightgray", 
    arrow = arrow(length = unit(0.2, "cm")), 
    end_cap = circle(1, 'cm'), 
    strength = 0.2
  ) +
  geom_node_point(aes(size = pagerank, color = pagerank), alpha = 0.6) +
  geom_node_label(aes(label = team_name), repel = TRUE) + 
  scale_alpha_continuous(range = c(0.4, 1)) + 
  scale_size_continuous(range = c(1, 10)) + 
  guides(
    color = guide_legend("PageRank"), 
    size = guide_legend("PageRank")
  ) + 
  theme_void()


## -----------------------------------------------------------------------------
P <- a10 %>%
  igraph::as_adjacency_matrix(sparse = FALSE, attr = "score_ratio") %>%
  t()


## -----------------------------------------------------------------------------
P <- scale(P, center = FALSE, scale = colSums(P))
round(P, 2)


## -----------------------------------------------------------------------------
num_vertices <- nrow(as_tibble(a10))
v0 <- rep(1, num_vertices) / num_vertices
v0


## -----------------------------------------------------------------------------
v <- v0
for (i in 1:20) {
  v <- P %*% v
}
as.vector(v)


## -----------------------------------------------------------------------------
igraph::page_rank(a10)$vector


## -----------------------------------------------------------------------------
igraph::page_rank(a10, damping = 1)$vector


## -----------------------------------------------------------------------------
w <- v0
d <- 0.85
for (i in 1:20) {
  w <- d * P %*% w + (1 - d) * v0
}
as.vector(w)
igraph::page_rank(a10, damping = 0.85)$vector

