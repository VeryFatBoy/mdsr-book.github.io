## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
options(digits=4)
fig.path='figures/datavizII-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----mdsr, message=FALSE-------------------------------------------------
library(mdsr)

## ----echo=FALSE, results="asis", message=FALSE---------------------------
library(xtable)
CIACountries %>%
  select(-area, -pop) %>%
  head() %>%
  xtable(label = "tab:countrydata", caption = "A selection of variables from the first six rows of the \\data{CIACountries} data table.") %>%
  print(include.rownames = FALSE)

## ----simple-glyph, warning=FALSE, echo=TRUE------------------------------
g <- ggplot(data = CIACountries, aes(y = gdp, x = educ))
g + geom_point(size = 3)

## ----net-use-color, warning=FALSE, echo=TRUE-----------------------------
g + geom_point(aes(color = net_users), size = 3)

## ----country-labels, warning=FALSE, echo=TRUE----------------------------
g + geom_text(aes(label = country, color = net_users), size = 3)

## ----four-variables, warning=FALSE, echo=TRUE----------------------------
g + geom_point(aes(color = net_users, size = roadways))

## ----graphics-frame, include=FALSE, fig.margin=TRUE, fig.cap="A graphics frame set by the \\cmd{GDP} and \\cmd{roadway} variables. No glyphs have been set in this frame."----
#ggplot(data = CIACountries, aes(y = gdp, x = roadways)) + 
#  scale_x_log10() + scale_y_log10() +
#  geom_blank()

## ----log-scale, warning=FALSE,echo=TRUE----------------------------------
g + geom_point(aes(color = net_users, size = roadways)) + 
  coord_trans(y = "log10")

## ----log-scale2, warning=FALSE,echo=TRUE, eval=FALSE---------------------
## g + geom_point(aes(color = net_users, size = roadways)) +
##   scale_y_continuous(name = "Gross Domestic Product", trans = "log10")

## ----facet-internet, warning=FALSE, echo=TRUE, message=FALSE-------------
g + geom_point(alpha = 0.9, aes(size = roadways)) + coord_trans(y="log10") + 
  facet_wrap(~net_users, nrow = 1) + theme(legend.position = "top")

## ------------------------------------------------------------------------
data(MedicareCharges)
ChargesNJ <- MedicareCharges %>% filter(stateProvider == "NJ")

## ----drg-NJ, echo=FALSE,warning=FALSE, message=FALSE, results='asis'-----
set.seed(101)
ChargesNJ %>% 
  head(10) %>%
  xtable(caption = "Glyph-ready data for the barplot layer in Figure \\ref{fig:compare-NJ}.", 
         label = "tab:drg-NJ") %>%
  print(include.rownames = FALSE)

## ----compare-NJ, echo=TRUE-----------------------------------------------
p <- ggplot(data = ChargesNJ, 
            aes(x = reorder(drg, mean_charge), y = mean_charge)) +
  geom_bar(fill = "gray", stat = "identity") +
  ylab("Statewide Average Charges ($)") + xlab("Medical Procedure (DRG)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

## ----compare-NJ-2, echo=TRUE---------------------------------------------
p + geom_point(data = MedicareCharges, size = 1, alpha = 0.3) 

## ------------------------------------------------------------------------
g <- ggplot(data = SAT_2010, aes(x = math))

## ----SAT-1---------------------------------------------------------------
g + geom_histogram(binwidth = 10)

## ----SAT-2---------------------------------------------------------------
g + geom_density(adjust = 0.3)

## ----eval=FALSE,echo=FALSE, fig.margin=TRUE, fig.cap="\\label{fig:histA} A histogram."----
## #ggplot(NHANES, aes(x=height)) +
## #    geom_histogram(aes(y = ..count..),
## #                   binwidth=0.1, color="black", fill=NA) +
## #    xlab("Height (m)") + ylab("Count")
## 

## ----bar2----------------------------------------------------------------
ggplot(data = head(SAT_2010, 10), aes(x = reorder(state, math), y = math)) +
  geom_bar(stat = "identity")

## ----stacked-bar---------------------------------------------------------
ggplot(data = HELPrct, aes(x = homeless)) + 
  geom_bar(aes(fill = substance), position = "fill") + 
  coord_flip()

## ----basic-scatterplot---------------------------------------------------
g <- ggplot(data = SAT_2010, aes(x = expenditure, y = math)) + geom_point()

## ------------------------------------------------------------------------
g <- g + geom_smooth(method = "lm", se = 0) + 
  xlab("Average expenditure per student ($1000)") +
  ylab("Average score on math SAT")

## ----echo=TRUE-----------------------------------------------------------
SAT_2010 <- SAT_2010 %>%
  mutate(SAT_rate = cut(sat_pct, breaks = c(0,30,60,100), 
    labels = c("low", "medium", "high")))
g <- g %+% SAT_2010

## ----groups-color--------------------------------------------------------
g + aes(color = SAT_rate)

## ----bar-facet-----------------------------------------------------------
g + facet_wrap(~ SAT_rate)

## ----NHANESheightage, echo=TRUE,message=FALSE,warning=FALSE--------------
library(NHANES)
ggplot(data = sample_n(NHANES, size = 1000), 
  aes(x = Age, y = Height, color = Gender)) + 
  geom_point() + geom_smooth() + xlab("Age (years)") + ylab("Height (cm)")

## ----macleishplot,message=FALSE------------------------------------------
library(macleish)
ggplot(data = whately_2015, aes(x = when, y = temperature)) + 
  geom_line(color = "darkgray") + geom_smooth() + 
  xlab(NULL) + ylab("Temperature (degrees Fahrenheit)")

## ------------------------------------------------------------------------
favstats(length ~ sex, data = KidsFeet)

## ----kidsfeetbox,eval=TRUE, message=FALSE--------------------------------
ggplot(data = KidsFeet, aes(x = sex, y = length)) + geom_boxplot()

## ----NHANESsmoke,echo=FALSE----------------------------------------------
library(NHANES)
NHANES2 <- NHANES %>%
  filter(Age > 19)
NHANES2 <- mutate(NHANES2, 
  Smoke = derivedFactor(
    Ever = SmokeNow=="Yes" | SmokeNow=="No",
    Never = Smoke100=="No"),
  AgeDecade = droplevels(AgeDecade)
)
NHANES2 <- NHANES2 %>%
  select(AgeDecade, Gender, Diabetes, BMI_WHO, PhysActive, Smoke) %>% 
  na.omit()
mosaicplot(~ AgeDecade + BMI_WHO + Diabetes, shade = TRUE, data = NHANES2)

## ----mosaicplot, echo=FALSE, eval=FALSE, fig.cap = "Mosaic plot illustrating the relationship between smoking status and living status for participants in the NHANES study. The area of each box is proportional to the number of people who share those two characteristics. "----
## #mosaicplot(smoker ~ isAlive, data = dtkNHANES, main = "Smoker Status vs. Living Status")

## ----oil-map, warning=FALSE,message=FALSE,echo=FALSE, fig.margin=TRUE,fig.cap="A choropleth map displaying oil production by countries around the world in barrels per day. "----
# Country outline
CIACountries %>%
  select(country, oil_prod) %>%
  mutate(oil_prod_disc = cut(oil_prod, 
                             breaks = c(0, 1e3, 1e5, 1e6, 1e7, 1e8), 
                             labels = c(">1000", ">10,000", ">100,000", ">1 million", ">10 million"))) %>%
mosaic::mWorldMap(key = "country") +
  geom_polygon(aes(fill = oil_prod_disc)) + 
  scale_fill_brewer("Oil Prod. (bbl/day)", na.value = "white") +
  theme(legend.position = "top")

## ----cancer-network, echo=FALSE, warning=FALSE, message=FALSE, fig.margin=TRUE, fig.cap="A network diagram displaying the relationship between types of cancer cell lines."----
# An example
library(igraph)
library(mdsr)
CellEdges <- Cancer
SmallEdges <- head(CellEdges,200)
#VV <- mdsr::edgesToVertices( SmallEdges, 
#                       from=cellLine, to=otherCellLine )
g <- SmallEdges %>%
  select(cellLine, otherCellLine, correlation) %>%
  graph.data.frame(directed = FALSE)
V(g)$type <- substr(V(g)$name, 0, 2)

# devtools::install_github("briatte/ggnetwork")
library(ggnetwork)
g_df <- ggnetwork(g)
ggplot(g_df, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(size = correlation), color = "lightgray", curvature = 0.2) +
  geom_nodes(aes(color = type), size = 10, alpha = 0.6) +
  geom_nodetext(aes(label = type)) + 
  scale_size_continuous(range = c(0.1, 1)) + 
  theme_blank()
# 
# xy <- layout.fruchterman.reingold(g)
# g <- g %>%
#   set_vertex_attr("x", value = xy[,1]) %>%
#   set_vertex_attr("y", value = xy[,2]) %>%
#   set_vertex_attr("width", value = 1:vcount(g)) %>%
#   set_vertex_attr("color", value = runif(vcount(g))) %>%
#   set_vertex_attr("type", value = substr(get.vertex.attribute(g, "name"), 0, 2))
# 
# # get.data.frame(g, what = "vertices")
# V <- get.data.frame(g, what = "vertices")
# E <- get.data.frame(g, what = "edges")
# plot(g, layout = layout_with_fr, 
#      edge.width = 2*E$correlation, vertex.label = V$type, 
#      vertex.color = factor(V$type),
#      vertex.size = 15, vertex.label.family = "sans", vertex.label.cex = 0.5)
# 
# EE <- mdsr::edgesForPlotting( VV, ID=ID, x=x, y=y,
#                         SmallEdges, 
#                         from=cellLine, to=otherCellLine) 
# 
# VV$type <- substr(VV$ID,0,2)                       
# ggplot(EE, aes(x=x,y=y)) + 
#   geom_segment(size=2,
#                aes(
#                  xend=xend,yend=yend,
#                  alpha=abs(correlation)),
#                ) + 
#   geom_point(data=VV,aes(x=x,y=y,color=type,size=size),
#              size=12,fill="lightgray",alpha=.4) +
#   geom_text( data=VV, aes(x=x,y=y, label=type)) +
#   theme(legend.position="none")

## ------------------------------------------------------------------------
library(babynames)
BabynamesDist <- make_babynames_dist()
head(BabynamesDist, 2)

## ----eval=FALSE----------------------------------------------------------
## BabynamesDist %>% filter(name == "Benjamin")

## ----name-plot-----------------------------------------------------------
joseph <- BabynamesDist %>%
  filter(name == "Joseph" & sex == "M")
name_plot <- ggplot(data = joseph, aes(x = year))

## ------------------------------------------------------------------------
name_plot <- name_plot +
  geom_bar(stat = "identity", aes(y = count_thousands * alive_prob), 
            fill = "#b2d7e9", colour = "white")

## ------------------------------------------------------------------------
name_plot <- name_plot + geom_line(aes(y = count_thousands), size = 2)

## ------------------------------------------------------------------------
name_plot <- name_plot +
  ylab("Number of People (thousands)") + xlab(NULL)

## ------------------------------------------------------------------------
summary(name_plot)

## ------------------------------------------------------------------------
wtd.quantile <- Hmisc::wtd.quantile
median_yob <- 
  with(joseph, wtd.quantile(year, est_alive_today, probs = 0.5))
median_yob

## ------------------------------------------------------------------------
name_plot <- name_plot +
  geom_bar(stat = "identity", colour = "white", fill = "#008fd5", 
           aes(y = ifelse(year == median_yob, est_alive_today / 1000, 0)))

## ----joseph, fig.height=6, fig.cap="Recreation of the age distribution of ``Joseph'' plot."----
name_plot +
  ggtitle("Age Distribution of American Boys Named Joseph") + 
  geom_text(x = 1935, y = 40, label = "Number of Josephs\nborn each year") + 
  geom_text(x = 1915, y = 13, label = 
    "Number of Josephs\nborn each year\nestimated to be alive\non 1/1/2014", 
    colour = "#b2d7e9") + 
  geom_text(x = 2003, y = 40, 
    label = "The median\nliving Joseph\nis 37 years old", 
            colour = "darkgray") + 
  geom_curve(x = 1995, xend = 1974, y = 40, yend = 24, 
    arrow = arrow(length = unit(0.3,"cm")), curvature = 0.5) + ylim(0, 42)

## ----josephine, fig.cap="Age distribution of American girls named ``Josephine''."----
name_plot %+% filter(BabynamesDist, name == "Josephine" & sex == "F")

## ----jessie, fig.cap="Comparison of the name ``Jessie'' across two genders."----
names_plot <- name_plot + facet_wrap(~sex)
names_plot %+% filter(BabynamesDist, name == "Jessie")

## ----many-names, fig.cap="Gender breakdown for the three most ``unisex'' names."----
many_names_plot <- name_plot + facet_grid(name ~ sex)
mnp <- many_names_plot %+% filter(BabynamesDist, name %in% 
  c("Jessie", "Marion", "Jackie"))
mnp

## ----many-names2, fig.cap="Gender breakdown for the three most ``unisex'' names, oriented vertically."----
mnp + facet_grid(sex ~ name)

## ----com_fem, message=FALSE----------------------------------------------
com_fem <- BabynamesDist %>%
  filter(sex == "F") %>% 
  group_by(name) %>%
  summarise(
    N = n(), est_num_alive = sum(est_alive_today),
    q1_age = wtd.quantile(age_today, est_alive_today, probs = 0.25),
    median_age = wtd.quantile(age_today, est_alive_today, probs = 0.5),
    q3_age = wtd.quantile(age_today, est_alive_today, probs = 0.75)) %>%
  arrange(desc(est_num_alive)) %>%
  head(25)

## ------------------------------------------------------------------------
w_plot <- ggplot(data = com_fem, aes(x = reorder(name, -median_age), 
  y = median_age)) + xlab(NULL) + ylab("Age (in years)") + 
  ggtitle("Median ages for females with the 25 most common names")

## ------------------------------------------------------------------------
w_plot <- w_plot + geom_linerange(aes(ymin = q1_age, ymax = q3_age), 
  color = "#f3d478", size = 10, alpha = 0.8)

## ------------------------------------------------------------------------
w_plot <- w_plot +
  geom_point(fill = "#ed3324", colour = "white", size = 4, shape = 21)

## ----women, fig.height=10, fig.cap="Recreation of FiveThirtyEight's plot of the age distributions for the 25 most common women's names."----
w_plot + 
  geom_point(aes(y = 55, x = 24), fill = "#ed3324", colour = "white", 
    size = 4, shape = 21) + 
  geom_text(aes(y = 58, x = 24, label = "median")) + 
  geom_text(aes(y = 26, x = 16, label = "25th")) + 
  geom_text(aes(y = 51, x = 16, label = "75th percentile")) + 
  geom_point(aes(y = 24, x = 16), shape = 17) + 
  geom_point(aes(y = 56, x = 16), shape = 17) +
  coord_flip()

