## ----cache=FALSE, echo=FALSE,include=FALSE------------------------------------
knitr::opts_chunk$set(error = TRUE)


## ----mdsr, message=FALSE------------------------------------------------------
library(mdsr)
library(tidyverse)


## ----countrydata, echo=FALSE, message=FALSE-----------------------------------
CIACountries %>%
  select(-area, -pop) %>%
  head() %>%
  mdsr_table(caption = "A selection of variables from the first six rows of the CIA countries data table.") %>%
  kableExtra::column_spec(1, width = "10em")


## ----simple-glyph, warning=FALSE, echo=TRUE, fig.cap="Scatterplot using only the position aesthetic for glyphs."----
g <- ggplot(data = CIACountries, aes(y = gdp, x = educ))
g + geom_point(size = 3)


## ----net-use-color, warning=FALSE, echo=TRUE, fig.cap="(ref:net-use-color-cap)"----
g + geom_point(aes(color = net_users), size = 3)


## ----country-labels, warning=FALSE, echo=TRUE, fig.cap="Scatterplot using both location and label as aesthetics."----
g + geom_text(aes(label = country, color = net_users), size = 3)


## ----four-variables, warning=FALSE, echo=TRUE, fig.cap="(ref:four-variables-cap)"----
g + geom_point(aes(color = net_users, size = roadways))


## ----log-scale, warning=FALSE,echo=TRUE, fig.cap="Scatterplot using a logarithmic transformation of GDP that helps to mitigate visual clustering caused by the right-skewed distribution of GDP among countries."----
g + 
  geom_point(aes(color = net_users, size = roadways)) + 
  coord_trans(y = "log10")


## ----log-scale2, warning=FALSE, echo=TRUE, eval=TRUE, fig.cap="(ref:log-scale2-cap)"----
g + 
  geom_point(aes(color = net_users, size = roadways)) + 
  scale_y_continuous(
    name = "Gross Domestic Product", 
    trans = "log10",
    labels = scales::comma
  )


## ----facet-internet, warning=FALSE, echo=TRUE, message=FALSE, fig.cap="Scatterplot using facets for different ranges of Internet connectivity."----
g + 
  geom_point(alpha = 0.9, aes(size = roadways)) + 
  coord_trans(y = "log10") + 
  facet_wrap(~net_users, nrow = 1) + 
  theme(legend.position = "top")


## -----------------------------------------------------------------------------
ChargesNJ <- MedicareCharges %>%
  filter(stateProvider == "NJ")


## ----drg-NJ, echo=FALSE,warning=FALSE, message=FALSE, results='asis'----------
set.seed(101)
ChargesNJ %>% 
  head(10) %>%
  mdsr_table(caption = "Glyph-ready data for the barplot layer.")


## ----compare-NJ, echo=TRUE, fig.cap="Bar graph of average charges for medical procedures in New Jersey."----
p <- ggplot(
  data = ChargesNJ, 
  aes(x = reorder(drg, mean_charge), y = mean_charge)
) +
  geom_col(fill = "gray") +
  ylab("Statewide Average Charges ($)") + 
  xlab("Medical Procedure (DRG)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5)))
p


## ----compare-NJ-2, echo=TRUE, fig.cap="Bar graph adding a second layer to provide a comparison of New Jersey to other states. Each dot represents one state, while the bars represent New Jersey."----
p + geom_point(data = MedicareCharges, size = 1, alpha = 0.3) 


## -----------------------------------------------------------------------------
g <- ggplot(data = SAT_2010, aes(x = math))


## ----SAT-1, fig.cap="Histogram showing the distribution of math SAT scores by state."----
g + geom_histogram(binwidth = 10) + labs(x = "Average Math SAT score")


## ----SAT-2, fig.cap="Density plot showing the distribution of average math SAT scores by state."----
g + geom_density(adjust = 0.3)


## ----bar2, fig.cap="A bar plot showing the distribution of average math SAT scores for a selection of states."----
ggplot(
  data = head(SAT_2010, 10), 
  aes(x = reorder(state, math), y = math)
) +
  geom_col() +
  labs(x = "State", y = "Average Math SAT score")


## ----stacked-bar, fig.cap="(ref:stacked-bar-cap)"-----------------------------
ggplot(data = mosaicData::HELPrct, aes(x = homeless)) + 
  geom_bar(aes(fill = substance), position = "fill") +
  scale_fill_brewer(palette = "Spectral") + 
  coord_flip()


## ----basic-scatterplot--------------------------------------------------------
g <- ggplot(
  data = SAT_2010, 
  aes(x = expenditure, y = math)
) + 
  geom_point()


## -----------------------------------------------------------------------------
g <- g + 
  geom_smooth(method = "lm", se = FALSE) + 
  xlab("Average expenditure per student ($1000)") +
  ylab("Average score on math SAT")


## ----echo=TRUE----------------------------------------------------------------
SAT_2010 <- SAT_2010 %>%
  mutate(
    SAT_rate = cut(
      sat_pct, 
      breaks = c(0, 30, 60, 100), 
      labels = c("low", "medium", "high")
    )
  )
g <- g %+% SAT_2010


## ----groups-color, fig.cap="(ref:groups-color-cap)", message=FALSE------------
g + aes(color = SAT_rate)


## ----bar-facet, fig.cap="(ref:bar-facet-cap)", message=FALSE------------------
g + facet_wrap(~ SAT_rate)


## ----NHANES-height-age, echo=TRUE, message=FALSE, warning=FALSE, fig.cap="(ref:nhanes-cap)"----
library(NHANES)
ggplot(
  data = slice_sample(NHANES, n = 1000), 
  aes(x = Age, y = Height, color = fct_relevel(Gender, "male"))
) + 
  geom_point() + 
  geom_smooth() + 
  xlab("Age (years)") + 
  ylab("Height (cm)") +
  labs(color = "Gender")


## ----time-series, message=FALSE, fig.cap="A time series showing the change in temperature at the MacLeish field station in 2015."----
library(macleish)
ggplot(data = whately_2015, aes(x = when, y = temperature)) + 
  geom_line(color = "darkgray") + 
  geom_smooth() + 
  xlab(NULL) + 
  ylab("Temperature (degrees Celsius)")


## ----render=knitr::normal_print-----------------------------------------------
whately_2015 %>%
  mutate(month = as.factor(lubridate::month(when, label = TRUE))) %>%
  group_by(month) %>% 
  skim(temperature) %>%
  select(-na)


## ----macleishbox, eval=TRUE, message=FALSE, fig.cap="A box-and-whisker of temperatures by month at the MacLeish field station."----
ggplot(
  data = whately_2015, 
  aes(
    x = lubridate::month(when, label = TRUE), 
    y = temperature
  )
) + 
  geom_boxplot() +
  xlab("Month") + 
  ylab("Temperature (degrees Celsius)")


## ----prepmosaic, echo=FALSE---------------------------------------------------
library(NHANES)
library(ggmosaic)
mosaic_to_plot <- NHANES %>%
  filter(Age > 19) %>%
  mutate(AgeDecade = droplevels(AgeDecade)) %>%
  select(AgeDecade, Diabetes, BMI_WHO) %>% 
  na.omit()


## ----NHANES-smoke, echo=FALSE, fig.cap="Mosaic plot (eikosogram) of diabetes by age and weight status (BMI).", message=FALSE----
ggplot(mosaic_to_plot) +
  geom_mosaic(
    aes(x = product(BMI_WHO, AgeDecade), fill = Diabetes)
  ) + 
  ylab("BMI") + 
  xlab("Age (by decade)") + 
  coord_flip()


## ----lattice, echo=FALSE------------------------------------------------------
func_table <- tribble(
  ~"response ($y$)", ~"explanatory ($x$)", ~"plot type", ~"geom_*()",
  "", "numeric", "histogram, density", paste(func('geom_histogram', FALSE), func('geom_density', FALSE), sep = ", "), 
  "", "categorical", "stacked bar", func('geom_bar', FALSE), 
  "numeric", "numeric", "scatter", func('geom_point', FALSE), 
  "numeric", "categorical", "box", func('geom_boxplot', FALSE), 
  "categorical", "categorical", "mosaic", func('geom_mosaic', FALSE)
) 

func_caption <- paste0(
  "Table of canonical data graphics and their corresponding ", 
  "**ggplot2**", 
  " commands. Note that ",
  #      func("geom_mosaic"), 
  "the mosaic plot function",
  " is not part of the ", 
  "**ggplot2**", " package."
)

if (knitr::is_latex_output()) {
  func_table %>%
    rename(`geom\\_*()` = `geom_*()`) %>%
    mutate(`geom\\_*()` = str_remove_all(`geom\\_*()`, "`")) %>%
    mutate(`geom\\_*()` = knitr:::escape_latex(`geom\\_*()`)) %>%
    mdsr_table(
      caption = str_replace_all(func_caption, "\\*\\*ggplot2\\*\\*", "\\\\textbf{ggplot2}"), escape = FALSE,
#      format = "latex"
    )
} else {
  func_table %>%
    mdsr_table(caption = func_caption)
}





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


## ----cancer-network, fig.asp=0.6, echo=FALSE, warning=FALSE, message=FALSE, fig.margin=TRUE, fig.cap="A network diagram displaying the relationship between types of cancer cell lines."----
# An example
library(tidygraph)
library(mdsr)
CellEdges <- Cancer
SmallEdges <- head(CellEdges,200)
#VV <- mdsr::edgesToVertices( SmallEdges, 
#                       from=cellLine, to=otherCellLine )
g <- SmallEdges %>%
  select(cellLine, otherCellLine, correlation) %>%
  as_tbl_graph(directed = FALSE) %>%
  mutate(type = substr(name, 0, 2))

library(ggraph)
ggraph(g, layout = 'kk') +
  geom_edge_arc(aes(width = correlation), color = "lightgray", strength = 0.2) +
  geom_node_point(aes(color = type), size = 10, alpha = 0.6) +
  geom_node_text(aes(label = type)) + 
  scale_edge_width_continuous(range = c(0.1, 1)) +
  guides(color = guide_legend(override.aes = list(size = 6))) + 
  theme_void() + 
  coord_cartesian(clip = "off")


## -----------------------------------------------------------------------------
library(babynames)
BabynamesDist <- make_babynames_dist()
BabynamesDist


## ----eval=FALSE---------------------------------------------------------------
## BabynamesDist %>%
##   filter(name == "Benjamin")


## ----name-plot----------------------------------------------------------------
joseph <- BabynamesDist %>%
  filter(name == "Joseph" & sex == "M")
name_plot <- ggplot(data = joseph, aes(x = year))


## -----------------------------------------------------------------------------
name_plot <- name_plot +
  geom_col(
    aes(y = count_thousands * alive_prob), 
    fill = "#b2d7e9", 
    color = "white",
    size = 0.1
  )


## -----------------------------------------------------------------------------
name_plot <- name_plot + 
  geom_line(aes(y = count_thousands), size = 2)


## -----------------------------------------------------------------------------
name_plot <- name_plot +
  ylab("Number of People (thousands)") + 
  xlab(NULL)


## -----------------------------------------------------------------------------
summary(name_plot)


## -----------------------------------------------------------------------------
wtd_quantile <- Hmisc::wtd.quantile
median_yob <- joseph %>%
  summarize(
    year = wtd_quantile(year, est_alive_today, probs = 0.5)
  ) %>% 
  pull(year)
median_yob


## -----------------------------------------------------------------------------
name_plot <- name_plot +
  geom_col(
    color = "white", fill = "#008fd5", 
    aes(y = ifelse(year == median_yob, est_alive_today / 1000, 0))
  )


## ----joseph, fig.cap="(ref:joseph-cap)"---------------------------------------
context <- tribble(
  ~year, ~num_people, ~label,
  1935, 40, "Number of Josephs\nborn each year",
  1915, 13, "Number of Josephs\nborn each year
  \nestimated to be alive\non 1/1/2014", 
  2003, 40, "The median\nliving Joseph\nis 37 years old", 
)

name_plot +
  ggtitle("Age Distribution of American Boys Named Joseph") + 
  geom_text(
    data = context, 
    aes(y = num_people, label = label, color = label)
  ) + 
  geom_curve(
    x = 1990, xend = 1974, y = 40, yend = 24, 
    arrow = arrow(length = unit(0.3, "cm")), curvature = 0.5
  ) + 
  scale_color_manual(
    guide = FALSE, 
    values = c("black", "#b2d7e9", "darkgray")
  ) + 
  ylim(0, 42)


## ----josephine, fig.cap='(ref:josephine-cap)'---------------------------------
name_plot %+% filter(
  BabynamesDist, 
  name == "Josephine" & sex == "F"
)


## ----jessie, fig.cap='(ref:jessie-cap)'---------------------------------------
names_plot <- name_plot + 
  facet_wrap(~sex)
names_plot %+% filter(BabynamesDist, name == "Jessie")


## ----many-names, fig.cap="Gender breakdown for the three most unisex names."----
many_names_plot <- name_plot + 
  facet_grid(name ~ sex)
mnp <- many_names_plot %+% filter(
  BabynamesDist, 
  name %in% c("Jessie", "Marion", "Jackie")
)
mnp


## ----many-names2, fig.cap="Gender breakdown for the three most unisex names, oriented vertically."----
mnp + facet_grid(sex ~ name)


## ----com_fem, message=FALSE---------------------------------------------------
com_fem <- BabynamesDist %>%
  filter(n > 100, sex == "F") %>% 
  group_by(name) %>%
  mutate(wgt = est_alive_today / sum(est_alive_today)) %>%
  summarize(
    N = n(), 
    est_num_alive = sum(est_alive_today),
    quantiles = list(
      wtd_quantile(
        age_today, est_alive_today, probs = 1:3/4, na.rm = TRUE
      )
    )
  ) %>%
  mutate(measures = list(c("q1_age", "median_age", "q3_age"))) %>%
  unnest(cols = c(quantiles, measures)) %>%
  pivot_wider(names_from = measures, values_from = quantiles) %>%
  arrange(desc(est_num_alive)) %>%
  head(25)


## -----------------------------------------------------------------------------
w_plot <- ggplot(
  data = com_fem, 
  aes(x = reorder(name, -median_age), y = median_age)
) + 
  xlab(NULL) + 
  ylab("Age (in years)") + 
  ggtitle("Median ages for females with the 25 most common names")


## -----------------------------------------------------------------------------
w_plot <- w_plot + 
  geom_linerange(
    aes(ymin = q1_age, ymax = q3_age), 
    color = "#f3d478", 
    size = 4.5, 
    alpha = 0.8
  )


## -----------------------------------------------------------------------------
w_plot <- w_plot +
  geom_point(
    fill = "#ed3324", 
    color = "white", 
    size = 2, 
    shape = 21
  )


## ----women, fig.asp=3/4, fig.cap="Recreation of FiveThirtyEight's plot of the age distributions for the 25 most common women's names."----
context <- tribble(
  ~median_age, ~x, ~label, 
  65, 24, "median",
  29, 16, "25th", 
  48, 16, "75th percentile",
)

age_breaks <- 1:7 * 10 + 5

w_plot + 
  geom_point(
    aes(y = 60, x = 24), 
    fill = "#ed3324", 
    color = "white", 
    size = 2, 
    shape = 21
  ) + 
  geom_text(data = context, aes(x = x, label = label)) + 
  geom_point(aes(y = 24, x = 16), shape = 17) + 
  geom_point(aes(y = 56, x = 16), shape = 17) +
  geom_hline(
    data = tibble(x = age_breaks), 
    aes(yintercept = x), 
    linetype = 3
  ) +
  scale_y_continuous(breaks = age_breaks) + 
  coord_flip()

