## ----echo=FALSE, eval=TRUE----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
options(tigris_use_cache = TRUE)


## ----eval=FALSE, message=FALSE, include=FALSE---------------------------------
## # remotes::install_github("baumer-lab/fec12")
## library(fec12)
## contributions <- read_all_contributions()
## candidate_lkup <- candidates %>%
##   select(
##     cand_id,
##     name = cand_name,
##     party = cand_pty_affiliation,
##     state = cand_office_st,
##     district = cand_office_district,
##     office = cand_office,
##   )
## 
## spent <- contributions %>%
##   filter(transaction_pgi == "G") %>%
##   group_by(cand_id, phase = transaction_pgi) %>%
##   summarize(
##     N = n(),
##     total = sum(transaction_amt),
##     avg = mean(transaction_amt)
##   ) %>%
##   left_join(candidate_lkup, by = c("cand_id")) %>%
##   arrange(desc(total))
## 
## save(spent, file = here::here("data", "fec12_spent.rda"), compress = "xz")
## 
## spent2 <- contributions %>%
##   filter(transaction_pgi == "G") %>%
##   mutate(
##     spent_against = ifelse(transaction_tp %in% c('24A', '24N'), transaction_amt, 0),
##     spent_for = ifelse(!transaction_tp %in% c('24A', '24N'), transaction_amt, 0)
##   ) %>%
##   group_by(cand_id, phase = transaction_pgi) %>%
##   summarize(
##     N = n(),
##     total = sum(transaction_amt),
##     avg = mean(transaction_amt),
##     supporting = sum(spent_for),
##     against = sum(spent_against)
##   ) %>%
##   left_join(candidate_lkup, by = c("cand_id")) %>%
##   mutate(attack_pct = against / total) %>%
##   arrange(desc(total))
## 
## save(spent2, file = here::here("data", "fec12_spent2.rda"), compress = "xz")
## 
## ma_contributions <- contributions %>%
##   filter(state == "MA") %>%
##   mutate(attack = ifelse(!transaction_tp %in% c('24A', '24N'), FALSE, TRUE)) %>%
##   group_by(cmte_id, cand_id, attack) %>%
##   summarize(amount = sum(transaction_amt))
## 
## save(ma_contributions, file = here::here("data", "fec12_ma_contributions.rda"), compress = "xz")


## ----load-spent, include=FALSE------------------------------------------------
load(here::here("data", "fec12_spent.rda"))
load(here::here("data", "fec12_spent2.rda"))


## ----spent, echo=FALSE, warning=FALSE, fig.cap="Amount of money spent on individual candidates in the general election phase of the 2012 federal election cycle, in millions of dollars. Candidacies with at least \\$4 million in spending are depicted."----
spent %>%
  filter(total > 4000000) %>%
  mutate(spent = total / 1000000) %>%
ggplot(aes(x = name, y = spent)) + 
  geom_col() + 
  scale_x_discrete(name = NULL) + 
  scale_y_continuous(name = "Money Spent (millions of USD)", labels = scales::dollar) + 
  theme(axis.text = element_text(size = rel(0.7))) +
  coord_flip()


## ----spent-attack, echo=FALSE, warning=FALSE, fig.cap="Amount of money spent on individual candidates in the general election phase of the 2012 federal election cycle, in millions of dollars, broken down by type of spending. Candidacies with at least \\$4 million in spending are depicted."----
spent2 %>%
  filter(total > 4000000) %>%
  select(cand_id, name, supporting, against) %>%
  pivot_longer(-c(cand_id, name), names_to = "type", values_to = "spent") %>%
  mutate(spent = spent / 1000000) %>%
ggplot(aes(x = name, y = spent, fill = type)) + 
  geom_col() + 
  scale_x_discrete(name = NULL) + 
  scale_y_continuous(name = "Money Spent (millions of USD)", labels = scales::dollar) + 
  theme(axis.text = element_text(size = rel(0.6))) +
  coord_flip()


## ----raised-party, message=FALSE, echo=FALSE, warning=FALSE, fig.cap="Amount of money spent on individual candidacies by political party affiliation during the general election phase of the 2012 federal election cycle."----
# Convert NAs to "Unknown"
#spent2$party[is.na(spent2$party)] <- "NA"
spent_tidy <- spent2 %>%
  group_by(party) %>%
  summarize(supporting = sum(supporting), against = sum(against)) %>%
  pivot_longer(-party, names_to = "type", values_to = "spent") %>%
  filter(spent > 1000000)

ggplot(data = spent_tidy, aes(x = party, y = spent / 1e6, fill = type)) +
  scale_x_discrete(name = NULL) + 
  scale_y_continuous(name = "Money Spent (millions of USD)", labels = scales::dollar) + 
  geom_col()


## ----raised-office, echo=FALSE, message=FALSE, warning=FALSE, fig.cap="Amount of money spent on individual candidacies by political party affiliation during the general election phase of the 2012 federal election cycle, broken down by office being sought (House, President, or Senate)."----
spent2 %>%
  filter(!is.na(office)) %>%
  group_by(party, office) %>%
  summarize(supporting = sum(supporting), against = sum(against)) %>%
  pivot_longer(-c(party, office), names_to = "type", values_to = "spent") %>%
  filter(spent > 1000000) %>%
ggplot(aes(x = party, y = spent / 1e6, fill = type)) +
  scale_x_discrete(name = NULL) + 
  scale_y_continuous(name = "Money Spent (millions of USD)", labels = scales::dollar) + 
  geom_col() + 
  facet_wrap(~office)


## ----read-all-individuals, eval=FALSE, include=FALSE--------------------------
## indiv <- fec12::read_all_individuals()
## donations <- indiv %>%
##   filter(cmte_id %in% c("C00431171", "C00431445", "C00518282", "C00494740")) %>%
##   mutate(candidate = ifelse(cmte_id %in% c("C00431171", "C00518282"), "Romney", "Obama")) %>%
##   filter(transaction_pgi > "") %>%
##   mutate(phase = ifelse(transaction_pgi == "G", "General", "Primary")) %>%
##   filter(transaction_amt > 0 & transaction_amt < 100000) %>%
##   arrange(desc(transaction_amt))
## 
## save(donations, file = here::here("data", "fec12_donations.rda"), compress = "xz")


## ----donations, message=FALSE, echo=FALSE, fig.cap="Donations made by individuals to the PACs supporting the two major presidential candidates in the 2012 election."----
load(here::here("data", "fec12_donations.rda"))

ggplot(data = donations, aes(x = transaction_amt)) + 
  geom_histogram() + 
  scale_x_continuous(name = "Amount of Donation (USD)", labels = scales::dollar) + 
  scale_color_discrete(name = NULL) + 
  ylab("Number of Donations") + 
  facet_wrap(~candidate)


## ----donations-split, fig.cap="Donations made by individuals to the PACs supporting the two major presidential candidates in the 2012 election, separated by election phase."----
ggplot(data = donations, aes(x = transaction_amt)) + 
  geom_density(aes(color = candidate), adjust = 4) + 
  scale_x_log10(name = "Amount of Donation (USD)", labels = scales::dollar) + 
  scale_color_discrete(name = NULL) +
#  coord_trans(x = "log10") + 
  facet_wrap(~phase)


## ----include=FALSE------------------------------------------------------------
district_money <- spent2 %>%
  filter(office == "H") %>%
  group_by(state, district) %>%
  summarize(
    num_PACs = n(), 
    total_spent = sum(total), 
    dem_support = sum(ifelse(party == "DEM", supporting, 0)) + sum(ifelse(party != "DEM", against, 0))
  ) %>%
  mutate(dem_money_pct = dem_support / total_spent)

district_results <- fec12::results_house %>%
#  mutate(district = readr::parse_number(district)) %>%
  filter(!state %in% c("PR", "VI", "GU", "AS", "MP")) %>%
  group_by(state, district = district_id) %>%
  summarize(
    num_candidates = n(), 
    total_votes = sum(general_votes, na.rm = TRUE),
    dem_votes = sum(ifelse(party == "D", general_votes, 0), na.rm = TRUE)
  ) %>%
  mutate(dem_vote_pct = dem_votes / total_votes)
money_results <- district_results %>%
  left_join(district_money, by = c("state", "district"))


## ----votes, warning=FALSE, fig.cap="Scatterplot illustrating the relationship between number of dollars spent supporting and number of votes earned by Democrats in 2012 elections for the House of Representatives."----
ggplot(data = money_results, aes(x = dem_support / 1000000, y = dem_votes)) + 
  geom_point() + 
  scale_x_continuous(name = "Money spent supporting Democratic candidate (millions of USD)", labels = scales::dollar) +
  scale_y_continuous(name = "Number of Votes by Democratic candidate", labels = scales::comma)


## ----votes-better, warning=FALSE, message=FALSE, fig.cap="Scatterplot illustrating the relationship between proportion of dollars spent supporting and proportion of votes earned by Democrats in the 2012 House of Representatives elections. Each dot represents one district. The size of each dot is proportional to the total spending in that election, and the alpha transparency of each dot is proportional to the total number of votes in that district."----
ggplot(data = money_results, aes(x = dem_money_pct, y = dem_vote_pct)) + 
  geom_hline(yintercept = c(0.45, 0.55), lty = 3) + 
  geom_point(aes(size = total_spent, alpha = total_votes)) +
  geom_smooth() + 
  scale_x_continuous(name = "Proportion of Money supporting Democratic candidate") +
  scale_y_continuous(name = "Proportion of Votes by Democratic candidate") + 
  scale_size(labels = scales::dollar) + 
  scale_alpha(labels = scales::comma)


## ----message=FALSE, include=FALSE---------------------------------------------
load(here::here("data", "fec12_ma_contributions.rda"))

library(tidygraph)
g <- as_tbl_graph(ma_contributions, directed = TRUE) %>%
  left_join(
    select(fec12::candidates, cand_id, cand_name, cand_pty_affiliation), 
    by = c("name" = "cand_id")
  ) %>%
  left_join(
    select(fec12::committees, cmte_id, cmte_nm, cmte_pty_affiliation), 
    by = c("name" = "cmte_id")
  ) %>%
  mutate(
    node_name = ifelse(is.na(cmte_nm), cand_name, cmte_nm), 
    node_name = gsub(" / ", "\n", node_name), 
    # V(g)$degree <- degree(g)
    pr = centrality_pagerank(), 
    strength = centrality_degree(mode = "all", weights = amount)
  )
# g


## ----ma-funding, warning=FALSE, fig.cap="Campaign funding network for candidates from Massachusetts, 2012 federal elections. Each edge represents a contribution from a PAC to a candidate."----
set.seed(21)
library(ggraph)
ggraph(g, layout = 'graphopt') + 
  geom_edge_fan(
    aes(edge_alpha = amount, lty = attack), 
    color = "darkgray"
  ) +
  geom_node_point(
    aes(size = strength, color = cand_pty_affiliation), 
    alpha = 0.6
  ) + 
  geom_node_label(
    aes(filter = strength > 1e5, label = node_name, size = strength), 
  repel = TRUE) + 
  scale_size_continuous(
    name = "money", range = c(1, 7), 
    labels = scales::dollar
  ) + 
  scale_color_manual(
    name = "party", 
    values = c("REP" = "red", "DEM" = "blue", "DFL" = "gray", "GRE" = "gray", "IND" = "gray", "LIB" = "gray"), 
    na.value = "gray") + 
  scale_edge_alpha(name = "money", labels = scales::dollar) + 
  theme_void()


## ----visual-cues--------------------------------------------------------------
tribble(
  ~`Visual Cue`, ~`Variable Type`, ~Question,
  "Position", "numerical", "where in relation to other things?",
  "Length", "numerical", "how big (in one dimension)?",
  "Angle", "numerical", "how wide? parallel to something else?",
  "Direction", "numerical", "at what slope? in a time series, going up or down?",
  "Shape", "categorical", "belonging to which group?",
  "Area", "numerical", "how big (in two dimensions)?",
  "Volume", "numerical", "how big (in three dimensions)?",
  "Shade", "either", "to what extent? how severely?",
  "Color", "either", "to what extent? how severely?"
) %>%
  mdsr_table(caption = "Visual cues and what they signify.") %>%
  kableExtra::column_spec(3, width = "24em")


## ----brewer2, fig.asp=0.4, fig.cap="Diverging red-blue color palette."--------
library(RColorBrewer)
display.brewer.pal(9, "RdBu")
palette <- brewer.pal(9, "RdBu")


## ----brewer, message=FALSE, fig.asp=1.3, fig.cap="(ref:brewer-cap)"-----------
display.brewer.all()


## ----sat-dot, message=FALSE, echo=FALSE, fig.cap="Bar graph of average SAT scores among states with at least two-thirds of students taking the test."----
library(mdsr)
ggplot(data = filter(mosaicData::SAT, frac > 100*(2/3))) +
  geom_col(aes(x = state, y = math)) + 
  xlab(NULL) + ylab("Average SAT score") + 
  ggtitle("Average SAT math score, 1994-1995") + 
  coord_cartesian(ylim = c(200, 525)) +
  theme(axis.text.x = element_text(size = rel(0.7)))


## ----swimgg, echo=FALSE, fig.cap="Scatterplot of world record time in 100-m freestyle swimming."----
ggplot(data = mosaicData::SwimRecords, aes(x = year, y = time, color = sex)) + 
  geom_point(aes(shape = sex), size = 3) + 
  geom_line() + 
  labs(title = "World Record time in 100-m Freestyle", x = "Year", 
    y = "Time (s)") 


## ----pie, echo=FALSE, fig.cap="(ref:pie)"-------------------------------------
ggplot(data = mosaicData::HELPrct, aes(x = factor(1))) + 
  geom_bar(aes(fill = substance), position = "fill") + 
  scale_fill_brewer(palette = "Spectral") + 
  coord_polar(theta = "y") +
  labs(
    title = "Substance of Abuse among housed HELP participants", 
    x = NULL
  ) + 
  facet_wrap(~homeless)


## ---- echo=FALSE--------------------------------------------------------------
mosaicData::HELPrct %>%
  group_by(homeless, substance) %>%
  count() %>%
  group_by(homeless) %>%
  mutate(proportion = n / sum(n)) %>%
  pivot_wider(id_cols = substance, names_from = homeless, values_from = c(n, proportion)) %>%
  mutate(
    Homeless = paste(
      "n = ", n_homeless, " (", round(100 * proportion_homeless, 1), "%)", sep = ""
    ),
    Housed = paste(
      "n = ", n_housed, " (", round(100 * proportion_housed, 1), "%)", sep = ""
    )
  ) %>% 
  select(substance, Homeless, Housed)


## ---- include=FALSE, echo=FALSE, message=FALSE--------------------------------
library(tidycensus)

ma_pop <- get_acs(geography = "tract", variables = "B01003_001", state = "MA", geometry = TRUE)


## ----choropleth-ma, message=FALSE, fig.cap="Choropleth map of population among Massachusetts Census tracts, based on 2018 American Community Survey.", echo=FALSE, eval=TRUE----
ggplot(ma_pop) + 
  geom_sf(aes(fill = cut(estimate/1000, 10)), color = NA) + 
  coord_sf(datum = NA) + 
  theme_minimal() + 
  scale_fill_brewer("Population (in thousands)", palette = "Blues") + 
  labs(
    title = "Massachusetts Census Tracts by Population",
    subtitle = "Based on 2010 US Census"
  )


## ----tufte0, message=FALSE, echo=FALSE, fig.cap="A scatterplot with smoother demonstrating the relationship between temperature and O-ring damage on solid rocket motors. The dots are semi-transparent, so that darker dots indicate multiple observations with the same values."----
set.seed(38)
library(alr3)

challeng <- challeng %>%
  mutate(
    label = case_when(
      Damage == 11 ~ "SRM 15",
      Damage == 4 & Temp == 75 ~ "SRM 22",
      TRUE ~ ""
    )
  )

challenger_plot <- ggplot(data = challeng, aes(x = Temp, y = Damage)) + 
  geom_jitter(size = 5, height = 0.25, width = 0.25, alpha = 0.5) + 
  geom_smooth() + 
  ggrepel::geom_label_repel(aes(label = label)) + 
  ylab("Tufte's O-ring damage index") + 
  xlab("Temperature (degrees F) of field joints at time of launch")
challenger_plot


## ----tufte, message=FALSE, echo=FALSE, fig.cap="A recreation of Tufte's scatterplot demonstrating the relationship between temperature and O-ring damage on solid rocket motors."----
library(ggrepel)

extra <- tibble(
  Temp = 42, Damage = 2, 
  label = "26 - 29 degree range of forecasted temperatures\n(as of January 27th, 1986) for the launch\nof space shuttle Challenger on January 28th"
)

challenger_plot + 
  geom_rect(xmin = 26, xmax = 29, ymin = -0.1, ymax = 0.1, fill = "red") + 
  xlim(c(25, 85)) + 
  geom_label(
    data = extra, aes(label = label),
    hjust = "center", vjust = "bottom"
  ) + 
  geom_curve(
    data = extra, angle = 135,
    curvature = -0.5,
    aes(xend = 30, yend = 0),
    arrow = arrow(length = unit(0.02, "npc"))
  )


## ----challenger1, echo=FALSE, fig.cap = "(ref:challenger1-cap)"---------------
knitr::include_graphics("gfx/thiokol.png")

## ----challenger2, echo=FALSE, fig.cap = "(ref:challenger2-cap)"---------------
knitr::include_graphics("gfx/challenger2.png")


## ----forms, echo=FALSE, fig.show='hold', out.width = "49%", fig.cap = "(ref:forms-cap)"----
knitr::include_graphics("gfx/forms04-05.jpg")
knitr::include_graphics("gfx/forms08-05.jpg")
knitr::include_graphics("gfx/forms02-03.jpg")
knitr::include_graphics("gfx/forms07-02.jpg")
knitr::include_graphics("gfx/forms09-02.jpg")
knitr::include_graphics("gfx/forms11-03.jpg")

