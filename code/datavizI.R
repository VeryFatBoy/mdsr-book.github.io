## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
options(digits=4)
fig.path='figures/datavizI-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")
library(knitr)
opts_chunk$set(echo = FALSE)

## ----eval=TRUE, message=FALSE, include=FALSE-----------------------------
# devtools::install_github("beanumber/fec")
library(fec)
# must have pre-existing database "fec"
# if not, try
# system("mysql -e 'CREATE DATABASE IF NOT EXISTS fec;'")
db <- src_scidb(dbname = "fec")

fec <- etl("fec", db, dir = "~/dumps/fec")
# will take a couple of minutes, but should not be too long. 
# etl_create(fec)

## ----echo=FALSE, include=FALSE-------------------------------------------
library(methods)
individuals <- tbl(fec, "individuals") %>% collect(n = Inf)
candidates <- tbl(fec, from = "candidates") %>% collect()
committees <- tbl(fec, "committees") %>% collect()
contributions <- tbl(fec, "contributions") %>% collect(n = Inf)
house_elections <- tbl(fec, "house_elections") %>% collect()

## ----echo=FALSE----------------------------------------------------------
library(mdsr)
spent <- contributions %>%
  filter(transaction_pgi == "G") %>%
  group_by(cand_id) %>%
  left_join(candidates, by = c("cand_id")) %>%
  summarize(
    name = first(cand_name), party = first(cand_party_affiliation), 
    state = first(cand_state), district = first(cand_office_district),
    phase = first(transaction_pgi), office = first(cand_office),
            N = n(), total = sum(transaction_amt), avg = mean(transaction_amt)) %>%
  arrange(desc(total))

## ----spent, echo=FALSE, warning=FALSE, fig.cap="Amount of money spent on individual candidates in the general election phase of the 2012 federal election cycle, in millions of dollars. Candidacies with at least four million dollars in spending are depicted."----
spent %>%
  filter(total > 4000000) %>%
  mutate(spent = total / 1000000) %>%
ggplot(aes(x = name, y = spent)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(name = NULL) + 
  scale_y_continuous(name = "Money Spent (millions of USD)", labels = scales::dollar) + 
#  theme(axis.text = element_text(size = 11)) +
  coord_flip()

## ----spent-attack, echo=FALSE, warning=FALSE, fig.cap="Amount of money spent on individual candidates in the general election phase of the 2012 federal election cycle, in millions of dollars, broken down by type of spending. Candidacies with at least four million dollars in spending are depicted."----
spent2 <- contributions %>%
  filter(transaction_pgi == "G") %>%
  mutate(spent_against = ifelse(transaction_type %in% c('24A', '24N'), transaction_amt, 0), 
         spent_for = ifelse(!transaction_type %in% c('24A', '24N'), transaction_amt, 0)) %>%
  group_by(cand_id) %>%
  left_join(candidates, by = c("cand_id")) %>%
  summarize(
    name = first(cand_name), party = first(cand_party_affiliation), 
    state = first(cand_state), district = first(cand_office_district),
    phase = first(transaction_pgi), office = first(cand_office),
    N = n(), total = sum(transaction_amt), avg = mean(transaction_amt),
    supporting = sum(spent_for), against = sum(spent_against)) %>%
  mutate(attack_pct = against / total) %>%
  arrange(desc(total))
spent2 %>%
  filter(total > 4000000) %>%
  mutate(spent = total / 1000000) %>%
  select(cand_id, name, supporting, against) %>%
  tidyr::gather(key = "type", value = "spent", -cand_id, -name) %>%
ggplot(aes(x = name, y = spent, fill = type)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(name = NULL) + 
  scale_y_continuous(name = "Money Spent (millions of USD)", labels = scales::dollar) + 
#  theme(axis.text = element_text(size = 11)) +
  coord_flip()

## ----raised-party, echo=FALSE, warning=FALSE, fig.cap="Amount of money spent on individual candidacies by political party affiliation during the general election phase of the 2012 federal election cycle."----
# Convert NAs to "Unknown"
spent2$party[is.na(spent2$party)] <- "NA"
spent_tidy <- spent2 %>%
  group_by(party) %>%
  summarize(supporting = sum(supporting), against = sum(against)) %>%
  tidyr::gather(key = "type", value = "spent", -party) %>%
  filter(spent > 1000000)
ggplot(data = spent_tidy, aes(x = party, y = spent, fill = type)) +
  scale_x_discrete(name = NULL) + 
  scale_y_continuous(name = "Money Spent (millions of USD)", labels = scales::dollar) + 
  geom_bar(stat = "identity")

## ----raised-office, echo=FALSE, warning=FALSE, fig.cap="Amount of money spent on individual candidacies by political party affiliation during the general election phase of the 2012 federal election cycle, broken down by office being sought."----
spent2 %>%
  filter(!is.na(office)) %>%
  group_by(party, office) %>%
  summarize(supporting = sum(supporting), against = sum(against)) %>%
  tidyr::gather(key = "type", value = "spent", -party, -office) %>%
  filter(spent > 1000000) %>%
ggplot(aes(x = party, y = spent, fill = type)) +
  scale_x_discrete(name = NULL) + 
  scale_y_continuous(name = "Money Spent (millions of USD)", labels = scales::dollar) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~office)

## ----donations, message=FALSE, echo=FALSE, fig.cap="Donations made by individuals to the PACs supporting the two major presidential candidates in the 2012 election."----
donations <- individuals %>%
  filter(cmte_id %in% c("C00431171", "C00431445", "C00518282", "C00494740")) %>%
  mutate(candidate = ifelse(cmte_id %in% c("C00431171", "C00518282"), "Romney", "Obama")) %>%
  filter(transaction_pgi > "") %>%
  mutate(phase = ifelse(transaction_pgi == "G", "General", "Primary")) %>%
  filter(transaction_amt > 0 & transaction_amt < 100000) %>%
  arrange(desc(transaction_amt)) 

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

## ----include=FALSE-------------------------------------------------------
district_money <- spent2 %>%
  filter(office == "H") %>%
  group_by(state, district) %>%
  summarize(num_PACs = n(), 
            total_spent = sum(total), 
            dem_support = sum(ifelse(party == "DEM", supporting, 0)) + sum(ifelse(party != "DEM", against, 0))) %>%
  mutate(dem_money_pct = dem_support / total_spent)
district_results <- house_elections %>%
  mutate(district = readr::parse_number(district)) %>%
  filter(!state %in% c("PR", "VI", "GU", "AS", "MP")) %>%
  group_by(state, district) %>%
  summarize(num_candidates = n(), 
            total_votes = sum(general_votes, na.rm = TRUE),
            dem_votes = sum(ifelse(party == "D", general_votes, 0))) %>%
  mutate(dem_vote_pct = dem_votes / total_votes)
money_results <- district_results %>%
  left_join(district_money, by = c("state", "district"))

## ----votes, warning=FALSE, fig.cap="Scatterplot illustrating the relationship between number of dollars spent supporting and number of votes earned by Democrats in 2012 elections for the House of Representatives."----
ggplot(data = money_results, aes(x = dem_support, y = dem_votes)) + 
  geom_point() + 
  scale_x_continuous(name = "Money spent supporting Democratic candidate (USD)", labels = scales::dollar) +
  scale_y_continuous(name = "Number of Votes Earned by Democratic candidate", labels = scales::comma)

## ----votes-better, warning=FALSE, message=FALSE, fig.cap="Scatterplot illustrating the relationship between percentage of dollars spent supporting and percentage of votes earned by Democrats in the 2012 House of Representatives elections. Each dot represents one district. The size of each dot is proportional to the total spending in that election, and the alpha transparency of each dot is proportional to the total number of votes in that district."----
ggplot(data = money_results, aes(x = dem_money_pct, y = dem_vote_pct)) + 
  geom_hline(yintercept = c(0.45, 0.55), lty = 3) + 
  geom_point(aes(size = total_spent, alpha = total_votes)) +
  geom_smooth() + 
  scale_x_continuous(name = "Percentage of Money supporting Democratic candidate") +
  scale_y_continuous(name = "Percentage of Votes Earned by Democratic candidate") + 
  scale_size(labels = scales::dollar) + 
  scale_alpha(labels = scales::comma)

## ----message=FALSE, include=FALSE----------------------------------------
E <- contributions %>%
  filter(state == "MA") %>%
  mutate(attack = ifelse(!transaction_type %in% c('24A', '24N'), FALSE, TRUE)) %>%
  group_by(cmte_id, cand_id, attack) %>%
  summarize(amount = sum(transaction_amt))

library(igraph)
g <- graph_from_data_frame(E, directed = TRUE)
V(g)$degree <- degree(g)
V(g)$pr <- page_rank(g)$vector
V(g)$strength <- graph.strength(g, mode = "all", weights = E(g)$amount)
g

## ----warning=FALSE, message=FALSE, include=FALSE-------------------------
library(ggnetwork)
g_df <- ggnetwork(g)
g_df <- candidates %>%
  select(cand_id, cand_name, cand_party_affiliation) %>%
  right_join(g_df, by = c("cand_id" = "vertex.names"))
g_df <- committees %>%
  select(cmte_id, cmte_name, cmte_party_affiliation) %>%
  right_join(g_df, by = c("cmte_id" = "cand_id"))
g_df <- g_df %>%
  mutate(node_name = ifelse(is.na(cmte_name), cand_name, cmte_name)) %>%
  mutate(node_name = gsub(" / ", "\n", node_name))

## ----ma-funding, warning=FALSE, fig.width=10, fig.height=6.7, out.width="8.5in", fig.cap="Campaign funding network for candidates from Massachusetts, 2012 federal elections. Each edge represents a contribution from a PAC to a candidate.", out.extra='angle=90'----
ggplot(g_df, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(alpha = amount, lty = attack), color = "darkgray") +
  geom_nodes(aes(size = strength, color = cand_party_affiliation), alpha = 0.6) +
  geom_nodetext(data = filter(g_df, strength > 100000), 
                aes(label = node_name, size = strength)) + 
  scale_size_continuous(name = "money", range = c(0.1, 10), labels = scales::dollar) + 
  scale_color_manual(name = "party", values = c("REP" = "red", "DEM" = "blue", "DFL" = "gray", "GRE" = "gray", "IND" = "gray", "LIB" = "gray"), na.value = "gray") + 
  scale_alpha(labels = scales::dollar) + 
  theme_blank()

## ----brewer2,fig.height=2.5, fig.cap="Diverging red-blue color palette."----
library(RColorBrewer)
display.brewer.pal(9, "RdBu")
palette <- brewer.pal(9, "RdBu")

## ----brewer, message=FALSE, fig.height=7, fig.cap="Palettes available through the \\pkg{RColorBrewer} package."----
display.brewer.all()

## ----sat-dot, message=FALSE, echo=FALSE, fig.pos='h', fig.cap="Bar graph of average SAT scores among states with at least two-thirds of students taking the test."----
library(mdsr)
ggplot(data = filter(SAT, frac > 100*(2/3))) +
  geom_bar(aes(x = state, y = math), stat = "identity") + 
  xlab(NULL) + ylab("Average SAT score") + 
  ggtitle("Average SAT math score, 1994-1995") + 
  coord_cartesian(ylim = c(200, 525)) 

## ----swim1, echo=FALSE---------------------------------------------------
ggplot(data = SwimRecords, aes(x = year, y = time, colour = sex)) + 
  geom_point(aes(shape = sex), size = 3) + 
  geom_line() + 
  labs(title = "World Record time in 100 m Freestyle", x = "Year", 
    y = "Time (s)") 

## ----pie, echo=FALSE, fig.pos='h', fig.cap="Pie charts showing the breakdown of substance of abuse among HELP study participants, faceted by homeless status."----
ggplot(data = HELPrct, aes(x = factor(1))) + 
  geom_bar(aes(fill = substance), position = "fill") + 
  coord_polar(theta = "y") +
  labs(title = "Substance of Abuse among housed HELP participants", 
    x = "") + 
  facet_wrap(~homeless)

## ------------------------------------------------------------------------
library(mdsr)
tally(substance ~ homeless, data = HELPrct, format = "proportion")

## ----choropleth-ma, message=FALSE, include=FALSE, fig.height=7, echo=FALSE, eval=TRUE----
library(UScensus2010)
# install.tract("linux")  #  or install.tract("osx")
library(UScensus2010tract)
data("massachusetts.tract10")
ramp <- colorRamp(c("blue", "white"))
blues <- function(level, ...) {
  rgb(ramp(level, ...), max = 255)
}
choropleth(massachusetts.tract10, dem = "P0010001", 
  cuts = list("quantile", seq(0, 1, by = 1/10)), 
  color = list(fun = "blues", attr = list(seq(1, 0, by = -1/9))), 
  main = "2010 Massachusetts Census Tracts by Population", 
  border = "transparent")

## ----damagetemp,message=FALSE, echo=FALSE--------------------------------
library(alr3)
library(mosaic)
xyplot(Damage ~ Temp, data = challeng, 
       pch = 19, alpha = 0.5, cex = 2, type=c("p", "smooth"),
       ylab = "Tufte's O-ring damage index", 
       xlab = "Temperature (degrees F) of field joints at time of launch",
       ylim = c(-0.5, 12.5),
       scales = list(x = list(at = seq(from = 25, to = 85, by = 5))),
       panel = function(x, y, ...) {
         panel.grid(h = 2, v = 11)
         panel.xyplot(x, y, ...)
         panel.rect(26, -0.1, 29, 0.1, col = "darkgray", border = "transparent")
         panel.arrows(30, 3, 27.5, 0.2, length = 0.1)
         panel.text(53, 11, "SRM 15", pos = 1, offset = 1)
         panel.text(75, 4, "SRM 22", adj = c(-0.2, -1))
       }
       )

## ----damagetemp2,message=FALSE, echo=FALSE-------------------------------
xyplot(Damage ~ Temp, data = challeng, 
       pch = 19, alpha = 0.5, cex = 2,
       ylab = "Tufte's O-ring damage index", 
       xlab = "Temperature (degrees F) of field joints at time of launch",
       xlim = c(25, 85), ylim = c(-0.5, 12.5),
       scales = list(x = list(at = seq(from = 25, to = 85, by = 5))),
       panel = function(x, y, ...) {
         panel.grid(h = 2, v = 11)
         panel.xyplot(x, y, ...)
         panel.rect(26, -0.1, 29, 0.1, col = "darkgray", border = "transparent")
         panel.text(30.5, 3, "26 - 29 degree range of forecasted temperatures
                    \n(as of January 27th, 1986) for the launch
                    \nof space shuttle Challenger on January 28th",
                    pos = 4, lineheight = 0.5)
         panel.arrows(30, 3, 27.5, 0.2, length = 0.1)
         panel.text(53, 11, "SRM 15", pos = 1, offset = 1)
         panel.text(75, 4, "SRM 22", adj = c(-0.2, -1))
       }
       )

