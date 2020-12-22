## ----eval=FALSE, include=FALSE------------------------------------------------
## url <- "http://www.fdle.state.fl.us/cms/FSAC/Documents/Excel/1971_fwd_murder_firearms.aspx"


## ----echo=FALSE, warning=FALSE------------------------------------------------
library(readxl)
fl_murders <- read_excel("data/1971_fwd_murder_firearms.xlsx", skip = 2) %>%
  na.omit() %>%
  janitor::clean_names()
fl_murders <- fl_murders %>%
  mutate(
    year = parse_number(year), 
    total_by_firearm = parse_number(total_by_firearm)
  )
fl_plot <- fl_murders %>%
  filter(year >= 1990) %>%
ggplot(aes(x = year, y = total_by_firearm)) + 
  geom_area(fill = "#b0272e", alpha = 0.9) + 
  geom_segment(aes(x = 2005, xend = 2005, y = 350, yend = 521), 
               color = "white", size = 0.1) + 
  geom_line(size = 2) +
  geom_point(size = 4, shape = 21, fill = "black", color = "white") + 
  annotate("text", x = 2004, y = 250, 
           label = "2005\nFlorida enacted\nits 'Stand Your\nGround' law", 
           color = "white", hjust = "left") + 
  annotate("text", x = 1990, y = 950, 
           label = "Source: Florida Department of Law Enforcement", 
           color = "darkgray", hjust = "left") + 
  theme_bw() + 
  theme(
    panel.grid.major.y = element_line(color = "darkgray", size = 0.7),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()) + 
  scale_x_continuous(expand = c(0.02, 0)) + 
  scale_y_reverse("Number of murders committed using firearms", 
                  breaks = seq(1000, 0, by = -200), limits = c(1000, 0))


## ----florida, echo=FALSE, fig.width=6, fig.cap="Reproduction of a data graphic reporting the number of gun deaths in Florida over time. The original image was published by Reuters."----
fl_plot


## ----eval=FALSE, include=FALSE------------------------------------------------
## library(webshot)
## webshot("https://twitter.com/NRO/status/676516015078039556/photo/1?ref_src=twsrc^tfw", file = "gfx/national_review_tweet.png", selector = ".permalink-inner")


## ----climate, echo=FALSE, out.width = "80%", fig.cap = "(ref:climate-cap)"----
knitr::include_graphics("gfx/national_review_tweet.png")


## ----covidga, echo=FALSE, out.width = "100%", fig.cap = "A recreation of a misleading display of confirmed COVID-19 cases in Georgia."----
knitr::include_graphics("gfx/covid-ga-recreation.jpg")


## ---- message=FALSE, warning=FALSE--------------------------------------------
library(tidyverse)
library(wru)
predict_race(voter.file = voters, surname.only = TRUE) %>% 
  select(surname, pred.whi, pred.bla, pred.his, pred.asi, pred.oth)

