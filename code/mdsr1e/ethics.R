## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/ethics-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----eval=FALSE, include=FALSE-------------------------------------------
## url <- "http://www.fdle.state.fl.us/cms/FSAC/Documents/Excel/1971_fwd_murder_firearms.aspx"

## ----echo=FALSE, warning=FALSE-------------------------------------------
library(readxl)
fl_murders <- read_excel("data/1971_fwd_murder_firearms.xlsx", skip=2) %>%
  na.omit()
names(fl_murders) <- make.names(names(fl_murders))
fl_murders <- fl_murders %>%
  mutate(Year = as.numeric(Year), 
         total_firearm = as.numeric(Total.by.Firearm))
fl_plot <- fl_murders %>%
  filter(Year >= 1990) %>%
ggplot(aes(x = Year, y = total_firearm)) + 
  geom_area(fill = "#b0272e", alpha = 0.9) + 
  geom_segment(aes(x = 2005, xend = 2005, y = 350, yend = 521), 
               color = "white", size = 0.1) + 
  geom_line(size = 2) +
  geom_point(size = 4, shape = 21, fill = "black", color = "white") + 
  annotate("text", x = 2004, y = 250, 
           label = "2005\nFlorida enacted\nits 'Stand Your\nGround' law", 
           color = "white", hjust = "left") + 
  annotate("text", x = 1990, y = 980, 
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

## ----eval=FALSE, include=FALSE-------------------------------------------
## library(webshot)
## webshot("https://twitter.com/NRO/status/676516015078039556/photo/1?ref_src=twsrc^tfw", file = "gfx/national_review_tweet.png", selector = ".permalink-inner")

