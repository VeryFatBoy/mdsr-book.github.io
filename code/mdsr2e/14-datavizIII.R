## ----beatles, fig.cap = "(ref:beatles-cap)"-----------------------------------
library(tidyverse)
library(mdsr)
library(babynames)
Beatles <- babynames %>%
  filter(name %in% c("John", "Paul", "George", "Ringo") & sex == "M") %>%
  mutate(name = factor(name, levels = c("John", "George", "Paul", "Ringo")))
beatles_plot <- ggplot(data = Beatles, aes(x = year, y = n)) +
  geom_line(aes(color = name), size = 2)
beatles_plot


## ----echo=TRUE, results='hide', eval=FALSE------------------------------------
## library(plotly)
## ggplotly(beatles_plot)


## ----plotly-beatles, echo=FALSE, eval=TRUE, message=FALSE, fig.cap="An interactive plot of the frequency of Beatles names over time."----
if (knitr::is_latex_output()) {
#    mdsr::save_webshot("gfx/plotly-beatles.png")
    knitr::include_graphics("gfx/plotly-beatles.png")
} else {
  beatles_plot %>%
    plotly::ggplotly()
}


## ---- eval=FALSE, echo=TRUE---------------------------------------------------
## datatable(Beatles, options = list(pageLength = 10))


## ----DT-beatles, eval=TRUE, echo=FALSE, message=FALSE, fig.cap="(ref:DT-beatles-cap)"----
if (knitr::is_latex_output()) {
#  Beatles %>%
#    datatable(options = list(pageLength = 10)) %>%
#    mdsr::save_webshot("gfx/beatles-dt.png", vheight = 500))
  knitr::include_graphics("gfx/beatles-dt.png")
} else {
  library(DT)
  Beatles %>%
    datatable(options = list(pageLength = 10)) 
}


## ----eval=FALSE, echo=TRUE, results='hide'------------------------------------
## library(dygraphs)
## Beatles %>%
##   filter(sex == "M") %>%
##   select(year, name, prop) %>%
##   pivot_wider(names_from = name, values_from = prop) %>%
##   dygraph(main = "Popularity of Beatles names over time") %>%
##   dyRangeSelector(dateWindow = c("1940", "1980"))


## ----dygraphs-beatles, echo=FALSE, eval=TRUE, fig.cap="(ref:dygraphs-beatles-cap)"----
if (knitr::is_latex_output()) {
#   mdsr::save_webshot(beatles_dygraph, "gfx/beatles-dygraph.png", vheight = 400)
  knitr::include_graphics("gfx/beatles-dygraph.png")
} else {
  library(dygraphs)
  Beatles %>%
    filter(sex == "M") %>%
    select(year, name, prop) %>%
    pivot_wider(names_from = name, values_from = prop) %>%
    dygraph(main = "Popularity of Beatles names over time") %>% 
    dyRangeSelector(dateWindow = c("1940", "1980"))
}


## ----streambeatles, eval=TRUE, fig.cap="(ref:streambeatles-cap)"--------------
# remotes::install_github("hrbrmstr/streamgraph")
library(streamgraph)
Beatles %>% 
  streamgraph(key = "name", value = "n", date = "year") %>%
  sg_fill_brewer("Accent")


## ----echo=FALSE, eval=FALSE, include=FALSE------------------------------------
## library(streamgraph)
## beatles_stream <- Beatles %>%
##   streamgraph(key = "name", value = "n", date = "year") %>%
##   sg_fill_brewer("Accent")
## 
## mdsr::save_webshot(beatles_stream, "gfx/beatles-stream.png", vheight = 500)


## ----beatles-gganminate-generate, eval=FALSE----------------------------------
## library(gganimate)
## library(transformr)
## beatles_animation <- beatles_plot +
##   transition_states(
##     name,
##     transition_length = 2,
##     state_length = 1
##   ) +
##   enter_grow() +
##   exit_shrink()
## 
## animate(beatles_animation, height = 400, width = 800)


## ---- include=FALSE, eval=FALSE-----------------------------------------------
## anim_save(filename = here::here("gfx/beatles-gganimate.gif"))
## 
## gif <- here::here("gfx/beatles-gganimate.gif")
## 
## png <- gif %>%
##   magick::image_read() %>%
##   magick::image_write(path = here::here("gfx/beatles-gganimate.png"), format = "png")


## ----beatles-gganminate, fig.cap="(ref:gganimate-cap)", echo=FALSE------------
if (knitr::is_latex_output()) {
  knitr::include_graphics(here::here("gfx/beatles-gganimate.png"))
} else {
  knitr::include_graphics(here::here("gfx/beatles-gganimate.gif"))
}


## ----flex1, echo=FALSE, fig.cap="(ref:flexdash1-cap)", out.width='0%'---------
cat(readChar("code/flexdash.Rmd", 1e5))
knitr::include_graphics("gfx/1x1.png")


## ----flex2, echo=FALSE, fig.cap="(ref:flexdash2-cap)"-------------------------
knitr::include_graphics("gfx/flexdash.png")


## ---- eval=FALSE, echo=TRUE, code=readLines("code/shiny1/ui.R")---------------
## NA

## ----shiny-ui, echo=FALSE, fig.cap="User interface code for a simple Shiny app.", out.width='0%', fig.pos="h"----
knitr::include_graphics("gfx/1x1.png")


## ---- eval=FALSE, code=readLines("code/shiny1/server.R")----------------------
## NA

## ----shiny-server, eval=TRUE, echo=FALSE, fig.cap="Server processing code for a simple Shiny app.", out.width='0%', fig.pos="h"----
knitr::include_graphics("gfx/1x1.png")


## ----echo=FALSE, eval=FALSE, include=FALSE------------------------------------
## library(shiny)
## runApp(appDir = "code/shiny1/")
## # rsconnect::deployApp("code/shiny1/")
## webshot::appshot("code/shiny1/", "gfx/beatles-shiny.png", delay = 5,
##                  vwidth = "800", vheight = "600")


## ----eval=FALSE---------------------------------------------------------------
## library(shiny)
## runApp('.')


## ----beatlesshiny, echo=FALSE, fig.cap="A screenshot of the Shiny app displaying babies with Beatles names."----
knitr::include_graphics("gfx/beatles-shiny.png")


## ---- eval=FALSE, code=readLines("code/shiny2/part1.R")-----------------------
## NA

## ----shiny-part1, eval=TRUE, echo=FALSE, fig.cap="User interface processing code for a more sophisticated Shiny app.", out.width='0%', fig.pos="h"----
knitr::include_graphics("gfx/1x1.png")


## ---- eval=FALSE, code=readLines("code/shiny2/part2.R")-----------------------
## NA

## ----shiny-part2, eval=TRUE, echo=FALSE, fig.cap="Server processing code for a more sophisticated Shiny app.", out.width='0%', fig.pos="h"----
knitr::include_graphics("gfx/1x1.png")


## ----bakeshoppe, echo=FALSE, fig.cap="A screenshot of the Shiny app displaying New York City restaurants."----
knitr::include_graphics("gfx/bakeshoppe.png")


## ----include=FALSE------------------------------------------------------------
n_attr <- length(theme_grey())


## -----------------------------------------------------------------------------
length(theme_grey())


## -----------------------------------------------------------------------------
theme_grey() %>%
  pluck("panel.background")
theme_grey() %>%
  pluck("panel.grid")


## ----theme-bw, fig.cap="(ref:theme-bw-cap)", out.width='49%', fig.show='hold'----
beatles_plot
beatles_plot + theme_bw()


## ----theme-mod, fig.cap="(ref:theme-mod-cap)"---------------------------------
beatles_plot + 
  theme(
    panel.background = element_rect(fill = "cornsilk"),
    panel.grid.major = element_line(color = "dodgerblue")
  )


## -----------------------------------------------------------------------------
head(colors())


## ----theme-mdsr---------------------------------------------------------------
theme_mdsr <- function(base_size = 12, base_family = "Helvetica") {
   theme_grey(base_size = base_size, base_family = base_family) %+replace%
     theme(
       axis.text         = element_text(size = rel(0.8)),
       axis.ticks        = element_line(color = "black"),
       legend.key        = element_rect(color = "grey80"),
       panel.background  = element_rect(fill = "whitesmoke", color = NA),
       panel.border      = element_rect(fill = NA, color = "grey50"),
       panel.grid.major  = element_line(color = "grey80", size = 0.2),
       panel.grid.minor  = element_line(color = "grey92", size = 0.5),
       strip.background  = element_rect(fill = "grey80", color = "grey50", 
         size = 0.2)
     )
}


## ----theme-mod2, fig.cap="(ref:theme-mod2-cap)"-------------------------------
beatles_plot + facet_wrap(~name) + theme_mdsr()


## ----xkcd, message=FALSE, eval=TRUE-------------------------------------------
library(xkcd)


## ----message=FALSE, eval=FALSE------------------------------------------------
## download.file(
##   "http://simonsoftware.se/other/xkcd.ttf",
##   # ~/Library/Fonts/ for Mac OS X
##   dest = "~/.fonts/xkcd.ttf", mode = "wb"
## )


## ----message=FALSE, warning=FALSE---------------------------------------------
font_import(pattern = "[X/x]kcd", prompt = FALSE)
loadfonts()


## ----beatles-xkcd, fig.cap="(ref:beatles-xkcd)"-------------------------------
beatles_plot + theme_xkcd()


## ----hot-dogs, echo=FALSE, fig.cap="(ref:hot-dogs-cap)"-----------------------
knitr::include_graphics("gfx/hot-dogs.jpg")


## ----message=FALSE, echo=TRUE-------------------------------------------------
library(tidyverse)
library(mdsr)
hd <- read_csv(
  "http://datasets.flowingdata.com/hot-dog-contest-winners.csv"
) %>%
  janitor::clean_names()
glimpse(hd)


## ----warning=FALSE------------------------------------------------------------
new_data <- tibble(
  year = c(1979, 1978, 1974, 1972, 1916), 
  winner = c(NA, "Walter Paul", NA, NA, "James Mullen"),
  dogs_eaten = c(19.5, 17, 10, 14, 13),
  country = rep(NA, 5), new_record = c(1,1,0,0,0)
)
hd <- hd %>%
  bind_rows(new_data)
glimpse(hd)


## -----------------------------------------------------------------------------
xlabs <- c(1916, 1972, 1980, 1990, 2007)
ylabs <- seq(from = 0, to = 70, by = 10)


## -----------------------------------------------------------------------------
hd_plot <- hd %>% 
  filter(year < 2008)


## ----hd-basic, fig.cap="A simple bar graph of hot dog eating."----------------
p <- ggplot(data = hd_plot, aes(x = year, y = dogs_eaten)) +
  geom_col()
p


## ----warning=FALSE------------------------------------------------------------
ticks_y <- tibble(x = 1912, y = ylabs)


## ----warning=FALSE------------------------------------------------------------
text <- tribble(
  ~x, ~y, ~label, ~adj,
  # Frank Dellarosa
  1953, 37, paste(
    "Frank Dellarosa eats 21 and a half HDBs over 12",
    "\nminutes, breaking the previous record of 19 and a half."), 0,
  # Joey Chestnut
  1985, 69, paste(
    "For the first time since 1999, an American",
    "\nreclaims the title when Joey Chestnut",
    "\nconsumes 66 HDBs, a new world record."), 0, 
  # Kobayashi
  1972, 55, paste(
    "Through 2001-2005, Takeru Kobayashi wins by no less",
    "\nthan 12 HDBs. In 2006, he only wins by 1.75. After win-",
    "\nning 6 years in a row and setting the world record 4 times,",
    "\nKobayashi places second in 2007."), 0,
  # Walter Paul
  1942, 26, paste(
    "Walter Paul sets a new",
    "\nworld record with 17 HDBs."), 0,
  # James Mullen
  1917, 10.5, paste(
    "James Mullen wins the inaugural",
    "\ncontest, scarfing 13 HDBs. Length",
    "\nof contest unavailable."), 0, 
  1935, 72, "NEW WORLD RECORD", 0,
  1914, 72, "Hot dogs and buns (HDBs)", 0,
  1940, 2, "*Data between 1916 and 1972 were unavailable", 0, 
  1922, 2, "Source: FlowingData", 0,
)


## -----------------------------------------------------------------------------
segments <- tribble(
  ~x, ~y,
  c(1978, 1991, 1991, NA), c(37, 37, 21, NA),
  c(2004, 2007, 2007, NA), c(69, 69, 66, NA), 
  c(1998, 2006, 2006, NA), c(58, 58, 53.75, NA),
  c(2005, 2005, NA), c(58, 49, NA),
  c(2004, 2004, NA), c(58, 53.5, NA),
  c(2003, 2003, NA), c(58, 44.5, NA),
  c(2002, 2002, NA), c(58, 50.5, NA),
  c(2001, 2001, NA), c(58, 50, NA),
  c(1955, 1978, 1978), c(26, 26, 17)
) %>% 
  unnest(cols = c(x, y))


## ----hd_plot, warning=FALSE, fig.show='hide'----------------------------------
p + 
  geom_col(aes(fill = factor(new_record))) +
  geom_hline(yintercept = 0, color = "darkgray") + 
  scale_fill_manual(name = NULL, 
    values = c("0" = "#006f3c", "1" = "#81c450")
  ) +
  scale_x_continuous(
    name = NULL, breaks = xlabs, minor_breaks = NULL, 
    limits = c(1912, 2008), expand = c(0, 1)
  ) +
  scale_y_continuous(
    name = NULL, breaks = ylabs, labels = NULL, 
    minor_breaks = NULL, expand = c(0.01, 1)
  ) + 
  geom_text(
    data = ticks_y, aes(x = x, y = y + 2, label = y), 
    size = 3
  ) +
  labs(
    title = "Winners from Nathan's Hot Dog Eating Contest",
    subtitle = paste(
      "Since 1916, the annual eating competition has grown substantially", 
      "attracting competitors from around\nthe world.",
      "This year's competition will be televised on July 4, 2008",
      "at 12pm EDT live on ESPN.\n\n\n"
    )
  ) +
  geom_text(
    data = text, aes(x = x, y = y, label = label), 
    hjust = "left", size = 3
  ) +
  geom_path(
    data = segments, aes(x = x, y = y), col = "darkgray"
  ) + 
  # Key
  geom_rect(
    xmin = 1933, ymin = 70.75, xmax = 1934.3, ymax = 73.25, 
    fill = "#81c450", color = "white"
  ) + 
  guides(fill = FALSE) + 
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = 
      element_line(color = "gray", linetype = "dotted"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 10),
    axis.ticks.length = unit(0, "cm")
  )


## ---- include=FALSE-----------------------------------------------------------
ggsave(filename = here::here("gfx/hot-dogs_R.png"), width = 12, height = 5)


## ----our-hot-dogs, fig.asp = 1.2, echo=FALSE, fig.cap="(ref:hot-dogs-cap-2)"----
knitr::include_graphics(here::here("gfx/hot-dogs_R.png"))

