## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/datavizIII-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----beatles, fig.cap = "\\pkg{ggplot2} depiction of the frequency of Beatles names over time."----
library(mdsr)
library(babynames)
Beatles <- babynames %>%
  filter(name %in% c("John", "Paul", "George", "Ringo") & sex == "M")
beatles_plot <- ggplot(data = Beatles, aes(x = year, y = n)) +
  geom_line(aes(color = name), size = 2)
beatles_plot

## ----eval=FALSE----------------------------------------------------------
## library(plotly)
## ggplotly(beatles_plot)

## ----echo=FALSE, eval=FALSE, include=FALSE-------------------------------
## library(plotly)
## library(htmlwidgets)
## library(webshot)
## beatles_plotly <- ggplotly(beatles_plot)
## saveWidget(beatles_plotly, file = "plotly-beatles.html", selfcontained = FALSE)
## webshot("plotly-beatles.html", file = "gfx/plotly-beatles.png", vwidth = 800, vheight = 600, cliprect = "viewport")

## ----eval=FALSE----------------------------------------------------------
## library(DT)
## datatable(Beatles, options = list(pageLength = 25))

## ----echo=FALSE, eval=TRUE, include=FALSE--------------------------------
library(DT)
beatles_dt <- datatable(Beatles, options = list(pageLength = 10))
library(htmlwidgets)
library(webshot)
saveWidget(beatles_dt, file = "beatles-dt.html", selfcontained = FALSE)
webshot("beatles-dt.html", file = "gfx/beatles-dt.png", vwidth = 800, vheight = 500, cliprect = "viewport")

## ----eval=FALSE----------------------------------------------------------
## library(dygraphs)
## Beatles %>%
##   filter(sex == "M") %>%
##   select(year, name, prop) %>%
##   tidyr::spread(key = name, value = prop) %>%
##   dygraph(main = "Popularity of Beatles names over time") %>%
##   dyRangeSelector(dateWindow = c("1940", "1980"))

## ----echo=FALSE, eval=TRUE, include=FALSE--------------------------------
library(dygraphs)
beatles_dygraph <- Beatles %>%
  filter(sex == "M") %>%
  select(year, name, prop) %>%
  tidyr::spread(key = name, value = prop) %>%
  dygraph(main = "Popularity of Beatles names over time") %>% 
  dyRangeSelector(dateWindow = c("1940", "1980"))
library(htmlwidgets)
library(webshot)
saveWidget(beatles_dygraph, file = "beatles-dygraph.html", selfcontained = FALSE)
webshot("beatles-dygraph.html", file = "gfx/beatles-dygraph.png", vwidth = 800, vheight = 400, cliprect = "viewport")

## ----eval=FALSE----------------------------------------------------------
## # devtools::install_github("hrbrmstr/streamgraph")
## library(streamgraph)
## Beatles %>% streamgraph(key = "name", value = "n", date = "year") %>%
##   sg_fill_brewer("Accent")

## ----echo=FALSE, eval=TRUE, include=FALSE--------------------------------
library(streamgraph)
beatles_stream <- Beatles %>%
  streamgraph(key = "name", value = "n", date = "year") %>%
  sg_fill_brewer("Accent")
library(htmlwidgets)
library(webshot)
saveWidget(beatles_stream, file = "beatles-stream.html", selfcontained = FALSE)
webshot("beatles-stream.html", file = "gfx/beatles-stream.png", vwidth = 800, vheight = 500, cliprect = "viewport")

## ----eval=FALSE----------------------------------------------------------
## John <- filter(Beatles, name=="John")
## all_values <- function(x) {
##   if (is.null(x)) return(NULL)
##   row <- John[John$year == x$year, ]
##   paste0(names(row), ": ", format(row), collapse = "<br />")
## }

## ----eval=FALSE----------------------------------------------------------
## library(ggvis)
## John %>%
##   ggvis(~n, ~prop, fill = ~year) %>%
##   layer_points() %>%
##   add_tooltip(all_values, "hover")

## ----eval=TRUE, echo=FALSE-----------------------------------------------
cat(readChar("code/shiny/ui.R", 1e5))

## ----eval=TRUE, echo=FALSE-----------------------------------------------
cat(readChar("code/shiny/server.R", 1e5))

## ----echo=FALSE, eval=FALSE, include=FALSE-------------------------------
## library(shiny)
## runApp(appDir = "code/shiny/")
## webshot::appshot("code/shiny/", "gfx/beatles-shiny.pdf", delay = 5,
##                  vwidth = "800", vheight = "600")

## ----eval=FALSE----------------------------------------------------------
## library(shiny)
## runApp('.')

## ----include=FALSE-------------------------------------------------------
n_attr <- length(theme_grey())

## ------------------------------------------------------------------------
length(theme_grey())

## ------------------------------------------------------------------------
theme_grey()["panel.background"]
theme_grey()["panel.grid.major"]

## ----theme-bw, fig.cap="Comparison of two \\pkg{ggplot2} themes.", out.width='.49\\linewidth', fig.subcap=c("The default grey theme.", "The black-and-white theme.")----
beatles_plot
beatles_plot + theme_bw()

## ----theme-mod, fig.cap="Beatles plot with custom \\pkg{ggplot2} theme."----
beatles_plot + theme(panel.background = element_rect(fill = "cornsilk"),
                     panel.grid.major = element_line(color = "dodgerblue"))

## ------------------------------------------------------------------------
head(colors())

## ----theme-mdsr----------------------------------------------------------
theme_mdsr <- function(base_size = 12, base_family = "Bookman") {
   theme_grey(base_size = base_size, base_family = base_family) %+replace%
     theme(
       axis.text         = element_text(size = rel(0.8)),
       axis.ticks        = element_line(colour = "black"),
       legend.key        = element_rect(colour = "grey80"),
       panel.background  = element_rect(fill = "whitesmoke", colour = NA),
       panel.border      = element_rect(fill = NA, colour = "grey50"),
       panel.grid.major  = element_line(colour = "grey80", size = 0.2),
       panel.grid.minor  = element_line(colour = "grey92", size = 0.5),
       strip.background  = element_rect(fill = "grey80", colour = "grey50", 
         size = 0.2)
     )
}

## ----theme-mod2, fig.cap="Beatles plot with customized \\pkg{mdsr} theme."----
beatles_plot + facet_wrap(~name) + theme_mdsr()

## ----xkcd, message=FALSE, eval=TRUE--------------------------------------
library(xkcd)

## ----message=FALSE, warning=FALSE----------------------------------------
download.file("http://simonsoftware.se/other/xkcd.ttf", 
              dest = "~/.fonts/xkcd.ttf", mode = "wb")
font_import(pattern = "[X/x]kcd", prompt = FALSE)
loadfonts()

## ----beatles-xkcd, fig.cap="Prevalence of Beatles names drawn in the style of an xkcd Web comic."----
beatles_plot + theme_xkcd()

## ----message=FALSE, echo=TRUE--------------------------------------------
library(mdsr)
hd <- readr::read_csv(
  "http://datasets.flowingdata.com/hot-dog-contest-winners.csv")
names(hd) <- gsub(" ", "_", names(hd)) %>% tolower()
glimpse(hd)

## ------------------------------------------------------------------------
new_data <- data.frame(
  year = c(1979, 1978, 1974, 1972, 1916), 
  winner = c(NA, "Walter Paul", NA, NA, "James Mullen"),
  dogs_eaten = c(19.5, 17, 10, 14, 13),
  country = rep(NA, 5), new_record = c(1,1,0,0,0)
)
hd <- bind_rows(hd, new_data)
glimpse(hd)

## ------------------------------------------------------------------------
xlabs <- c(1916, 1972, 1980, 1990, 2007)
ylabs <- seq(from = 0, to = 70, by = 10)

## ------------------------------------------------------------------------
hd_plot <- hd %>% filter(year < 2008)

## ----hd-basic, fig.cap="A simple bar graph of hot dog eating."-----------
p <- ggplot(data = hd_plot, aes(x = year, y = dogs_eaten)) +
  geom_bar(stat = "identity")
p

## ----warning=FALSE-------------------------------------------------------
ticks_y <- data.frame(x = 1912, y = ylabs)

## ----warning=FALSE-------------------------------------------------------
text <- bind_rows(
  # Frank Dellarosa
  data.frame(x = 1951.5, y = 37, 
    label = paste("Frank Dellarosa eats 21 and a half HDBs over 12\n",
      "minutes, breaking the previous record of 19 and a half."), adj = 0),
  # Joey Chestnut
  data.frame(x = 1976.5, y = 69, 
    label = paste("For the first time since 1999, an American\n",
      "reclaims the title when Joey Chestnut\n",
      "consumes 66 HDBs, a new world record."), adj = 0), 
  # Kobayashi
  data.frame(x = 1960.5, y = 55, 
    label = paste("Through 2001-2005, Takeru Kobayashi wins by no less\n",
      "than 12 HDBs. In 2006, he only wins by 1.75. After win-\n",
      "ning 6 years in a row and setting the world record 4 times,\n",
      "Kobayashi places second in 2007."), adj = 0),
  # Walter Paul
  data.frame(x = 1938, y = 26, label = "Walter Paul sets a new
world record with 17 HDBs.", adj = 0),
  # James Mullen
  data.frame(x = 1917, y = 10, label = "James Mullen wins the inaugural
contest, scarfing 13 HDBs. Length
of contest unavailable.", adj = 0), 
  data.frame(x = 1935, y = 72, label = "NEW WORLD RECORD"),
  data.frame(x = 1914, y = 72, label = "Hot dogs and buns (HDBs)"),
  data.frame(x = 1940, y = 2, 
    label = "*Data between 1916 and 1972 were unavailable"), 
  data.frame(x = 1922, y = 2, label = "Source: FlowingData")
)

## ------------------------------------------------------------------------
segments <- bind_rows(
  data.frame(x = c(1984, 1991, 1991, NA), y = c(37, 37, 21, NA)),
  data.frame(x = c(2001, 2007, 2007, NA), y = c(69, 69, 66, NA)), 
  data.frame(x = c(2001, 2007, 2007, NA), y = c(69, 69, 66, NA)),
  data.frame(x = c(1995, 2006, 2006, NA), y = c(58, 58, 53.75, NA)),
  data.frame(x = c(2005, 2005, NA), y = c(58, 49, NA)),
  data.frame(x = c(2004, 2004, NA), y = c(58, 53.5, NA)),
  data.frame(x = c(2003, 2003, NA), y = c(58, 44.5, NA)),
  data.frame(x = c(2002, 2002, NA), y = c(58, 50.5, NA)),
  data.frame(x = c(2001, 2001, NA), y = c(58, 50, NA)),
  data.frame(x = c(1955, 1978, 1978), y = c(26, 26, 17)))

## ----our-hot-dogs, warning=FALSE, fig.cap="Recreating the hot dog graphic in \\R."----
p + geom_bar(stat = "identity", aes(fill = factor(new_record))) +
  geom_hline(yintercept = 0, color = "darkgray") + 
  scale_fill_manual(name = NULL, 
    values = c("0" = "#006f3c", "1" = "#81c450")) +
  scale_x_continuous(name = NULL, breaks = xlabs, minor_breaks = NULL, 
    limits = c(1912, 2008), expand = c(0, 1)) +
  scale_y_continuous(name = NULL, breaks = ylabs, labels = NULL, 
    minor_breaks = NULL, expand = c(0.01, 1)) + 
  geom_text(data = ticks_y, aes(x = x, y = y + 2, label = y), size = 3) +
  ggtitle("Winners from Nathan's hot dog eating contest") +
  geom_text(data = text, aes(x = x, y = y, label = label), 
    hjust = "left", size = 3) +
  geom_path(data = segments, aes(x = x, y = y), col = "darkgray") + 
  # Key
  geom_rect(xmin = 1933, ymin = 70.75, xmax = 1934.3, ymax = 73.25, 
    fill = "#81c450", color = "white") + 
  guides(fill = FALSE) + 
  theme(panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
    plot.title = element_text(size = rel(2)),
    axis.ticks.length = unit(0, "cm"))

