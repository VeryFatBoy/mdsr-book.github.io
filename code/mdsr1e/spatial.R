## ----cache=FALSE, echo=FALSE,include=FALSE-------------------------------
source('hooks.R', echo=TRUE)
fig.path='figures/spatial-'

## ----echo=FALSE,eval=TRUE------------------------------------------------
options(continue="  ")

## ----set-root, echo=FALSE------------------------------------------------
root <- path.expand("data/shp/")

## ----snow-simple, eval=FALSE, fig.cap="Context-free plot of 1854 cholera deaths.", message=FALSE----
## library(mdsr)
## library(sp)
## plot(CholeraDeaths)

## ----message=FALSE-------------------------------------------------------
library(rgdal)
dsn <- paste0(root, "snow/SnowGIS_SHP/")
list.files(dsn)

## ------------------------------------------------------------------------
ogrListLayers(dsn)

## ------------------------------------------------------------------------
ogrInfo(dsn, layer = "Cholera_Deaths")

## ------------------------------------------------------------------------
CholeraDeaths <- readOGR(dsn, layer = "Cholera_Deaths")
summary(CholeraDeaths)

## ------------------------------------------------------------------------
str(CholeraDeaths@data)

## ----cholera-ggplot, fig.cap="A simple \\pkg{ggplot2} of the cholera deaths, with no context provided."----
cholera_coords <- as.data.frame(coordinates(CholeraDeaths))
ggplot(cholera_coords) +
  geom_point(aes(x = coords.x1, y = coords.x2)) + coord_quickmap()

## ----ggmap-simple, message=FALSE, fig.height=6, fig.cap="A modern-day map of the area surrounding Broad Street in London."----
library(ggmap)
m <- get_map("John Snow, London, England", zoom = 17, maptype = "roadmap")
ggmap(m)

## ----ggmap-empty, eval=FALSE, fig.width = 9, fig.height = 9, fig.cap="Reproduction of John Snow's original map of the 1854 cholera outbreak."----
## ggmap(m) + geom_point(data = as.data.frame(CholeraDeaths),
##                       aes(x = coords.x1, y = coords.x2, size = Count))

## ------------------------------------------------------------------------
head(as.data.frame(CholeraDeaths))

## ------------------------------------------------------------------------
attr(m, "bb")

## ----mercator, warning=FALSE, message=FALSE, fig.show='hold', fig.cap="The world according to the Mercator (left) and Gall--Peters (right) projections.", fig.subcap=c("Mercator", "Gall-Peters"), out.width='.49\\linewidth', fig.height=10----
library(maps)
map("world", projection = "mercator", wrap = TRUE)
map("world", projection = "cylequalarea", param = 45, wrap = TRUE)

## ----lambert, fig.show='hold', fig.cap="The contiguous United States according to the Lambert conformal conic (left) and Albers equal area (right) projections. We have specified that the scales are true on the 20th and 50th parallels.", fig.subcap=c("Lambert conformal conic", "Albers equal area"), out.width='.49\\linewidth', fig.height=10----
map("state", projection = "lambert", 
    parameters = c(lat0 = 20, lat1 = 50), wrap = TRUE)
map("state", projection = "albers", 
    parameters = c(lat0 = 20, lat1 = 50), wrap = TRUE)

## ------------------------------------------------------------------------
proj4string(CholeraDeaths) %>% strwrap()

## ------------------------------------------------------------------------
CRS("+init=epsg:4326")
CRS("+init=epsg:3857")
CRS("+init=epsg:27700")

## ------------------------------------------------------------------------
cholera_latlong <- CholeraDeaths %>% spTransform(CRS("+init=epsg:4326"))

## ------------------------------------------------------------------------
bbox(cholera_latlong)

## ----snow-wrong, warning=FALSE,fig.height = 6, fig.cap="Erroneous reproduction of John Snow's original map of the 1854 cholera outbreak. The dots representing the deaths from cholera are off by hundreds of meters."----
ggmap(m) + geom_point(aes(x = coords.x1, y = coords.x2, 
  size = Count), data = as.data.frame(cholera_latlong))

## ----eval=FALSE----------------------------------------------------------
## help("spTransform-methods", package = "rgdal")

## ------------------------------------------------------------------------
CholeraDeaths %>% proj4string() %>% showEPSG()

## ----warning=FALSE-------------------------------------------------------
proj4string(CholeraDeaths) <- CRS("+init=epsg:27700")

## ----snow-correct, warning=FALSE-----------------------------------------
cholera_latlong <- CholeraDeaths %>%
spTransform(CRS("+init=epsg:4326"))
snow <- ggmap(m) + 
geom_point(aes(x = coords.x1, y = coords.x2, 
                 size = Count), data = as.data.frame(cholera_latlong))

## ----snow-final, warning=FALSE, fig.height=10, fig.cap="Reproduction of John Snow's original map of the 1854 cholera outbreak. The size of each black dot is proportional to the number of people who died from cholera at that location. The red dots indicate the location of public water pumps. The strong clustering of deaths around the water pump on Broad(wick) Street suggests that perhaps the cholera was spread through water obtained at that pump."----
pumps <- readOGR(dsn, layer = "Pumps")
proj4string(pumps) <- CRS("+init=epsg:27700")
pumps_latlong <- pumps %>% spTransform(CRS("+init=epsg:4326"))
snow + geom_point(data = as.data.frame(pumps_latlong), 
                aes(x = coords.x1, y = coords.x2), size = 3, color = "red") 

## ----collegelane, message=FALSE------------------------------------------
smith <- "Smith College, Northampton, MA 01063"
geocode(smith)

## ----amherst-college, message=FALSE--------------------------------------
library(RgoogleMaps)
amherst <- "Amherst College, Amherst, MA"
getGeoCode(amherst)

## ----distance, message=FALSE---------------------------------------------
mapdist(from = smith, to = amherst, mode = "driving")
mapdist(from = smith, to = amherst, mode = "bicycling")

## ----message=FALSE-------------------------------------------------------
legs_df <- route(smith, amherst, alternatives = TRUE)
head(legs_df) %>% 
  select(m, km, miles, miles, seconds, minutes, hours, startLon, startLat)

## ----smith-amherst, message=FALSE, fig.cap="The fastest route from Smith College to Amherst College."----
qmap("The Quarters, Hadley, MA", zoom = 12, maptype = 'roadmap') +
  geom_leg(aes(x = startLon, y = startLat, xend = endLon, yend = endLat), 
           alpha = 3/4, size = 2, color = "blue", data = legs_df) 

## ----citifield, eval=TRUE, message=FALSE, fig.cap="Alternative commuting routes from Ben's old apartment in Brooklyn to Citi Field. Note that the Google API only returns the endpoints of each segment, so they appear on the map as straight lines even when the actual road curves."----
legs_df <- route(from = "736 Leonard St, Brooklyn, NY", 
                 to = "Citi Field, Roosevelt Ave, Flushing, NY", 
                 alternatives = TRUE, structure = "legs")

qmap("74th St and Broadway, Queens, NY", zoom = 12, maptype = 'roadmap') +
  geom_leg(aes(x = startLon, y = startLat, xend = endLon, yend = endLat, 
               color = route), alpha = 0.7, size = 2, data = legs_df)

## ----leaflet-white-house, eval=FALSE-------------------------------------
## white_house <- geocode("The White House, Washington, DC")
## library(leaflet)
## map <- leaflet() %>%
##   addTiles() %>%
##   addMarkers(lng = ~lon, lat = ~lat, data = white_house)

## ----echo=FALSE, eval=FALSE, include=FALSE-------------------------------
## # sudo apt-get install phantomjs
## library(htmlwidgets)
## library(webshot)
## saveWidget(map, file = "leaflet-white-house.html", selfcontained = FALSE)
## webshot("leaflet-white-house.html", file = "gfx/leaflet-white-house.png", vwidth = 800, vheight = 600, cliprect = "viewport")

## ----white-house, eval=FALSE---------------------------------------------
## white_house <- white_house %>%
##   mutate(title = "The White House", address = "2600 Pennsylvania Ave")
## map %>%
##   addPopups(lng = ~lon, lat = ~lat, data = white_house,
##             popup = ~paste0("<b>", title, "</b></br>", address))

## ----echo=FALSE, eval=FALSE, include=FALSE-------------------------------
## saveWidget(map, file = "leaflet-white-house2.html", selfcontained = FALSE)
## webshot("leaflet-white-house2.html", file = "gfx/leaflet-white-house2.png", vwidth = 800, vheight = 600, cliprect = "viewport")

## ----eval=TRUE, message=FALSE--------------------------------------------
library(fec)
db <- src_mysql(default.file = "~/.my.cnf", groups = "rs-dbi", 
                dbname = "fec", user = NULL, password = NULL)
fec <- etl("fec", db, dir = "~/dumps/fec")
us_elections <- tbl(fec, "house_elections") %>%
  collect()

## ------------------------------------------------------------------------
us_elections %>% 
  group_by(state, district) %>% 
  summarize(N = n()) %>% 
  nrow()

## ------------------------------------------------------------------------
us_elections %>% 
  select(state, district, candidate_name, party, general_votes) %>%
  arrange(desc(general_votes))

## ----nc_results----------------------------------------------------------
district_elections <- us_elections %>%
  mutate(district = stringr::str_sub(district, 1, 2)) %>%
  group_by(state, district) %>%
  summarize(N = n(), total_votes = sum(general_votes, na.rm = TRUE),
    d_votes = sum(ifelse(party == "D", general_votes, 0)),
    r_votes = sum(ifelse(party == "R", general_votes, 0))) %>%
  mutate(other_votes = total_votes - d_votes - r_votes,
         r_pct = r_votes / total_votes, 
         r_win = r_votes > d_votes)
nc_results <- district_elections %>% filter(state == "NC")
nc_results

## ------------------------------------------------------------------------
favstats(~ total_votes, data = nc_results)

## ------------------------------------------------------------------------
nc_results %>%
  summarize(N = n(), repub_win = sum(r_win), 
            state_votes = sum(total_votes), state_d = sum(d_votes), 
            state_r = sum(r_votes)) %>%
  mutate(d_pct = state_d / state_votes, r_pct = state_r / state_votes)

## ----message=FALSE-------------------------------------------------------
nc_results %>% 
  select(district, r_pct) %>% 
  arrange(desc(r_pct))

## ----eval=FALSE----------------------------------------------------------
## src <- "http://cdmaps.polisci.ucla.edu/shp/districts113.zip"
## lcl <- paste0(root, "districts113.zip")
## download.file(src, destfile = lcl)
## unzip(zipfile = lcl, exdir = root)

## ------------------------------------------------------------------------
library(rgdal)
dsn_districts <- paste0(root, "districtShapes/")
ogrListLayers(dsn_districts)
districts <- readOGR(dsn_districts, layer = "districts113")
glimpse(districts@data)

## ----nc-basic, fig.cap="A basic map of the North Carolina congressional districts."----
nc_shp <- subset(districts, STATENAME == "North Carolina")
plot(nc_shp, col = gray.colors(nrow(nc_shp)))

## ------------------------------------------------------------------------
class(nc_shp)
class(nc_results)

## ------------------------------------------------------------------------
nc_merged <- merge(nc_shp, as.data.frame(nc_results), 
  by.x = c("DISTRICT"), by.y = c("district"))
glimpse(nc_merged@data)

## ----nc-tidy, message=FALSE, warning=FALSE-------------------------------
library(broom)
library(maptools)
nc_tidy <- tidy(nc_merged, region = "ID")
nc_full <- nc_tidy %>% left_join(nc_merged@data, by = c("id" = "ID"))
glimpse(nc_full)

## ----eval=TRUE, message=FALSE--------------------------------------------
library(rgeos)
nc_centroids <- gCentroid(nc_shp, byid = TRUE)
class(nc_centroids)

## ------------------------------------------------------------------------
nc_centroids <- SpatialPointsDataFrame(nc_centroids, nc_shp@data)

## ----warning=FALSE-------------------------------------------------------
nc_centroids_tidy <- as.data.frame(nc_centroids)
nc_centroids_full <- nc_centroids_tidy %>%
  inner_join(nc_results, 
    by = c("STATENAME" = "state", "DISTRICT" = "district"))

## ----nc-bicolor, warning=FALSE, message=FALSE, eval=TRUE, fig.height=9, fig.width=9, fig.cap="Bichromatic choropleth map of the results of the 2012 congressional elections in North Carolina."----
library(ggmap)
nc <- get_map("charlotte, north carolina", zoom = 6, maptype = "roadmap")
ggmap(nc) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = r_win), 
               alpha = 0.8, data = nc_full) +
  scale_fill_manual(values = c("blue", "red")) + 
  geom_text(aes(x = x, y = y, label = DISTRICT), data = nc_centroids_full) + 
  theme_map()

## ----nc-map, warning=FALSE, eval=TRUE, fig.height=10, fig.width=10, fig.cap="Full color choropleth of the results of the 2012 congressional elections in North Carolina. The clustering of Democratic voters is evident from the deeper blue in Democratic districts, versus the pale red in the more numerous Republican districts."----
ggmap(nc) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = r_pct), 
               alpha = 0.8, data = nc_full) +
  scale_fill_distiller(palette = "RdBu", limits = c(0.2,0.8)) +
  geom_text(aes(x = x, y = y, label = DISTRICT), data = nc_centroids_full) + 
  theme_map()


## ------------------------------------------------------------------------
library(leaflet)
pal <- colorNumeric(palette = "RdBu", domain = c(0, 1))

## ----nc-leaflet, eval=FALSE----------------------------------------------
## nc_dynamic <- leaflet() %>%
##   addTiles() %>%
##   addPolygons(
##     data = nc_merged, weight = 1, fillOpacity = 0.7, color = ~pal(1- r_pct),
##     popup = ~paste("District", DISTRICT, "</br>", round(r_pct, 4))) %>%
##   setView(lng = -80, lat = 35, zoom = 7)

## ----echo=FALSE, eval=FALSE, include=FALSE-------------------------------
## library(htmlwidgets)
## library(webshot)
## saveWidget(nc_dynamic, file = "leaflet-nc.html", selfcontained = FALSE)
## webshot("leaflet-nc.html", file = "gfx/leaflet-nc.png", vwidth = 800, vheight = 600, cliprect = "viewport")

## ------------------------------------------------------------------------
my_carrier <- "DL"
my_year <- 2006

## ----eval=TRUE, warning=FALSE--------------------------------------------
db <- src_scidb("airlines")
airports <- tbl(db, "airports")
flights <- tbl(db, "flights")

## ----dests, warning=FALSE------------------------------------------------
destinations <- flights %>%
  filter(year == my_year, carrier == my_carrier) %>%
  left_join(airports, by = c("dest" = "faa")) %>% 
  group_by(dest) %>%
  summarize(N = n(), lon = max(lon), lat = max(lat), 
            # note use of MySQL syntax instead of dplyr
            name = min(CONCAT("(", dest, ") ", 
              REPLACE(name, " Airport", "")))) %>%
  collect() %>%
  na.omit() 
glimpse(destinations)

## ----warning=FALSE-------------------------------------------------------
segments <- flights %>%
  filter(year == my_year, carrier == my_carrier) %>%
  group_by(origin, dest) %>% 
  summarize(N = n()) %>%
  left_join(airports, by = c("origin" = "faa")) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  collect() %>%
  na.omit()
dim(segments)

## ----delta-hub-map, message=FALSE, warning=FALSE, fig.height=8, fig.cap="Airports served by Delta Airlines in 2006."----
library(ggmap)
route_map <- qmap("junction city, kansas", zoom = 4, maptype = "roadmap") + 
  geom_point(data = destinations, alpha = 0.5, 
             aes(x = lon, y = lat, size = N)) + 
  scale_size() + 
  theme_map()
route_map

## ------------------------------------------------------------------------
destinations %>% arrange(desc(N))

## ----delta-route-map, warning=FALSE, fig.height=8, fig.cap="Full route map for Delta Airlines in 2006."----
route_map + geom_segment(
  aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y, color = N), 
  size = 0.05, arrow = arrow(length = unit(0.3, "cm")), data = segments)

## ------------------------------------------------------------------------
lines <- bind_rows(
  segments %>%
    select(origin, dest, lat.x, lon.x) %>%
    rename(lat = lat.x, lon = lon.x),
  segments %>%
    select(origin, dest, lat.y, lon.y) %>%
    rename(lat = lat.y, lon = lon.y)) %>%
  arrange(origin, dest) %>% 
  na.omit() %>%
  group_by(origin, dest) %>%
  do(line = Line(as.data.frame(select(., lon, lat)))
)
head(lines, 3)

## ------------------------------------------------------------------------
make_line <- function(x) {
  Lines(list(x[["line"]]), ID = paste0(x$origin, "-", x$dest))
}
lines_list <- apply(lines, MARGIN = 1, make_line)

## ------------------------------------------------------------------------
segments_sp <- SpatialLines(lines_list, CRS("+proj=longlat"))
summary(segments_sp)
segments_sp <- segments_sp %>% spTransform(CRS("+init=epsg:4326"))

## ----eval=FALSE----------------------------------------------------------
## library(leaflet)
## l_map <- leaflet() %>%
##   addTiles() %>%
##   addCircles(lng = ~lon, lat = ~lat, weight = 1,
##     radius = ~sqrt(N) * 500, popup = ~name, data = destinations) %>%
##   addPolylines(weight = 0.4, data = segments_sp) %>%
##   setView(lng = -80, lat = 38, zoom = 6)
## l_map

## ----echo=FALSE, eval=FALSE, include=FALSE-------------------------------
## # sudo apt-get install phantomjs
## # devtools::install_github("wch/webshot")
## library(htmlwidgets)
## library(webshot)
## saveWidget(l_map, file = "leaflet-airlines.html", selfcontained = FALSE)
## webshot("leaflet-airlines.html", file = "gfx/leaflet-airlines.png",
##         vwidth = 800, vheight = 600, cliprect = "viewport")

## ----warning=FALSE-------------------------------------------------------
districts_tidy <- tidy(districts, region = "ID")
districts_full <- districts_tidy %>%
  left_join(districts@data, by = c("id" = "ID")) %>%
  left_join(data.frame(state.abb, state.name), 
            by = c("STATENAME" = "state.name")) %>%
  left_join(district_elections, by = c("state.abb" = "state", 
                                       "DISTRICT" = "district"))

## ----warning=FALSE-------------------------------------------------------
box <- bbox(districts)
us_map <- ggplot(data = map_data("world"), 
                 aes(x = long, y = lat, group = group)) +
  geom_path(color = "black", size = 0.1) + 
  geom_polygon(aes(fill = r_pct), data = districts_full) +
  scale_fill_distiller(palette = "RdBu", limits = c(0,1)) + 
  theme_map() + xlim(-180, -50) + ylim(box[2,])

## ----us-unprojected, warning=FALSE, fig.cap="U.S. congressional election results, 2012 (Mercator projection)."----
us_map + coord_map()

## ----us-albers, warning=FALSE, fig.cap="U.S. congressional election results, 2012 (Albers equal area projection)."----
us_map + coord_map(projection = "albers", lat0 = 20, lat1 = 50)

## ----plotKML, eval=FALSE-------------------------------------------------
## nc_merged %>% spTransform(CRS("+init=epsg:4326")) %>%
##   plotKML::kml(file = "nc_congress113.kml",
##                folder.name = "113th Congress (NC)",
##                colour = r_pct, fill = c("red", "blue", "white"),
##                labels = DISTRICT, alpha = 0.5, kmz = TRUE)

