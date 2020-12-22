## ----cholera-data, echo=FALSE-------------------------------------------------
fake_names <- tribble(
  ~Date, ~Last_Name, ~First_Name, ~Address, ~Age, ~Cause_death, 
  "Aug 31, 1854", "Jones", "Thomas", "26 Broad St.", 37, "cholera",
  "Aug 31, 1854", "Jones", "Mary", "26 Broad St.", 11, "cholera", 
  "Oct 1, 1854", "Warwick", "Martin", "14 Broad St.", 23, "cholera"
)

mdsr_table(fake_names, caption = "Hypothetical data from 1854 cholera outbreak.") %>%
  kableExtra::column_spec(1, width = "6em") %>%
  kableExtra::column_spec(4, width = "6em") %>%
  kableExtra::column_spec(6, width = "6em")


## ----set-root, echo=FALSE-----------------------------------------------------
root <- fs::path("data/shp/")


## ----snow-simple, eval=TRUE, fig.cap="Context-free plot of 1854 cholera deaths.", message=FALSE----
library(tidyverse)
library(mdsr)
library(sf)
plot(CholeraDeaths["Count"])


## ----cholera, echo=FALSE, out.width = "100%", fig.cap = "John Snow's original map of the 1854 Broad Street cholera outbreak. Source: Wikipedia."----
knitr::include_graphics("gfx/1280px-Snow-cholera-map-1.jpg")


## ----message=FALSE------------------------------------------------------------
library(sf)
# The correct path on your computer may be different
dsn <- fs::path(root, "snow", "SnowGIS_SHP")
list.files(dsn)


## -----------------------------------------------------------------------------
st_layers(dsn)


## -----------------------------------------------------------------------------
CholeraDeaths <- st_read(dsn, layer = "Cholera_Deaths")
class(CholeraDeaths)
CholeraDeaths


## ----cholera-ggplot, fig.cap="(ref:cholera-ggplot-cap)"-----------------------
ggplot(CholeraDeaths) +
  geom_sf()


## ----snow-wrong, message=FALSE, warning=FALSE, fig.asp=1, fig.cap="Erroneous reproduction of John Snow's original map of the 1854 cholera outbreak. The dots representing the deaths from cholera are off by hundreds of meters."----
library(ggspatial)
ggplot(CholeraDeaths) + 
  annotation_map_tile(type = "osm", zoomin = 0) + 
  geom_sf(aes(size = Count), alpha = 0.7)


## -----------------------------------------------------------------------------
st_bbox(CholeraDeaths)


## ----mercator, warning=FALSE, message=FALSE, fig.show='hold', fig.cap="The world according to the Mercator (left) and Gall--Peters (right) projections.", fig.subcap=c("Mercator", "Gall-Peters"), out.width='49%', fig.asp=1----
library(mapproj)
library(maps)
map("world", projection = "mercator", wrap = TRUE)
map("world", projection = "cylequalarea", param = 45, wrap = TRUE)


## ----lambert, fig.show='hold', fig.cap="The contiguous United States according to the Lambert conformal conic (left) and Albers equal area (right) projections. We have specified that the scales are true on the 20th and 50th parallels.", fig.subcap=c("Lambert conformal conic", "Albers equal area"), out.width='49%', fig.asp=1----
map(
  "state", projection = "lambert", 
  parameters = c(lat0 = 20, lat1 = 50), wrap = TRUE
)
map(
  "state", projection = "albers", 
  parameters = c(lat0 = 20, lat1 = 50), wrap = TRUE
)


## -----------------------------------------------------------------------------
st_crs(CholeraDeaths)


## -----------------------------------------------------------------------------
st_crs(4326)$epsg
st_crs(3857)$Wkt
st_crs(27700)$proj4string


## -----------------------------------------------------------------------------
cholera_4326 <- CholeraDeaths %>% 
  st_transform(4326)


## -----------------------------------------------------------------------------
st_bbox(cholera_4326)


## ----snow-wrong-again, eval=FALSE, warning=FALSE, fig.asp=1, fig.cap="Erroneous reproduction of John Snow's original map of the 1854 cholera outbreak. The dots representing the deaths from cholera are off by hundreds of meters."----
## ggplot(cholera_4326) +
##   annotation_map_tile(type = "osm", zoomin = 0) +
##   geom_sf(aes(size = Count), alpha = 0.7)


## ----eval=FALSE---------------------------------------------------------------
## help("spTransform-methods", package = "rgdal")


## -----------------------------------------------------------------------------
st_crs(CholeraDeaths)$proj4string


## ----snow-correct, warning=FALSE----------------------------------------------
cholera_latlong <- CholeraDeaths %>%
  st_set_crs(27700) %>%
  st_transform(4326)
snow <- ggplot(cholera_latlong) + 
  annotation_map_tile(type = "osm", zoomin = 0) + 
  geom_sf(aes(size = Count))


## ----snow-final, warning=FALSE, message=FALSE, fig.asp=1, fig.cap="Recreation of John Snow's original map of the 1854 cholera outbreak. The size of each black dot is proportional to the number of people who died from cholera at that location. The red dots indicate the location of public water pumps. The strong clustering of deaths around the water pump on Broad(wick) Street suggests that perhaps the cholera was spread through water obtained at that pump."----
pumps <- st_read(dsn, layer = "Pumps")
pumps_latlong <- pumps %>% 
  st_set_crs(27700) %>%
  st_transform(4326)
snow +
  geom_sf(data = pumps_latlong, size = 3, color = "red")


## ----leaflet-white-house, eval=TRUE, message=FALSE, out.width = "80%", fig.cap = "(ref:leaflet-white-house-cap)"----
white_house <- tibble(
  address = "The White House, Washington, DC"
) %>%
  tidygeocoder::geocode(address, method = "osm")

library(leaflet)
white_house_map <- leaflet() %>% 
  addTiles() %>%
  addMarkers(data = white_house)
white_house_map


## ----leaflet-white-house-popup, message = FALSE, eval=TRUE, out.width = "80%", fig.cap="(ref:leaflet-white-house-cap) with a popup."----
white_house <- white_house %>%
  mutate(
    title = "The White House", 
    street_address = "1600 Pennsylvania Ave"
  )
white_house_map %>% 
  addPopups(
    data = white_house, 
    popup = ~paste0("<b>", title, "</b></br>", street_address)
  )


## ----eval=TRUE, message=FALSE-------------------------------------------------
library(fec12)


## ----message=FALSE, warning=FALSE---------------------------------------------
results_house %>% 
  group_by(state, district_id) %>% 
  summarize(N = n()) %>% 
  nrow()


## -----------------------------------------------------------------------------
results_house %>% 
  left_join(candidates, by = "cand_id") %>%
  select(state, district_id, cand_name, party, general_votes) %>%
  arrange(desc(general_votes))


## ----nc_results---------------------------------------------------------------
district_elections <- results_house %>%
  mutate(district = parse_number(district_id)) %>%
  group_by(state, district) %>%
  summarize(
    N = n(), 
    total_votes = sum(general_votes, na.rm = TRUE),
    d_votes = sum(ifelse(party == "D", general_votes, 0), na.rm = TRUE),
    r_votes = sum(ifelse(party == "R", general_votes, 0), na.rm = TRUE)
  ) %>%
  mutate(
    other_votes = total_votes - d_votes - r_votes,
    r_prop = r_votes / total_votes,  
    winner = ifelse(r_votes > d_votes, "Republican", "Democrat")
  )
nc_results <- district_elections %>% 
  filter(state == "NC")
nc_results %>%
  select(-state)


## ----nc-skim, skimr_include_summary=FALSE, render=knitr::normal_print---------
nc_results %>%
  skim(total_votes) %>%
  select(-na)


## ----nc-results-table---------------------------------------------------------
nc_results %>%
  summarize(
    N = n(), 
    state_votes = sum(total_votes), 
    state_d = sum(d_votes), 
    state_r = sum(r_votes)
  ) %>%
  mutate(
    d_prop = state_d / state_votes, 
    r_prop = state_r / state_votes
  )


## ----message=FALSE------------------------------------------------------------
nc_results %>% 
  select(district, r_prop, winner) %>% 
  arrange(desc(r_prop))


## ----eval=FALSE---------------------------------------------------------------
## src <- "http://cdmaps.polisci.ucla.edu/shp/districts113.zip"
## dsn_districts <- usethis::use_zip(src, destdir = fs::path("data_large"))


## ---- include=FALSE-----------------------------------------------------------
dsn_districts <- fs::path("data_large", "districtShapes")


## -----------------------------------------------------------------------------
library(sf)
st_layers(dsn_districts)
districts <- st_read(dsn_districts, layer = "districts113") %>%
  mutate(DISTRICT = parse_number(as.character(DISTRICT)))
glimpse(districts)


## ----nc-basic, fig.cap="A basic map of the North Carolina congressional districts."----
nc_shp <- districts %>%
  filter(STATENAME == "North Carolina")
nc_shp %>%
  st_geometry() %>%
  plot(col = gray.colors(nrow(nc_shp)))


## -----------------------------------------------------------------------------
nc_merged <- nc_shp %>%
  st_transform(4326) %>%
  inner_join(nc_results, by = c("DISTRICT" = "district"))
glimpse(nc_merged)


## ----nc-bicolor, warning=FALSE, message=FALSE, eval=TRUE, fig.asp=1, fig.cap="Bichromatic choropleth map of the results of the 2012 congressional elections in North Carolina."----
nc <- ggplot(data = nc_merged, aes(fill = winner)) +
  annotation_map_tile(zoom = 6, type = "osm") + 
  geom_sf(alpha = 0.5) +
  scale_fill_manual("Winner", values = c("blue", "red")) + 
  geom_sf_label(aes(label = DISTRICT), fill = "white") + 
  theme_void()
nc


## ----nc-map, warning=FALSE, message=FALSE, eval=TRUE, fig.cap="Full color choropleth map of the results of the 2012 congressional elections in North Carolina. The clustering of Democratic voters is evident from the deeper blue in Democratic districts, versus the pale red in the more numerous Republican districts."----
nc +
  aes(fill = r_prop) + 
  scale_fill_distiller(
    "Proportion\nRepublican", 
    palette = "RdBu", 
    limits = c(0.2, 0.8)
  )


## -----------------------------------------------------------------------------
library(leaflet)
pal <- colorNumeric(palette = "RdBu", domain = c(0, 1))


## -----------------------------------------------------------------------------
leaflet_nc <- leaflet(nc_merged) %>% 
  addTiles() %>%
  addPolygons(
    weight = 1, fillOpacity = 0.7, 
    color = ~pal(1 - r_prop),
    popup = ~paste("District", DISTRICT, "</br>", round(r_prop, 4))
  ) %>%
  setView(lng = -80, lat = 35, zoom = 7)


## ----leaflet-nc, echo=FALSE, fig.cap = "(ref:leaflet-nc-cap)"-----------------
if (knitr::is_latex_output()) {
#  mdsr::save_webshot(leaflet_nc, path_to_img = "gfx/leaflet-nc.png")
  knitr::include_graphics("gfx/leaflet-nc.png")
} else {
  leaflet_nc
}


## ----warning=FALSE------------------------------------------------------------
districts_full <- districts %>%
  left_join(
    tibble(state.abb, state.name), 
    by = c("STATENAME" = "state.name")
  ) %>%
  left_join(
    district_elections, 
    by = c("state.abb" = "state", "DISTRICT" = "district")
  )


## ----world, warning=FALSE-----------------------------------------------------
box <- st_bbox(districts_full)
world <- map_data("world") %>%
  st_as_sf(coords = c("long", "lat")) %>%
  group_by(group) %>%
  summarize(region = first(region), do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  st_set_crs(4269)


## ----us-unprojected, warning=FALSE, message=FALSE, fig.cap="U.S. congressional election results, 2012 (Mercator projection)."----
map_4269 <- ggplot(data = districts_full) +
  geom_sf(data = world, size = 0.1) +
  geom_sf(aes(fill = r_prop), size = 0.1) +
  scale_fill_distiller(palette = "RdBu", limits = c(0, 1)) +
  theme_void() +
  labs(fill = "Proportion\nRepublican") +
  xlim(-180, -50) + ylim(box[c("ymin", "ymax")]) 
map_4269


## ----us-albers, warning=FALSE, message=FALSE, fig.cap="U.S. congressional election results, 2012 (Albers equal area projection)."----
districts_aea <- districts_full %>%
  st_transform(5070)
box <- st_bbox(districts_aea)
map_4269 %+% districts_aea +
  xlim(box[c("xmin", "xmax")]) + ylim(box[c("ymin", "ymax")])


## ----plotKML, eval=FALSE------------------------------------------------------
## nc_merged %>%
##   st_transform(4326) %>%
##   st_write("/tmp/nc_congress113.kml", driver = "kml")


## ----googleearth, echo=FALSE, out.width = "100%", fig.cap = "(ref:googleearth-cap)"----
knitr::include_graphics("gfx/googleearth-nc.png")

