## ----collegelane, message=FALSE, warning=FALSE--------------------------------
library(tidyverse)
library(mdsr)
library(sf)
library(tidygeocoder)
colleges <- tribble(
  ~school, ~address, 
  "Smith", "44 College Lane, Northampton, MA 01063",
  "Macalester", "1600 Grand Ave, St Paul, MN 55105",
  "Amherst", "Amherst College, Amherst, MA 01002"
) %>%
  geocode(address, method = "osm") %>%
  st_as_sf(coords = c("long", "lat")) %>%
  st_set_crs(4326)
colleges


## -----------------------------------------------------------------------------
colleges %>%
  filter(school != "Macalester") %>%
  st_distance()


## ----openrouteservice, echo=FALSE, message=FALSE------------------------------
# * deb: libv8-dev or libnode-dev (Debian / Ubuntu)
# * brew: v8 (OSX)
# * deb: libsodium-dev (Debian, Ubuntu, etc)
# * brew: libsodium (OSX)
# remotes::install_github("GIScience/openrouteservice-r")
mdsr_ors_api_key <- readLines("mdsr_ors_api_key.txt")


## ----openrouteservice-show, message=FALSE, warning=FALSE----------------------
library(openrouteservice)
ors_api_key(mdsr_ors_api_key)


## ----distance, message=FALSE, warning=FALSE-----------------------------------
smith_amherst <- colleges %>%
  filter(school != "Macalester") %>%
  st_coordinates() %>%
  as_tibble()

route_driving <- smith_amherst %>%
  ors_directions(profile = "driving-car", output = "sf")


## ----drivedist, message=FALSE-------------------------------------------------
route_driving %>%
  st_length()


## ----miles--------------------------------------------------------------------
route_driving %>%
  st_length() %>%
  units::set_units("miles")


## ----cycling, warning=FALSE---------------------------------------------------
route_cycling <- smith_amherst %>%
  ors_directions(profile = "cycling-regular", output = "sf")

route_cycling %>%
  st_length()


## ----smith-amherst, message=FALSE, fig.cap="The fastest route from Smith College to Amherst College, by both car (blue) and bike (green)."----
library(leaflet)
leaflet() %>%
  addTiles() %>%
  addPolylines(data = route_driving, weight = 10) %>%
  addPolylines(data = route_cycling, color = "green", weight = 10)


## ----citifield, eval=TRUE, message=FALSE, warning=FALSE, fig.cap="Alternative commuting routes from Ben's old apartment in Brooklyn to Citi Field. Note that the routes overlap for most of the way from Brooklyn to the I-278 E onramp on Roosevelt Avenue. "----
commute <- tribble(
  ~place, ~address, 
  "home", "736 Leonard St, Brooklyn, NY",
  "lga", "LaGuardia Airport, Queens, NY",
  "work", "Citi Field, 41 Seaver Way, Queens, NY 11368",
) %>%
  geocode(address, method = "osm") %>%
  st_as_sf(coords = c("long", "lat")) %>%
  st_set_crs(4326)

route_direct <- commute %>%
  filter(place %in% c("home", "work")) %>%
  st_coordinates() %>%
  as_tibble() %>%
  ors_directions(output = "sf",  preference = "recommended")

route_gcp <- commute %>%
  st_coordinates() %>%
  as_tibble() %>%
  ors_directions(output = "sf")

leaflet() %>%
  addTiles() %>%
  addMarkers(data = commute, popup = ~place) %>%
  addPolylines(data = route_direct, color = "green", weight = 10) %>%
  addPolylines(data = route_gcp, weight = 10)


## ----macleish-boundary, message=FALSE, fig.cap="Streams cross through the boundary of the MacLeish property."----
library(sf)
library(macleish)

boundary <- macleish_layers %>%
  pluck("boundary")
streams <- macleish_layers %>%
  pluck("streams")

boundary_plot <- ggplot(boundary) + 
  geom_sf() + 
  scale_x_continuous(breaks = c(-72.677, -72.683))

boundary_plot +
  geom_sf(data = streams, color = "blue", size = 1.5)


## -----------------------------------------------------------------------------
boundary %>%
  pull(Shape_Area)


## -----------------------------------------------------------------------------
st_area(boundary)


## -----------------------------------------------------------------------------
st_area(boundary) / 4046.8564224


## ---- warning=FALSE-----------------------------------------------------------
streams %>%
  mutate(length = st_length(geometry))

boundary %>%
  st_centroid()


## ---- message=FALSE, warning=FALSE--------------------------------------------
st_intersects(boundary, streams)
st_intersection(boundary, streams)


## ----macleish-boundary-streams, fig.width=10, out.width='49%', fig.show='hold', message=FALSE, warning=FALSE, fig.cap="Streams on the MacLeish property.", fig.subcap=c("Contained within the boundary.", "Outside the boundary.")----
boundary_plot + 
  geom_sf(
    data = st_intersection(boundary, streams), 
    color = "blue", 
    size = 1.5
  )

boundary_plot +
  geom_sf(
    data = st_difference(streams, boundary), 
    color = "blue", 
    size = 1.5
  )


## ---- message=FALSE, warning=FALSE--------------------------------------------
trails <- macleish_layers %>%
  pluck("trails")

st_intersection(trails, streams) 


## ---- message=FALSE, warning=FALSE--------------------------------------------
bridges <- st_intersection(trails, streams) %>%
  st_cast("MULTIPOINT") %>% 
  st_cast("POINT")

nrow(bridges)


## ----macleish-bridges, fig.asp = 0.5, fig.width = 8, fig.cap="Bridges on the MacLeish property where trails and streams intersect."----
boundary_plot + 
  geom_sf(data = trails, color = "brown", size = 1.5) + 
  geom_sf(data = streams, color = "blue", size = 1.5) + 
  geom_sf(data = bridges, pch = 21, fill = "yellow", size = 3)


## -----------------------------------------------------------------------------
trails


## -----------------------------------------------------------------------------
trails_full <- trails %>%
  group_by(name) %>%
  summarize(num_segments = n()) %>%
  mutate(trail_length = st_length(geometry)) %>%
  arrange(desc(trail_length))

trails_full


## ----macleish-campsites, fig.asp = 0.5, fig.width = 10, warning=FALSE, fig.cap="The MacLeish property has two campsites, and many different types of forest."----
forests <- macleish_layers %>%
  pluck("forests")

camp_sites <- macleish_layers %>%
  pluck("camp_sites")

boundary_plot + 
  geom_sf(data = forests, fill = "green", alpha = 0.1) + 
  geom_sf(data = camp_sites, size = 4) + 
  geom_sf_label(
    data = camp_sites, aes(label = name), 
    nudge_y = 0.001
  )


## ---- message=FALSE-----------------------------------------------------------
st_join(camp_sites, forests, left = FALSE, join = st_within) %>%
  select(name, type)


## -----------------------------------------------------------------------------
elevations <- macleish_layers %>%
  pluck("elevation")


## -----------------------------------------------------------------------------
longest_trail <- trails_full %>%
  head(1)
longest_trail


## ---- warning=FALSE, message=FALSE--------------------------------------------
trail_elevations <- longest_trail %>%
  st_intersection(elevations) %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT")


## ----macleish-snowmobile-elevation, warning=FALSE, fig.asp = 0.5, fig.width = 10, fig.cap="The Snowmobile Trail at MacLeish, with contour lines depicted. "----
boundary_plot +
  geom_sf(data = elevations, color = "dark gray") + 
  geom_sf(data = longest_trail, color = "brown", size = 1.5) + 
  geom_sf(data = trail_elevations, fill = "yellow", pch = 21, size = 3) + 
  geom_sf_label(
    data = trail_elevations, 
    aes(label = CONTOUR_FT), 
    hjust = "right", 
    size = 2.5,
    nudge_x = -0.0005
  )


## -----------------------------------------------------------------------------
trail_elevations <- trail_elevations %>%
  mutate(lat = st_coordinates(geometry)[, 2]) %>%
  arrange(lat) %>%
  mutate(distance_from_start = as.numeric(st_distance(geometry)[, 1]))


## ----snowmobile-trail-map, fig.width = 8, fig.cap="Trail elevation map for the Snowmobile trail at MacLeish."----
ggplot(trail_elevations, aes(x = distance_from_start)) + 
  geom_ribbon(aes(ymax = CONTOUR_FT, ymin = 750)) +
  scale_y_continuous("Elevation (feet above sea level)") + 
  scale_x_continuous("Geodesic distance from trail head (meters)") +
  labs(
    title = "Trail elevation map: Snowmobile trail",
    subtitle = "Whately, MA",
    caption = "Source: macleish package for R"
  )


## -----------------------------------------------------------------------------
trail_elevations %>%
  summarize(
    gain = max(CONTOUR_FT) - min(CONTOUR_FT),
    trail_length = max(units::set_units(trail_length, "miles")),
    rating = sqrt(gain * 2 * as.numeric(trail_length))
  )

