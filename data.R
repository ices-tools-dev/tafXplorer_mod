## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(sf)
library(leaflet)

mkdir("data")

######## this is the library I used to reduce the shape file size for quicker app loading (we can test a keep = 0.01 and see how it looks)
# library(rmapshaper)
# # object.size(shape_eco)
# # df1 <- ms_simplify(shape_eco, keep = 0.05, keep_shapes = TRUE)
# # object.size(df1)
# # df2 <- ms_simplify(eu_shape, keep = 0.05, keep_shapes = TRUE)
# eu_shape <-df2
# shape_eco <- df1
# st_write(df1, "shape_eco_simplified.shp")
# st_write(df2, "shape_EU_simplified.shp")


# work on ecoregion shape file
eco_shape <- st_read(taf.data.path("shape_eco_simplified"), "shape_eco_simplified")

# Change one Ecoregion name (this comes handy when we filter the stock list table)
filter_out <-
  c(
    "Western Mediterranean Sea", "Ionian Sea and the Central Mediterranean Sea",
    "Adriatic Sea", "Black Sea", "Aegean-Levantine Sea"
  )

eco_shape <- eco_shape[!eco_shape$Ecoregion %in% filter_out, ]
# Add an id to each ecoregion (this potentially can be eliminated because the ecoregions in the shape file have already an id)
eco_shape$uid <- paste0("P", 1:12)


# eu_shape shape file
eu_shape <- st_read(taf.data.path("world_map"), "world_map_simplified")


# create the map
minZoom <- 0.5
maxZoom <- 14
resolutions <- 1.8 * (2^(maxZoom:minZoom))
crs_laea <- leafletCRS(
  crsClass = "L.Proj.CRS", code = "EPSG:3035",
  proj4def = "+proj=laea +x_0=0 +y_0=0 +lon_0= -1.235660 +lat_0=60.346958",
  resolutions = resolutions
)

# i am having issues with the leafletOptions
map <-
#  leaflet(options = leafletOptions(crs = crs_laea, minZoom = minZoom, maxZoom = 3)) %>%
  leaflet() %>%
  # addTiles() %>%
  addPolygons(
    # data = st_set_precision(eu_shape, precision=10^2),
    data = eu_shape,
    color = "black",
    weight = 1,
    fillOpacity = 0.4,
    fillColor = "#fddfc2", # "#E8EAEA"
    group = "Europe"
  ) %>%
  addPolygons(
    # data = st_set_precision(shape_eco, precision=10^2),
    data = eco_shape,
    fillColor = "#71B5BC",
    fillOpacity = 0.08,
    color = "black",
    stroke = TRUE,
    weight = 1,
    layerId = ~Ecoregion,
    group = "Eco_regions",
    label = ~Ecoregion
  ) %>%
  addPolygons(
    # data = st_set_precision(shape_eco, precision=10^2),
    data = eco_shape,
    fillColor = "#F15D2A",
    fillOpacity = 0.7,
    weight = 1,
    color = "black",
    stroke = TRUE,
    layerId = ~OBJECTID,
    group = ~Ecoregion
  ) %>%
  setView(lng = -1.235660, lat = 60.346958, zoom = 0.5) %>%
  # fitBounds(lng1 = -5, lat1 = 35, lng2=40, lat2 = 90) %>%
  hideGroup(group = eco_shape$Ecoregion)

# save map data
vocabs <- list(ecoregions = sort(eco_shape$Ecoregion))

save(map, vocabs, file = "data/map_data.RData")
