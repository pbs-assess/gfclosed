library(gfclosed)
library(dplyr)
library(ggplot2)
library(future)
library(sf)
library(rgdal)
# ------------------------------------------------------------------------
# Load MPA draft polygon shapefiles (from gdb from Katie Gale Apr 2020); filter for desired restricted zones
closed <- sf::st_read(dsn="data/NSB_MPA", layer = "NSB_MPA") %>%
  select(-OBJECTID) %>%  # remove extra field added by ArcGIS in shapefile export
  st_transform(crs = 4326)
names(closed) <- c(names(st_read(dsn="data/MPA.gdb", layer = "Spatial_J1_20200403_Full_Attributes"))[1:73], "geometry")


select_restriction <- function(data, restriction){
  if (restriction == "bottom trawl"){
    data <- data %>%
      select(UID, hu_co_demersalfishing_bottomtrawling_d) %>%
      filter(hu_co_demersalfishing_bottomtrawling_d == "X")
  }
  if(restriction == "longline"){
    data <- data %>%
      select(UID, hu_co_demersalfishing_bottomlongline_d) %>%
      filter(hu_co_demersalfishing_bottomlongline_d == "X")
  }
  return(data)
}
closed <- select_restriction(data = closed, restriction = "bottom trawl")

# ------------------------------------------------------------------------
# Load synoptic survey data (eg. yelloweye rockfish)
# ------------------------------------------------------------------------
# data <- gfdata::get_survey_sets(442, ssid = c(1, 3, 4, 16), joint_sample_ids = TRUE)
data_all <- readRDS("D:\\GitHub\\pbs-assess\\gfsynopsis-old\\report\\data-cache\\yelloweye-rockfish.rds")$survey_sets %>%
  filter(survey_series_id %in% c(1, 3, 4, 16)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs(4326), remove = FALSE)

# ------------------------------------------------------------------------
# Load other map components
# ------------------------------------------------------------------------
theme_set(theme_bw())

BC_coast_albers <- sf::st_read(dsn = "data/baselayer_shps", layer = "BC_coast_albers")
syn <- "data/SynopticTrawlSurveyBoundaries"
HS <- sf::st_read(dsn=syn, layer = "HS_BLOB")
QCS <- sf::st_read(dsn=syn, layer = "QCS_BLOB")
WCHG <- sf::st_read(dsn=syn, layer = "WCHG_BLOB")
WCVI <- sf::st_read(dsn=syn, layer = "WCVI_BLOB")
cols <- paste0(c(RColorBrewer::brewer.pal(8L, "Set1")))


# ------------------------------------------------------------------------
# Plot survey data before excluding MPA network closed areas
# ------------------------------------------------------------------------
ggplot() +
  geom_sf(data = st_transform(BC_coast_albers, crs = 4326)) +
  # geom_sf(data = HS, fill = paste0(cols[2], "60")) +
  # geom_sf(data = QCS, fill = paste0(cols[3], "60")) +
  # geom_sf(data = WCHG, fill = paste0(cols[1], "60")) +
  # geom_sf(data = WCVI, fill = paste0(cols[4], "60")) +
  geom_sf(data = closed, fill =paste0(cols[6], "60")) +
  geom_point(data = data_all, aes(y = latitude, x = longitude), size = 0.5, pch = 4, color = "dark blue") +
  coord_sf(xlim = c(-135, -123), ylim = c(48, 55))+
  guides(fill = guide_legend())

# ------------------------------------------------------------------------
# Clip polygons of proposed closed areas from survey dataset
# ------------------------------------------------------------------------
# Long processing time...
# diff <- st_difference(data_all, st_union(closed))

# alternate method using st_intersects()

int <- sf::st_intersects(closed, data_all[,"fishing_event_id"])
excluded <- data_all$fishing_event_id[unlist(int)]
data_exclude <- filter(data_all, !fishing_event_id %in% excluded)

message(length(excluded), " fishing events removed")

ggplot() +
  geom_sf(data = st_transform(BC_coast_albers, crs = 4326)) +
  # geom_sf(data = HS, fill = paste0(cols[2], "60")) +
  # geom_sf(data = QCS, fill = paste0(cols[3], "60")) +
  # geom_sf(data = WCHG, fill = paste0(cols[1], "60")) +
  # geom_sf(data = WCVI, fill = paste0(cols[4], "60")) +
  geom_sf(data = closed, fill =paste0(cols[8], "60")) +
  geom_point(data = data_all, aes(y = latitude, x = longitude), size = 0.5, pch = 4, color = "dark blue") +
  geom_point(data = data_exclude, aes(y = latitude, x = longitude), size = 0.5, pch = 4, color = "light blue") +
  coord_sf(xlim = c(-135, -123), ylim = c(48, 55))

# ------------------------------------------------------------------------
# Survey index
# ------------------------------------------------------------------------


