library(dplyr)
library(ggplot2)
library(future)
library(sf)
library(rgdal)
# ------------------------------------------------------------------------
# Load MPA draft polygon shapefile (from gdb from Katie Gale Apr 2020); filter for desired restricted zones
closed_areas <- function(mpa_network_shp = closed, ssid = NULL, fishery = NULL, level = c("X", "C")){
  if (ssid %in% c(1, 3, 4, 16) && is.null(fishery) || is.null(ssid) && fishery == "trawl"){
    closed <- mpa_network_shp %>%
      select(UID, hu_co_demersalfishing_bottomtrawling_d) %>%
      filter(hu_co_demersalfishing_bottomtrawling_d %in% level)}
  else if(ssid %in% c(22, 36) && is.null(fishery) || is.null(ssid) && fishery == "ll"){
    closed <- mpa_network_shp %>%
      select(UID, hu_co_demersalfishing_bottomlongline_d) %>%
      filter(hu_co_demersalfishing_bottomlongline_d %in% level)}
  # if(ssid %in% c()){  # sablefish surveys
  #   closed <- mpa_network_shp %>%
  #     select(UID, hu_co_demersalfishing_sabletrapcom_d) %>%
  #     filter(hu_co_demersalfishing_sabletrapcom_d %in% level)}
  return(closed)
}
# trawl <- closed_areas(fishery = "trawl")
# ll <- closed_areas(fishery = "longline")
# trap <- closed_areas(fishery = "trap")

# ------------------------------------------------------------------------
# Load synoptic survey data (eg. yelloweye rockfish)
# ------------------------------------------------------------------------
# note ssid 1 = QCS, 3 = HS, 4 = wcvi, 16 = wchg, 22 = HBLL Outside North, 36 = HBLL Outside North

import_survey_sets <- function(spp, ssid = c(1, 3, 4, 16, 22, 36), data_cache = "D:/GitHub/pbs-assess/gfsynopsis-old/report/data-cache/"){
  spp <- spp %>% gsub(pattern = " ", replacement = "-")

  if(file.exists(paste0(data_cache, spp, ".rds"))){
    survey_sets <- readRDS(paste0(data_cache, spp, ".rds"))$survey_sets %>%
      filter(survey_series_id %in% ssid) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs(4326), remove = FALSE)
  }
  else{data <- gfdata::get_survey_sets(spp, ssid)}
}
# survey_sets <- import_survey_sets("yelloweye-rockfish")

# ------------------------------------------------------------------------
# Load other map components
# ------------------------------------------------------------------------
# theme_set(theme_bw())
# # BC_coast_albers <- sf::st_read(dsn = "data/baselayer_shps", layer = "BC_coast_albers")
# syn <- "data/SynopticTrawlSurveyBoundaries"
# hbll <- "data/HBLL_boundaries"
# hs <- sf::st_read(dsn=syn, layer = "HS_BLOB")
# qcs <- sf::st_read(dsn=syn, layer = "QCS_BLOB")
# wchg <- sf::st_read(dsn=syn, layer = "WCHG_BLOB")
# wcvi <- sf::st_read(dsn=syn, layer = "WCVI_BLOB")
# hbll_out_n <- sf::st_read(dsn=hbll, layer = "PHMA_N_boundaries")
# hbll_out_s <- sf::st_read(dsn=hbll, layer = "PHMA_S_boundaries")

# ------------------------------------------------------------------------
# Plot survey data before excluding MPA network closed areas
# ------------------------------------------------------------------------
plot_survey_pts <- function(data = survey_sets, ssid = NULL){
 if (is.null(ssid)) {
   stop("Please provide ssid.",
     call. = FALSE
   )
 }

  cols <- paste0(c(RColorBrewer::brewer.pal(8L, "Set1")))
  data <- data %>% filter(survey_series_id %in% ssid)

  g <- ggplot() + geom_sf(data = st_transform(BC_coast_albers, crs = 4326)) +
    geom_sf(data = closed_areas(ssid = ssid, level = c("X", "C")), fill =paste0(cols[6], "60"))

  if(ssid == 1){g <- g + geom_sf(data = qcs, fill = paste0(cols[3], "60"))}
  if(ssid == 3){g <- g + geom_sf(data = hs, fill = paste0(cols[2], "60"))}
  if(ssid == 4){g <- g + geom_sf(data = wcvi, fill = paste0(cols[4], "60"))}
  if(ssid == 16){g <- g + geom_sf(data = wchg, fill = paste0(cols[1], "60"))}
  if(ssid == 22){g <- g + geom_sf(data = hbll_out_n, fill = paste0(cols[5], "60"))}
  if(ssid == 36){g <- g + geom_sf(data = hbll_out_s, fill = paste0(cols[7], "60"))}

  g + geom_point(data = data, aes(y = latitude, x = longitude), size = 0.5, pch = 4, color = "dark blue") +
    coord_sf(xlim = c(-134.5, -122), ylim = c(48, 55)) +
    guides(fill = guide_legend()) # TO DO: make legend
}

# plot_survey_pts(data = survey_sets, ssid = 1)

# ------------------------------------------------------------------------
# Clip polygons of proposed closed areas from survey dataset
# ------------------------------------------------------------------------

# clip_by_mpa <- function(data = survey_sets, ssid = 1){
#   data <- data %>% filter(survey_series_id %in% ssid)
#   message(nrow(data), " unclipped fishing events in ", unique(data$survey_series_desc), " survey for ", unique(data$species_common_name))
#   int <- suppressMessages(sf::st_intersects(closed_areas(ssid = ssid), data[,"fishing_event_id"]))
#   excluded <- data$fishing_event_id[unlist(int)]
#   data_exclude <- filter(data, !fishing_event_id %in% excluded)
#   removed <- filter(data, fishing_event_id %in% excluded)
#   stopifnot(identical(nrow(data) - nrow(removed), nrow(data_exclude)))
#   message(nrow(removed), " fishing events removed")
#   return(data_exclude)
# }

clip_by_mpa <- function(data = survey_sets, fishery = "trawl"){
  data <- data %>% filter(survey_series_id %in% ssid)
  message(nrow(data), " unclipped fishing events in ", unique(data$survey_series_desc), " survey for ", unique(data$species_common_name))
  int <- suppressMessages(sf::st_intersects(closed_areas(fishery = fishery), data[,"fishing_event_id"]))
  excluded <- data$fishing_event_id[unlist(int)]
  data_exclude <- filter(data, !fishing_event_id %in% excluded)
  removed <- filter(data, fishing_event_id %in% excluded)
  stopifnot(identical(nrow(data) - nrow(removed), nrow(data_exclude)))
  message(nrow(removed), " fishing events removed")
  return(data_exclude)
}

 # data_exclude <- clip_by_mpa(survey_sets, ssid = 1)

# plot_survey_pts(data = survey_sets, ssid = 1) +
  # geom_point(data = data_exclude, aes(y = latitude, x = longitude), size = 0.5, pch = 4, color = "light blue")

# ------------------------------------------------------------------------
# Survey index
# ------------------------------------------------------------------------
# length(unique(data_exclude$grouping_code))
# length(unique(survey_sets$grouping_code))
#
# # separate by ssid,
#
# ggplot(data_exclude, aes(longitude, latitude)) +
#   geom_point(colour = "grey40", pch = 4, alpha = 0.4) +
#   coord_equal() +
#   facet_wrap(~year) +
#   gfplot::theme_pbs() +
#   geom_point(data = removed, colour = "red", pch = 21)



# ss <- split(survey_sets, survey_sets$survey_series_id)
# t <- map(ss, clip_by_mpa, ssid = c(1,3,4,16, 22, 36))

