library(dplyr)
library(ggplot2)
library(future)
library(sf)
library(rgdal)
# ------------------------------------------------------------------------

#' Create a spatial file of closed areas by fishery/survey type
#'
#' @param mpa_network_shp MPA polygon shapefile (draft from gdb from Katie Gale Apr 2020); filter for desired restricted zones
#' @param ssid Survey series id to select restricted areas for
#' @param fishery Fishery type to select restricted areas for (eg. "trawl", "ll" (longline), "trap" (not yet implemented))
#' @param level Level of restriction. "X" = closed to all activity of associated fishery/survey type, "C" = closed on certain conditions
#' @param out_crs Coordinate reference system for the output polygon
#' @export

closed_areas <- function(mpa_network_shp = closed, ssid = NULL, fishery = NULL, level = c("X", "C"), out_crs = 3156){
  closed %<>% sf::st_transform(crs = out_crs)
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

#' Load survey set data
#'
#' @param spp Species name(s) to get survey data for (in lower case; e.g., spp = "yelloweye rockfish" or spp = c("yelloweye rockfish", "pacific cod"))
#' @param ssid Survey series id(s) to get survey data for
#' @param dir Directory where species data from gfdata::get_survey_sets() exists (.rds file(s))
#' @param min_year Earliest year to retrieve survey data for
#' @export
#' @example
#' \dontrun{
#' survey_sets <- import_survey_sets("yelloweye-rockfish", ssid = c(1, 3, 4, 16), dir = dir, min_year = 2003)
#' }
import_survey_sets <- function(spp, ssid, dir, min_year = 1950){
  spp <- spp %>% gsub(pattern = " ", replacement = "-")

  if(file.exists(paste0(dir, spp, ".rds"))){
    survey_sets <- readRDS(paste0(dir, spp, ".rds"))$survey_sets %>%
      filter(survey_series_id %in% ssid) %>%
      filter(year >= min_year) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs(4326), remove = FALSE) %>%
      st_transform(3156)
  }
  else{data <- gfdata::get_survey_sets(spp, ssid)}
}

#' Plot survey point data
#'
#' @param data Data for one species from `gfdata::get_survey_sets()` (or `import_survey_sets()[spp]`)
#' @param ssid Survey series id(s) to plot data for
#' @export
#' @example
#' \dontrun{
#' gfdata::get_survey_sets("yeloloweye rockfish") %>%
#'  plot_survey_pts(ssid = 1)}
plot_survey_pts <- function(data = survey_sets, ssid = NULL){
 if (is.null(ssid)) {
   stop("Please provide ssid.",
     call. = FALSE
   )
 }

  cols <- paste0(c(RColorBrewer::brewer.pal(8L, "Set1")))
  data <- data %>% filter(survey_series_id %in% ssid)

  g <- ggplot() + geom_sf(data = st_transform(BC_UTM9, crs = 4326)) +
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



# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------

#' Clip survey set data by proposed MPA closed areas
#'
#' @param data Survey set data for a species from `gfdata::get_survey_sets()`
#' @param ssid Survey series id(s) to select closed areas
#' @export
clip_by_mpa <- function(data = survey_sets, ssid){
  data <- data %>% filter(survey_series_id %in% ssid)
  message(nrow(data), " unclipped fishing events in ", unique(data$survey_series_desc), " survey for ", unique(data$species_common_name))
  int <- suppressMessages(sf::st_intersects(closed_areas(ssid = ssid), data[,"fishing_event_id"]))
  excluded <- data$fishing_event_id[unlist(int)]
  data_exclude <- filter(data, !fishing_event_id %in% excluded)
  removed <- filter(data, fishing_event_id %in% excluded)
  stopifnot(identical(nrow(data) - nrow(removed), nrow(data_exclude)))
  message(nrow(removed), " fishing events removed")
  return(data_exclude)
}


# ------------------------------------------------------------------------
# Survey indices
# ------------------------------------------------------------------------

#' Survey design-based biomass index
#'
#' @param dat List containing survey set data by species (alread filtered for desired survey series id(s))
#' @export
design_biomass <- function(dat){
  dat %>% group_split(survey_series_id) %>%
    # purrr::map(boot_biomass) # old way (retained here for trouble-shooting)
    furrr::future_map_dfr(boot_biomass)
}

#' Survey st model-based biomass index
#'
#' @param dat List containing survey set data by species (alread filtered for desired survey series id(s))
#' @export
sdmTMB_biomass <- function(dat){
  dat %>% group_split(survey_series_id) %>%
    # purrr::map(st_biomass)
    furrr::future_map(st_biomass) %>%
    purrr::discard(is.null) %>%
    purrr::map_dfr(~mutate(.$index, species_name = .$species_name, survey = .$survey)) %>%
    as_tibble()
}

