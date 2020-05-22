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
  mpa_network_shp %<>% sf::st_transform(crs = out_crs)
  if (ssid %in% c(1, 3, 4, 16) && is.null(fishery) || is.null(ssid) && fishery == "trawl"){
    closed <- mpa_network_shp %>%
      select(UID, hu_co_demersalfishing_bottomtrawling_d) %>%
      filter(hu_co_demersalfishing_bottomtrawling_d %in% level)}
  else if(ssid %in% c(22, 36) && is.null(fishery) || is.null(ssid) && fishery == "ll"){
    closed <- mpa_network_shp %>%
      select(UID, hu_co_demersalfishing_bottomlongline_d) %>%
      filter(hu_co_demersalfishing_bottomlongline_d %in% level)}
  # else if(ssid %in% c()){  # sablefish surveys
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

#' Clip survey set data by proposed MPA closed areas
#'
#' @param data Survey set data for a species from `gfdata::get_survey_sets()`
#' @param ssid Survey series id(s) to select closed areas
#' @export
clip_by_mpa <- function(data = survey_sets, ssid){
  data <- data %>% filter(survey_series_id %in% ssid)
  message(nrow(data), " unclipped fishing events in ", unique(data$survey_series_desc), " survey for ", unique(data$species_common_name), " since ", min(data$year))
  int <- suppressMessages(sf::st_intersects(closed_areas(ssid = ssid), data[,"fishing_event_id"]))
  excluded <- data$fishing_event_id[unlist(int)]
  data_exclude <- filter(data, !fishing_event_id %in% excluded)
  removed <- filter(data, fishing_event_id %in% excluded)
  stopifnot(identical(nrow(data) - nrow(removed), nrow(data_exclude)))
  message(nrow(removed), " fishing events removed")
  return(data_exclude)
}

convert2utm <- function(df, coords = c("X", "Y"), out_crs = 3156) { # from yelloweye-inside utils
  x <- sf::st_as_sf(df,
    coords = coords, crs = 4326
  ) %>%
    sf::st_transform(crs = out_crs) %>%
    sf::st_coordinates() %>%
    as.data.frame()
  x$X <- x$X / 1000
  x$Y <- x$Y / 1000
  dplyr::bind_cols(x,
    df[, which(!names(df) %in% coords), drop = FALSE])
}

expand_prediction_grid <- function(grid, years) { # from yelloweye-inside utils
  nd <- do.call("rbind",
    replicate(length(years), grid, simplify = FALSE))
  nd[["year"]] <- rep(years, each = nrow(grid))
  nd
}

#' Calculate geostatistical survey biomass index
#'
#' @param dat Survey set data for one species and one survey series id.
#' @param n_knots Number of knots for creating spde mesh.
#' @param anisotropy Include TRUE/FALSE
#' @param silent
#' @param bias_correct
#' @param species_name
#'
#' @return
#' @export
#'
#' @examples
st_biomass <- function(dat, fishery = "trawl",
  n_knots = 150, anisotropy = FALSE, silent = TRUE,
  bias_correct = FALSE, species_name){

  ssid <- unique(dat$survey_series_id)
  survey <- unique(dat$survey_series_desc)
  species_name <- unique(dat$species_common_name) %>%
    gsub(pattern = " ", replacement = "_")

  # set up data
  dat$density <- if (ssid %in% c(1, 3, 4, 16)) dat$density_kgpm2 else dat$density_ppkm2
  dat$density <- dat$density * 1000 # to keep numbers from being too small for optimizer
  dat <- dat %>%
    dplyr::filter(!(year == 2014 & survey_series_id == 16)) %>%
    select(year, survey_series_id, fishing_event_id, longitude, latitude, density, species_code) %>%
    mutate(Y = latitude, X = longitude)
  dat <- dat[!duplicated(select(dat, year, fishing_event_id)), , drop = FALSE]
  dat <- as_tibble(convert2utm(dat))

  dat <- mutate(dat, present = ifelse(density > 0, 1, 0))
  if (mean(dat$present) < 0.05) {
    warning("Not enough data for SSID ", ssid, "; ", species_name, ".")
    return(NULL)
  }

  # prepare survey grid for each year
  synoptic_grid <- gfplot::synoptic_grid %>%
    mutate(survey_series_id = case_when(
      survey == "SYN QCS" ~ 1,
      survey == "SYN HS" ~ 3,
      survey == "SYN WCVI" ~ 4,
      survey == "SYN WCHG" ~ 16
    ))
  # prepare MPA-reduced survey grid for each year
  #
  # reduced_synoptic_grid <- readRDS("data/MPA_reduced_syn_surveys.rds")[[ssid]]
  #
  # if (ssid %in% c(1, 3, 4, 16)) {
  #   survey_grid <- synoptic_grid
  #   reduced_survey_grid <- reduced_synoptic_grid
  # }
  # if (ssid == 22) {
  #   survey_grid <- gfplot::hbll_n_grid$grid
  #   stop("Non-synoptic surveys are not implemented yet.", call. = FALSE)
  # }
  # if (ssid == 36) {
  #   survey_grid <- gfplot::hbll_s_grid$grid
  #   stop("Non-synoptic surveys are not implemented yet.", call. = FALSE)
  # }
  #
  # survey_grid <- survey_grid %>%
  #   dplyr::filter(survey_series_id == ssid) %>%
  #   dplyr::select(.data$X, .data$Y) %>%
  #   expand_prediction_grid(years = unique(dat$year))
  # reduced_survey_grid <- reduced_survey_grid %>%
  #   dplyr::select(geometry) %>%
  #   expand_prediction_grid(years = unique(dat$year))

  reduced_synoptic_grid <- gfplot::synoptic_grid %>%
    mutate(survey_series_id = case_when(
      survey == "SYN QCS" ~ 1,
      survey == "SYN HS" ~ 3,
      survey == "SYN WCVI" ~ 4,
      survey == "SYN WCHG" ~ 16
    )) %>% sf::st_as_sf(coords = c("X", "Y"), agr = "constant", crs = st_crs(3156), remove = FALSE) %>%
    mutate(ID = seq(1:nrow(gfplot::synoptic_grid))) %>%
    clip_survey(fishery = "trawl")

  if (ssid %in% c(1, 3, 4, 16)) {
    survey_grid <- synoptic_grid
    reduced_survey_grid <- reduced_synoptic_grid
  }
  if (ssid == 22) {
    survey_grid <- gfplot::hbll_n_grid$grid
    stop("Non-synoptic surveys are not implemented yet.", call. = FALSE)
  }
  if (ssid == 36) {
    survey_grid <- gfplot::hbll_s_grid$grid
    stop("Non-synoptic surveys are not implemented yet.", call. = FALSE)
  }

  survey_grid <- survey_blockgrid %>%
    dplyr::filter(survey_series_id == ssid) %>%
    dplyr::select(.data$X, .data$Y) %>%
    expand_prediction_grid(years = unique(dat$year))

  # fit model
  formula <- stats::as.formula(density ~ 0 + as.factor(year))
  spde <- sdmTMB::make_spde(dat$X, dat$Y, n_knots = n_knots)

  tictoc::tic(paste0("sdmTMB fit for ", species_name, "; SSID ", ssid))
  m <- sdmTMB::sdmTMB(
    formula = formula,
    data = dat, time = "year", spde = spde, family = sdmTMB::tweedie(link = "log"),
    anisotropy = anisotropy, silent = silent#,
    #nlminb_loops = 2, newton_steps = 1
  )
  tictoc::toc()

  predictions <- stats::predict(m, newdata = survey_grid, return_tmb_object = TRUE)
  restricted_predictions <- stats::predict(m, newdata = reduced_survey_grid, return_tmb_object = TRUE)
  index <- sdmTMB::get_index(predictions, bias_correct = bias_correct)
  index <- dplyr::mutate(index, cv = sqrt(exp(se^2) - 1))
  list(
    data = dat,
    model = m,
    spde = spde,
    predictions = predictions,
    index = index,
    survey = survey,
    species_name = species_name
  )
}


clip_survey <- function(survey_dat, fishery = "trawl", id_col = "block"){
  int <- suppressMessages(sf::st_intersects(closed_areas(fishery = fishery), survey_dat[,id_col]))
  excluded <- survey_dat[[id_col]][unlist(int)]
  survey_exclude <- filter(survey_dat, !survey_dat[[id_col]] %in% excluded)
  removed <- filter(survey_dat, survey_dat[[id_col]] %in% excluded)
  message(nrow(removed), " blocks removed of ", nrow(survey_dat), " for survey series id ", unique(survey_dat$survey_series_id))
  return(survey_exclude)
}

# ------------------------------------------------------------------------
# Survey indices
# ------------------------------------------------------------------------


design_biomass <- function(dat, restricted = FALSE){
  if (restricted){
    dat <- dat %>% select(-area_km2) %>% rename(area_km2 = restricted_area)
  }
  dat %>%
    group_split(survey_series_id) %>%
    # purrr::map(boot_biomass) # retained here for trouble-shooting
    furrr::future_map_dfr(boot_biomass) %>%
    select(analysis, species_name = species_common_name, survey_series_desc, year, index = mean_boot,
      lwr, upr, cv)
}


sdmTMB_biomass <- function(dat){
  dat %>% group_split(survey_series_id) %>%
    # purrr::map(st_biomass) # retained here for trouble-shooting
    furrr::future_map(st_biomass) %>%
    purrr::discard(is.null) %>%
    purrr::map_dfr(~mutate(analysis = "geostatistical", .$index, species_name = gsub("_", " ", .$species_name), survey = .$survey)) %>%
    as_tibble() %>%
    select(analysis, species_name, survey_series_desc = survey, year, index = est,
      lwr, upr, cv)
}

