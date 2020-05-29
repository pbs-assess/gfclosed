# This script loads shapefiles for the proposed Northern bioshelf region MPA
# network and DFO Pacific Region surveys (currently works with the synoptic
# bottom trawl surveys) and creates spatial objects Species data from the
# surveys can be brought in with gfdata::get_survey_sets("species of interest").
# The survey data can be clipped by appropriate MPA's using clip_by_mpa() and
# specifying the fishery/gear type of interest (default fishery = "trawl"), and
# desired restriction level ("X" = closed to all activity related to given
# fishery, "C" = conditionally closed to activities related to given fishery).
# The survey spatial objects can be similarly clipped by the MPA's using
# clip_survey()

# plan(multisession, workers = availableCores() / 2)

# create MPA spatial object from original MPA shapefile (created by Dana from gdb from Katie Gale)
if (Sys.info()[['user']] == "keppele") {
  closed <- read_sf(dsn = "data/NSB_MPA", layer = "NSB_MPA") %>%
    select(-OBJECTID_1)
  names(closed) <- c(names(read_sf("D:/MPA/draft MPA network Apr15 2020/MPA.gdb", layer = "Spatial_J1_20200403_Full_Attributes"))[1:73], "geometry")
} else {
  closed <- readRDS("data/closed.rds")
}

# Filter closed areas spatial file for restricted areas by fishery type/gear of interest.
# This is for later plotting. Clip_by_mpa() will determine appropriate zones to clip by based on input ssid's or fishery
trawl <- closed_areas(closed, fishery = "trawl")
# ll <- closed_areas(fishery = "longline")
# trap <- closed_areas(fishery = "trap") # not yet functional for trap

# get survey set data
if (Sys.info()[['user']] %in% c("KeppelE", "keppele" )) {
dir = "D:/GitHub/pbs-assess/gfsynopsis-old/report/data-cache/"
}
spp <- c("yelloweye rockfish", "pacific cod") # example species
ye_all <- import_survey_sets("yelloweye rockfish", ssid = c(1,3,4,16), dir, min_year = 2016) # smaller dataset for testing with one species
data_all <- purrr::map(spp, import_survey_sets, ssid = c(1, 3, 4, 16), dir, min_year = 2016) # using years since 2016 for testing
names(data_all) <- spp





# Clip survey sets data by applicable MPA restricted zones
data_exclude <- purrr::map(data_all, clip_by_mpa, ssid = c(1, 3, 4, 16))

#-------------------------------------------------
# Calculate new area_km2 for each stratum/ssid (=grouping code) - should only need to do this once
# then join into original data_all for calculating density in calc_bio() based on clipped extent

# Create list of spatial synoptic survey objects and reduced-area synoptic survey objects
# Using the active block syn shape files updated from 2019 survey
import_survey_shps <- function(shp_name){
  syn <- "data/SynSurveyShps"
  col <- c("block", "grouping_code", "selection_ind", "survey_series_id", "survey_series_desc", "survey_series_abbrev", "geometry")
  sf::st_read(dsn=syn, layer = shp_name) %>%
    st_transform(crs = 3156) %>%
    set_names(col)
}

# Create spatial objects from survey shapefiles
syn_names <- c("HS_active_2020", "QCS_active_2020", "WCHG_active_2020", "WCVI_active_2020")
syn_surveys <- map(syn_names, import_survey_shps)
names(syn_surveys) <- c("hs", "qcs", "wchg", "wcvi")

make_survey_grid <- function(dat){
  dat %>% st_centroid() %>%
    dplyr::mutate(Y = sf::st_coordinates(.)[,1],
      X = sf::st_coordinates(.)[,2])
}

syn_grid <- map_df(syn_surveys, make_survey_grid)
# saveRDS(syn_surveys, "data/syn_surveys.rds")
# saveRDS(syn_grid, "data/syn_grid.rds")
# syn_surveys <- readRDS("data/syn_surveys.rds")

# Clip by mpa restrictions
syn_surveys_exclude <- map(syn_surveys, clip_survey, fishery = "trawl")
syn_grid_exclude <- map_df(syn_surveys_exclude, make_survey_grid)
# saveRDS(syn_surveys_exclude, "data/MPA_reduced_syn_surveys.rds")
# saveRDS(syn_grid_exclude, "data/syn_grid_exclude.rds")
# syn_surveys_exclude <- readRDS("data/MPA_reduced_syn_surveys.rds")

# Determine area of original survey grid and MPA-clipped survey grid
survey_area <- function(dat){
  g <- dat %>% group_by(grouping_code) %>% group_keys()
  area <- dat %>% group_split(grouping_code)  %>% map(st_combine)  %>% map(st_area) %>% unlist()/1000000
  area <- data.frame(grouping_code = g$grouping_code, area)
}

shp_area <- map_df(syn_surveys, survey_area) %>% rename(shp_area = area)
shp_exclude_area <- map_df(syn_surveys_exclude, survey_area) %>% rename(restricted_area = area)

# Compare calculated areas from active survey block shapefiles against survey grid area reported in gfbio.
# TO DO: Connect to VPN, pull gfbio GROUPING table and update YE & pcod cache_pbs_data() .rds files.
# Recheck area summary.
data_all_df <- data_all[[1]] %>% as.data.frame() %>% select(-geometry)
gfbio_areas <- unique(data_all_df[c("survey_series_id", "grouping_code", "area_km2")]) %>%
  arrange(survey_series_id, area_km2)
area_summary <- inner_join(gfbio_areas, shp_area) %>% inner_join(shp_exclude_area)
# saveRDS(area_summary, "data/area_summary.rds")


# Join MPA-reduced survey area by stratum to data_all (for design-based analysis)
shp_exclude_area_list <- replicate(length(data_all), shp_exclude_area, simplify = FALSE)
data_all_w_reduced <- map2(data_all, shp_exclude_area_list, left_join)
data_exclude_w_reduced <- map2(data_exclude, shp_exclude_area_list, left_join)

#ggplot(reduced_synoptic_grid) + geom_sf(aes()) +coord_sf(xlim = c(-133.2, -129), ylim = c(52.5, 54.5), crs = sf::st_crs(4326))

### TO DO: FIX SO it RUNS FOR SINGLE SPECIES

# d <- design_biomass(dat)
# m <- sdmTMB_biomass(dat)

# scenarios:
# design_biomass(data_all, full_survey_extent)
# design_biomass(data_exclude, full_survey_extent)
#
# design_biomass(data_all, reduced_survey_extent) TO DO
# design_biomass(data_exclude, reduced_survey_extent) TO DO
#
# sdmTMB_biomass(data_all, full_survey_extent)
# sdmTMB_biomass(data_exclude, full_survey_extent)
#
# sdmTMB_biomass(data_all, reduced_survey_extent) TO DO
# sdmTMB_biomass(data_exclude, reduced_survey_extent) TO DO



# dat = list of survey_sets data for various species
get_indices <- function(dat, dat_exclude = NULL, ssid = c(1, 3, 4, 16), design = TRUE, model = TRUE){
  # dat %<>% filter(servey_series_id %in% ssid)
  # d_exclude %<>% filter(servey_series_id %in% ssid)

  if (design) {
    d <- furrr::future_map_dfr(dat, design_biomass)
    if (!is.null(dat_exclude)) {
      d_exclude <- furrr::future_map_dfr(dat_exclude, design_biomass)
    }
  }

  if (model){
    m <- furrr::future_map_dfr(dat, sdmTMB_biomass)
    if (!is.null(dat_exclude)) {
      m_exclude <- furrr::future_map_dfr(dat_exclude, sdmTMB_biomass)
      }
  }
  rbind(if(exists("d")) d, if(exists("d_exclude")) d_exclude, if(exists("m")) m, if(exists("m_exclude")) m_exclude)
}


i <- get_indices(data_all, data_exclude)


# ------------------------------ PLOTTING -------------------------------------
coastUTM <- read_sf("data/baselayer_shps/BC_coast_UTM9.shp") %>% st_transform(3156)
syn <- "data/SynopticTrawlSurveyBoundaries"
# hbll <- "data/HBLL_boundaries"
hs <- sf::st_read(dsn=syn, layer = "HS_BLOB")
qcs <- sf::st_read(dsn=syn, layer = "QCS_BLOB")
wchg <- sf::st_read(dsn=syn, layer = "WCHG_BLOB")
wcvi <- sf::st_read(dsn=syn, layer = "WCVI_BLOB")

theme_set(theme_bw())
cols <- paste0(c(RColorBrewer::brewer.pal(8L, "Set1")))

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

# Plot survey boundaries and trawl-restricting MPAs
g <- ggplot() + geom_sf(data = coastUTM)

g +
  geom_sf(data = hs, fill = paste0(cols[3], "60")) +
  #
  # geom_sf(data = qcs, fill = paste0(cols[2], "60")) +
  # geom_sf(data = wchg, fill = paste0(cols[4], "60")) +
  # geom_sf(data = wcvi, fill = paste0(cols[1], "60")) +
  # geom_sf(data = trawl, fill = paste0(cols[6], "60")) +
  geom_sf(data = reduced_synoptic_grid, fill = paste0(cols[5])) +
  coord_sf(xlim = c(-134, -123), ylim = c(48, 55), crs = sf::st_crs(4326)) #+
# coord_sf(xlim =c(360, 653), ylim = c(5275, 6155), crs = sf::st_crs(3156)) +
#ggtitle("Proposed bottom trawl restriction zones in MPA network")

g +
  #geom_sf(data = hs, fill = hs$grouping_code) +
  #
  # geom_sf(data = qcs, fill = paste0(cols[2], "60")) +
  # geom_sf(data = wchg, fill = paste0(cols[4], "60")) +
  # geom_sf(data = wcvi, fill = paste0(cols[1], "60")) +
  # geom_sf(data = trawl, fill = paste0(cols[6], "60")) +
  ggplot() + geom_sf(data = gfplot::synoptic_grid)
  coord_sf(xlim = c(-134, -123), ylim = c(48, 55), crs = sf::st_crs(4326)) #+
  # coord_sf(xlim =c(360, 653), ylim = c(5275, 6155), crs = sf::st_crs(3156)) +
  #ggtitle("Proposed bottom trawl restriction zones in MPA network")

# Plot survey data points, overlain with reduced survey data set
ggplot() + geom_sf(data = coastUTM) +
  geom_point(data_all, mapping = aes(y = latitude, x = longitude), size = 0.1, pch = 4, color = "dark blue") +
  geom_point(data_exclude, mapping = aes(y = latitude, x = longitude), size = 0.1, pch = 4, color = "light blue") +
  coord_sf(xlim = c(-134, -123), ylim = c(48, 55), crs = sf::st_crs(4326)) +
  # coord_sf(xlim =c(360, 653), ylim = c(5275, 6155), crs = sf::st_crs(3156)) +
  ggtitle("Proposed bottom trawl restriction zones in MPA network")

# Plot design-based biomass indices


# TO DO: make a function to plot biomass for a given species and ssid
plot_design_based_biomass <- function(dat){

  ggplot(i) +
    geom_line(aes(year, index)), ymin = lwr, ymax = upr)) +
  geom_ribbon(alpha = 0.4) +
  geom_line() +
  facet_wrap(~survey_series_id)
}

# ggplot(data_exclude, aes(longitude, latitude)) +
#   geom_point(colour = "grey40", pch = 4, alpha = 0.4) +
#   coord_equal() +
#   facet_wrap(~year) +
#   gfplot::theme_pbs() +
#   geom_point(data = removed, colour = "red", pch = 21)



# ss <- split(survey_sets, survey_sets$survey_series_id)
# t <- map(ss, clip_by_mpa, ssid = c(1,3,4,16, 22, 36))

design_based_index <- i %>% group_split(species_common_name) %>% map(i, plot_design_based_biomass)
