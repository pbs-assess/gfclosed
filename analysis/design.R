# remember to load functions locally; must have dat filtered by syn trawl surveys only
library(dplyr)
library(ggplot2)
library(sdmTMB)
library(gfplot)
library(sf)
library(tidyr)
library(magrittr)
library(future)
library(purrr)
library(furrr)
plan(multisession, workers = availableCores() / 2)

# get survey set data
dir = "D:/GitHub/pbs-assess/gfsynopsis-old/report/data-cache/"
spp <- c("yelloweye rockfish", "pacific cod") # example species
# data_all <- import_survey_sets("yelloweye rockfish", ssid = c(1, 4), dir) %>% filter(year > 2010)
data_all <- purrr::map(spp, import_survey_sets, ssid = c(1, 3, 4, 16), dir)
names(data_all) <- spp


# get MPA spatial file from original shapefile (created by Dana from gdb from Katie Gale)
# closed <- read_sf(dsn = "data/NSB_MPA", layer = "NSB_MPA") %>%
#   select(-OBJECTID_1)
# names(closed) <- c(names(read_sf("D:/MPA/draft MPA network Apr15 2020/MPA.gdb", layer = "Spatial_J1_20200403_Full_Attributes"))[1:73], "geometry")
# names(closed) <- readRDS(MPA_names, "data/MPA_cols.rds")
# saveRDS(closed, "data/closed.rds")
closed <- readRDS("data/closed.rds")

# filter closed areas spatial file for fishery type/gear of interest
trawl <- closed_areas(closed, fishery = "trawl")

# clip survey sets data by applicable MPA restricted zones
data_exclude <- purrr::map(data_all, clip_by_mpa, ssid = c(1, 3, 4, 16))



# d <- design_biomass(dat)
#
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
get_indices <- function(dat, dat_exclude, design = TRUE, model = TRUE){
  if (design) {
    d <- furrr::future_map_dfr(dat, design_biomass) %>%
      select(species_name = species_common_name, survey_series_desc, year, index = mean_boot,
        lwr, upr, cv)
    if (!is.null(dat_exclude)) {
      d_exclude <- furrr::future_map_dfr(dat_exclude, design_biomass) %>%
        select(species_name = species_common_name, survey_series_desc, year, index = mean_boot,
          lwr, upr, cv)
    }
  }
  if (model){
    m <- furrr::future_map_dfr(dat, sdmTMB_biomass) %>%
      select(species_name, survey_series_desc = survey, survey, year, index = est,
        lwr, upr, cv)
    if (!is.null(dat_exclude)) {
      d_exclude <- furrr::future_map_dfr(dat_exclude, sdmTMB_biomass) %>%
        select(species_name = species_common_name, survey_series_desc, year, index = mean_boot,
          lwr, upr, cv)
    }
  }
  rbind(d, m)
}

# TO DO: add in label/column for analysis type
i <- get_indices(data_all, dat_exclude = data_exclude)


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

# Plot survey boundaries and trawl-restricting MPAs
ggplot() + geom_sf(data = coastUTM) +
  geom_sf(data = hs, fill = paste0(cols[3], "60")) +
  geom_sf(data = qcs, fill = paste0(cols[2], "60")) +
  geom_sf(data = wchg, fill = paste0(cols[4], "60")) +
  geom_sf(data = wcvi, fill = paste0(cols[1], "60")) +
  geom_sf(data = trawl, fill = paste0(cols[6], "60")) +
  coord_sf(xlim = c(-134, -123), ylim = c(48, 55), crs = sf::st_crs(4326)) +
  # coord_sf(xlim =c(360, 653), ylim = c(5275, 6155), crs = sf::st_crs(3156)) +
  ggtitle("Proposed bottom trawl restriction zones in MPA network")

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
dat %>%
  ggplot(aes(year, index, ymin = lwr, ymax = upr)) +
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
