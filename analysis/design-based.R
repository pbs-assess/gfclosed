library(gfclosed)
library(dplyr)
library(ggplot2)
library(future)
plan(multiprocess)

# ------------------------------------------------------------------------
# Load closed-area polygon shapefiles
wd <- getwd()
setwd("~/Downloads/closed_area_shps") # FIXME: make this local
sponge_reefs <- sf::st_read(dsn = ".", layer = "WDPA_gsr")
# GH_all <- sf::st_read(dsn = ".", layer = "WDPA_gwaii_haanas")
GH_plan <- sf::st_read(dsn = ".", layer = "gwaii_haanas_plan_georef")
setwd(wd) # FIXME: make this local

# ------------------------------------------------------------------------
# Get survey data, filter by closed-area polygons

# data_all <- gfdata::get_survey_sets("pacific ocean perch", ssid = c(1, 3, 4, 16),
#   joint_sample_ids = TRUE)
data_all <- readRDS("../gfsynopsis/report/data-cache/pacific-cod.rds")$survey_sets # FIXME: remove
data_all <- filter(data_all, survey_series_id %in% c(1, 3, 4, 16)) # FIXME: remove
data_exclude <- exclude_areas(
  data_all, list(sponge_reefs, GH_plan), "longitude", "latitude"
)
removed <- anti_join(data_all, select(data_exclude, fishing_event_id),
  by = "fishing_event_id"
)

message(nrow(removed), " fishing events removed")
stopifnot(identical(nrow(data_all) - nrow(removed), nrow(data_exclude)))

ggplot(data_exclude, aes(longitude, latitude)) +
  geom_point(colour = "grey40") +
  coord_equal() +
  gfplot::theme_pbs()
  geom_point(data = removed, colour = "red")

# ------------------------------------------------------------------------
# Get design-based bootstrap survey index values

get_design_based_index <- function(dat, reps = 1000L) {
  combinations <- expand.grid(
    x = unique(dat$species_code),
    y = unique(dat$survey_series_id), stringsAsFactors = FALSE
  )
  combinations <- purrr::pmap(combinations, function(x, y) c(x, y))
  index <- future.apply::future_lapply(combinations, function(x) {
    .dat <- filter(dat, species_code == x[1], survey_series_id == x[2])
    boot_biomass(.dat, reps = reps)
  })
  dplyr::bind_rows(index)
}

index_all <- get_design_based_index(data_all)
index_exclude <- get_design_based_index(data_exclude)

index_all$type <- "All"
index_exclude$type <- "Excluded"

dplyr::bind_rows(index_all, index_exclude) %>%
  group_by(species_common_name, survey_series_desc) %>%
  mutate(geo_mean = exp(mean(log(biomass), na.rm = TRUE))) %>%
  mutate(biomass = biomass/geo_mean) %>%
  mutate(lwr = lwr/geo_mean) %>%
  mutate(upr = upr/geo_mean) %>%
  ggplot(aes(x = year, fill = type, colour = type)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) +
  geom_line(aes(y = biomass)) +
  geom_point(aes(y = biomass)) +
  facet_grid(species_common_name~survey_series_desc, scales = "free_y") +
  xlab("") +
  ylab("Relative biomass (divided by the geometric mean)") +
  gfplot::theme_pbs()
