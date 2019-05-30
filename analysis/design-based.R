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
  theme_light() +
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
