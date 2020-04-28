library(gfclosed)
library(dplyr)
library(ggplot2)
library(future)
plan(multiprocess)

# ------------------------------------------------------------------------
# Load closed-area polygon shapefiles
# wd <- getwd()
# setwd("~/Downloads/Gwaii Haanas Land-Sea-People plan FINAL ZONING_Nov 2018") # FIXME: make this local
# setwd("~/Downloads/closed_area_shps")
# sponge_reefs <- sf::st_read(dsn = ".", layer = "WDPA_gsr")
# GH_all <- sf::st_read(dsn = ".", layer = "WDPA_gwaii_haanas")
GH_plan <- sf::st_read(dsn = "data/closed_area_shps", layer = "gwaii_haanas_plan_georef")
gh_plan_multi <- GH_plan[GH_plan$Zone_Type == "Multiple Use (IUCN VI)",]
gh_plan_strict <- GH_plan[GH_plan$Zone_Type == "Strict Protection (IUCN II)",]

gh_plan_multi <- st_transform(gh_plan_multi, crs = 4326)
gh_plan_strict <- st_transform(gh_plan_strict, crs = 4326)

# setwd(wd) # FIXME: make this local


# ------------------------------------------------------------------------
# Get survey data, filter by closed-area polygons

spp <- c("silvergray rockfish")
data_all <- gfdata::get_survey_sets(spp, ssid = c(1, 3, 4, 16),
  joint_sample_ids = TRUE)
# data_all <- readRDS("../gfsynopsis/report/data-cache/pacific-cod.rds")$survey_sets # FIXME: remove
data_all <- readRDS("D:\\GitHub\\pbs-assess\\gfsynopsis-old\\report\\data-cache\\yelloweye-rockfish.rds")$survey_sets # FIXME: remove
# data_all <- readRDS("../gfsynopsis/report/data-cache/pacific-ocean-perch.rds")$survey_sets # FIXME: remove
# data_all <- filter(data_all, survey_series_id %in% c(1, 3, 4, 16)) # FIXME: remove

data_all <- filter(data_all, survey_series_id %in% c(1))
# data_exclude <- exclude_areas(
#   data_all, list(gh_plan_multi, gh_plan_strict), "longitude", "latitude"
# )

xx <- data_all %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
    crs = sf::st_crs(gh_plan_multi), remove = FALSE)

cols <- RColorBrewer::brewer.pal(4, "Set3")
pdf("map.pdf", width = 7, height = 9)
plot(st_geometry(gh_plan_multi), col = paste0(cols[1], "60"))
plot(st_geometry(gh_plan_strict), add = TRUE, reset=FALSE, col  = paste0(cols[2], "60"))
plot(st_geometry(xx[,"fe_major_level_id"]), col = "black", pch = 4,
  add = TRUE, reset = FALSE)
dev.off()

int <- sf::st_intersects(gh_plan_multi, xx[,"fe_major_level_id"])
int2 <- sf::st_intersects(gh_plan_strict, xx[,"fe_major_level_id"])
excluded <- union(xx$fishing_event_id[unlist(int)], xx$fishing_event_id[unlist(int2)])
excluded2 <- xx$fishing_event_id[unlist(int2)]

message(length(excluded), " fishing events removed")
message(length(excluded2), " fishing events removed")

data_exclude <- filter(data_all, !fishing_event_id %in% excluded)
data_exclude <- filter(data_all, !fishing_event_id %in% excluded2)

removed <- anti_join(data_all, select(data_exclude, fishing_event_id),
  by = "fishing_event_id"
)

message(nrow(removed), " fishing events removed")
stopifnot(identical(nrow(data_all) - nrow(removed), nrow(data_exclude)))

length(unique(data_exclude$grouping_code))
length(unique(data_all$grouping_code))

ggplot(data_exclude, aes(longitude, latitude)) +
  geom_point(colour = "grey40", pch = 4, alpha = 0.4) +
  coord_equal() +
  facet_wrap(~year) +
  gfplot::theme_pbs() +
  geom_point(data = removed, colour = "red", pch = 21)

# ------------------------------------------------------------------------
# Get design-based bootstrap survey index values
data_all <- survey_sets %>% filter(survey_series_id == 1)
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
  mutate(biomass = biomass / geo_mean) %>%
  mutate(lwr = lwr / geo_mean) %>%
  mutate(upr = upr / geo_mean) %>%
  ggplot(aes(x = year, fill = type, colour = type)) +
  geom_vline(xintercept = 2003:2018, lty = 1, col = "grey90") +
  geom_vline(xintercept = seq(2005, 2015, 5), lty = 1, col = "grey75") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5, colour = NA) +
  geom_line(aes(y = biomass), lwd = 0.8) +
  geom_point(aes(y = biomass), size = 2) +
  facet_grid(species_common_name ~ survey_series_desc, scales = "free_y") +
  xlab("") +
  ylab("Relative biomass\n(divided by the geometric mean)") +
  gfplot::theme_pbs() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  ylim(0, NA)

ggsave("ts.pdf", width = 6, height = 4)
ggsave("ts-strict.pdf", width = 6, height = 4)
