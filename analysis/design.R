# remember to load functions locally; must have dat filtered by syn trawl surveys only
if (Sys.info()[['user']] == "seananderson") {
  dat <- readRDS("../gfsynopsis/report/data-cache/yelloweye-rockfish.rds")$survey_sets %>%
    dplyr::filter(survey_series_id %in% c(1, 3, 4, 16))
} else {
  dat1 <- readRDS("D:/GitHub/pbs-assess/gfsynopsis-old/report/data-cache/yelloweye-rockfish.rds")$survey_sets %>%
    dplyr::filter(survey_series_id %in% c(1, 3), year > 2016)
}

dat2 <- readRDS("D:/GitHub/pbs-assess/gfsynopsis-old/report/data-cache/pacific-cod.rds")$survey_sets %>%
  dplyr::filter(survey_series_id %in% c(1, 3), year > 2016)

dat <- list(dat1, dat2)



plan(multisession, workers = availableCores() / 2)




# pre-MPA data design-based predict to original survey area
# pre-MPA data design-based predict to reduced survey area

# MPA-reduced data design-based predict to original survey area
# MPA-reduced data design-based predict to reduced survey area

# pre-MPA data st model-based predict to original survey area
# pre-MPA data st model-based predict to reduced survey area

# MPA-reduced st model-based predict to original survey area
# MP-reducedA st model-based predict to reduced survey area

design_biomass <- function(dat){
  dat <- dat %>% group_split(survey_series_id) %>%
    # purrr::map(boot_biomass)
    furrr::future_map_dfr(boot_biomass)
  dat
}

sdmTMB_biomass <- function(dat){
  dat %>% group_split(survey_series_id) %>%
    # purrr::map(my_function)
    furrr::future_map(my_function) %>%
    purrr::discard(is.null) %>%
    purrr::map_dfr(~mutate(.$index, species_name = .$species_name, survey = .$survey)) %>%
    as_tibble()
}

# d <- design_biomass(dat)
#
# m <- sdmTMB_biomass(dat)

# scenarios:
design_biomass(data_all, full_survey_extent)
design_biomass(data_exclude, full_survey_extent)

design_biomass(data_all, reduced_survey_extent)
design_biomass(data_exclude, reduced_survey_extent)

sdmTMB_biomass(data_all, full_survey_extent)
sdmTMB_biomass(data_exclude, full_survey_extent)

sdmTMB_biomass(data_all, reduced_survey_extent)
sdmTMB_biomass(data_exclude, reduced_survey_extent)



# dat = list of survey_sets data for various species
get_indices <- function(dat, design = TRUE, model = TRUE){
  if (design) {
    d <- furrr::future_map_dfr(dat, design_biomass) %>%
      select(species_name = species_common_name, survey_series_desc, year, index = mean_boot,
        lwr, upr, cv)
  }
  if (model){
    m <- furrr::future_map_dfr(dat, sdmTMB_biomass) %>%
      select(species_name, survey_series_desc = survey, year, index = est,
        lwr, upr, cv)
  }
  rbind(d, m)
}

# TO DO: add in label/column for analysis type
i <- get_indices(dat)
