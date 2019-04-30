library(dplyr)
library(ggplot2)
library(sdmTMB)
library(gfplot)
library(sf)
library(tidyr)
library(magrittr)

#filter output of get_survey_sets() to exclude sets within a given set of polygons
exclude_areas <- function(data, exclude_poly, X, Y){
  if (class(exclude_poly) == "list" && length(exclude_poly) > 1){
    for (i in seq_along(exclude_poly)){
      exclude_poly[[i]] <- as.data.frame(exclude_poly[[i]])
      exclude_poly[[i]] <- select(exclude_poly[[i]], geometry)
    }
    exclude_poly <- do.call(rbind, exclude_poly) %>%
      sf::st_sf() %>%
      sf::st_union()
  }
  data %>%
    sf::st_as_sf(coords = c(X, Y), agr = "constant", crs = sf::st_crs(exclude_poly), remove = FALSE) %>%
    sf::st_difference(exclude_poly) %>%
    as.data.frame() %>%
    select(-geometry) %>%
    return()
}

#calculate design-based biomass estimate from output of get_survey_sets()
calc_bio <- function(dat, i = seq_len(nrow(dat))) {
  dat[i, ] %>% group_by(year, survey_id, area_km2, grouping_code) %>%
    summarise(density = mean(density_kgpm2*1e6)) %>%
    group_by(year) %>%
    summarise(biomass = sum(density * area_km2)) %>%
    pull(biomass)
}

#bootstrap statistics for design-based index
boot_biomass <- function(dat, reps = 100) {
  out <- dat %>%
    group_by(year) %>%
    do({
      b <- boot::boot(., statistic = calc_bio, strata = .$grouping_code, R = reps)
      suppressWarnings(bci <- boot::boot.ci(b, type = "perc"))
      tibble::tibble(
        index = mean(b$t),
        median_boot = median(b$t),
        lwr = bci$percent[[4]],
        upr = bci$percent[[5]],
        cv = sd(b$t)/mean(b$t),
        biomass = calc_bio(.))
    })
}

#fit spatiotemporal biomass model using sdmTMB()
#predict onto 2km grid, calculate biomass index
st_biomass <- function(dat, survey, include_depth) {
  dat %<>%
    gfplot::tidy_survey_sets(survey, years = seq(1, 1e6)) %>%
    gfplot:::interp_survey_bathymetry() %$%
    gfplot:::scale_survey_predictors(data)

  pred_grid <- dat %>%
    filter(., year == max(.$year)) %>%
    gfplot:::make_prediction_grid(cell_width = 2, survey = survey) %$%
    rename(grid, depth = akima_depth)

  dat_spde <- sdmTMB::make_spde(dat$X, dat$Y, 200)
  formula <- if (include_depth)
    as.formula(density ~ 0 + as.factor(year) + depth_scaled + depth_scaled2)
  else
    as.formula(density ~ 0 + as.factor(year))
  dat_m <- sdmTMB::sdmTMB(dat, formula, time = "year", spde = dat_spde, family = tweedie(link = "log"), silent = TRUE)
  preds <- sdmTMB:::predict.sdmTMB(dat_m, newdata = pred_grid)
  index <- sdmTMB::get_index(preds, bias_correct = FALSE) %>%
    mutate(cv = sqrt(exp(se^2) - 1)) %>%
    return()
}

#given data for a single species, calculate design-based and spatiotemporal indices using boot_biomass() and st_biomass()
#join indices into single data frame, standardize all by their geometric mean
create_survey_indices <- function(dat, scenario_name, species) {

  print("fitting design-based model")
  design_ind <- boot_biomass(dat) %>%
    mutate(scenario = scenario_name, method = "design-based", species = species)

  print("fitting spatiotemporal model")
  st_ind <- st_biomass(dat, "SYN QCS", TRUE) %>%
    rename(index = est) %>%
    mutate(scenario = scenario_name, method = "spatiotemporal", species = species)

  design_geo_mean <- exp(mean(log(design_ind$index), na.rm = TRUE))
  st_geo_mean <- exp(mean(log(st_ind$index), na.rm = TRUE))
  design_mean_cv <- mean(design_ind$cv)
  st_mean_cv <- mean(st_ind$cv)

  design_ind %<>%
    mutate(index = index/design_geo_mean,
      lwr = lwr/design_geo_mean,
      upr = upr/design_geo_mean,
      mean_cv = design_mean_cv) %>%
    select(year, scenario, method, species, index, lwr, upr, cv, mean_cv)

  st_ind %<>%
    mutate(index = index/st_geo_mean,
      lwr = lwr/st_geo_mean,
      upr = upr/st_geo_mean,
      mean_cv = st_mean_cv) %>%
    select(year, scenario, method, species, index, lwr, upr, cv, mean_cv)

  x <- bind_rows(design_ind, st_ind)
  return(x)
}





#load closed area polygon shapefiles
sponge_reefs <- sf::st_read(dsn = ".", layer = "WDPA_gsr")
#GH_all <- sf::st_read(dsn = ".", layer = "WDPA_gwaii_haanas")
GH_plan <- sf::st_read(dsn = ".", layer = "gwaii_haanas_plan_georef")
#RCAs <- sf::st_read(dsn = ".", layer = "RCAs_unprojected")

#get survey data, filter by closed area polygons
#spp <- c('044','056','059','222','228','394','396','401','403','405','407','410','417','418','424',
#         '435','437','439','440','442','451','453','455','467','602','607','610','621','626','628')
spp <- c('044','056')
data_all <- gfplot::get_survey_sets(spp, ssid = 1, join_sample_ids = TRUE)
data_exc <- exclude_areas(data_all, list(sponge_reefs, GH_plan), "longitude", "latitude")

#for each species and each scenario, call create_survey_indices() on that subset of data. rbind into single data frame.
indices <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(indices) <- c("year", "scenario", "method", "species", "index", "lwr", "upr", "cv", "mean_cv")
for (sp in spp){
  data_control <- filter(data_all, species_code == sp)
  data_exclude <- filter(data_exc, species_code == sp)
  print(paste("calculating indices for species code", sp))
  indices <- bind_rows(indices,
    create_survey_indices(data_control, "unfiltered", sp),
    create_survey_indices(data_exclude, "filtered", sp)
  )
}

#plot and label grouped by species and scenario

yrs <- range(indices$year)

indices_stats <- indices %>%
  gather(var, value, index:mean_cv, factor_key = TRUE) %>%
  unite(temp, scenario, var, sep = ".") %>%
  spread(temp, value) %>%
  mutate(
    prop_error = (filtered.index - unfiltered.index)/unfiltered.index,
    abs_prop_error = abs(prop_error)
  ) %>%
  group_by(method, species) %>%
  mutate(
    MdAPE = median(abs_prop_error),
    MdPE = median(prop_error),
    MdAPE_text = paste0("MdAPE: ", round(MdAPE, 3)),
    MdPE_text = paste0("MdPE: ", round(MdPE, 3))
  ) %>%
  ungroup() %>%
  select(year:species, prop_error:MdPE_text)

indices_map <- indices %>%
  mutate(scenario_int = as.integer(as.factor(scenario)) + 2) %>%
  group_by(species) %>%
  mutate(
    y_cv = (1-(0.1*scenario_int))*max(upr),
    y_MdAPE = 0.9*max(upr),
    y_MdPE = 0.8*max(upr)
  ) %>%
  group_by(species, scenario, method) %>%
  mutate(cv_text = paste0("Mean CV (", min(scenario),"): ", round(min(mean_cv), 3))) %>%
  ungroup() %>%
  left_join(indices_stats, c("year","method","species"))

x <- ggplot(indices_map, mapping = aes(x = year, group = scenario)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = scenario), alpha = 0.5) +
  geom_line(aes(y = index, linetype = scenario)) +
  geom_point(aes(y = index, colour = scenario)) +
  facet_grid(species~method, scales = "free") +
  geom_text(aes(label = cv_text, x = yrs[1] + 0.5, y = y_cv), size = 2.65, hjust = 0) +
  geom_text(aes(label = MdAPE_text, x = yrs[1] + 0.5, y = y_MdAPE), size = 2.65, hjust = 0) +
  geom_text(aes(label = MdPE_text, x = yrs[1] + 0.5, y = y_MdPE), size = 2.65, hjust = 0) +
  xlab("") +
  ylab("Relative biomass")

ggsave("x.pdf", x, height = 60, limitsize = FALSE)
View(indices)
View(indices_stats)
