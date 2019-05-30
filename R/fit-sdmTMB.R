#fit spatiotemporal biomass model using sdmTMB()
#predict onto 2km grid, calculate biomass index
sdmTMB_biomass <- function(dat, survey, include_depth) {
  dat_tidy <- dat %>%
    gfplot::tidy_survey_sets(survey, years = seq(1, 1e6)) %>%
    gfplot:::interp_survey_bathymetry()
  dat_tidy <- dat_tidy$data %>%
    gfplot:::scale_survey_predictors()

  pred_grid <- dat_tidy %>%
    filter(year == max(dat_tidy$year)) %>%
    gfplot:::make_prediction_grid(cell_width = 2, survey = survey)
  pred_grid <- pred_grid$grid %>%
    rename(depth = akima_depth)

  dat_spde <- sdmTMB::make_spde(dat_tidy$X, dat_tidy$Y, 200)
  formula <- if (include_depth)
    as.formula(density ~ 0 + as.factor(year) + depth_scaled + depth_scaled2)
  else
    as.formula(density ~ 0 + as.factor(year))
  dat_m <- sdmTMB::sdmTMB(dat_tidy, formula, time = "year",
    spde = dat_spde, family = sdmTMB::tweedie(link = "log"), silent = TRUE)
  preds <- sdmTMB:::predict.sdmTMB(dat_m, newdata = pred_grid)
  index <- sdmTMB::get_index(preds, bias_correct = FALSE) %>%
    mutate(st_cv = sqrt(exp(.data$se^2) - 1))
  index
}
