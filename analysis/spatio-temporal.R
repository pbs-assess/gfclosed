# density calcs, design-based

#arg's
species_rds = "D:/GitHub/pbs-assess/gfsynopsis-old/report/data-cache/yelloweye-rockfish.rds"



my_function <- function(dat, species_rds, ssid = NULL,
  n_knots = 200, anisotropy = FALSE, silent = TRUE,
  bias_correct = FALSE, species_name){

  if (is.null(ssid)) stop("Please provide survey series id.")

# set up data
  dat <- readRDS(species_rds)$survey_sets %>%
    filter(survey_series_id == ssid) %>%
    filter(!(year == 2014 & survey_series_id == 16))
  survey <- unique(dat$survey_series_desc)
species_name <- unique(dat$species_common_name) %>%
  gsub(" ", "-")
  col <- if (ssid %in% c(1, 3, 4, 16)) "density_kgpm2" else "density_ppkm2"
  dat <- dat %>%
    select(year, survey_series_id, fishing_event_id, longitude, latitude, density = col, species_code) %>%
    mutate(Y = latitude, X = longitude)
  dat <- dat[!duplicated(select(dat, year, fishing_event_id)), , drop = FALSE]

  if (nrow(dat) >= 1) {
    dat <- as_tibble(convert2utm(dat))
  }
  dat <- mutate(dat, present = ifelse(density > 0, 1, 0))
  if (mean(dat$present) < 0.05) stop("Not enough data.")

# prepare survey grid for each year

  synoptic_grid <- gfplot::synoptic_grid %>%
    mutate(survey_series_id = case_when(
      survey == "SYN QCS" ~ 1,
      survey == "SYN HS" ~ 3,
      survey == "SYN WCVI" ~ 4,
      survey == "SYN WCHG" ~ 16
    ))
  if(ssid %in% c(1, 3, 4, 16)){
  survey_grid <- synoptic_grid
  }
  if(ssid == 22) {survey_grid <- gfplot::hbll_n_grid$grid %>%
    message("Non-synoptic surveys are not implemented yet.")}
  if(ssid == 36) {survey_grid <- gfplot::hbll_s_grid$grid
    message("Non-synoptic surveys are not implemented yet.")}

  survey_grid <- survey_grid %>%
    filter(survey_series_id == ssid) %>%
    dplyr::select(.data$X, .data$Y) %>%
    expand_prediction_grid(years = unique(dat$year))

  formula <- stats::as.formula(density ~ 0 + as.factor(year))
  spde <- sdmTMB::make_spde(dat$X, dat$Y, n_knots = n_knots)
  tictoc::tic()
  m <- sdmTMB::sdmTMB(
    formula = formula,
    data = dat, time = "year", spde = spde, family = sdmTMB::tweedie(link = "log"),
    anisotropy = anisotropy, silent = silent)
  tictoc::toc()
  predictions <- stats::predict(m, newdata = survey_grid, return_tmb_object = TRUE)
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

t <- my_function(dat = dat, species_rds = "D:/GitHub/pbs-assess/gfsynopsis-old/report/data-cache/yelloweye-rockfish.rds", ssid = 1)

path <- "D:/GitHub/pbs-assess/gfsynopsis-old/report/data-cache"

map(dat,
  dat <- group_split()
  my_function(dat = dat, species_rds = file.path(path, species_name, ".rds"), ssid = ssid)


