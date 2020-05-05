# spatiotemporal model of biomass

library(future)
library(dplyr)
plan(multisession, workers = availableCores() / 2)


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

st_biomass <- function(dat,
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
    if (ssid %in% c(1, 3, 4, 16)) {
      survey_grid <- synoptic_grid
    }
    if (ssid == 22) {
      survey_grid <- gfplot::hbll_n_grid$grid
      stop("Non-synoptic surveys are not implemented yet.", call. = FALSE)
    }
    if (ssid == 36) {
      survey_grid <- gfplot::hbll_s_grid$grid
      stop("Non-synoptic surveys are not implemented yet.", call. = FALSE)
    }

    survey_grid <- survey_grid %>%
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


