#' Calculate a design-based biomass index
#'
#' @param dat A data frame returned by [gfdata::get_survey_sets()].
#' @param i A vector of indices to include. By default this includes all rows of
#'   data. This is set up to work with bootstrapping via [boot_biomass()].
calc_bio <- function(dat, i = seq_len(nrow(dat))) {
  dat[i, , drop = FALSE] %>%
    group_by(year, survey_id, area_km2, grouping_code) %>%
    summarise(density = mean(density_kgpm2 * 1e6)) %>%
    group_by(year) %>%
    summarise(biomass = sum(density * area_km2)) %>%
    pull(biomass)
}

#' Bootstrap the biomass estimates
#'
#' @param dat A data frame returned by [gfdata::get_survey_sets()].
#' @param reps The number of bootstrap replicates.
#'
#' @export
boot_biomass <- function(dat, reps = 1000) {
  out <- dat %>%
    group_by(year, species_common_name, survey_series_desc) %>%
    dplyr::do({
      b <- boot::boot(., statistic = calc_bio, strata = .$grouping_code, R = reps)
      suppressWarnings(bci <- boot::boot.ci(b, type = "perc"))
      d <- dplyr::tibble(
        analysis = "design-based",
        species_common_name = unique(dat$species_common_name),
        mean_boot = mean(b$t),
        median_boot = median(b$t),
        lwr = bci$percent[[4]],
        upr = bci$percent[[5]],
        cv = sd(b$t) / mean(b$t),
        biomass = calc_bio(.)
      )
    }) %>% ungroup()
}
