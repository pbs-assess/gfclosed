calc_bio <- function(dat, i = seq_len(nrow(dat))) {
  dat[i, , drop = FALSE] %>%
    group_by(year, survey_id, area_km2, grouping_code) %>%
    summarise(density = mean(density_kgpm2 * 1e6)) %>%
    group_by(year) %>%
    summarise(biomass = sum(density * area_km2)) %>%
    pull(biomass)
}

#' TODO
#'
#' @param dat TODO
#' @param reps TODO
#'
#' @export
boot_biomass <- function(dat, reps = 1000) {
  out <- dat %>%
    group_by(year, species_common_name, survey_series_desc) %>%
    dplyr::do({
      b <- boot::boot(., statistic = calc_bio, strata = .$grouping_code, R = reps)
      suppressWarnings(bci <- boot::boot.ci(b, type = "perc"))
      dplyr::tibble(
        mean_boot = mean(b$t),
        median_boot = median(b$t),
        lwr = bci$percent[[4]],
        upr = bci$percent[[5]],
        cv = sd(b$t) / mean(b$t),
        biomass = calc_bio(.)
      )
    })
}

# surv <- d_survey_sets %>%
#   filter(survey_series_desc == survey) %>%
#   select(-sample_id) %>%
#   unique()
# out_boot <- boot_biomass(surv)
