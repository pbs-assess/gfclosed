#' @importFrom dplyr filter mutate summarise select group_by n arrange ungroup
#' @importFrom dplyr inner_join left_join right_join anti_join full_join
#' @importFrom dplyr semi_join %>%
#' @importFrom dplyr bind_rows case_when pull contains tibble rename as_tibble
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_vline scale_fill_manual
#'   scale_colour_manual scale_x_continuous scale_size_area coord_cartesian
#'   guides geom_point facet_wrap xlab ylab geom_col ylim xlim geom_rect
#'   geom_text scale_fill_continuous geom_line labs scale_y_continuous
#'   guide_legend geom_ribbon element_text scale_shape_manual element_line
#'   geom_path geom_polygon coord_equal stat_summary_hex facet_grid
#'   position_identity coord_fixed
#' @importFrom methods as
#' @importFrom stats as.formula density median sd
NULL

if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "area_km2",  "biomass", "density", "density_kgpm2", "grouping_code",
    "species_common_name", "survey_id", "survey_series_desc",
    "year"
  ))
}
