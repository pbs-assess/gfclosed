#' Exclude areas from the survey domain
#'
#' Filter output of [gfdata::get_survey_sets()] to exclude sets within a given set of polygons.
#'
#' @param data A data frame from [gfdata::get_survey_sets()].
#' @param exclude_poly FIXME:
#' @param X FIXME:
#' @param Y FIXME:
#' @export
exclude_areas <- function(data, exclude_poly, X, Y){
  if (class(exclude_poly) == "list" && length(exclude_poly) > 1){
    for (i in seq_along(exclude_poly)){
      exclude_poly[[i]] <- as.data.frame(exclude_poly[[i]])
      exclude_poly[[i]] <- select(exclude_poly[[i]], .data$geometry)
    }
    exclude_poly <- dplyr::bind_rows(exclude_poly) %>%
      sf::st_sf() %>%
      sf::st_union()
  }
  data %>%
    sf::st_as_sf(coords = c(X, Y), agr = "constant",
      crs = sf::st_crs(exclude_poly), remove = FALSE) %>%
    sf::st_difference(exclude_poly) %>%
    as.data.frame() %>%
    dplyr::select(-.data$geometry)
}
