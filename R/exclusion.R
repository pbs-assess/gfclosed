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
