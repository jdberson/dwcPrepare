#' Find polygons (sites) that intersect
#'
#' This function identifies polygons that intersect in some way. Its main use is
#' to identify sites within a data set that cover the same or similar area, and
#' could be considered a single site. A cluster identifier (\code{.cluster}) is
#' assigned to each polygon, with intersecting polygons identified by a common
#' \code{.cluster} id.
#'
#' @param x An \code{sf} object that contains the site locations as polygons.
#'
#' @return
#' \code{x} with the additional column \code{.cluster}.
#'
#' @export
#'
#' @examples
#' data("thylacine_data")
#'
#' # Generate some interseting polygons
#' thylacine_polygons <-
#'   thylacine_data |>
#'   dplyr::group_by(trap) |>
#'   dplyr::slice(1) |>
#'   sf::st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = "EPSG:4326") |>
#'   sf::st_buffer(dist = 5000) |>
#'   dplyr::select(site, trap, geometry) |>
#'   dplyr::ungroup()
#'
#' # Run function
#' dwc_polygon_cluster(thylacine_polygons)
dwc_polygon_cluster <- function(x) {
  # Inspired by answer at:
  # https://gis.stackexchange.com/questions/323038/dissolve-only-overlapping-polygons-in-r-using-sf

  # Note: sf::st_make_valid is needed due to issues with s2
  # See https://github.com/r-spatial/sf/issues/1771 and
  # https://github.com/r-spatial/sf/issues/1762

  # Union polygons that intersect
  x_union <- sf::st_make_valid(sf::st_cast(sf::st_union(x), "POLYGON"))

  # Assign cluster id to polygons that intersect
  .cluster <-
    tibble::tibble(.cluster = base::unlist(sf::st_intersects(x, x_union)))

  dplyr::bind_cols(x, .cluster)
}
