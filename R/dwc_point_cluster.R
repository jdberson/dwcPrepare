#' Cluster a set of geographic coordinates (longitude, latitude)
#'
#' This function uses \code{\link[geosphere]{distm}} to create a distance matrix
#' and \code{\link[stats]{hclust}} with \code{\link[stats]{cutree}} to assign
#' each point to a cluster.
#'
#' @param df A data frame object.
#' @param longitude The name of the column in \code{df} containing the longitude
#' values.
#' @param latitude The name of the column in \code{df} containing the latitude
#' values.
#' @param distm_method The method passed to \code{\link[geosphere]{distm}} for
#' calculating the distance between points.
#' @param hclust_method The method passed to \code{\link[stats]{hclust}} for
#' performing the hierarchical clustering.
#' @param k The number of desired groups passed to \code{\link[stats]{cutree}}.
#' @param h The distance cutoff passed to \code{\link[stats]{cutree}}.
#'
#' @return
#' A data frame with a new column \code{cluster} that shows the cluster each
#' point has been assigned to.
#'
#' @export
#'
#' @examples
#'df <-
#'data.frame(
#'longitude = c(116.018, 116.010, 116.011, 115.969, 115.976, 115.973),
#'latitude = c(-31.709, -31.707, -31.713, -32.339, -32.339, -32.344)
#')
#'
#' dwc_point_cluster(df = df,
#'                   longitude = "longitude",
#'                   latitude = "latitude",
#'                   hclust_method = "average",
#'                   h = 5000)
dwc_point_cluster <- function(df, longitude, latitude,
                             distm_method = geosphere::distHaversine,
                             hclust_method = "average",
                             k = NULL,
                             h = NULL) {

  # Remove rows with missing coordinates
  na_rows <- base::is.na(df[, longitude]) | base::is.na(df[, latitude])
  df <- df[!na_rows, ]

  # Perform clustering only if multiple points, otherwise assign a cluster value
  # of 1
  if(base::nrow(df) > 1) {

  # Make a distance matrix
  base::print("Making distance matrix - this can be slow")
  dist_mat <- geosphere::distm(df[,c(longitude, latitude)], fun = distm_method)
  dist_mat <- base::as.data.frame(dist_mat)
  dist_mat[base::is.na(dist_mat)] <- 0
  dmat <- stats::as.dist(dist_mat)

  # Hierarchical clustering
  base::print("Performing hierarchical clustering")
  hc <- stats::hclust(dmat, method = hclust_method)
  df$cluster <- stats::cutree(hc, k = k, h = h)

  # Provide a warning if rows with missing coordinates were removed
  if(base::any(na_rows)) {
    base::warning("Rows with missing coordinates removed from data frame",
                  call. = FALSE)
  }

  } else{
    df$cluster <- 1
  }

  return(df)


}
