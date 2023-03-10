test_that("dwc_point_cluster function works", {

  df_raw <- data.frame(longitude = c(116.018, 116.010, 116.011,
                               115.969,  115.976, 115.973,
                               115.887, 115.886, 115.895),
                       latitude = c(-31.709, -31.707, -31.713,
                               -32.339, -32.339, -32.344,
                               -32.693, -32.686, -32.691),
                       site = c(rep(1, 3), rep(2, 3), rep(3, 3)))

  df_clustered <- dwc_point_cluster(df_raw, "longitude", "latitude",
                                   hclust_method = "average",
                                   h = 5000)

  expect_equal(df_raw$site, df_clustered$cluster)



})

# Note that group_modify puts grouping variable at start of df, also
# returns named vector
test_that("dwc_point_cluster function works on groups", {

  df_raw <- data.frame(longitude = c(116.018, 116.010, 116.011,
                                     115.969,  115.976, 115.973,
                                     115.887, 115.886, 115.895),
                       latitude = c(-31.709, -31.707, -31.713,
                                    -32.339, -32.339, -32.344,
                                    -32.693, -32.686, -32.691),
                       location = c(rep(1, 6), rep(2, 3)),
                       site = c(rep(1, 3), rep(2, 3), rep(1, 3)))

  df_clustered <-
    df_raw |>
    dplyr::group_by(location) |>
    dplyr::group_modify(.f = ~dwc_point_cluster(.x, "longitude", "latitude",
                                           hclust_method = "average",
                                           h = 5000)) |>
    dplyr::mutate(cluster = base::unname(cluster))


  expect_equal(df_raw$site, df_clustered$cluster)



})

test_that("dwc_point_cluster function works for a single value", {

  df_raw <- data.frame(longitude = c(116.018),
                       latitude = c(-31.709),
                       site = 1)

  df_clustered <- dwc_point_cluster(df_raw, "longitude", "latitude",
                                    hclust_method = "average",
                                    h = 5000)

  expect_equal(df_raw$site, df_clustered$cluster)



})

test_that("dwc_point_cluster warning works", {

  expect_warning(
    dwc_point_cluster(data.frame(longitude = c(116.018, 116.010, 116.011,
                                        115.969,  115.976, 115.973,
                                        115.887, 115.886, 115.895, NA),
                                 latitude = c(-31.709, -31.707, -31.713,
                                        -32.339, -32.339, -32.344,
                                        -32.693, -32.686, -32.691, NA)),
                     hclust_method = "average", "longitude", "latitude",
                     h = 5000),

    "Rows with missing coordinates removed from data frame")

})


