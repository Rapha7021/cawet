test_that("create_parcelles_autour_point_centre and create_parcelles_autour_point_repgraph works as expected", {
  lon = 3
  lat = 43
  crops = c("banana", "kiwi", "apple")
  areas = c(30, 20, 45)
  col_crop = "crop"
  col_area = "area"
  sf_centre <- creer_parcelles_autour_point_centre(lon, lat, crops, areas, col_crop, col_area)
  sf_point_repgraph <- creer_parcelles_autour_point_repgraph(lon, lat, crops, areas, col_crop, col_area)

  for (sf in list(sf_centre, sf_point_repgraph)) {
    expect_true(all(c(col_crop, col_area, "geometry") %in% names(sf)))
    expect_true(is(sf[["geometry"]], "sfc_POLYGON"))
    expect_equal(crops, sf[[col_crop]])
    expect_equal(as.numeric(sf::st_area(sf)), areas, tolerance = 0.01)
  }

  # test that polygons are centered on lat/lon
  sf_centre$centroid <- sf::st_centroid(sf_centre$geometry)
  sf_centre <- sf::st_sf(sf_centre[, col_crop, col_area], geometry = sf_centre[["centroid"]])
  sf_centre <- sf::st_transform(sf_centre, 4326)
  coords <- sf::st_coordinates(sf_centre[["geometry"]])
  for (i in 1:nrow(coords)) {
    expect_equal(coords[[i, "X"]], lon, tolerance = 0.0001)
    expect_equal(coords[[i, "Y"]], lat, tolerance = 0.0001)
  }   
})