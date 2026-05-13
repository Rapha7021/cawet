test_that("process_soil_characteristic works with get_soil_awc info&sols", {
  shapefile_data <- sf::read_sf(
    system.file(
      "extdata/study_area/test.shp",
      package = "RADIS"
    )
  )
  shapefile_data$id_parcel <- 1:nrow(shapefile_data)
  shapefile_data$surface_m2 = sf::st_area(shapefile_data)
  Depth <- get_soil_depth(
    sf = shapefile_data
  )
  AWC <- as.data.frame(RADIS::get_soil_awc(
    sf = shapefile_data,
    source = "Info&Sols",
    with_coarse_elements = FALSE
  ))
  # Rename columns to match the expected format in process_soil_characteristic
  AWC <- AWC |>
    dplyr::rename_with(
      ~ sub("^awc_mm_", "awc_mm.", .x),
      dplyr::starts_with("awc_mm_")
    )
  AWC <- AWC %>%
    dplyr::select(-geometry) %>%
    dplyr::full_join(Depth, by = join_by(id_parcel))
  AWC$AWC_mean <- process_soil_characteristic(
    AWC,
    "awc_mm",
    sum
  )
  # Force the depth to ensure the following test is consistent (i.e. sum of the 3 first columns)
  AWC$soil_depth <- 0.3
  expect_equal(AWC$AWC_mean, rowSums(AWC[, 1:3]))
})
