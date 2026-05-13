COL <- list(
  poly = "poly_id",
  plot = "plot_id",
  crop = "code_cultu",
  area = "area"
)

test_that("prepare_territory_for_existing_plots works as expected", {
  sfP <- sf::read_sf(
    system.file(
      "extdata/study_area/test.shp",
      package = "RADIS"
    )
  )
  sfP$crop <- "banana"
  sfP$surface <- c(1, 2)
  Scenario <- build_scenario_for_test(wd, sfP)

  col_map <- data.frame(
    col_user = c(NA, NA, "test", NA),
    col_res = c(COL$poly, COL$plot, COL$crop, COL$area)
  )

  # If external_plots is TRUE, the column containing crop info in the original file must be defined properly.
  expect_error(
    prepare_territory_for_existing_plots(
      wd,
      Scenario,
      COL,
      col_map,
      external_plots = TRUE
    ),
    "Column test does not exist in the file indicated in the Shp_link column of the Ordonnanceur"
  )

  # Other tests when external_plots is TRUE and crop is properly defined.
  col_map[col_map$col_res == COL$crop, "col_user"] <- "crop"
  sf <- prepare_territory_for_existing_plots(
    wd,
    Scenario,
    COL,
    col_map,
    external_plots = TRUE
  )

  # Check that sf contains COL$crop, COL$poly, COL$area, COL$plot
  expect_true(all(c(COL$poly, COL$plot, COL$area, COL$crop) %in% names(sf)))

  # If Colname_idpoly is NA, resulting COL$poly column should contain only 'polyA'
  expect_true(all(sf[[COL$poly]] == "polyA"))

  # If Colname_idplots is NA, resulting COL$plot column should contain row names
  expect_true(all(sf[[COL$plot]] == row.names(sf)))

  # If original file contains MULTIPOLYGONS, resulting sf must contain POLYGONS
  sfP <- sf::st_cast(sfP, "MULTIPOLYGON")
  sf::st_write(sfP, file.path(wd, Scenario$Shp_link[1]), append = FALSE)

  sf <- prepare_territory_for_existing_plots(
    wd,
    Scenario,
    COL,
    col_map,
    external_plots = TRUE
  )

  expect_true(all(sf::st_is(sf, "POLYGON")))

  # If COL$area is properly defined, it should not be overwritten
  col_map[col_map$col_res == COL$area, "col_user"] <- "surface"

  sf <- prepare_territory_for_existing_plots(
    wd,
    Scenario,
    COL,
    col_map,
    external_plots = TRUE
  )

  expect_equal(sf[[COL$area]], sfP$surface)
})

test_that("prepare_territory_for_non_existing_plots works as expected", {
  pth_non_existing <- file.path(
    wd,
    "Territoires_fantoches",
    "Territoire_fantoche_param_exemple.xlsx"
  )

  col_map <- data.frame(
    col_user = c("a", "b", "c", "d"),
    col_res = c(COL$plot, COL$poly, COL$crop, COL$area)
  )

  # COL$poly, COL$plot, COL$crop, COL$area must be properly defined
  expect_error(
    prepare_territory_for_non_existing_plots(
      wd,
      pth_non_existing,
      COL,
      col_map
    ),
    "Column(s) missing in the file indicated in the Shp_link column of the Ordonnanceur: a, b, c, d",
    fixed = TRUE
  )

  # If everything is properly defined, COL$poly, COL$plot, COL$crop, COL$area must be in results columns
  col_map$col_user <- c("id_parcel", "id_poly", "code_cultu", "surf_parc_ha")
  list_sf <- prepare_territory_for_non_existing_plots(
    wd,
    pth_non_existing,
    COL,
    col_map
  )
  for (sf in list_sf) {
    expect_true(all(c(COL$poly, COL$plot, COL$crop, COL$area) %in% names(sf)))
  }

  # If there is a column in the original dataframe with a standardized name but which should not be used,
  # it should be dropped and replaced by the column chosen by the user
  df <- openxlsx::read.xlsx(pth_non_existing) %>% as.data.frame()
  df[[COL$poly]] <- "banana"
  openxlsx::write.xlsx(df, pth_non_existing)

  list_sf <- prepare_territory_for_non_existing_plots(
    wd,
    pth_non_existing,
    COL,
    col_map
  )
  for (sf in list_sf) {
    expect_true(all(
      sf[[COL$poly]] == df[[col_map[col_map$col_res == COL$poly, "col_user"]]]
    ))
  }
})
