test_that("CAWET retrieves the same plots from RPG than RADIS", {
  # define a study area with several pixels
  sfP <- sf::read_sf(
    system.file(
      "extdata/study_area/sub_basins/sub-basins.geojson",
      package = "RADIS"
    )
  )
  sfP <- sfP[6, ]

  Scenario <- build_scenario_for_test(wd, sfP)

  cfgRun <- get_Run_Config(
    Working_path = wd,
    Scenario = Scenario
  )

  run_script1(cfgRun = cfgRun)

  # retrieve RPG processed with CAWET
  rpg_cawet <- sf::read_sf(
    file.path(
      cfgRun$Path_Run_Chargementdata,
      "1_Test_Run",
      "Plots_all_RADIS_information_2020.shp"
    )
  )
  # retrieve RPG directly from RADIS
  rpg <- RADIS::get_rpg_data(sfP, 2020, "id", source = c("IGN", "ODR"))
  rpg <- sf::st_transform(rpg, sf::st_crs(rpg_cawet))
  # correct names from ODR
  rpg$code_cultu <- toupper(rpg$code_cultu)
  odr_to_rpg <- read.csv(file.path(wd, "Model_param", "odr_to_rpg.csv"))
  for (i in 1:nrow(odr_to_rpg)) {
    odr_name <- odr_to_rpg[i, "ODR"]
    rpg_name <- odr_to_rpg[i, "RPG"]
    rpg[rpg$code_cultu == odr_name, "code_cultu"] <- rpg_name
  }

  # Check that RPG from CAWET corresponds to RPG from RADIS
  expect_equal(
    sort(unique(rpg$code_cultu)),
    sort(unique(rpg_cawet$cod_clt))
  )

  for (crop in unique(rpg$code_cultu)) {
    rpg_cawet_crop <- rpg_cawet[rpg_cawet$cod_clt == crop, ]
    expect_equal(
      sum(sf::st_area(rpg[rpg$code_cultu == crop, ])),
      sum(sf::st_area(rpg_cawet_crop)),
      tolerance = 0.01
    )
    expect_equal(
      as.numeric(sum(sf::st_area(rpg_cawet[
        rpg_cawet$cod_clt == crop,
      ]))),
      sum(rpg_cawet_crop$srfc__2),
      tolerance = 0.01
    )
  }
  dfRepCrop <- read.csv2(file.path(
    cfgRun$Path_Run_Chargementdata,
    "1_Test_Run/Formodification/Territoire_repartition_cultures_2020.csv"
  )) %>%
    mutate(surf_parc = as.numeric(surf_parc)) %>%
    group_by(id_poly, code_cultu) %>%
    summarise(surf_parc = sum(surf_parc) * 10000, .groups = "drop")
  dfRepCropCawet <- rpg_cawet %>%
    st_drop_geometry() %>%
    group_by(id_poly, cod_clt) %>%
    summarise(surf_cawet = sum(srfc__2), .groups = "drop")
  dfRepCrop <- left_join(
    dfRepCrop,
    dfRepCropCawet,
    by = c("id_poly", "code_cultu" = "cod_clt")
  )
  expect_equal(
    dfRepCrop$surf_parc,
    dfRepCrop$surf_cawet,
    tolerance = 0.01
  )
})
