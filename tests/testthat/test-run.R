test_that("CAWET run function works with RPG for years 2023 and 2024", {
  sfP <- sf::read_sf(
    system.file(
      "extdata/study_area/test.shp",
      package = "RADIS"
    )
  )
  Scenario <- build_scenario_for_test(wd, sfP)
  Scenario$First_year_simulation[1] <- 2023
  Scenario$Last_year_simulation[1] <- 2024

  cfgRun <- get_Run_Config(
    Working_path = wd,
    Scenario = Scenario
  )

  run(
    chemin_ordonnanceur = NULL,
    cfgRun = cfgRun
  )
  expect_true(dir.exists(cfgRun$Path_Run))
  expect_true(length(list.files(cfgRun$Path_Run)) > 1)
})

# !!! This test is commented because of issue #56 that need to be solved
# test_that("CAWET works with Scenario$Sol_fixed_RADIS[Scen_i] == 'No'", {
#   ordo <- openxlsx::read.xlsx(file.path(
#     wd,
#     "Ordonnanceur/Ordonnanceur_exemple.xlsx"
#   ))
#   ordo <- ordo[3, , drop = FALSE]
#   openxlsx::write.xlsx(
#     ordo,
#     file.path(wd, "Ordonnanceur/Ordo_sol_fixed_RADIS_No.xlsx")
#   )
#   cfgRun <- run(
#     Working_path = wd,
#     Chemin_ordonnanceur = file.path(
#       wd,
#       "Ordonnanceur/Ordo_sol_fixed_RADIS_No.xlsx"
#     )
#   )
#   # Add tests to verify expected outputs
#   expect_true(dir.exists(cfgRun$Path_Run))
#   expect_true(length(list.files(cfgRun$Path_Run)) > 1)
# })

cfgRunEx <- run(
  Working_path = wd,
  chemin_ordonnanceur = file.path(
    wd,
    "Ordonnanceur/Ordonnanceur_exemple.xlsx"
  )
)

test_that("CAWET Example works", {
  # Add tests to verify expected outputs
  expect_true(dir.exists(cfgRunEx$Path_Run))
  expect_true(length(list.files(cfgRunEx$Path_Run)) > 1)
})

test_that("Crop areas and irrigation volumes in CAWET Example results are correct", {
  # Read crop areas by polygon from inputs
  dfRepCrop <- read.csv2(file.path(
    cfgRunEx$Path_Run_Chargementdata,
    "1_S1/Formodification/Territoire_repartition_cultures_2020.csv"
  )) |>
    dplyr::mutate(surf_parc = as.numeric(surf_parc)) |>
    dplyr::group_by(id_poly, code_cultu) |>
    dplyr::summarise(surf_parc = sum(surf_parc) * 10000, .groups = "drop")
  # Read Crop areas by polygon from CAWET results
  dfRepCropRes <- read.csv2(file.path(
    cfgRunEx$Path_Run,
    "Results/1_S1/Resultats_Aggreges/Modelisation_for_polygones_and_network.csv"
  ))
  # Check irrigation_mm and irrigation_m3 are correct
  dfIrrig <- dfRepCropRes |> dplyr::filter(surf_tot > 0)
  expect_equal(
    dfIrrig$irrigation_mm,
    dfIrrig$Irrigation_m3 / dfIrrig$surf_tot * 1000,
    tolerance = 0.01
  )
  dfRepCropRes <- dfRepCropRes |>
    dplyr::filter(An == 2020, surf_tot > 0) |>
    dplyr::group_by(Poly, CODE_CU, Mois, An) |>
    dplyr::summarise(surf_tot = sum(surf_tot), .groups = "drop")
  # Check that all months have the same crop areas
  dfRepCropRes <- dfRepCropRes |>
    dplyr::group_by(Poly, CODE_CU, An) |>
    dplyr::summarise(surf_tot = mean(surf_tot), .groups = "drop") |>
    dplyr::mutate(id_poly = sub("polypoly", "poly", Poly))
  # Join the two dataframes and check that the areas are close
  dfJoin <- dplyr::left_join(
    dfRepCropRes,
    dfRepCrop,
    by = c("id_poly", "CODE_CU" = "code_cultu")
  )
  expect_equal(nrow(dfJoin), nrow(dfRepCropRes))
  expect_equal(dfJoin$surf_parc, dfJoin$surf_tot, tolerance = 0.01)
})
