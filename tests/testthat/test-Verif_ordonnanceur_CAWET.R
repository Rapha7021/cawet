test_that("Verif_ordonnanceur_CAWET works without errors or warnings", {
  chemin_ordonnanceur = file.path(wd, "Ordonnanceur/Ordonnanceur_exemple.xlsx")

  Scenario <- expect_no_error(
    expect_no_warning(
      read_ordonnanceur(chemin_ordonnanceur)
    )
  )

  Working_path <- dirname(dirname(chemin_ordonnanceur))

  cfgRun <- expect_no_error(
    expect_no_warning(
      get_Run_Config(
        Working_path = Working_path,
        Scenario = Scenario
      )
    )
  )
  expect_no_error(
    expect_no_warning(
      Verif_ordonnanceur_CAWET(cfgRun = cfgRun)
    )
  )
})
