chemin_ordonnanceur <- file.path(
  wd,
  "Ordonnanceur/Ordonnanceur_exemple.xlsx"
)
Scenario <- read_ordonnanceur(chemin_ordonnanceur)
cfgRun <- get_Run_Config(
  Working_path = wd,
  Scenario = Scenario
)

test_that("get_Run_Config should create cfgRun object and folders", {
  expect_true(is.list(cfgRun))
  expect_true(all(
    c("Path_Run", "Scenario", "Path_Run_Chargementdata") %in% names(cfgRun)
  ))
  expect_type(cfgRun$Path_Run, "character")
  expect_true(dir.exists(cfgRun$Path_Run))
  expect_true(is.data.frame(cfgRun$Scenario))
  expect_type(cfgRun$Path_Run_Chargementdata, "character")
  expect_true(dir.exists(cfgRun$Path_Run_Chargementdata))
})

test_that("get_Run_Config should read last config with path = NA", {
  cfgRunNA <- get_Run_Config(
    path = NA,
    Working_path = wd
  )
  expect_equal(cfgRunNA$Path_Run, cfgRun$Path_Run)
  expect_equal(cfgRunNA$Scenario, cfgRun$Scenario)
  expect_equal(
    cfgRunNA$Path_Run_Chargementdata,
    cfgRun$Path_Run_Chargementdata
  )
})
