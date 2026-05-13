#' Test helper : Build a single test scenario
#'
#' Select the first scenario of the package example and use custom polygone features
#' for testing
#'
#' @param wd The working directory
#' @param sfPoly The sf object containing polygon features
#' @returns The scenario table from the ordonnanceur
#'
build_scenario_for_test <- function(wd, sfPoly) {
  Scenario <- read_ordonnanceur(
    file.path(
      wd,
      "Ordonnanceur/Ordonnanceur_exemple.xlsx"
    )
  )
  Scenario <- Scenario[1, , drop = FALSE] # Use only the first scenario for testing
  Scenario$Scenario[1] <- "Test_Run"
  Scenario$Shp_link[1] <- "shp/test_RADIS.gpkg"
  Scenario$Last_year_simulation <- 2020

  sf::st_write(sfPoly, file.path(wd, Scenario$Shp_link[1]), delete_dsn = TRUE)

  return(Scenario)
}
