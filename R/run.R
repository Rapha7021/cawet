#' Run the complete CAWET workflow
#'
#' This function orchestrates the entire CAWET workflow, including verification of the scheduler,
#' extraction of relevant information, modeling/simulation of crops, and generation of result illustrations.
#'
#' @param chemin_ordonnanceur (Optional) Character string specifying the path
#' to the scheduler (ordonnanceur) file.
#' Uses an interactive file chooser ([file.choose()]) by default.
#' @param Scenario (Optional) [data.frame] produced by [CAWET::read_ordonnanceur()].
#' If provided it is reused and passed to [CAWET::get_Run_Config()] to avoid
#' re-reading the ordonnanceur.
#' By default the function calls [CAWET::read_ordonnanceur()].
#' @param Working_path (Optional) Character string specifying the working directory path.
#' @param cfgRun (Optional) Running configuration created by [CAWET::get_Run_Config()].
#' @return Returns invisibly the running configuration (See [CAWET::get_Run_Config()]).
#' @export
#'
run <- function(
  chemin_ordonnanceur = file.choose(),
  Scenario = read_ordonnanceur(chemin_ordonnanceur),
  Working_path = dirname(dirname(chemin_ordonnanceur)),
  cfgRun = get_Run_Config(
    Working_path = Working_path,
    Scenario = Scenario
  )
) {
  #Verification ordonnanceur
  Verif_ordonnanceur_CAWET(cfgRun = cfgRun)

  #Recuperation des informations d'interet
  run_script1(cfgRun = cfgRun)

  #Modelisation/simulation des cultures du territoire
  run_script2(cfgRun)

  #Illustration des resultats
  #Par scenario
  run_script3_1(cfgRun)
  #inter-scenarii
  run_script3_2(cfgRun)

  #Creation d'indicateurs climatiques
  run_script3_3(cfgRun)

  invisible(cfgRun)
}
