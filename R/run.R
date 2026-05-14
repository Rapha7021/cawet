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
#' @param charged_inputs_path (Optional) Character string specifying the path to an
#' existing `Charged_inputs` folder (from a previous run). If provided, Script 1
#' (data download: RPG, soils, weather) is skipped entirely and the contents of
#' this folder are copied into the new run. This drastically reduces iteration time
#' during debugging (from ~30 min to ~2 min).
#' Example: `"/home/user/Bureau/CAWET/Runs/Run_2026-05-14_141506/Charged_inputs"`
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
  ),
  charged_inputs_path = NULL
) {
  #Verification ordonnanceur
  Verif_ordonnanceur_CAWET(cfgRun = cfgRun)

  if (!is.null(charged_inputs_path)) {
    # Mode debug rapide : réutiliser des données chargées existantes
    if (!dir.exists(charged_inputs_path)) {
      stop("charged_inputs_path introuvable : ", charged_inputs_path)
    }
    cli::cli_alert_info("Mode debug : r\u00e9utilisation de {charged_inputs_path}")
    dir.create(cfgRun$Path_Run_Chargementdata, recursive = TRUE, showWarnings = FALSE)
    subdirs <- list.dirs(charged_inputs_path, recursive = FALSE, full.names = TRUE)
    for (d in subdirs) {
      file.copy(d, cfgRun$Path_Run_Chargementdata, recursive = TRUE)
    }
    cli::cli_alert_success("Charged_inputs copi\u00e9s dans {cfgRun$Path_Run_Chargementdata}")
  } else {
    #Recuperation des informations d'interet
    run_script1(cfgRun = cfgRun)
  }

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
