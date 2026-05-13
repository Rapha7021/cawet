#' Get or create a CAWET run configuration
#'
#' @describeIn get_Run_Config This function retrieves or creates a run configuration for CAWET,
#' including the run path, scenario data, and path to charged input data.
#'
#' @param path Character string specifying the path to an existing run directory.
#' If NA the function retrieves the most recent run directory.
#' By default, a folder selection dialog will be opened.
#' @param Working_path Character string specifying the working directory.
#' @param Scenario Optional data frame containing scenario data.
#' If provided, it will be saved to the run directory. If NULL, the function
#' will attempt to read the scenario data from the run directory.
#'
#' @return [CAWET::get_Run_Config] returns a [list] with the following elements:
#' - `Working_path`: The working directory used for the run.
#' - `Path_Run`: The path to the created or selected run directory.
#' - `Scenario`: A data frame containing the scenario information used for the run.
#' - `Path_Run_Chargementdata`: The path to the `Charged_inputs` directory inside the run.
#'
#' [CAWET::get_Path_Run] returns a character string with the path to the run directory.
#'
#' @details
#' Behavior depending on the value of `Scenario`:
#' - If `Scenario` is provided (non-NULL):
#'   - `get_Run_Config` calls `get_Path_Run(..., new = TRUE)` which creates a new
#'     run directory at `file.path(Working_path, "Runs", paste0("Run_", <timestamp>))`.
#'   - The provided `Scenario` data frame is written into `Scenarii.csv` inside the
#'     new run directory.
#'   - A `Charged_inputs` directory is created inside the run directory and its path is
#'     returned as `Path_Run_Chargementdata`.
#' - If `Scenario` is NULL:
#'   - `get_Run_Config` calls `get_Path_Run(..., new = FALSE)` which:
#'     - If `path` is NA: finds the most recent existing run directory under
#'       `file.path(Working_path, "Runs")` (the last `Run_` entry).
#'     - If `path` is provided: validates that the directory exists and returns it.
#'   - The function attempts to read `Scenarii.csv` from the chosen run directory using
#'     `read_ordonnanceur()` to populate `Scenario`.
#'   - The `Charged_inputs` directory is created if missing and its path returned.
#'
#' Note: the default `path` uses `choose.dir()`; when creating a new run the run
#' timestamp ensures unique run folder names.
#'
#' @export
get_Run_Config <- function(
  path = NULL,
  Working_path = dirname(dirname(path)),
  Scenario = NULL
) {
  Path_Run <- get_Path_Run(path, Working_path, !is.null(Scenario))
  Path_Run_chargementdata <- paste0(Path_Run, "/Charged_inputs")
  dir.create(Path_Run_chargementdata, showWarnings = FALSE)
  if (!is.null(Scenario)) {
    write.csv2(Scenario, paste0(Path_Run, '/', 'Scenarii.csv'), row.names = F)
  } else {
    Scenario <- read_ordonnanceur(
      paste0(Path_Run, "/Scenarii.csv")
    )
  }
  return(
    list(
      Working_path = Working_path,
      Path_Run = Path_Run,
      Scenario = Scenario,
      Path_Run_Chargementdata = Path_Run_chargementdata
    )
  )
}

#' @describeIn get_Run_Config Get or create a run path
#' @param new Logical indicating whether to create a new run directory.
#' If TRUE, a new run directory will be created. If FALSE, the most recent
#' existing run directory will be retrieved.
#' @export
get_Path_Run <- function(
  path = choose.dir(),
  Working_path,
  new = FALSE
) {
  if (new) {
    run_time <- format(
      Sys.time(),
      "%Y-%m-%d_%H%M%S"
    )
    Path_Run <- file.path(
      Working_path,
      "Runs",
      paste0("Run_", run_time)
    )
    dir.create(Path_Run, recursive = TRUE)
    cli_alert_success("Création du répertoire de run : {Path_Run}")
    return(Path_Run)
  } else if (is.na(path)) {
    Run_paths <- list.dirs(
      file.path(Working_path, "Runs"),
      full.names = TRUE,
      recursive = FALSE
    )
    Run_Paths <- dplyr::last(Run_paths[grep("^Run_", basename(Run_paths))])
    if (length(Run_paths) == 0) {
      stop("No existing runs found in the 'Runs' directory.")
    } else {
      return(Run_Paths)
    }
  } else {
    if (!dir.exists(path)) {
      stop("Le chemin specifie n'existe pas : ", path)
    }
    return(path)
  }
}

#' Suppress messages from ggplot2::ggsave
#'
#' This function is a wrapper around `ggplot2::ggsave` that suppresses messages.
#' It can be used to avoid cluttering the console with messages when saving plots.
#' @param ... Arguments passed to `ggplot2::ggsave`.
#' @return The return value of `ggplot2::ggsave`, invisibly.
#' @export
ggsave <- function(...) {
  suppressMessages(ggplot2::ggsave(...))
}

#' Suppress messages from sf::st_read
#'
#' This function is a wrapper around `sf::st_read` that suppresses messages.
#' It can be used to avoid cluttering the console with messages when reading spatial data.
#' @param ... Arguments passed to `sf::st_read`.
#' @return The return value of `sf::st_read`, invisibly.
#' @export
st_read <- function(...) {
  sink(tempfile())
  on.exit(sink())
  suppressMessages(sf::st_read(...))
}
