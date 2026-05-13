#' Read CAWET Data File
#'
#' @describeIn read_CAWET_data Reads a CAWET data file (CSV or Excel) from the specified working path.
#'
#' This function reads a CAWET data file (CSV or Excel) from the specified working path.
#' @param Working_path The working directory path where the CAWET data file is located.
#' @param file The name of the CAWET data file to read.
#' @return A data frame containing the contents of the CAWET data file.
#' `read_ordonnanceur` reads an Ordonnanceur file (Excel or CSV) and returns
#' its contents as a data frame with all columns converted to character.
#' @export
read_CAWET_data <- function(Working_path, file) {
  file_path <- FpCAWET(Working_path, file)
  # Check if file exists
  if (!file.exists(file_path)) {
    stop(paste("Le fichier n'existe pas:", file_path))
  }
  # Read the data
  # Check file extension
  file_extension <- tools::file_ext(file_path)
  # Is it CSV or Excel?
  if (file_extension == "csv") {
    data <- read.csv2(file_path, stringsAsFactors = FALSE)
  } else if (file_extension %in% c("xls", "xlsx")) {
    data <- openxlsx::read.xlsx(file_path) %>% as.data.frame()
  } else {
    stop(
      "Le format de fichier ",
      file_extension,
      " n'est pas supporté. Utilisez .csv, .xls ou .xlsx."
    )
  }
}

#' @describeIn read_CAWET_data Read Ordonnanceur File (Excel or CSV)
#' @param chemin_ordonnanceur The full path to the Ordonnanceur file.
#' @export
read_ordonnanceur <- function(chemin_ordonnanceur) {
  cli_alert_info("Lecture de l'ordonnanceur:\n{chemin_ordonnanceur}")
  Scenario <- read_CAWET_data(
    Working_path = dirname(chemin_ordonnanceur),
    file = basename(chemin_ordonnanceur)
  )
  # Convert all columns to character except the one containing year in the column name
  Scenario <- Scenario %>%
    dplyr::mutate(
      dplyr::across(
        .cols = -dplyr::matches("(?i)year"),
        .fns = as.character
      )
    )
  # Add extra parameters deduced from the Ordonnanceur
  Scenario <- Scenario %>%
    dplyr::mutate(
      uses_textures = !Model %in% c("CropWat")
    )
  return(Scenario)
}
