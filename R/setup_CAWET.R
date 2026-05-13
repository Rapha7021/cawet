#' Setup CAWET instance in a specified directory
#'
#' This function copies all necessary files from the CAWET package's
#' extdata directory to a user-specified destination directory.
#' @param destdir Character string specifying the destination directory
#' where the CAWET instance should be set up.
#' @return None. The function performs file copy operations.
#' @examples
#' destdir <- file.path(tempdir(), "CAWET")
#' setup_CAWET(destdir)
#' list.files(destdir)
#' @export
#'
setup_CAWET <- function(destdir) {
  extdata_dir <- system.file("extdata", package = "CAWET")

  files_to_copy <- list.files(
    extdata_dir,
    full.names = TRUE,
    recursive = TRUE
  )

  if (!dir.exists(destdir)) {
    warning(
      "The specified destination directory does not exist. It will be created."
    )
    dir.create(destdir, recursive = TRUE)
  }

  # Get relative paths to preserve directory structure
  rel_paths <- sub(paste0("^", extdata_dir, "/"), "", files_to_copy)

  # Create subdirectories and copy files
  for (i in seq_along(files_to_copy)) {
    dest_file <- file.path(destdir, rel_paths[i])
    dest_subdir <- dirname(dest_file)

    if (!dir.exists(dest_subdir)) {
      dir.create(dest_subdir, recursive = TRUE)
    }

    file.copy(files_to_copy[i], dest_file, overwrite = TRUE)
  }

  invisible(NULL)
}
