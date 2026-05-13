#' Construct Full Path for CAWET
#'
#' This function constructs a full file path for CAWET by checking if the provided path is already an absolute path. If it is not, it combines it with the specified working directory.
#' @param Working_path A string representing the working directory path.
#' @param path A string representing the file path to be checked or combined.
#' @return A string representing the full file path.
#' @export
#'
FpCAWET <- function(
  Working_path,
  path
) {
  # Normalize separators to forward slashes
  norm_working <- gsub("\\\\", "/", Working_path)
  norm_path <- gsub("\\\\", "/", path)

  # Detect absolute paths (Unix or Windows-style after normalization)
  is_absolute <- grepl("^/", norm_path) || grepl("^[A-Za-z]:/", norm_path)

  full <- if (is_absolute) {
    norm_path
  } else {
    file.path(norm_working, norm_path)
  }

  # Collapse duplicate slashes (but keep protocol-like patterns untouched)
  full <- gsub("/{2,}", "/", full)

  return(full)
}
