#' Computes the value of a soil characteristic for all relevant soil horizons, for each row of a data.frame.
#'
#' This function returns a numeric vector with a soil characteristic averaged or
#' summed for all soil horizons, depending on the soil depth.
#' @param df [data.frame] result of [RADIS::get_soil_texture] or
#' [RADIS::get_soil_awc] joined with the result of [RADIS::get_soil_depth]
#' (depth in \[m\]).
#' @param variable [character] one of the column returned by get_soil_texture
#' @param FUN [function] the function to use to aggregate the soil
#' characteristic (mean or sum)
#' @return A [numeric] vector containing averaged texture characteristic.
#' @export
#'
process_soil_characteristic <- function(df, variable, FUN) {
  cols_variable <- grep(variable, names(df), value = TRUE)
  max_depths <- sapply(cols_variable, function(col) {
    as.numeric(strsplit(strsplit(col, "\\.")[[1]][2], "_")[[1]][2])
  })
  processed_variable <- sapply(1:nrow(df), function(i) {
    cols_to_consider <- cols_variable[max_depths / 100 <= df$soil_depth[i]]
    return(FUN(as.numeric(df[i, cols_to_consider])))
  })
  return(processed_variable)
}
