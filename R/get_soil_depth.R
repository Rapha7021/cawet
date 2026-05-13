#' Get soil depth
#' @description
#' This function retrieves soil depth information for the given parcels with a
#' specific grid (maille_y).
#' It performs a spatial intersection between the parcels and the soil depth
#' data obtained from the specified source, then calculates the total surface
#' area for each soil depth category within each parcel.
#' @param sf An sf object containing parcels for which soil depth information is to be retrieved.
#' The sf object should have a column named "id_parcel" that uniquely identifies
#' each parcel and a column named "surface_m2" representing the surface area of
#' each parcel.
#' @param source A character string specifying the source of soil depth data. Default is "BDGSF".
#' @return A data frame with parcel identifiers and their corresponding soil depth information.
#' @export
#'
get_soil_depth <- function(sf, source = "BDGSF") {
  Depth <- RADIS::get_soil_depth(
    sf = sf,
    source = source
  ) %>%
    dplyr::mutate(surface_inter = as.numeric(surface_m2)) %>%
    dplyr::group_by(id_parcel, soil_depth) %>%
    dplyr::summarise(surface_totale = sum(surface_inter)) %>%
    dplyr::group_by(id_parcel) %>%
    dplyr::summarise(soil_depth = dplyr::first(soil_depth)) %>%
    as.data.frame() %>%
    dplyr::select(-geometry)
  cli_alert_success("Processing soil depth ok")
  return(Depth)
}
