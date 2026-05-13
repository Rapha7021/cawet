#' Get soil textures
#' @description
#' This function retrieves soil texture information for the given parcels with a
#' specific grid (maille_y).
#' It performs a spatial intersection between the parcels and the soil texture
#' data obtained from the specified source, then calculates the mean values of
#' clay, silt, and sand for each parcel.
#' @param parcelles_avec_maille_y An sf object containing parcels with a specific
#' grid (maille_y) for which soil texture information is to be retrieved. The sf
#' object should have a column named "id_parcel" that uniquely identifies each parcel.
#' @param Depth A data frame containing soil depth information for the parcels,
#' with a column named "id_parcel" that uniquely identifies each parcel and a
#' column named "soil_depth" representing the soil depth category.
#' @return A data frame with parcel identifiers and their corresponding mean
#' values of clay, silt, and sand.
get_soil_textures <- function(parcelles_avec_maille_y, Depth) {
  Texture <- as.data.frame(RADIS::get_soil_texture(
    sf = parcelles_avec_maille_y,
    source = "infosols",
    overlay_mode = "aggregate"
  ))
  Texture <- Texture %>%
    dplyr::select(-geometry) %>%
    dplyr::full_join(Depth, by = "id_parcel")
  Texture$Arg_mean <- process_soil_characteristic(
    Texture,
    "argile",
    mean
  )
  Texture$Lim_mean <- process_soil_characteristic(
    Texture,
    "limon",
    mean
  )
  Texture$Sab_mean <- process_soil_characteristic(
    Texture,
    "sable",
    mean
  )
  Texture <- Texture[, c(
    "id_parcel",
    "Arg_mean",
    "Lim_mean",
    "Sab_mean"
  )]
  cli_alert_success("Processing of soil texture ok")
  return(Texture)
}
