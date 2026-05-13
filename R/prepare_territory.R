#' Prepare the shapefile with the polygons for which data will be retrieved.
#'
#' This function creates a sf object with polygons associated to identifiers, area, and standardized column names.
#' Polygons can be either agricultural plots associated to a crop and a bigger polygon, or directly the big polygons
#' (in which case, agricultural plots will be retrieved for each year from the RPG database).
#' @param Working_path [character] working directory.
#' @param Scenario [vector] contains information regarding the scenario of interest.
#' @param COL [list] standardized column names for polygon, plot, crop, and area identifiers.
#' @param col_map [data.frame] correspondence table between standardized column names (`col_res`) and original data column names (`col_user`).
#' @param external_plots [logical] If TRUE, agricultural plots are provided by the user.
#'
#' @return A [sf] object containing polygons for which data must be retrieved.
#' @export
#'
prepare_territory_for_existing_plots <- function(
  Working_path,
  Scenario,
  COL = list(
    poly = "id_poly",
    plot = "id_parcel",
    crop = "code_cultu",
    area = "surface_m2"
  ),
  col_map,
  external_plots
) {
  #Download area location
  shapefile_data_base <- st_read(FpCAWET(
    Working_path,
    Scenario$Shp_link
  ))

  #Transformation of the projection to lambert 93
  shapefile_data_base <- sf::st_transform(shapefile_data_base, crs = 2154)

  # rename geometry column
  shapefile_data_base <- sf::st_set_geometry(shapefile_data_base, "geometry")

  if (is.na(Scenario$Colname_idpoly)) {
    shapefile_data_base[[COL$poly]] <- "polyA"
  }
  #if crop table already exist:
  if (external_plots) {
    # define columns of plot identifier, surface area, and geometry
    if (is.na(Scenario$Colname_idplots)) {
      shapefile_data_base[[COL$plot]] <- rownames(shapefile_data_base)
    }
    col_user_area <- col_map[col_map$col_res == COL$area, "col_user"]
    if (
      (is.na(col_user_area)) |
        (!(col_user_area %in% names(shapefile_data_base)))
    ) {
      shapefile_data_base[[COL$area]] <- as.numeric(sf::st_area(
        shapefile_data_base
      ))
    }

    #  separate multipolygons into polygons
    if (any(sf::st_is(shapefile_data_base, "MULTIPOLYGON"))) {
      shapefile_data_base <- shapefile_data_base %>%
        sf::st_cast("POLYGON") %>%
        dplyr::mutate(id_subpart = dplyr::row_number()) %>%
        dplyr::mutate("{COL$plot}" := paste0(get(COL$plot), id_subpart)) %>%
        dplyr::select(-id_subpart)
    }

    # check that column of crops exists
    col_crop_user <- col_map[col_map$col_res == COL$crop, "col_user"]
    if (!(col_crop_user %in% names(shapefile_data_base))) {
      stop(sprintf(
        "Column %s does not exist in the file indicated in the Shp_link column of the Ordonnanceur",
        col_crop_user
      ))
    }
    # define columns to keep
    cols_to_keep <- c(COL$poly, COL$plot, COL$crop, COL$area)
  } else {
    cols_to_keep <- c(COL$poly)
  }

  # harmonize columns names (COL_POLY, COL_PLOT, COL_CROP)
  shapefile_data_base <- dplyr::rename(
    shapefile_data_base,
    any_of(setNames(
      col_map[!is.na(col_map$col_user), "col_user"],
      col_map[!is.na(col_map$col_user), "col_res"]
    ))
  )
  #suppress underscore to identify future info in folders name
  for (col in intersect(names(shapefile_data_base), c(COL$poly, COL$plot))) {
    shapefile_data_base[[col]] <- gsub("_", "", shapefile_data_base[[col]])
  }

  return(shapefile_data_base[, cols_to_keep])
}


#' Prepare the shapefile with the polygons for which data will be retrieved in the case agricultural plots are fictional.
#'
#' This function creates a sf object with polygons associated to identifiers, area, and standardized column names.
#' Polygons are agricultural plots associated to a crop and a bigger polygon
#' @inheritParams prepare_territory_for_existing_plots
#' @param shp_link [character] name of the original data file.
#'
#' @return A [sf] object containing a polygons for which data must be retrieved.
#' @export
#'
prepare_territory_for_non_existing_plots <- function(
  Working_path,
  shp_link,
  COL = list(
    poly = "id_poly",
    plot = "id_parcel",
    crop = "code_cultu",
    area = "surface_m2"
  ),
  col_map
) {
  RadN_dbf <- read_CAWET_data(
    Working_path = Working_path,
    file = shp_link
  )

  # Check that all mandatory columns exist
  mandatory_cols <- c(
    c("Latitude", "Longitude"),
    col_map[
      col_map$col_res %in% c(COL$poly, COL$plot, COL$plot, COL$crop, COL$area),
      "col_user"
    ]
  )
  missing_cols <- mandatory_cols[!(mandatory_cols %in% names(RadN_dbf))]
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Column(s) missing in the file indicated in the Shp_link column of the Ordonnanceur: %s",
        paste0(missing_cols, collapse = ", ")
      )
    )
  }

  RadN_dbf <- RadN_dbf %>%
    dplyr::mutate(
      Longitude = ifelse(!is.na(Longitude), Longitude, 43.4),
      Latitude = ifelse(!is.na(Latitude), Latitude, 3.5)
    ) #case if no information of localisation -> Montpellier

  # normalize column names: drop standard column names not chosen by the user
  # and replace user names with standard names
  col_to_drop <- col_map[
    (col_map$col_res != col_map$col_user) | (is.na(col_map$col_user)),
    "col_res"
  ]
  RadN_dbf <- RadN_dbf %>%
    dplyr::select(-any_of(col_to_drop)) %>%
    dplyr::rename(any_of(setNames(col_map$col_user, col_map$col_res)))

  RadN_dbf_by_latlon <- RadN_dbf %>%
    dplyr::mutate(longlat = paste0(Longitude, '-', Latitude)) %>%
    dplyr::group_by(longlat)

  plots_by_longlat <- RadN_dbf_by_latlon %>%
    dplyr::group_map(
      ~ {
        #centrage for measurement
        shapefile_data_RadN <- creer_parcelles_autour_point_centre(
          lon = .x$Longitude[1],
          lat = .x$Latitude[1],
          crops = .x[[COL$crop]],
          areas = .x[[COL$area]],
          col_crop = COL$crop,
          col_area = COL$area
        ) %>%
          dplyr::mutate(
            "{COL$plot}" := .x[[COL$plot]],
            "{COL$poly}" := .x[[COL$poly]]
          )
      }
    )
  shapefile_data <- do.call(rbind, plots_by_longlat)

  #around center for graphical representation
  plots_by_longlat <- RadN_dbf_by_latlon %>%
    dplyr::group_map(
      ~ {
        #centrage for measurement
        shapefile_data_RadN <- creer_parcelles_autour_point_repgraph(
          lon = .x$Longitude[1],
          lat = .x$Latitude[1],
          crops = .x[[COL$crop]],
          areas = .x[[COL$area]],
          col_crop = COL$crop,
          col_area = COL$area
        ) %>%
          dplyr::mutate(
            "{COL$plot}" := .x[[COL$plot]],
            "{COL$poly}" := .x[[COL$poly]]
          )
      }
    )
  shp_data_forillu <- do.call(rbind, plots_by_longlat)
  message("shp created")
  return(list("shp_data" = shapefile_data, "shp_data_illu" = shp_data_forillu))
}
