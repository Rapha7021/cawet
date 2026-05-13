#' Create plots around a center, and change the center for each crop (for "RADIS noir" option)
#'
#' This function creates plots around a point, with a given surface area.
#' @param lon [numeric] longitude in decimal degrees.
#' @param lat [numeric] latitude in decimal degrees.
#' @param crops [character] [vector] crops to create.
#' @param areas [numeric] [vector] surface areas of the crops listed in `crops` in m2 (in the same order as `crops`).
#' @param col_crop [character] standardized name of the column with the crop names.
#' @param col_area [character] standardized name of the column with the surface area.
#'
#' @return A [sf] object containing a plot for each crop, with the corresponding surface area.
#' @export
#'
creer_parcelles_autour_point_repgraph <- function(
    lon,
    lat,
    crops,
    areas,
    col_crop,
    col_area
) {
    if (length(crops) != length(areas)) {
        stop("Length of 'crops' and 'areas' differ.")
    }
    # Point central en coordonnées géographiques (WGS84 - 4326)
    pt <- sf::st_point(c(lon, lat)) %>% sf::st_sfc(crs = 4326)
    # Conversion en projection métrique (ex : Lambert 93)
    pt_proj <- sf::st_transform(pt, 2154)
    pt_coords <- sf::st_coordinates(pt_proj)
    parcelles_geom <- list()
    angle <- 0
    delta_angle <- 360 / length(crops)
    for (i in seq_along(crops)) {
        rayon <- sqrt(areas[i] / pi)
        dx <- cospi(angle / 180) * rayon * 2
        dy <- sinpi(angle / 180) * rayon * 2
        # Créer le point déplacé - ou pas
        new_x <- pt_coords[1] + dx
        new_y <- pt_coords[2] + dy

        new_center <- sf::st_point(c(new_x, new_y)) # sfg
        new_center_sfc <- sf::st_sfc(new_center, crs = 2154)
        # Buffer autour du point
        buffer <- sf::st_buffer(new_center_sfc, dist = rayon)
        # Extraire la géométrie (sfg) du buffer pour l’ajouter à la liste
        parcelles_geom[[i]] <- sf::st_geometry(buffer)[[1]]
        angle <- angle + delta_angle
    }
    # Créer un sf à partir des géométries
    parcelles_sf <- sf::st_sf(geometry = sf::st_sfc(parcelles_geom, crs = 2154))
    parcelles_sf[[col_crop]] = crops
    parcelles_sf[[col_area]] = areas

    return(parcelles_sf)
}

#' Create plots around a center (for "RADIS noir" option)
#'
#' This function creates plots around a point, with a given surface area.
#' @param lon [numeric] longitude in decimal degrees.
#' @param lat [numeric] latitude in decimal degrees.
#' @param crops [character] [vector] crops to create.
#' @param areas [numeric] [vector] surface areas of the crops listed in `crops` in m2 (in the same order as `crops`).
#' @param col_crop [character] standardized name of the column with the crop names.
#' @param col_area [character] standardized name of the column with the surface area.
#'
#' @return A [sf] object containing a plot for each crop, with the corresponding surface area.
#' @export
#'
creer_parcelles_autour_point_centre <- function(
    lon,
    lat,
    crops,
    areas,
    col_crop,
    col_area
) {
    if (length(crops) != length(areas)) {
        stop("Length of 'crops' and 'areas' differ.")
    }
    # Point central en coordonnées géographiques (WGS84 - 4326)
    pt <- sf::st_point(c(lon, lat)) %>% sf::st_sfc(crs = 4326)
    # Conversion en projection métrique (ex : Lambert 93)
    pt_proj <- sf::st_transform(pt, 2154)
    parcelles_geom <- list()
    for (i in seq_along(crops)) {
        rayon <- sqrt(areas[i] / pi)

        # Ici : on garde le même centre pour toutes les parcelles
        buffer <- sf::st_buffer(pt_proj, dist = rayon)

        # Extraire la géométrie (sfg) du buffer pour l’ajouter à la liste
        parcelles_geom[[i]] <- sf::st_geometry(buffer)[[1]]
    }
    # Créer un sf à partir des géométries
    parcelles_sf <- sf::st_sf(
        geometry = sf::st_sfc(parcelles_geom, crs = 2154)
    )
    parcelles_sf[[col_area]] <- areas
    parcelles_sf[[col_crop]] <- crops
    return(parcelles_sf)
}
