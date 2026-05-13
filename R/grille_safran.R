#' SAFRAN climate grid used by CAWET
#'
#' `grille_safran` is an `sf` polygon grid derived from Météo-France SAFRAN
#' mesh coordinates. It is used to associate each territory/plot with a SAFRAN
#' climate cell for weather data retrieval.
#'
#' @format An `sf` object with 8,969 rows and 4 variables:
#' \describe{
#'   \item{id_maille}{Character. SAFRAN grid cell identifier.}
#'   \item{lambx}{Integer. SAFRAN X coordinate in Lambert-93 (hectometers).}
#'   \item{lamby}{Integer. SAFRAN Y coordinate in Lambert-93 (hectometers).}
#'   \item{geometry}{POLYGON geometry (CRS: EPSG 2154, RGF93 / Lambert-93).}
#' }
#'
#' @source
#' Météo-France public dataset "coordonnees-des-mailles_339.csv":
#' <https://donneespubliques.meteofrance.fr/client/document/coordonnees-des-mailles_339.csv>
#'
#' @examples
#' data(grille_safran)
#' sf::st_crs(grille_safran)
"grille_safran"
