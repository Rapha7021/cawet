## code to prepare `grille_safran` dataset goes here

#network
centroSafran <- sf::st_as_sf(
  read.csv2(
    "https://donneespubliques.meteofrance.fr/client/document/coordonnees-des-mailles_339.csv",
    skip = 4
  ),
  coords = c("lambx93", "lamby93"),
  crs = 2154
)

grille <- sf::st_make_grid(centroSafran, cellsize = 8000, square = TRUE)
grille_safran <- sf::st_sf(geometry = grille) %>%
  sf::st_join(centroSafran, join = st_contains, largest = TRUE) %>%
  dplyr::filter(!is.na(X.num_maille)) %>%
  dplyr::select(id_maille = X.num_maille, lambx, lamby, geometry) %>%
  dplyr::mutate(id_maille = as.character(id_maille))
sf::st_crs(grille_safran) <- 2154

usethis::use_data(grille_safran, overwrite = TRUE)
