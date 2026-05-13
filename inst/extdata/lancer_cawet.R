# ============================================================
#  CAWET - Script de lancement
#  Modifier uniquement les lignes ci-dessous
# ============================================================

dossier_travail  <- "C:/MonProjet/CAWET"   # dossier où seront copiés les données et résultats
nom_ordonnanceur <- "mon_ordonnanceur.xlsx" # nom de ton fichier ordonnanceur

# ============================================================
#  NE PAS MODIFIER EN DESSOUS
# ============================================================

# -- Installation de CAWET (seulement si pas encore installé) --
if (!requireNamespace("CAWET", quietly = TRUE)) {
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
  }
  repo_path <- dirname(dirname(dirname(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE))))
  pak::pak(paste0("local::", repo_path))
}

# -- Création du dossier de travail (seulement la première fois) --
if (!dir.exists(dossier_travail)) {
  library(CAWET)
  setup_CAWET(dossier_travail)
  message("Dossier de travail créé : ", dossier_travail)
  message("Placer votre ordonnanceur dans : ", file.path(dossier_travail, "Ordonnanceur"))
  stop("Dossier initialisé. Placer votre ordonnanceur puis relancer le script.")
}

# -- Lancement --
chemin_ordonnanceur <- file.path(dossier_travail, "Ordonnanceur", nom_ordonnanceur)

library(CAWET)
run(
  chemin_ordonnanceur = chemin_ordonnanceur,
  Working_path        = dossier_travail
)
