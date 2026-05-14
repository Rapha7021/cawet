# ============================================================
#  CAWET - Script de lancement
#  Modifier uniquement les lignes ci-dessous
# ============================================================

dossier_travail  <- "/home/shiff/Bureau/CAWET"   # dossier où seront copiés les données et résultats
nom_ordonnanceur <- "Ordonnanceur_exemple.xlsx" # nom de ton fichier ordonnanceur

# Valeurs saisies via lancer_CAWET.bat (prioritaires sur les lignes ci-dessus)
if (nzchar(Sys.getenv("CAWET_DOSSIER_TRAVAIL")))  dossier_travail  <- Sys.getenv("CAWET_DOSSIER_TRAVAIL")
if (nzchar(Sys.getenv("CAWET_NOM_ORDONNANCEUR"))) nom_ordonnanceur <- Sys.getenv("CAWET_NOM_ORDONNANCEUR")

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
if (!dir.exists(file.path(dossier_travail, "Ordonnanceur"))) {
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
