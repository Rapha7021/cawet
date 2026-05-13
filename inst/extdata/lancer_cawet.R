# ============================================================
#  CAWET - Script de lancement
#  Modifier uniquement les deux lignes ci-dessous
# ============================================================

dossier_travail     <- "C:/MonProjet/CAWET"          # dossier contenant Ordonnanceur/, Sols/, shp/...
nom_ordonnanceur    <- "mon_ordonnanceur.xlsx"        # nom de ton fichier ordonnanceur

# ============================================================
#  NE PAS MODIFIER EN DESSOUS
# ============================================================

chemin_ordonnanceur <- file.path(dossier_travail, "Ordonnanceur", nom_ordonnanceur)

library(CAWET)
run(
  chemin_ordonnanceur = chemin_ordonnanceur,
  Working_path        = dossier_travail
)
