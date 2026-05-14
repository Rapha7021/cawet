#' Get or create a CAWET run configuration
#'
#' @describeIn get_Run_Config This function retrieves or creates a run configuration for CAWET,
#' including the run path, scenario data, and path to charged input data.
#'
#' @param path Character string specifying the path to an existing run directory.
#' If NA the function retrieves the most recent run directory.
#' By default, a folder selection dialog will be opened.
#' @param Working_path Character string specifying the working directory.
#' @param Scenario Optional data frame containing scenario data.
#' If provided, it will be saved to the run directory. If NULL, the function
#' will attempt to read the scenario data from the run directory.
#'
#' @return [CAWET::get_Run_Config] returns a [list] with the following elements:
#' - `Working_path`: The working directory used for the run.
#' - `Path_Run`: The path to the created or selected run directory.
#' - `Scenario`: A data frame containing the scenario information used for the run.
#' - `Path_Run_Chargementdata`: The path to the `Charged_inputs` directory inside the run.
#'
#' [CAWET::get_Path_Run] returns a character string with the path to the run directory.
#'
#' @details
#' Behavior depending on the value of `Scenario`:
#' - If `Scenario` is provided (non-NULL):
#'   - `get_Run_Config` calls `get_Path_Run(..., new = TRUE)` which creates a new
#'     run directory at `file.path(Working_path, "Runs", paste0("Run_", <timestamp>))`.
#'   - The provided `Scenario` data frame is written into `Scenarii.csv` inside the
#'     new run directory.
#'   - A `Charged_inputs` directory is created inside the run directory and its path is
#'     returned as `Path_Run_Chargementdata`.
#' - If `Scenario` is NULL:
#'   - `get_Run_Config` calls `get_Path_Run(..., new = FALSE)` which:
#'     - If `path` is NA: finds the most recent existing run directory under
#'       `file.path(Working_path, "Runs")` (the last `Run_` entry).
#'     - If `path` is provided: validates that the directory exists and returns it.
#'   - The function attempts to read `Scenarii.csv` from the chosen run directory using
#'     `read_ordonnanceur()` to populate `Scenario`.
#'   - The `Charged_inputs` directory is created if missing and its path returned.
#'
#' Note: the default `path` uses `choose.dir()`; when creating a new run the run
#' timestamp ensures unique run folder names.
#'
#' @export
get_Run_Config <- function(
  path = NULL,
  Working_path = dirname(dirname(path)),
  Scenario = NULL
) {
  Path_Run <- get_Path_Run(path, Working_path, !is.null(Scenario))
  Path_Run_chargementdata <- paste0(Path_Run, "/Charged_inputs")
  dir.create(Path_Run_chargementdata, showWarnings = FALSE)
  if (!is.null(Scenario)) {
    write.csv2(Scenario, paste0(Path_Run, '/', 'Scenarii.csv'), row.names = F)
  } else {
    Scenario <- read_ordonnanceur(
      paste0(Path_Run, "/Scenarii.csv")
    )
  }
  return(
    list(
      Working_path = Working_path,
      Path_Run = Path_Run,
      Scenario = Scenario,
      Path_Run_Chargementdata = Path_Run_chargementdata
    )
  )
}

#' @describeIn get_Run_Config Get or create a run path
#' @param new Logical indicating whether to create a new run directory.
#' If TRUE, a new run directory will be created. If FALSE, the most recent
#' existing run directory will be retrieved.
#' @export
get_Path_Run <- function(
  path = choose.dir(),
  Working_path,
  new = FALSE
) {
  if (new) {
    run_time <- format(
      Sys.time(),
      "%Y-%m-%d_%H%M%S"
    )
    Path_Run <- file.path(
      Working_path,
      "Runs",
      paste0("Run_", run_time)
    )
    dir.create(Path_Run, recursive = TRUE)
    cli_alert_success("Création du répertoire de run : {Path_Run}")
    return(Path_Run)
  } else if (is.na(path)) {
    Run_paths <- list.dirs(
      file.path(Working_path, "Runs"),
      full.names = TRUE,
      recursive = FALSE
    )
    Run_Paths <- dplyr::last(Run_paths[grep("^Run_", basename(Run_paths))])
    if (length(Run_paths) == 0) {
      stop("No existing runs found in the 'Runs' directory.")
    } else {
      return(Run_Paths)
    }
  } else {
    if (!dir.exists(path)) {
      stop("Le chemin specifie n'existe pas : ", path)
    }
    return(path)
  }
}

#' Suppress messages from ggplot2::ggsave
#'
#' This function is a wrapper around `ggplot2::ggsave` that suppresses messages.
#' It can be used to avoid cluttering the console with messages when saving plots.
#' @param ... Arguments passed to `ggplot2::ggsave`.
#' @return The return value of `ggplot2::ggsave`, invisibly.
#' @export
ggsave <- function(...) {
  suppressMessages(ggplot2::ggsave(...))
}

#' Suppress messages from sf::st_read
#'
#' This function is a wrapper around `sf::st_read` that suppresses messages.
#' It can be used to avoid cluttering the console with messages when reading spatial data.
#' @param ... Arguments passed to `sf::st_read`.
#' @return The return value of `sf::st_read`, invisibly.
#' @export
st_read <- function(...) {
  sink(tempfile())
  on.exit(sink())
  suppressMessages(sf::st_read(...))
}

# Catalogue des fichiers SIM2 disponibles sur data.gouv.fr (Météo-France)
.sim2_datagouv_catalog <- list(
  list(year_min = 1958, year_max = 1959,
       url = "https://www.data.gouv.fr/api/1/datasets/r/5dfb33b3-fae5-4d0e-882d-7db74142bcae",
       filename = "QUOT_SIM2_1958-1959.csv.gz"),
  list(year_min = 1960, year_max = 1969,
       url = "https://www.data.gouv.fr/api/1/datasets/r/eb0d6e42-cee6-4d7c-bc5b-646be4ced72e",
       filename = "QUOT_SIM2_1960-1969.csv.gz"),
  list(year_min = 1970, year_max = 1979,
       url = "https://www.data.gouv.fr/api/1/datasets/r/33417617-c0dd-4513-804e-c3f563cb81b4",
       filename = "QUOT_SIM2_1970-1979.csv.gz"),
  list(year_min = 1980, year_max = 1989,
       url = "https://www.data.gouv.fr/api/1/datasets/r/08ad5936-cb9e-4284-a6fc-36b29aca9607",
       filename = "QUOT_SIM2_1980-1989.csv.gz"),
  list(year_min = 1990, year_max = 1999,
       url = "https://www.data.gouv.fr/api/1/datasets/r/ad584d65-7d2d-4ff1-bc63-4f93357ed196",
       filename = "QUOT_SIM2_1990-1999.csv.gz"),
  list(year_min = 2000, year_max = 2009,
       url = "https://www.data.gouv.fr/api/1/datasets/r/10d2ce77-5c3b-44f8-bb46-4df27ed48595",
       filename = "QUOT_SIM2_2000-2009.csv.gz"),
  list(year_min = 2010, year_max = 2019,
       url = "https://www.data.gouv.fr/api/1/datasets/r/da6cd598-498b-4e39-96ea-fae89a4a8a46",
       filename = "QUOT_SIM2_2010-2019.csv.gz"),
  list(year_min = 2020, year_max = 2026,
       url = "https://www.data.gouv.fr/api/1/datasets/r/92065ec0-ea6f-4f5e-8827-4344179c0a7f",
       filename = "QUOT_SIM2_2020-2026.csv.gz")
)

#' Télécharge et lit les données SIM2 depuis data.gouv.fr (Météo-France)
#'
#' Fallback utilisé quand l'API G-EAU (`get_sim2_daily`) ne couvre pas la période
#' demandée (ex: années > 2019). Télécharge les fichiers CSV bulk de Météo-France,
#' les met en cache, filtre par maille et période, et renvoie un data.frame au même
#' format que `RADIS::get_sim2_daily(api_format="csv")`.
#'
#' @param lambx Coordonnée LAMBX brute de la maille (ex: 3560, pas *100)
#' @param lamby Coordonnée LAMBY brute de la maille (ex: 18810, pas *100)
#' @param year_from Première année de la période
#' @param year_to Dernière année de la période
#' @param cache_dir Répertoire de cache pour les fichiers téléchargés
#'
#' @return data.frame avec les mêmes colonnes que `get_sim2_daily(api_format="csv")`
get_sim2_from_datagouv <- function(lambx, lamby, year_from, year_to, cache_dir) {
  needed <- Filter(
    function(f) f$year_max >= year_from && f$year_min <= year_to,
    .sim2_datagouv_catalog
  )

  if (length(needed) == 0) {
    stop(paste0(
      "Aucun fichier SIM2 disponible sur data.gouv.fr pour la periode ",
      year_from, "-", year_to
    ))
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  date_from <- as.Date(paste0(year_from, "-01-01"))
  date_to   <- as.Date(paste0(year_to,   "-12-31"))

  all_chunks <- list()

  for (file_info in needed) {
    cache_path <- file.path(cache_dir, file_info$filename)

    if (!file.exists(cache_path)) {
      size_mo <- switch(
        file_info$filename,
        "QUOT_SIM2_2020-2026.csv.gz" = "~800 Mo",
        "QUOT_SIM2_2010-2019.csv.gz" = "~1.2 Go",
        "~1.2 Go"
      )
      cli::cli_alert_info(paste0(
        "Telechargement ", file_info$filename, " depuis data.gouv.fr (Meteo-France, ",
        size_mo, "). Ce fichier sera mis en cache dans : ", cache_dir
      ))
      utils::download.file(
        url      = file_info$url,
        destfile = cache_path,
        method   = "auto",
        mode     = "wb",
        quiet    = FALSE
      )
    } else {
      cli::cli_alert_info(paste0("Cache SIM2 trouve : ", cache_path))
    }

    # Lecture efficace : filtre LAMBX/LAMBY via awk avant chargement en memoire
    cmd <- paste0(
      "zcat '", cache_path, "' | ",
      "awk -F';' 'NR==1 || ($1==", lambx, " && $2==", lamby, ")'"
    )

    df <- tryCatch({
      data.table::fread(cmd = cmd, sep = ";", header = TRUE, data.table = FALSE)
    }, error = function(e) {
      # Fallback sans awk (ex: Windows)
      cli::cli_alert_warning(paste0(
        "Lecture integrale du fichier (fallback) : ", conditionMessage(e)
      ))
      full <- data.table::fread(cache_path, sep = ";", header = TRUE, data.table = FALSE)
      full[full$LAMBX == lambx & full$LAMBY == lamby, ]
    })

    if (nrow(df) == 0) next

    # Conversion de la date (format YYYYMMDD entier → Date)
    df$DATE <- as.Date(as.character(df$DATE), format = "%Y%m%d")

    # Filtrage par periode
    df <- df[df$DATE >= date_from & df$DATE <= date_to, ]
    if (nrow(df) == 0) next

    # Renommage des colonnes : ajout du suffixe _Q pour correspondre au format G-EAU
    cols_to_rename <- setdiff(colnames(df), c("LAMBX", "LAMBY", "DATE"))
    colnames(df)[colnames(df) %in% cols_to_rename] <- paste0(cols_to_rename, "_Q")

    # LAMBX/LAMBY multipliés par 100 pour correspondre au format G-EAU
    df$LAMBX <- df$LAMBX * 100
    df$LAMBY <- df$LAMBY * 100

    all_chunks[[length(all_chunks) + 1]] <- df
  }

  if (length(all_chunks) == 0) return(data.frame())

  do.call(rbind, all_chunks)
}

#' Télécharge le RPG IGN via WFS BBOX (contournement des erreurs 502 avec CQL)
#'
#' Utilise le paramètre BBOX+CRS:84 au lieu d'un filtre CQL géométrique, ce qui
#' évite les erreurs 502 retournées par `happign::get_wfs()` sur le service
#' `data.geopf.fr`. Télécharge l'ensemble des parcelles dans la bbox du territoire,
#' puis filtre au polygone réel, et met en cache le résultat.
#'
#' @param sf_poly Polygone sf du territoire (une ou plusieurs zones)
#' @param year Année RPG (2015–2022)
#' @param crs CRS cible pour la sortie
#' @param cache_dir Répertoire de cache (NULL = pas de cache)
#'
#' @return sf avec colonnes id_parcel, surf_parc, code_cultu, source, geometry
#' @keywords internal
get_rpg_from_ign_wfs_bbox <- function(sf_poly, year, crs, cache_dir = NULL) {
  if (!year %in% 2015:2022) {
    cli::cli_alert_warning("RPG IGN WFS BBOX: annee {year} non supportee (2015-2022), abandon.")
    return(NULL)
  }

  # Cache : identifiant basé sur la bbox et l'année
  bb4326 <- sf::st_bbox(sf::st_transform(sf_poly, 4326))
  cache_key <- sprintf(
    "RPG_IGN_BBOX_%d_%.2f_%.2f_%.2f_%.2f",
    year, bb4326["xmin"], bb4326["ymin"], bb4326["xmax"], bb4326["ymax"]
  )
  cache_file <- if (!is.null(cache_dir)) {
    file.path(cache_dir, paste0(cache_key, ".gpkg"))
  } else NULL

  if (!is.null(cache_file) && file.exists(cache_file)) {
    cli::cli_alert_info("RPG IGN BBOX {year}: cache trouve -> {cache_file}")
    rpg_cached <- sf::st_read(cache_file, quiet = TRUE)
    # Retro-compat: ancien cache sans id_poly
    if ("id_poly" %in% colnames(sf_poly) && !"id_poly" %in% colnames(rpg_cached)) {
      rpg_cached$id_poly <- sf_poly$id_poly[1]
    }
    # Retro-compat: ancien cache avec surf_parc (ha) au lieu de surface_m2 (m2)
    if ("surf_parc" %in% colnames(rpg_cached) && !"surface_m2" %in% colnames(rpg_cached)) {
      rpg_cached$surface_m2 <- as.numeric(rpg_cached$surf_parc) * 10000
      rpg_cached$surf_parc  <- NULL
    }
    return(rpg_cached)
  }

  # Format BBOX accepté par data.geopf.fr WFS : minLon,minLat,maxLon,maxLat,CRS:84
  bbox_str <- paste(
    bb4326["xmin"], bb4326["ymin"], bb4326["xmax"], bb4326["ymax"],
    "CRS:84", sep = ","
  )
  layer    <- paste0("RPG.", year, ":parcelles_graphiques")
  base_url <- "https://data.geopf.fr/wfs/ows"
  page_sz  <- 2000

  # Compter le total avant de paginer
  count_url <- paste0(
    base_url,
    "?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature&TYPENAMES=", layer,
    "&BBOX=", bbox_str, "&outputFormat=json&count=1&startindex=0"
  )
  resp0 <- tryCatch(
    jsonlite::fromJSON(count_url),
    error = function(e) {
      cli::cli_alert_warning("RPG IGN BBOX: erreur comptage - {e$message}")
      NULL
    }
  )
  if (is.null(resp0)) return(NULL)

  total <- resp0$totalFeatures
  if (is.null(total) || total == 0) {
    cli::cli_alert_warning("RPG IGN BBOX {year}: aucune parcelle dans le bbox du territoire")
    return(NULL)
  }

  n_pages <- ceiling(total / page_sz)
  cli::cli_alert_info(
    "RPG IGN BBOX {year}: {total} parcelles -> {n_pages} pages de {page_sz}..."
  )

  # Téléchargement paginé
  all_pages <- vector("list", n_pages)
  for (i in seq_len(n_pages)) {
    page_url <- paste0(
      base_url,
      "?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature&TYPENAMES=", layer,
      "&BBOX=", bbox_str, "&outputFormat=json",
      "&count=", page_sz, "&startindex=", (i - 1L) * page_sz
    )
    # 3 essais avec délai croissant avant d'abandonner la page
    for (attempt in 1:3) {
      result <- tryCatch(
        sf::st_read(page_url, quiet = TRUE),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        if (attempt < 3) Sys.sleep(2 ^ attempt)
        else cli::cli_alert_warning("RPG IGN BBOX page {i}/{n_pages} abandonnee apres 3 essais")
      } else {
        all_pages[[i]] <- result
        break
      }
    }
    if (i %% 10 == 0 || i == n_pages) {
      cli::cli_alert_info("  {i}/{n_pages} pages recues")
    }
  }

  all_pages <- Filter(Negate(is.null), all_pages)
  if (length(all_pages) == 0) {
    cli::cli_alert_warning("RPG IGN BBOX {year}: aucune page recuperee")
    return(NULL)
  }

  rpg_raw <- do.call(rbind, all_pages)

  # Filtrer au polygone réel du territoire (intersection spatiale)
  sf::sf_use_s2(FALSE)
  rpg_proj <- sf::st_transform(rpg_raw, sf::st_crs(sf_poly))
  rpg_proj <- sf::st_make_valid(rpg_proj)
  idx      <- lengths(sf::st_intersects(rpg_proj, sf_poly)) > 0L
  rpg_proj <- rpg_proj[idx, ]

  if (nrow(rpg_proj) == 0L) {
    cli::cli_alert_warning("RPG IGN BBOX {year}: aucune parcelle intersecte le territoire apres filtrage")
    return(NULL)
  }

  rpg_out            <- sf::st_transform(rpg_proj, crs)
  rpg_out$source     <- "IGN"
  # Convertir surf_parc (ha) en surface_m2 (m2)
  rpg_out$surface_m2 <- as.numeric(rpg_out$surf_parc) * 10000
  # Ajouter id_poly depuis le polygone territoire
  if ("id_poly" %in% colnames(sf_poly)) {
    rpg_out$id_poly <- sf_poly$id_poly[1]
  }
  cols_keep <- intersect(c("id_poly", "id_parcel", "surface_m2", "code_cultu", "source"), colnames(rpg_out))
  rpg_out   <- rpg_out[, cols_keep]

  cli::cli_alert_success(
    "RPG IGN BBOX {year}: {nrow(rpg_out)} parcelles recuperees (sur {total} dans le bbox)"
  )

  # Mise en cache
  if (!is.null(cache_file)) {
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    sf::st_write(rpg_out, cache_file, quiet = TRUE,
                 delete_dsn = file.exists(cache_file))
    cli::cli_alert_success("RPG IGN BBOX {year} mis en cache: {cache_file}")
  }

  return(rpg_out)
}
