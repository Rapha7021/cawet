#' Preparation of input data for CAWET model using RADIS data
#'
#' This script loads and prepares input data for the CAWET model using RADIS data.
#' It reads scenario information from an Excel file, processes spatial data for plots,
#' and prepares meteorological and soil data for each scenario and year.
#' It saves the prepared data in a structured directory for further analysis.
#'
#' @details
#' **Input files and data sources**
#'
#' - `chemin_ordonnanceur`: scheduler file read with [CAWET::read_ordonnanceur()] (scenario-level
#'   configuration used throughout the workflow).
#' - **Important convention**: every `Scenario$XXX` used in this function corresponds to the
#'   values found in the `XXX` column of the scheduler file (one value per scenario row).
#' - `Working_path/Model_param/odr_to_rpg.csv`: mapping table used to harmonize crop codes
#'   between ODR and RPG nomenclatures.
#' - Météo-France grid coordinates file (downloaded from
#'   `https://donneespubliques.meteofrance.fr/client/document/coordonnees-des-mailles_339.csv`),
#'   used to build the climate network mesh.
#' - `Scenario$Wanted_output`: scenario-specific file read with [CAWET::read_CAWET_data()] to control
#'   optional map/graph exports.
#' - Plot/territory inputs (depending on `Scenario$Plots_RADIS_RPG`):
#'   - Existing plots (`"Yes"`/`"External_plot_map"`) through
#'     [CAWET::prepare_territory_for_existing_plots()].
#'   - Synthetic territory (`"Non_existant_plots"`) through
#'     [CAWET::prepare_territory_for_non_existing_plots()].
#' - Optional soil input files:
#'   - `Scenario$Soil_information_link` when `Scenario$Sol_fixed_RADIS == "No"`.
#'   - Optional supplementary AWC raster directory from
#'     `Scenario$Sol_carte_sup_RU_link_to_doss` (files matching `RU_*.tif`).
#' - Optional climate inputs:
#'   - `Scenario$Climat_link_fixe` for forced/unique climate mode.
#'   - `Scenario$Climat_link_Drias` parameter table for DRIAS extraction.
#'
#' **Main processing steps**
#'
#' 1. Read scheduler, initialize run directories with [CAWET::get_Run_Config()], and validate unique
#'    scenario names.
#' 2. Build the SAFRAN grid geometry and scenario-specific working folders.
#' 3. Load and save wanted outputs table (`Wanted_outputs.csv`).
#' 4. Build plot geometries per scenario and year:
#'    - Download RPG polygons ([RADIS::get_rpg_data()]) when requested.
#'    - Harmonize crop codes using `odr_to_rpg.csv`.
#'    - Keep/rename standard columns (`id_poly`, `id_parcel`, `code_cultu`, `surface_m2`).
#' 5. Spatially link plots to climate meshes and sub-meshes:
#'    - Create network/subnetwork intersections.
#'    - Export link table between plots and meshes.
#'    - Optionally generate cartographic diagnostic figures.
#' 6. Enrich plots with soil attributes (depth, AWC, texture), depending on scenario mode:
#'    - External fixed soil table.
#'    - RADIS BDGSF / Info&Sols retrieval.
#'    - Optional AWC replacement from supplementary raster maps.
#' 7. Aggregate yearly plot information by crop/polygon/mesh/sub-mesh and write spatial outputs.
#' 8. Retrieve climate time series (Unique/Forced, SAFRAN, or DRIAS) and write per-scenario/per-mesh files.
#' 9. Build and save linear yearly tables (surface, depth, AWC, clay, silt, sand) per crop,
#'    polygon and sub-mesh.
#' 10. For synthetic territories, export additional illustration and map files.
#'
#' **Outputs created**
#'
#' For each scenario, files are written under `cfgRun$Path_Run_Chargementdata/<index>_<Scenario>/`:
#'
#' - `Wanted_outputs.csv`
#' - `Networkplots_<Year>.shp`
#' - `Link_plots_mailles_<Year>.csv`
#' - `Plots_all_RADIS_information_<Year>.shp`
#' - `Crops_all_RADIS_information.shp` (all years combined)
#' - `Meteo/Meteo.csv` (unique/forced climate mode) or
#'   `Meteo/Meteo_maille<id>.csv` (network mode)
#' - Optional maps in `Meteo/`:
#'   - `Plots_in_climaticalnetwork_<Year>.png`
#'   - `Plots_in_climaticalnetwork_<Year>_withsubnetwork.png`
#'   - `Illustration_plots_maille.png` (synthetic territory mode)
#' - `Formodification/Territoire_repartition_cultures_<Year>.csv`
#' - `Formodification/Soil_average_<Year>.csv`
#' - Linear tables:
#'   - `Surf_table_<Scenario>.csv`
#'   - `Prof_table_<Scenario>.csv`
#'   - `AWC_table_<Scenario>.csv`
#'   - `Arg_mean_table_<Scenario>.csv`
#'   - `Lim_mean_table_<Scenario>.csv`
#'   - `Sab_mean_table_<Scenario>.csv`
#' - Additional file for synthetic territory:
#'   - `Plots_location_notcenteredforillustration.shp`
#'
#' @inheritParams run
#' @return A list containing:
#' - `Run_path`: The path to the run directory.
#' - `Scenario`: A data frame containing scenario data.
#' - `Path_Run_Chargementdata`: The path to the charged input data directory.
#'
#' @export
#'
run_script1 <- function(
  cfgRun
) {
  # Global variables initialization
  Scenario <- cfgRun$Scenario
  Path_Run <- cfgRun$Path_Run
  Path_Run_chargementdata <- cfgRun$Path_Run_Chargementdata
  Working_path <- cfgRun$Working_path
  utils::data("grille_safran", package = "CAWET")

  cli_alert_info(
    "Demarrage run_script1 - Preparation des donnees dans le dossier: {Path_Run}"
  )

  # load mapping between odr and rpg names
  if (any(Scenario$Plots_RADIS_RPG == "Yes")) {
    odr_to_rpg <- read.csv(file.path(
      Working_path,
      "Model_param",
      "odr_to_rpg.csv"
    ))
  }

  # define column names
  COL <- list(
    poly = "id_poly",
    plot = "id_parcel",
    crop = "code_cultu",
    area = "surface_m2"
  )
  col_map <- data.frame(
    col_ordo = c(
      "Colname_idpoly",
      "Colname_idplots",
      "Colname_codecultureRPG",
      "Colname_area"
    ),
    col_res = c(COL$poly, COL$plot, COL$crop, COL$area)
  )

  if (any(duplicated(Scenario$Scenario))) {
    stop(
      "Duplicate scenario names found in the Ordonnanceur. Please ensure all scenario names are unique."
    )
  }

  # Loop of creation charged_inputs for each scenario
  for (Scen_i in seq_along(Scenario$Scenario)) {
    Scen <- Scenario$Scenario[Scen_i]

    cli_alert_info("Starting Scenario {Scen_i} : {Scen}")

    retrieve_rpg <- Scenario$Plots_RADIS_RPG[Scen_i]
    col_map$col_user = as.character(Scenario[Scen_i, col_map$col_ordo])

    Input_run_path_Scen <- paste0(
      Path_Run_chargementdata,
      "/",
      Scen_i,
      '_',
      Scen
    ) #path in charged_inputs for the scenario data
    if (!file.exists(Input_run_path_Scen)) {
      dir.create(Input_run_path_Scen)
    }
    for (folder in c("Meteo", "Formodification")) {
      if (!file.exists(file.path(Input_run_path_Scen, folder))) {
        dir.create(file.path(Input_run_path_Scen, folder))
      }
    }
    if (
      (!file.exists(paste0(Input_run_path_Scen, "/Data_for_Aquacrop"))) &
        Scenario$Model[Scen_i] == "Aquacrop"
    ) {
      dir.create(paste0(Input_run_path_Scen, "/Data_for_Aquacrop"))
    }

    #Wanted outputs saving
    Wanted_output <- read_CAWET_data(
      Working_path = Working_path,
      file = Scenario$Wanted_output[Scen_i]
    )
    write.csv2(
      Wanted_output,
      paste0(Input_run_path_Scen, "/Wanted_outputs.csv"),
      row.names = F
    )

    # Prepare the area for which the data will be retrieved
    if (retrieve_rpg %in% c("Yes", "External_plot_map")) {
      external_plots <- ifelse(retrieve_rpg == "External_plot_map", TRUE, FALSE)
      shapefile_data_base <- prepare_territory_for_existing_plots(
        Working_path,
        Scenario[Scen_i, ],
        COL,
        col_map,
        external_plots
      )
    } else if (retrieve_rpg == "Non_existant_plots") {
      #if crop table is fake
      res <- prepare_territory_for_non_existing_plots(
        Working_path,
        Scenario[Scen_i, "Shp_link"],
        COL,
        col_map
      )

      shapefile_data <- res$shp_data
      shapefile_data_forillustration <- res$shp_data_illu
    } else {
      stop(
        "Invalid value for Plots_RADIS_RPG. Please choose one of: 'Yes', 'External_plot_map', 'Non_existant_plots'."
      )
    }

    #creation of empty tables for saving information
    Date_Table <- data.frame(
      date = seq(
        as.Date(
          paste0("01-01-", Scenario$First_year_simulation[Scen_i]),
          format = "%d-%m-%Y"
        ),
        as.Date(
          paste0("31-12-", Scenario$Last_year_simulation[Scen_i]),
          format = "%d-%m-%Y"
        ),
        by = "days"
      )
    )

    for (Year in c(
      Scenario$First_year_simulation[Scen_i]:Scenario$Last_year_simulation[
        Scen_i
      ]
    )) {
      cli_alert_info("Processing Crops for scenario {Scen} - year {Year}")

      #Creation of the crop table
      if (retrieve_rpg == "Yes") {
        #If map is a location map, need to download RPG plots
        for (poly in unique(shapefile_data_base$id_poly)) {
          shapefile_data_base_pol <- shapefile_data_base %>%
            dplyr::filter(id_poly == poly)
          # Support colonne optionnelle Forced_RPG_year dans l'ordonnanceur
          forced_rpg <- if ("Forced_RPG_year" %in% names(Scenario) &&
                            !is.na(Scenario$Forced_RPG_year[Scen_i]) &&
                            nchar(trimws(Scenario$Forced_RPG_year[Scen_i])) > 0) {
            as.integer(Scenario$Forced_RPG_year[Scen_i])
          } else {
            NULL
          }
          if (!is.null(forced_rpg)) {
            Year_rpg <- forced_rpg
            cli_alert_info(
              "Forced_RPG_year detecte : utilisation du RPG {Year_rpg} pour l'annee de simulation {Year}."
            )
          } else if (Year > 2022) {
            Year_rpg <- 2022
            cli_alert_danger(
              "Téléchargement des données RPG pour l'année {Year} est supérieur à 2022. Utilisation de 2022 à la place."
            )
          } else if (Year < 2015) {
            Year_rpg <- 2015
            cli_alert_danger(
              "Téléchargement des données RPG pour l'année {Year} est inférieur à 2015. Utilisation de 2015 à la place."
            )
          } else {
            Year_rpg <- Year
          }
          if (toupper(Scenario$RPG_complete[Scen_i]) == "YES") {
            sources_RPG <- c("IGN", "ODR")
            if (Year_rpg < 2018) {
              cli_alert_danger(
                "Téléchargement des données RPG complétées pour l'année {Year_rpg} est inférieur à 2018. Le parcellaire obtenu ne sera pas complet."
              )
            } else if (Year_rpg > 2023) {
              cli_alert_danger(
                "Téléchargement des données RPG complétées pour l'année {Year_rpg} est supérieur à 2023. Le parcellaire obtenu ne sera pas complet."
              )
            }
          } else {
            sources_RPG <- c("IGN")
          }
          shapefile_data_pol <- tryCatch(
            RADIS::get_rpg_data(
              sf = shapefile_data_base_pol,
              year = Year_rpg,
              id = "id_poly",
              source = sources_RPG
            ),
            error = function(e) {
              msg <- conditionMessage(e)
              if (!grepl("502|503|Bad Gateway|Service Unavailable|404", msg)) stop(e)

              # Fallback 1 : WFS BBOX (contourne les erreurs 502 du filtre CQL)
              cli::cli_alert_warning(
                "Echec telechargement IGN ({msg}). Tentative WFS BBOX..."
              )
              rpg_bbox <- get_rpg_from_ign_wfs_bbox(
                sf_poly   = shapefile_data_base_pol,
                year      = Year_rpg,
                crs       = sf::st_crs(shapefile_data_base_pol),
                cache_dir = file.path(Working_path, "Cache_RPG")
              )
              if (!is.null(rpg_bbox) && nrow(rpg_bbox) > 0) return(rpg_bbox)

              # Fallback 2 : ODR (données partielles)
              if (!("ODR" %in% sources_RPG) && Year_rpg >= 2018 && Year_rpg <= 2023) {
                cli::cli_alert_warning(
                  "WFS BBOX sans resultats. Tentative avec ODR (donnees partielles)..."
                )
                RADIS::get_rpg_data(
                  sf = shapefile_data_base_pol,
                  year = Year_rpg,
                  id = "id_poly",
                  source = "ODR"
                )
              } else {
                stop(e)
              }
            }
          )

          #Correction of some wrong information code in the RPG download -> can add others if needed
          shapefile_data_pol$code_cultu <- toupper(
            shapefile_data_pol$code_cultu
          )
          odr_to_rpg_pol <- odr_to_rpg[
            odr_to_rpg$ODR %in% shapefile_data_pol$code_cultu,
          ]
          if (nrow(odr_to_rpg_pol) > 0) {
            for (i in 1:nrow(odr_to_rpg_pol)) {
              odr_name <- odr_to_rpg_pol[i, "ODR"]
              rpg_name <- odr_to_rpg_pol[i, "RPG"]
              shapefile_data_pol[
                shapefile_data_pol$code_cultu == odr_name,
                "code_cultu"
              ] <- rpg_name
            }
          }

          if (poly == dplyr::first(unique(shapefile_data_base$id_poly))) {
            shapefile_data <- shapefile_data_pol
          }
          if (poly != dplyr::first(unique(shapefile_data_base$id_poly))) {
            shapefile_data <- shapefile_data %>%
              dplyr::bind_rows(shapefile_data_pol)
          }
        }

        shapefile_data <- shapefile_data %>%
          sf::st_cast("POLYGON") %>% #correction of multipolygons attribution in the RPG
          dplyr::mutate(id_subpart = dplyr::row_number()) %>%
          sf::st_set_geometry("geometry") %>%
          dplyr::mutate(id_parcel = paste0(id_parcel, id_subpart)) %>%
          dplyr::select(-id_subpart) %>%
          dplyr::group_by(geometry) %>%
          dplyr::summarise(
            across(
              .cols = everything(),
              .fns = ~ dplyr::first(.x),
              .names = "{.col}"
            ),
            .groups = "drop"
          )
        shapefile_data[[COL$area]] <- sf::st_area(shapefile_data)
      } else if (retrieve_rpg == "External_plot_map") {
        shapefile_data <- shapefile_data_base[, c(
          COL$poly,
          COL$plot,
          COL$area,
          COL$crop
        )]
        #Correction of multilocation duplicated plots
        shapefile_data <- shapefile_data[!duplicated(shapefile_data), ]
      }

      #Identification of plot's attribution
      list_maille_avec_parcelles <- grille_safran[
        unique(
          unlist(sf::st_intersects(shapefile_data, grille_safran))
        ),
      ]$id_maille

      maille_avec_parcelles <- grille_safran %>%
        dplyr::filter(id_maille %in% list_maille_avec_parcelles)
      sf::st_write(
        maille_avec_parcelles,
        paste0(Input_run_path_Scen, "/Networkplots_", Year, ".shp"),
        delete_layer = TRUE
      )

      grid_all <- sf::st_make_grid(
        maille_avec_parcelles,
        cellsize = as.numeric(Scenario$Subgrid_size_km[Scen_i]) * 1000,
        square = TRUE
      ) #sub division of nets in 1km squares
      sous_maillage <- sf::st_intersection(maille_avec_parcelles, grid_all) %>%
        sf::st_as_sf() %>%
        dplyr::group_by(id_maille) %>%
        dplyr::mutate(
          sub_id = dplyr::row_number(),
          id_sous_maille = paste0(id_maille, "_", sub_id)
        ) %>%
        dplyr::ungroup()

      parcelles_avec_maille <- sf::st_join(
        sf::st_make_valid(shapefile_data),
        sf::st_make_valid(sous_maillage),
        left = FALSE,
        largest = TRUE
      )

      write.csv2(
        sf::st_drop_geometry(parcelles_avec_maille),
        paste0(Input_run_path_Scen, "/Link_plots_mailles_", Year, ".csv"),
        row.names = F
      )

      #visualisation graph
      Wanted_output$Wanted = toupper(Wanted_output$Wanted)
      if (
        Wanted_output[
          Wanted_output$Outputs == "Cartographic description of the territory",
          "Wanted"
        ] ==
          "YES"
      ) {
        if (
          Wanted_output[
            Wanted_output$Outputs == "Detailed graph by grid",
            "Wanted"
          ] ==
            "YES"
        ) {
          ggplot2::ggplot() +
            ggplot2::geom_sf(
              data = maille_avec_parcelles,
              fill = NA,
              color = "black"
            ) +
            ggplot2::geom_sf(data = shapefile_data, fill = NA) +
            ggplot2::geom_sf(
              data = parcelles_avec_maille,
              ggplot2::aes(
                fill = as.factor(id_maille),
                col = as.factor(id_maille)
              )
            ) +
            ggplot2::ggtitle(Year) +
            ggplot2::labs(fill = "Mailles") +
            ggplot2::guides(color = "none") +
            ggplot2::theme_minimal()
          ggsave(paste0(
            Input_run_path_Scen,
            "/Meteo/Plots_in_climaticalnetwork_",
            Year,
            ".png"
          ))
        }
        if (
          Wanted_output[
            Wanted_output$Outputs == "Detailed graph by subgrid",
            "Wanted"
          ] ==
            "YES"
        ) {
          ggplot2::ggplot() +
            ggplot2::geom_sf(
              data = sous_maillage,
              fill = NA,
              color = "black"
            ) +
            ggplot2::geom_sf(
              data = maille_avec_parcelles,
              fill = NA,
              color = "red"
            ) +
            ggplot2::geom_sf(data = shapefile_data, fill = NA) +
            ggplot2::geom_sf(
              data = parcelles_avec_maille,
              ggplot2::aes(
                fill = as.factor(id_sous_maille),
                col = as.factor(id_sous_maille)
              )
            ) +
            ggplot2::ggtitle(Year) +
            ggplot2::labs(fill = "Sous Mailles") +
            ggplot2::guides(color = "none") +
            ggplot2::theme_minimal() +
            theme(legend.position = "none")
          ggsave(paste0(
            Input_run_path_Scen,
            "/Meteo/Plots_in_climaticalnetwork_",
            Year,
            "_withsubnetwork.png"
          ))
        }
      }
      cli_alert_success("Network loaded")

      parcelles_avec_maille_y <- parcelles_avec_maille %>%
        dplyr::mutate(year = Year)

      #Add soil depth & AWC & texture
      if (Scenario$Sol_fixed_RADIS[Scen_i] == "No") {
        Sol_data_ext <- read_CAWET_data(
          Working_path = Working_path,
          file = Scenario$Soil_information_link[Scen_i]
        )

        Sol_data_ext$AWC_mean <- mean(
          c(Sol_data_ext$AWC_min, Sol_data_ext$AWC_max, Sol_data_ext$AWC_mean),
          na.rm = TRUE
        )
        Sol_data_ext$AWC_mean <- max(c(1, Sol_data_ext$AWC_mean))
        for (variable in c("Sand", "Arg")) {
          var_030 = ifelse(
            !is.na(Sol_data_ext[[paste0(variable, "_030")]]),
            Sol_data_ext[[paste0(variable, "_030")]],
            Sol_data_ext[[paste0(variable, "_030plus")]]
          )
          Sol_data_ext[[paste0(variable, "_perc_030")]] = ifelse(
            !is.na(var_030),
            var_030,
            33
          )

          var_030plus = ifelse(
            !is.na(Sol_data_ext[[paste0(variable, "_030plus")]]),
            Sol_data_ext[[paste0(variable, "_030plus")]],
            Sol_data_ext[[paste0(variable, "_030")]]
          )
          Sol_data_ext[[paste0(variable, "_perc_030plus")]] = ifelse(
            !is.na(var_030plus),
            var_030plus,
            33
          )
        }

        parcelles_avec_maille_y_sol <- parcelles_avec_maille_y %>%
          dplyr::mutate(
            soil_depth = Sol_data_ext$Soil_prof,
            AWC_mean = Sol_data_ext$AWC_mean,
            Sand_030 = Sol_data_ext$Sand_perc_030,
            Sand_30plus = Sol_data_ext$Sand_perc_30plus,
            Arg_030 = Sol_data_ext$Arg_perc_030,
            Arg_30plus = Sol_data_ext$Arg_perc_plus
          )
      } else if (Scenario$Sol_fixed_RADIS[Scen_i] == "No_forced_crops") {
        Sol_data_ext <- read_CAWET_data(
          Working_path = Working_path,
          file = Scenario$Soil_information_link[Scen_i]
        )
        if (Scenario$uses_textures[Scen_i]) {
          Sol_data_ext <- Sol_data_ext %>%
            dplyr::mutate(
              Lim_perc_030 = 100 - Arg_perc_030 - Sand_perc_030,
              Lim_perc_30plus = 100 - Arg_perc_30plus - Sand_perc_30plus
            ) %>%
            dplyr::rename(
              Sab_perc_030 = Sand_perc_030,
              Sab_perc_30plus = Sand_perc_30plus
            )
        }

        Sol_data_ext <- Sol_data_ext %>%
          dplyr::mutate(
            AWC_mean = ifelse(
              !is.na(AWC_mean),
              AWC_mean,
              ifelse(
                ((!is.na(AWC_min)) & (!is.na(AWC_max))),
                (AWC_min + AWC_max) / 2,
                ifelse(
                  !is.na(AWC_max),
                  AWC_max,
                  ifelse(!is.na(AWC_min), AWC_min, 1)
                )
              )
            )
          ) %>%
          dplyr::mutate(
            soil_depth = ifelse(!is.na(Soil_prof), Soil_prof, 1)
          )
        if (Scenario$uses_textures[Scen_i]) {
          Sol_data_ext <- Sol_data_ext %>%
            dplyr::mutate(
              Arg_mean = ifelse(
                soil_depth <= 0.3,
                ifelse(!is.na(Arg_perc_030), Arg_perc_030, 33),
                ifelse(
                  soil_depth >= 0.3,
                  ifelse(
                    !is.na(Arg_perc_030) & !is.na(Arg_perc_30plus),
                    ((Arg_perc_030 * 30) +
                      (Arg_perc_30plus * (soil_depth * 100 - 30))) /
                      (soil_depth * 100),
                    ifelse(
                      !is.na(Arg_perc_030),
                      Arg_perc_030,
                      ifelse(!is.na(Arg_perc_30plus), Arg_perc_30plus, 33)
                    )
                  )
                )
              )
            ) %>%
            dplyr::mutate(
              Lim_mean = ifelse(
                soil_depth <= 0.3,
                ifelse(!is.na(Lim_perc_030), Lim_perc_030, 33),
                ifelse(
                  soil_depth >= 0.3,
                  ifelse(
                    !is.na(Lim_perc_030) & !is.na(Lim_perc_30plus),
                    ((Lim_perc_030 * 30) +
                      (Lim_perc_30plus * (soil_depth * 100 - 30))) /
                      (soil_depth * 100),
                    ifelse(
                      !is.na(Lim_perc_030),
                      Lim_perc_030,
                      ifelse(!is.na(Lim_perc_30plus), Lim_perc_30plus, 33)
                    )
                  )
                )
              )
            ) %>%
            dplyr::mutate(
              Sab_mean = ifelse(
                soil_depth <= 0.3,
                ifelse(!is.na(Sab_perc_030), Sab_perc_030, 33),
                ifelse(
                  soil_depth >= 0.3,
                  ifelse(
                    !is.na(Sab_perc_030) & !is.na(Sab_perc_30plus),
                    ((Sab_perc_030 * 30) +
                      (Sab_perc_30plus * (soil_depth * 100 - 30))) /
                      (soil_depth * 100),
                    ifelse(
                      !is.na(Sab_perc_030),
                      Sab_perc_030,
                      ifelse(!is.na(Sab_perc_30plus), Sab_perc_30plus, 33)
                    )
                  )
                )
              )
            ) %>%
            dplyr::select(c(
              code_cultu,
              AWC_mean,
              soil_depth,
              Arg_mean,
              Lim_mean,
              Sab_mean
            ))
        } else {
          Sol_data_ext <- Sol_data_ext %>%
            select(code_cultu, AWC_mean, soil_depth)
        }

        parcelles_avec_maille_y_sol <- parcelles_avec_maille_y %>%
          full_join(Sol_data_ext) %>%
          dplyr::mutate(
            AWC_mean = ifelse(
              !is.na(AWC_mean),
              AWC_mean,
              (Sol_data_ext %>%
                dplyr::filter(code_cultu == 'Default'))$AWC_mean[1]
            )
          ) %>%
          dplyr::mutate(
            soil_depth = ifelse(
              !is.na(soil_depth),
              soil_depth,
              (Sol_data_ext %>%
                dplyr::filter(code_cultu == 'Default'))$soil_depth[1]
            )
          ) %>%
          dplyr::filter(!is.na(id_parcel))
      } else if (Scenario$Sol_fixed_RADIS[Scen_i] == "Yes_BDGSF") {
        # Cache des données sols BDGSF : évite ~10 min de re-téléchargement
        sol_cache_dir <- file.path(Working_path, "Cache_Sols")
        dir.create(sol_cache_dir, showWarnings = FALSE, recursive = TRUE)
        bb_sol <- sf::st_bbox(sf::st_transform(parcelles_avec_maille_y, 4326))
        sol_cache_key <- sprintf(
          "BDGSF_%d_%.2f_%.2f_%.2f_%.2f",
          Year, bb_sol["xmin"], bb_sol["ymin"], bb_sol["xmax"], bb_sol["ymax"]
        )
        depth_cache_file <- file.path(sol_cache_dir, paste0("Depth_", sol_cache_key, ".csv"))
        awc_cache_file   <- file.path(sol_cache_dir, paste0("AWC_",   sol_cache_key, ".csv"))

        if (file.exists(depth_cache_file) && file.exists(awc_cache_file)) {
          cli::cli_alert_info("Sols BDGSF: cache trouve -> {depth_cache_file}")
          Depth <- read.csv2(depth_cache_file, stringsAsFactors = FALSE)
          AWC   <- read.csv2(awc_cache_file,   stringsAsFactors = FALSE)
          # Forcer character pour éviter mismatch de type lors du join
          Depth$id_parcel <- as.character(Depth$id_parcel)
          AWC$id_parcel   <- as.character(AWC$id_parcel)
          cli_alert_success("Processing of {Year} for AWC ok")
        } else {
          Depth <- get_soil_depth(
            parcelles_avec_maille_y,
            source = "BDGSF"
          )
          AWC <- as.data.frame(RADIS::get_soil_awc(
            sf = parcelles_avec_maille_y,
            source = "BDGSF"
          )) %>%
            dplyr::select(-geometry) %>%
            dplyr::mutate(AWC_mean = soil_awc) %>%
            dplyr::select(c(id_parcel, AWC_mean))
          cli_alert_success("Processing of {Year} for AWC ok")
          write.csv2(Depth, depth_cache_file, row.names = FALSE)
          write.csv2(AWC,   awc_cache_file,   row.names = FALSE)
          cli::cli_alert_success("Sols BDGSF mis en cache: {sol_cache_dir}")
        }
      } else if (Scenario$Sol_fixed_RADIS[Scen_i] == "Yes_Infoetsol") {
        Depth <- get_soil_depth(
          parcelles_avec_maille_y,
          source = "BDGSF"
        )

        AWC <- as.data.frame(RADIS::get_soil_awc(
          sf = parcelles_avec_maille_y,
          source = "Info&Sols",
          with_coarse_elements = FALSE
        ))
        # Rename columns to match the expected format in process_soil_characteristic
        AWC <- AWC |>
          dplyr::rename_with(
            ~ sub("^awc_mm_", "awc_mm.", .x),
            dplyr::starts_with("awc_mm_")
          )
        AWC <- AWC %>%
          dplyr::select(-geometry) %>%
          dplyr::full_join(Depth)
        AWC$AWC_mean <- process_soil_characteristic(
          AWC,
          "awc_mm",
          sum
        )
        AWC <- dplyr::select(AWC, c(id_parcel, AWC_mean))
        cli_alert_success("Processing of {Year} for AWC ok")
      }

      if (substr(Scenario$Sol_fixed_RADIS[Scen_i], 1, 3) == "Yes") {
        parcelles_avec_maille_y_sol <- parcelles_avec_maille_y %>%
          dplyr::full_join(Depth, by = "id_parcel") %>%
          dplyr::full_join(AWC, by = "id_parcel") %>%
          dplyr::filter(!is.na(AWC_mean))

        if (Scenario$uses_textures[Scen_i]) {
          Texture <- get_soil_textures(parcelles_avec_maille_y, Depth)
          parcelles_avec_maille_y_sol <- parcelles_avec_maille_y_sol %>%
            dplyr::full_join(Texture, by = "id_parcel")
        }
      }

      #Adding a complementary external soil map for AWC
      if (!is.na(Scenario$Sol_carte_sup_RU_link_to_doss[Scen_i])) {
        parcelles_avec_maille_y_sol_2add <- parcelles_avec_maille_y_sol
        if (
          substr(Scenario$Sol_carte_sup_RU_link_to_doss[Scen_i], 10, 16) ==
            "exemple"
        ) {
          Sol_carte_path <- paste0(
            Working_path,
            substr(
              Scenario$Sol_carte_sup_RU_link_to_doss[Scen_i],
              49,
              nchar(Scenario$Sol_carte_sup_RU_link_to_doss[Scen_i])
            )
          )
        } else {
          Sol_carte_path <- Scenario$Sol_carte_sup_RU_link_to_doss[Scen_i]
        }

        Names_cartes_RU <- dir(Sol_carte_path)[which(
          stringr::str_detect(dir(Sol_carte_path), "RU_") &
            stringr::str_detect(dir(Sol_carte_path), ".tif")
        )]
        Decoupe_names_cartes_RU <- unlist(stringr::str_split(
          Names_cartes_RU,
          "_"
        ))
        Prof_cartes_RU <- Decoupe_names_cartes_RU[which(
          !is.na(as.numeric(Decoupe_names_cartes_RU))
        )]

        multip_to_mm_val <- Scenario$Sol_RU_unit[Scen_i]
        if (multip_to_mm_val == "mm") {
          multip_to_mm <- 1
        } else {
          if (multip_to_mm_val == "cm") {
            multip_to_mm <- 10
          } else {
            if (multip_to_mm_val == "dm") {
              multip_to_mm <- 100
            } else {
              if (multip_to_mm_val == "m") {
                multip_to_mm <- 1000
              } else {
                multip_to_mm <- 1
              }
            }
          }
        }

        Decoupe_names_allcartes_RU <- unlist(stringr::str_split(
          Names_cartes_RU,
          "_"
        ))
        Prof_allcartes_RU <- Decoupe_names_allcartes_RU[which(
          !is.na(as.numeric(Decoupe_names_allcartes_RU))
        )]

        # prof_add_map <- Names_cartes_RU[1]
        for (prof_add_map in Names_cartes_RU) {
          Decoupe_names_carte_RU <- unlist(stringr::str_split(
            prof_add_map,
            "_"
          ))
          Prof_carte_RU <- Decoupe_names_carte_RU[which(
            !is.na(as.numeric(Decoupe_names_carte_RU))
          )]
          message(
            'Traitement Profondeur ajoutee : ',
            Prof_carte_RU,
            'cm'
          )

          carte_sol_RU_complementaire <- terra::rast(paste0(
            Scenario$Sol_carte_sup_RU_link[Scen_i],
            "/",
            prof_add_map
          ))
          carte_sol_RU_complementaire <- terra::project(
            carte_sol_RU_complementaire,
            "EPSG:2154"
          )
          moyennes <- terra::extract(
            carte_sol_RU_complementaire,
            parcelles_avec_maille_y_sol_2add,
            fun = mean,
            na.rm = TRUE
          )
          parcelles_avec_maille_y_sol_2add$moyenne <- as.numeric(moyennes[,
            2
          ]) *
            multip_to_mm
          colnames(parcelles_avec_maille_y_sol_2add)[which(
            colnames(parcelles_avec_maille_y_sol_2add) == "moyenne"
          )] <- paste0('Added_AWC_mm_prof', Prof_carte_RU)
        }

        Prof_allcartes_RU <- as.numeric(Prof_allcartes_RU)
        AWC_gardees <- parcelles_avec_maille_y_sol_2add %>%
          dplyr::select(
            -c(
              (colnames(parcelles_avec_maille_y_sol_2add)[which(
                !stringr::str_detect(
                  colnames(parcelles_avec_maille_y_sol_2add),
                  "AWC"
                )
              )])
            )
          ) %>%
          dplyr::mutate(
            id_parcel = parcelles_avec_maille_y_sol_2add$id_parcel,
            soil_depth = parcelles_avec_maille_y_sol_2add$soil_depth * 100
          ) %>%
          dplyr::mutate(
            prof_proche = sapply(soil_depth, function(x) {
              Prof_allcartes_RU[which.min(abs(Prof_allcartes_RU - x))]
            })
          )
        AWC_gardees2 <- AWC_gardees %>% #Ne garde que la RU de la valeur la plus proche de la profondeur de sol identifiée
          tidyr::pivot_longer(
            starts_with("Added_AWC_mm_prof"),
            names_to = "profondeur_col",
            values_to = "valeur"
          ) %>%
          dplyr::mutate(
            profondeur = as.numeric(gsub(
              "Added_AWC_mm_prof",
              "",
              profondeur_col
            ))
          ) %>%
          dplyr::mutate(
            AWC_final = ifelse(
              (is.na(valeur) | is.nan(valeur)),
              AWC_mean,
              valeur
            )
          ) %>%
          dplyr::select(id_parcel, AWC_final)

        parcelles_avec_maille_y_sol$AWC_mean <- AWC_gardees2$AWC_final
      } #end adding a supplementary soil map for AWC

      #group information for each culture, polygones, maille and year
      if (Scenario$uses_textures[Scen_i]) {
        parcelle_avec_maille_y_grouped <- parcelles_avec_maille_y_sol %>%
          dplyr::mutate(surface_m2 = as.numeric(surface_m2)) %>%
          dplyr::group_by(
            code_cultu,
            id_poly,
            id_maille,
            id_sous_maille,
            year
          ) %>%
          dplyr::summarise(
            surface_totale_m2 = sum(surface_m2),
            numberplots = dplyr::n(),
            AWC_mean = round(weighted.mean(
              x = AWC_mean,
              w = surface_m2,
              na.rm = T
            )),
            depth_mean = round(weighted.mean(
              x = soil_depth,
              w = surface_m2,
              na.rm = T
            )),
            Arg_mean = round(weighted.mean(
              x = Arg_mean,
              w = surface_m2,
              na.rm = T
            )),
            Lim_mean = round(weighted.mean(
              x = Lim_mean,
              w = surface_m2,
              na.rm = T
            )),
            Sab_mean = round(weighted.mean(
              x = Sab_mean,
              w = surface_m2,
              na.rm = T
            )),
            geometry = sf::st_union(geometry), # multipolygones creation
            .groups = "drop"
          ) %>%
          sf::st_as_sf()
      } else {
        parcelle_avec_maille_y_grouped <- parcelles_avec_maille_y_sol %>%
          dplyr::mutate(surface_m2 = as.numeric(surface_m2)) %>%
          dplyr::group_by(
            code_cultu,
            id_poly,
            id_maille,
            id_sous_maille,
            year
          ) %>%
          dplyr::summarise(
            surface_totale_m2 = sum(surface_m2),
            numberplots = dplyr::n(),
            AWC_mean = round(weighted.mean(
              x = AWC_mean,
              w = surface_m2,
              na.rm = T
            )),
            depth_mean = round(weighted.mean(
              x = soil_depth,
              w = surface_m2,
              na.rm = T
            )),
            geometry = sf::st_union(geometry), # multipolygones creation
            .groups = "drop"
          ) %>%
          sf::st_as_sf()
      }

      parcelle_avec_maille_y_grouped_submail_extract <- parcelle_avec_maille_y_grouped[
        sf::st_geometry_type(parcelle_avec_maille_y_grouped) %in%
          c("POLYGON", "MULTIPOLYGON"),
      ]
      sf::st_write(
        parcelle_avec_maille_y_grouped_submail_extract,
        paste0(
          Input_run_path_Scen,
          "/Plots_all_RADIS_information_",
          Year,
          ".shp"
        ),
        delete_layer = TRUE
      )

      #Creation tableau pour relancer avec un territoire fantoche
      sf::sf_use_s2(FALSE) # s2 rejette certaines geometries unionisees ; GEOS suffit ici
      tabl_extract_terrfantoche <- parcelles_avec_maille_y %>%
        dplyr::group_by(code_cultu, id_poly) %>%
        dplyr::summarise(surf_parc = sum(surface_m2) / 10000) %>%
        sf::st_make_valid() %>% # correction geom invalides avant centroide
        sf::st_transform(4326) %>% # reprojection en WGS84 (pour lon/lat en degres)
        dplyr::mutate(
          centroide = sf::st_centroid(geometry), # Calcul du centroïde (ça marche pour polygon ET multipolygon)
          Longitude = sf::st_coordinates(centroide)[, 1],
          Latitude = sf::st_coordinates(centroide)[, 2]
        ) %>%
        dplyr::select(-centroide) %>%
        as.data.frame() %>%
        dplyr::select(-geometry)
      sf::sf_use_s2(TRUE)
      tabl_extract_terrfantoche <- tabl_extract_terrfantoche %>%
        dplyr::mutate(id_parcel = rownames(tabl_extract_terrfantoche)) %>%
        dplyr::select(
          id_parcel,
          id_poly,
          code_cultu,
          surf_parc,
          Latitude,
          Longitude
        )
      write.csv2(
        tabl_extract_terrfantoche,
        paste0(
          Input_run_path_Scen,
          "/Formodification/",
          "Territoire_repartition_cultures_",
          Year,
          ".csv"
        ),
        row.names = F
      )

      #Creation tableau pour relancer avec un sol fixé
      if (Scenario$uses_textures[Scen_i]) {
        tabl_extract_solcaract_parcult <- parcelles_avec_maille_y_sol %>%
          dplyr::group_by(code_cultu) %>%
          dplyr::summarise(
            Soil_prof = weighted.mean(
              soil_depth,
              as.numeric(surface_m2),
              na.rm = T
            ),
            AWC_min = min(AWC_mean),
            AWC_max = max(AWC_mean),
            AWC_mean = weighted.mean(
              AWC_mean,
              as.numeric(surface_m2),
              na.rm = T
            ),
            Sand_perc_030 = weighted.mean(
              Sab_mean,
              as.numeric(surface_m2),
              na.rm = T
            ),
            Sand_perc_30plus = weighted.mean(
              Sab_mean,
              as.numeric(surface_m2),
              na.rm = T
            ),
            Arg_perc_030 = weighted.mean(
              Arg_mean,
              as.numeric(surface_m2),
              na.rm = T
            ),
            Arg_perc_30plus = weighted.mean(
              Arg_mean,
              as.numeric(surface_m2),
              na.rm = T
            )
          )
        tabl_extract_solcaract_default <- data.frame(
          code_cultu = 'Default',
          Soil_prof = weighted.mean(
            parcelles_avec_maille_y_sol$soil_depth,
            as.numeric(parcelles_avec_maille_y_sol$surface_m2),
            na.rm = T
          ),
          AWC_min = min(parcelles_avec_maille_y_sol$AWC_mean),
          AWC_max = max(parcelles_avec_maille_y_sol$AWC_mean),
          AWC_mean = weighted.mean(
            parcelles_avec_maille_y_sol$AWC_mean,
            as.numeric(parcelles_avec_maille_y_sol$surface_m2),
            na.rm = T
          ),
          Sand_perc_030 = weighted.mean(
            parcelles_avec_maille_y_sol$Sab_mean,
            as.numeric(parcelles_avec_maille_y_sol$surface_m2),
            na.rm = T
          ),
          Sand_perc_30plus = weighted.mean(
            parcelles_avec_maille_y_sol$Sab_mean,
            as.numeric(parcelles_avec_maille_y_sol$surface_m2),
            na.rm = T
          ),
          Arg_perc_030 = weighted.mean(
            parcelles_avec_maille_y_sol$Arg_mean,
            as.numeric(parcelles_avec_maille_y_sol$surface_m2),
            na.rm = T
          ),
          Arg_perc_30plus = weighted.mean(
            parcelles_avec_maille_y_sol$Arg_mean,
            as.numeric(parcelles_avec_maille_y_sol$surface_m2),
            na.rm = T
          )
        )
      } else {
        tabl_extract_solcaract_parcult <- parcelles_avec_maille_y_sol %>%
          dplyr::group_by(code_cultu) %>%
          dplyr::summarise(
            Soil_prof = weighted.mean(
              soil_depth,
              as.numeric(surface_m2),
              na.rm = T
            ),
            AWC_min = min(AWC_mean),
            AWC_max = max(AWC_mean),
            AWC_mean = weighted.mean(
              AWC_mean,
              as.numeric(surface_m2),
              na.rm = T
            )
          )
        tabl_extract_solcaract_default <- data.frame(
          code_cultu = 'Default',
          Soil_prof = weighted.mean(
            parcelles_avec_maille_y_sol$soil_depth,
            as.numeric(parcelles_avec_maille_y_sol$surface_m2),
            na.rm = T
          ),
          AWC_min = min(parcelles_avec_maille_y_sol$AWC_mean),
          AWC_max = max(parcelles_avec_maille_y_sol$AWC_mean),
          AWC_mean = weighted.mean(
            parcelles_avec_maille_y_sol$AWC_mean,
            as.numeric(parcelles_avec_maille_y_sol$surface_m2),
            na.rm = T
          )
        )
      }
      tabl_extract_solcaract <- tabl_extract_solcaract_default %>%
        bind_rows(tabl_extract_solcaract_parcult)
      if ('geometry' %in% colnames(tabl_extract_solcaract)) {
        tabl_extract_solcaract <- data.frame(tabl_extract_solcaract) %>%
          dplyr::select(-geometry)
      }
      write.csv2(
        tabl_extract_solcaract,
        paste0(
          Input_run_path_Scen,
          "/Formodification/",
          "Soil_average_",
          Year,
          ".csv"
        ),
        row.names = F
      )

      #Saving sf object with plots and all information
      if (Year == Scenario$First_year_simulation[Scen_i]) {
        parcelle_avec_maille_grouped <- parcelle_avec_maille_y_grouped
      } else {
        parcelle_avec_maille_grouped <- parcelle_avec_maille_grouped %>%
          dplyr::bind_rows(parcelle_avec_maille_y_grouped)
      }
      if (Year == Scenario$Last_year_simulation[Scen_i]) {
        parcelle_avec_maille_grouped <- parcelle_avec_maille_grouped %>%
          dplyr::filter(!is.na(id_sous_maille))
        parcelle_avec_maille_y_grouped_extract <- parcelle_avec_maille_grouped[
          sf::st_geometry_type(parcelle_avec_maille_grouped) %in%
            c("POLYGON", "MULTIPOLYGON"),
        ]
        sf::st_write(
          parcelle_avec_maille_y_grouped_extract,
          paste0(Input_run_path_Scen, "/Crops_all_RADIS_information.shp"),
          delete_layer = TRUE
        )
      }
    } #end loop years

    #Meteo acquisition
    if (
      Scenario$Climat_data_unique_or_network[Scen_i] == "Unique" |
        Scenario$Climat_data_source[Scen_i] == "Forced_otherclimaticdata"
    ) {
      #Chargement données météo
      Meteo_charged <- read_CAWET_data(
        Working_path = Working_path,
        file = Scenario$Climat_link_fixe[Scen_i]
      )

      #Renommage colonnes
      colnames(Meteo_charged)[which(
        colnames(Meteo_charged) == Scenario$Colname_Date[Scen_i]
      )] <- "time"
      colnames(Meteo_charged)[which(
        colnames(Meteo_charged) == Scenario$Colname_ET0[Scen_i]
      )] <- "ETP_Q"
      colnames(Meteo_charged)[which(
        colnames(Meteo_charged) == Scenario$Colname_Pluie[Scen_i]
      )] <- "PRELIQ_Q"
      Meteo_charged <- Meteo_charged %>%
        dplyr::mutate(time = as.Date(time, format = "%d_%m_%Y")) %>%
        dplyr::mutate(DATE = as.Date(time, format = "%d_%m_%Y"))
      #correction des meteo manquantes -> Pluis mise a zero, ETO mise à 1
      Meteo_charged <- Meteo_charged %>%
        dplyr::mutate(PRELIQ_Q = ifelse(!is.na(PRELIQ_Q), PRELIQ_Q, 0)) %>%
        dplyr::mutate(ETP_Q = ifelse(!is.na(ETP_Q), ETP_Q, 0))
      #Saving meteo
      write.csv2(
        Meteo_charged,
        paste0(Input_run_path_Scen, "/Meteo/Meteo.csv"),
        row.names = F
      )
    }

    if (Scenario$Climat_data_unique_or_network[Scen_i] == "Network") {
      #recuperation des donnees climatiques
      cli_alert_info("Climat data recuperation")
      if (Scenario$Climat_data_source[Scen_i] %in% c("Safran", "Drias")) {
        #maille <- as.numeric(unique(parcelle_avec_maille_grouped$id_maille))[1]
        for (maille in as.numeric(unique(
          parcelle_avec_maille_grouped$id_maille
        ))) {
          message("Maille : ", maille)
          Maille_i <- grille_safran %>%
            dplyr::filter(as.numeric(id_maille) == maille)

          if (Scenario$Climat_data_source[Scen_i] == "Safran") {
            climate_mi <- RADIS::get_sim2_daily(
              LAMBX__greater = Maille_i$lambx * 100,
              LAMBX__less = Maille_i$lambx * 100,
              LAMBY__greater = Maille_i$lamby * 100,
              LAMBY__less = Maille_i$lamby * 100,
              DATE__greater = paste0(
                Scenario$First_year_simulation[Scen_i],
                "-01-01"
              ),
              DATE__less = paste0(
                Scenario$Last_year_simulation[Scen_i],
                "-12-31"
              ),
              api_format = "csv"
            )
            climate_mi_df <- as.data.frame(climate_mi) %>%
              dplyr::mutate(DATE = as.Date(DATE))
            if (nrow(climate_mi_df) == 0) {
              # L'API G-EAU ne couvre que 1958-2019. Fallback vers data.gouv.fr (Météo-France).
              yr_from <- Scenario$First_year_simulation[Scen_i]
              yr_to   <- Scenario$Last_year_simulation[Scen_i]
              cli::cli_alert_warning(paste0(
                "API SAFRAN (G-EAU) vide pour la maille ", maille,
                " (", yr_from, "-", yr_to, "). ",
                "Basculement sur data.gouv.fr (Meteo-France)..."
              ))
              cache_sim2 <- file.path(Working_path, "Cache_SIM2")
              climate_mi_df <- get_sim2_from_datagouv(
                lambx     = Maille_i$lambx,
                lamby     = Maille_i$lamby,
                year_from = yr_from,
                year_to   = yr_to,
                cache_dir = cache_sim2
              )
              if (nrow(climate_mi_df) == 0) {
                stop(paste0(
                  "Aucune donnee SIM2 trouvee pour la maille ", maille,
                  " sur la periode ", yr_from, "-", yr_to,
                  " (G-EAU API et data.gouv.fr consultes)."
                ))
              }
              cli::cli_alert_success(paste0(
                "Donnees SIM2 recuperees depuis data.gouv.fr : ",
                nrow(climate_mi_df), " lignes."
              ))
            }
          }

          if (Scenario$Climat_data_source[Scen_i] == "Drias") {
            param_clim_drias <- read_CAWET_data(
              Working_path = Working_path,
              file = Scenario$Climat_link_Drias[Scen_i]
            )

            param_clim_drias_scen <- param_clim_drias %>%
              dplyr::filter(Code_param == Scenario$Code_param_Drias[Scen_i])
            Scenario_clim <- Scenario$RCP_scenario[Scen_i]
            Code_Scenario_clim <- paste0("rcp", gsub("\\.", "", Scenario_clim))
            climate_mi <- RADIS::get_drias_daily(
              LAMBX__greater = Maille_i$lambx * 100,
              LAMBX__less = Maille_i$lambx * 100,
              LAMBY__greater = Maille_i$lamby * 100,
              LAMBY__less = Maille_i$lamby * 100,
              DATE__greater = paste0(
                Scenario$First_year_simulation[Scen_i],
                "-01-01"
              ),
              DATE__less = paste0(
                Scenario$Last_year_simulation[Scen_i],
                "-12-31"
              ),
              GCM = param_clim_drias_scen$GCM,
              RCM = param_clim_drias_scen$RCM,
              BC = param_clim_drias_scen$BC,
              RCP = Code_Scenario_clim,
              ETP = param_clim_drias_scen$ETP,
              fields = c(
                "evspsblpot",
                "huss",
                "prsn",
                "prtot",
                "rlds",
                "rsds",
                "sfcWind",
                "tas",
                "tasmax",
                "tasmin"
              )
            )
            climate_mi_df <- as.data.frame(climate_mi) %>%
              dplyr::group_by(time) %>%
              dplyr::summarise(
                ETP_Q = mean(evspsblpot, na.rm = T),
                Q_Q = mean(huss, na.rm = T),
                PRENEI_Q = mean(prsn, na.rm = T),
                PRELIQ_Q = mean(prtot, na.rm = T),
                rlds = mean(rlds, na.rm = T),
                rsds = mean(rsds, na.rm = T),
                FF_Q = mean(sfcWind, na.rm = T),
                T_Q = mean(tas, na.rm = T),
                TSUP_H_Q = mean(tasmax, na.rm = T),
                TINF_H_Q = mean(tasmin, na.rm = T)
              ) %>%
              dplyr::mutate(DATE = as.Date(time))
          }
          write.csv2(
            climate_mi_df,
            paste0(Input_run_path_Scen, "/Meteo/Meteo_maille", maille, ".csv"),
            row.names = F
          )
        }
      }

      # Mode Network + données externes : on duplique le CSV externe pour chaque maille
      if (Scenario$Climat_data_source[Scen_i] == "Forced_otherclimaticdata") {
        cli_alert_info(
          "Mode Network + Forced_otherclimaticdata : application de la même météo externe à chaque maille"
        )
        for (maille in as.numeric(unique(
          parcelle_avec_maille_grouped$id_maille
        ))) {
          write.csv2(
            Meteo_charged,
            paste0(Input_run_path_Scen, "/Meteo/Meteo_maille", maille, ".csv"),
            row.names = FALSE
          )
        }
      }
    }

    Surf_table <- Date_Table
    Prof_table <- Date_Table
    AWC_table <- Date_Table
    if (Scenario$uses_textures[Scen_i]) {
      Arg_table <- Date_Table
      Lim_table <- Date_Table
      Sab_table <- Date_Table
    }
    cults <- unique(parcelle_avec_maille_grouped$code_cultu)
    progress_cults <- cli_progress_bar(
      "Saving culture information",
      total = length(cults),
      clear = FALSE
    )
    for (Cult in cults) {
      cli_alert_info("Saving culture {Cult}")
      cli_progress_update(id = progress_cults)
      Info <- parcelle_avec_maille_grouped %>% dplyr::filter(code_cultu == Cult)

      for (pol in unique(Info$id_poly)) {
        Info_pol <- Info %>% dplyr::filter(id_poly == pol)

        for (ssmail in unique(Info_pol$id_sous_maille)) {
          Info_pol_ssmail <- Info_pol %>%
            dplyr::filter(id_sous_maille == ssmail)

          # Creation Surfaces linear table
          Surf_inf <- Info_pol_ssmail %>%
            dplyr::select(year, surface_totale_m2) %>%
            as.data.frame() %>%
            dplyr::select(-geometry)
          Surf_table <- Surf_table %>%
            dplyr::mutate(year = lubridate::year(date)) %>%
            dplyr::full_join(Surf_inf, by = dplyr::join_by(year))
          colnames(Surf_table)[which(
            colnames(Surf_table) == "surface_totale_m2"
          )] <- paste0(Cult, '_poly', pol, '_ssmail', ssmail)
          # Creation depth linear table
          Prof_inf <- Info_pol_ssmail %>%
            dplyr::select(year, depth_mean) %>%
            as.data.frame() %>%
            dplyr::select(-geometry)
          Prof_table <- Prof_table %>%
            dplyr::mutate(year = lubridate::year(date)) %>%
            dplyr::full_join(Prof_inf, by = dplyr::join_by(year))
          colnames(Prof_table)[which(
            colnames(Prof_table) == "depth_mean"
          )] <- paste0(Cult, '_poly', pol, '_ssmail', ssmail)
          # Creation AWC linear table
          AWC_inf <- Info_pol_ssmail %>%
            dplyr::select(year, AWC_mean) %>%
            as.data.frame() %>%
            dplyr::select(-geometry)
          AWC_table <- AWC_table %>%
            dplyr::mutate(year = lubridate::year(date)) %>%
            dplyr::full_join(AWC_inf, by = dplyr::join_by(year))
          colnames(AWC_table)[which(
            colnames(AWC_table) == "AWC_mean"
          )] <- paste0(Cult, '_poly', pol, '_ssmail', ssmail)
          if (Scenario$uses_textures[Scen_i]) {
            # Creation Arg linear table
            Arg_inf <- Info_pol_ssmail %>%
              dplyr::select(year, Arg_mean) %>%
              as.data.frame() %>%
              dplyr::select(-geometry)
            Arg_mean_table <- Arg_mean_table %>%
              dplyr::mutate(year = lubridate::year(date)) %>%
              dplyr::full_join(Arg_inf, by = dplyr::join_by(year))
            colnames(Arg_mean_table)[which(
              colnames(Arg_mean_table) == "Arg_mean"
            )] <- paste0(Cult, '_poly', pol, '_ssmail', ssmail)
            # Creation Lim linear table
            Lim_inf <- Info_pol_ssmail %>%
              dplyr::select(year, Lim_mean) %>%
              as.data.frame() %>%
              dplyr::select(-geometry)
            Lim_mean_table <- Lim_mean_table %>%
              dplyr::mutate(year = lubridate::year(date)) %>%
              dplyr::full_join(Lim_inf, by = dplyr::join_by(year))
            colnames(Lim_mean_table)[which(
              colnames(Lim_mean_table) == "Lim_mean"
            )] <- paste0(Cult, '_poly', pol, '_ssmail', ssmail)
            # Creation Lim linear table
            Sab_inf <- Info_pol_ssmail %>%
              dplyr::select(year, Sab_mean) %>%
              as.data.frame() %>%
              dplyr::select(-geometry)
            Sab_mean_table <- Sab_mean_table %>%
              dplyr::mutate(year = lubridate::year(date)) %>%
              dplyr::full_join(Sab_inf, by = dplyr::join_by(year))
            colnames(Sab_mean_table)[which(
              colnames(Sab_mean_table) == "Sab_mean"
            )] <- paste0(Cult, '_poly', pol, '_ssmail', ssmail)
            #Creation irrigation periodes linear table
            #A ajouter
            #Other pratices
            #A ajouter
          }
        }
      }
    }
    cli_progress_done(id = progress_cults)
    #end of creation linear date table

    #saving tables
    write.csv2(
      Surf_table,
      paste0(Input_run_path_Scen, '/Surf_table_', Scen, '.csv'),
      row.names = F
    )
    write.csv2(
      Prof_table,
      paste0(Input_run_path_Scen, '/Prof_table_', Scen, '.csv'),
      row.names = F
    )
    write.csv2(
      AWC_table,
      paste0(Input_run_path_Scen, '/AWC_table_', Scen, '.csv'),
      row.names = F
    )
    if (Scenario$uses_textures[Scen_i]) {
      write.csv2(
        Arg_mean_table,
        paste0(Input_run_path_Scen, '/Arg_mean_table_', Scen, '.csv'),
        row.names = F
      )
      write.csv2(
        Lim_mean_table,
        paste0(Input_run_path_Scen, '/Lim_mean_table_', Scen, '.csv'),
        row.names = F
      )
      write.csv2(
        Sab_mean_table,
        paste0(Input_run_path_Scen, '/Sab_mean_table_', Scen, '.csv'),
        row.names = F
      )
    }

    if (retrieve_rpg == "Non_existant_plots") {
      centre <- sf::st_centroid(parcelles_avec_maille_y)
      if (
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Cartographic description of the territory"
        )]) ==
          "YES"
      ) {
        ggplot2::ggplot() +
          ggplot2::geom_sf(
            data = shapefile_data_forillustration,
            ggplot2::aes(fill = code_cultu),
            color = "black"
          ) +
          ggplot2::geom_sf(
            data = centre,
            ggplot2::aes(shape = id_maille),
            size = 5
          ) +
          ggplot2::theme_minimal()
        ggsave(paste0(
          Input_run_path_Scen,
          "/Meteo/Illustration_plots_maille.png"
        ))
      }
      parcelles_avec_maille_y_join <- parcelles_avec_maille_y %>%
        as.data.frame() %>%
        dplyr::select(-geometry)
      shapefile_data_forillustration <- shapefile_data_forillustration %>%
        dplyr::full_join(parcelles_avec_maille_y_join)
      sf::write_sf(
        shapefile_data_forillustration,
        paste0(
          Input_run_path_Scen,
          "/Plots_location_notcenteredforillustration.shp"
        )
      )
    }
  } #end scenarii loop
  cli_alert_success(
    "run_script1 - Preparation des donnees dans le dossier: {Path_Run} -  execute avec succes"
  )
  return(cfgRun)
} #end function
