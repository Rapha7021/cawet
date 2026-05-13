#' Ordonnanceur verification function for CAWET
#'
#' This function verifies the content of the ordonnanceur (scenario file) for CAWET. It checks for the existence and correctness of various parameters and files specified in the ordonnanceur.
#'
#' @inheritParams run
#' @return None. The function stops execution and raises an error if any verification fails.
#' @export
#'
Verif_ordonnanceur_CAWET <- function(cfgRun) {
  Scenario <- cfgRun$Scenario
  Working_path <- cfgRun$Working_path

  cli_alert_info(glue::glue(
    "Vérification de la configuration et des chemins dans le fichier ordonnanceur:"
  ))

  # Helper function to check file existence
  check_file_exists <- function(
    path,
    Working_path,
    param_name,
    Scen_i,
    error_msg = NULL
  ) {
    resolved_path <- FpCAWET(Working_path, path)
    if (!file.exists(resolved_path)) {
      if (is.null(error_msg)) {
        error_msg <- sprintf(
          "Warning, the location of the %s file does not exist (%s) for Scenario %d in the ordonnanceur",
          param_name,
          param_name,
          Scen_i
        )
      }
      stop(error_msg)
    }
    resolved_path
  }

  # Helper function to check file extension
  check_file_extension <- function(
    path,
    allowed_exts,
    param_name,
    Scen_i,
    context = ""
  ) {
    ext <- substr(path, nchar(path) - 3, nchar(path))
    if (!ext %in% allowed_exts) {
      stop(sprintf(
        "Warning, %s file is not in a coherent extent (%s) for Scenario %d in the ordonnanceur. %s",
        context,
        param_name,
        Scen_i,
        sprintf(
          "Please select a %s file",
          paste(allowed_exts, collapse = " or ")
        )
      ))
    }
  }

  # Helper function to load data file (csv or xlsx)
  load_data_file <- function(path, Working_path) {
    resolved_path <- FpCAWET(Working_path, path)
    ext <- substr(resolved_path, nchar(resolved_path) - 3, nchar(resolved_path))

    if (ext == ".csv") {
      read.csv2(resolved_path)
    } else if (ext == "xlsx") {
      openxlsx::read.xlsx(resolved_path)
    } else {
      stop("Unsupported file format. Use .csv or .xlsx")
    }
  }

  # Helper function to check column exists
  check_column_exists <- function(
    df,
    colname,
    param_name,
    Scen_i,
    file_desc = "provided file"
  ) {
    if (!is.na(colname) && !colname %in% colnames(df)) {
      stop(sprintf(
        "Warning, the name %s is not a colname in the %s (%s) for Scenario %d in the ordonnanceur",
        colname,
        file_desc,
        param_name,
        Scen_i
      ))
    }
  }

  # Verification of the content of the scenario/ordonnanceur
  if (length(Scenario$Scenario) != length(unique(Scenario$Scenario))) {
    stop("Warning, different scenarii have the same name (Scenario)")
  }
  options(cli.progress_show_after = 0)
  cli_progress_bar(
    "Verifying ordonnanceur scenarii...",
    total = nrow(Scenario),
    type = "tasks",
    clear = FALSE
  )
  for (Scen_i in as.numeric(rownames(Scenario))) {
    cli_progress_update()

    # Scenario name
    if (is.na(Scenario$Scenario[Scen_i])) {
      stop(sprintf(
        "Warning, missing name for Scenario %d in the ordonnanceur",
        Scen_i
      ))
    }

    # Plots_RADIS_RPG
    if (
      !Scenario$Plots_RADIS_RPG[Scen_i] %in%
        c("Yes", "External_plot_map", "Non_existant_plots")
    ) {
      stop(sprintf(
        "Warning, missing information: type of location (Plots_RADIS_RPG) for Scenario %d in the ordonnanceur",
        Scen_i
      ))
    }

    # Shp_link
    shp_link_cas <- FpCAWET(
      Working_path = Working_path,
      path = Scenario$Shp_link[Scen_i]
    )
    shp_path <- check_file_exists(
      shp_link_cas,
      Working_path,
      "Shp_link",
      Scen_i,
      "location file"
    )

    # Check file extension based on plot type
    if (Scenario$Plots_RADIS_RPG[Scen_i] == "Non_existant_plots") {
      check_file_extension(
        shp_path,
        c(".csv", "xlsx"),
        "Shp_link",
        Scen_i,
        "location"
      )
    } else if (
      Scenario$Plots_RADIS_RPG[Scen_i] %in% c("Yes", "External_plot_map")
    ) {
      check_file_extension(
        shp_path,
        c("gpkg", ".shp"),
        "Shp_link",
        Scen_i,
        "location"
      )
    }
    if (Scenario$Plots_RADIS_RPG[Scen_i] == "Yes") {
      if (!toupper(Scenario$RPG_complete[Scen_i]) %in% c("YES", "NO")) {
        stop(sprintf(
          "Warning, missing information: please select 'Yes' or 'No' in RPG_complete colomne in for Scenario %d in the ordonnanceur",
          Scen_i
        ))
      }
    }

    # First_year_simulation, Last_year_simulation
    if (
      !is.numeric(Scenario$First_year_simulation[Scen_i]) ||
        is.na(Scenario$First_year_simulation[Scen_i])
    ) {
      stop(sprintf(
        "Warning, missing information: first simulation year (First_year_simulation) for Scenario %d in the ordonnanceur",
        Scen_i
      ))
    }
    if (
      !is.numeric(Scenario$Last_year_simulation[Scen_i]) ||
        is.na(Scenario$Last_year_simulation[Scen_i])
    ) {
      stop(sprintf(
        "Warning, missing information: last simulation year (Last_year_simulation) for Scenario %d in the ordonnanceur",
        Scen_i
      ))
    }

    # Verification of the shapefile database
    if (Scenario$Plots_RADIS_RPG[Scen_i] %in% c("Yes", "External_plot_map")) {
      shapefile_data_base_verif <- st_read(shp_path, quiet = TRUE)

      # Check geometry type
      if (
        !all(
          sf::st_geometry_type(shapefile_data_base_verif) %in%
            c("POLYGON", "MULTIPOLYGON")
        )
      ) {
        stop(sprintf(
          "Warning, the provided shapefile (Shp_link) is not only composed of polygons or multipolygons objects for Scenario %d in the ordonnanceur",
          Scen_i
        ))
      }

      # Check projection
      if (is.na(sf::st_crs(shapefile_data_base_verif))) {
        stop(sprintf(
          "Warning, the provided shapefile (Shp_link) has no defined projection for Scenario %d in the ordonnanceur",
          Scen_i
        ))
      }
    } else {
      shapefile_data_base_verif <- load_data_file(shp_link_cas, Working_path)
    }

    # Column names verification
    check_column_exists(
      shapefile_data_base_verif,
      Scenario$Colname_idpoly[Scen_i],
      "Colname_idpoly",
      Scen_i
    )
    check_column_exists(
      shapefile_data_base_verif,
      Scenario$Colname_idplots[Scen_i],
      "Colname_idplots",
      Scen_i
    )

    # Colname_codecultureRPG
    if (
      Scenario$Colname_codecultureRPG[Scen_i] %in%
        c("Non_existant_plots", "External_plot_map") &&
        Scenario$Colname_codecultureRPG[Scen_i] %in%
          colnames(shapefile_data_base_verif)
    ) {
      stop(sprintf(
        "Warning, missing information: the name %s is not a colname in the provided file (Colname_codecultureRPG) for Scenario %d in the ordonnanceur",
        Scenario$Colname_codecultureRPG[Scen_i],
        Scen_i
      ))
    }

    # Climat_data_unique_or_network
    if (
      !Scenario$Climat_data_unique_or_network[Scen_i] %in%
        c("Network", "Unique")
    ) {
      stop(sprintf(
        "Warning, missing information: type of climatic data (Climat_data_unique_or_network) for Scenario %d in the ordonnanceur",
        Scen_i
      ))
    }

    # Climat_data_source
    if (
      !Scenario$Climat_data_source[Scen_i] %in%
        c("Forced_otherclimaticdata", "Safran", "Drias")
    ) {
      stop(sprintf(
        "Warning, missing or incorrect information: origine of climatic data (Climat_data_source) for Scenario %d in the ordonnanceur",
        Scen_i
      ))
    }

    # Drias climate data
    if (Scenario$Climat_data_source[Scen_i] == "Drias") {
      drias_path <- check_file_exists(
        Scenario$Climat_link_Drias[Scen_i],
        Working_path,
        "Climat_link_Drias",
        Scen_i,
        "climatic data parameter file"
      )

      Param_Drias_verif <- load_data_file(
        Scenario$Climat_link_Drias[Scen_i],
        Working_path
      )

      if (
        !Scenario$Code_param_Drias[Scen_i] %in% Param_Drias_verif$Code_param
      ) {
        stop(sprintf(
          "Warning, the line Code_param: %s is not in the provided param climatic file (Code_param_Drias) for Scenario %d in the ordonnanceur",
          Scenario$Code_param_Drias[Scen_i],
          Scen_i
        ))
      }

      if (is.na(Scenario$RCP_scenario[Scen_i])) {
        stop(sprintf(
          "Warning, RCP scenario selected: %s is not recognized (RCP_scenario) for Scenario %d in the ordonnanceur",
          Scenario$RCP_scenario[Scen_i],
          Scen_i
        ))
      }
    }

    # Fixed climate data
    if (Scenario$Climat_data_source[Scen_i] == "Forced_otherclimaticdata") {
      climat_path <- check_file_exists(
        Scenario$Climat_link_fixe[Scen_i],
        Working_path,
        "Climat_link_fixe",
        Scen_i,
        "climatic data file"
      )

      Meteo_charged_verif <- load_data_file(
        Scenario$Climat_link_fixe[Scen_i],
        Working_path
      )

      check_column_exists(
        Meteo_charged_verif,
        Scenario$Colname_Date[Scen_i],
        "Colname_Date",
        Scen_i,
        "climatic file"
      )
      check_column_exists(
        Meteo_charged_verif,
        Scenario$Colname_ET0[Scen_i],
        "Colname_ET0",
        Scen_i,
        "climatic file"
      )
      check_column_exists(
        Meteo_charged_verif,
        Scenario$Colname_Pluie[Scen_i],
        "Colname_Pluie",
        Scen_i,
        "climatic file"
      )
    }

    # Sol_fixed_RADIS
    if (
      !Scenario$Sol_fixed_RADIS[Scen_i] %in%
        c("Yes_BDGSF", "Yes_Infoetsol", "No", "No_forced_crops")
    ) {
      stop(sprintf(
        "Warning, Invalid information: origine of soil information (Sol_fixed_RADIS) for Scenario %d in the ordonnanceur",
        Scen_i
      ))
    }

    # Soil information
    if (
      Scenario$Sol_fixed_RADIS[Scen_i] %in%
        c("No", "No_forced_crops") &&
        !is.na(Scenario$Soil_information_link[Scen_i])
    ) {
      soil_path <- check_file_exists(
        Scenario$Soil_information_link[Scen_i],
        Working_path,
        "Soil_information_link",
        Scen_i,
        "soil data file"
      )

      if (Scenario$Sol_fixed_RADIS[Scen_i] == "No_forced_crops") {
        Soil_path_charged_verif <- load_data_file(soil_path, Working_path)

        if (!"code_cultu" %in% colnames(Soil_path_charged_verif)) {
          stop(sprintf(
            "Warning, missing column 'code_cultu' in the soil forced table (Soil_information_link) for Scenario %d in the ordonnanceur",
            Scen_i
          ))
        }

        if (!"Default" %in% Soil_path_charged_verif$code_cultu) {
          stop(sprintf(
            "Warning, missing 'Default' value in the soil forced table (Soil_information_link) for Scenario %d in the ordonnanceur",
            Scen_i
          ))
        }
      }
    }

    # Sol_carte_sup_RU_link_to_doss
    if (!is.na(Scenario$Sol_carte_sup_RU_link_to_doss[Scen_i])) {
      Sol_carte_path <- check_file_exists(
        Scenario$Sol_carte_sup_RU_link_to_doss[Scen_i],
        Working_path,
        "Sol_carte_sup_RU_link_to_doss",
        Scen_i,
        "soil additional map file"
      )

      Names_cartes_RU <- dir(Sol_carte_path)[which(
        stringr::str_detect(dir(Sol_carte_path), "RU_") &
          stringr::str_detect(dir(Sol_carte_path), ".tif")
      )]

      if (length(Names_cartes_RU) < 1) {
        stop(sprintf(
          "Warning, no correct file in the additional map file selected for Scenario %d in the ordonnanceur. Be careful to provide .tiff file/s with conforme name (RU_[depth]_.tiff)",
          Scen_i
        ))
      }

      if (!Scenario$Sol_RU_unit[Scen_i] %in% c("mm", "cm", "dm", "m")) {
        stop(sprintf(
          "Warning, missing or non conventional information of RU unit for the provided additional map (Sol_RU_unit) for Scenario %d in the ordonnanceur",
          Scen_i
        ))
      }
    }

    # Pratique_irrigation_table_percentages
    if (is.na(Scenario$Pratique_irrigation_table_percentages[Scen_i])) {
      stop(sprintf(
        "Warning the location of the information table for percentages of irrigation does not exist (Pratique_irrigation_table_percentages) for Scenario %d in the ordonnanceur",
        Scen_i
      ))
    }

    Irr_perc_path <- check_file_exists(
      Scenario$Pratique_irrigation_table_percentages[Scen_i],
      Working_path,
      "Pratique_irrigation_table_percentages",
      Scen_i,
      "information table for percentages of irrigation"
    )

    Irr_perc_verif <- load_data_file(
      Scenario$Pratique_irrigation_table_percentages[Scen_i],
      Working_path
    )

    # Pratique_irrigation_param
    if (is.na(Scenario$Pratique_irrigation_param[Scen_i])) {
      stop(sprintf(
        "Warning, the location of the information table for irrigation parameters does not exist (Pratique_irrigation_param) for Scenario %d in the ordonnanceur",
        Scen_i
      ))
    }

    Irr_param_path <- check_file_exists(
      Scenario$Pratique_irrigation_param[Scen_i],
      Working_path,
      "Pratique_irrigation_param",
      Scen_i,
      "information table for irrigation parameters"
    )

    Irr_param_verif <- load_data_file(
      Scenario$Pratique_irrigation_param[Scen_i],
      Working_path
    )

    # Verification of junction between crops and irrigation percentages
    if (Scenario$Plots_RADIS_RPG[Scen_i] == "Non_existant_plots") {
      shp_cult_verif <- shapefile_data_base_verif
      colnames(shp_cult_verif)[which(
        colnames(shp_cult_verif) == Scenario$Colname_codecultureRPG[Scen_i]
      )] <- "CC_verif_test"
      list_fakeplot_sim <- unique(shp_cult_verif$CC_verif_test)

      if (!all(list_fakeplot_sim %in% unique(Irr_perc_verif$CODE_CU))) {
        stop(sprintf(
          "Warning, the irrigation percentage table (Pratique_irrigation_param) and the list of provided crops (Shp_link) provided do not match for each provided crops for Scenario %d in the ordonnanceur\nMissing crops in the irrigation percentage table: %s",
          Scen_i,
          paste(
            setdiff(list_fakeplot_sim, unique(Irr_perc_verif$CODE_CU)),
            collapse = ", "
          )
        ))
      }
    }

    # Irrigation parametered methods
    if (any(!Irr_param_verif$Irri %in% colnames(Irr_perc_verif))) {
      stop(sprintf(
        "Warning, the irrigation parameter table (Pratique_irrigation_param) and the irrigation percentage table (Pratique_irrigation_table_percentages) provided have different Irrigation practices names for Scenario %d in the ordonnanceur",
        Scen_i
      ))
    }

    # Irrigation restriction periods
    if (!is.na(Scenario$Interdiction_usage_eau[Scen_i])) {
      Irr_interd_period_path <- check_file_exists(
        Scenario$Interdiction_usage_eau[Scen_i],
        Working_path,
        "Interdiction_usage_eau",
        Scen_i,
        "irrigation restriction table"
      )

      Irr_interd_period <- load_data_file(
        Scenario$Interdiction_usage_eau[Scen_i],
        Working_path
      )

      # Check date format for start and end of restriction periods
      check_date_format <- function(dates, col_name) {
        if (any(is.na(dates))) {
          return(FALSE)
        }
        lengths <- sapply(stringr::str_split(dates, "_"), length)
        all(lengths >= 2 & lengths <= 3)
      }

      if (
        !check_date_format(
          Irr_interd_period$Debut_interdiction,
          "Debut_interdiction"
        )
      ) {
        stop(sprintf(
          "Warning, the irrigation restriction table (Interdiction_usage_eau) provided have a non coherent value in the col 'Debut_interdiction' for Scenario %d in the ordonnanceur",
          Scen_i
        ))
      }

      if (
        !check_date_format(
          Irr_interd_period$Fin_interdiction,
          "Fin_interdiction"
        )
      ) {
        stop(sprintf(
          "Warning, the irrigation parameter table (Interdiction_usage_eau) provided have a non coherent value in the col 'Fin_interdiction' for Scenario %d in the ordonnanceur",
          Scen_i
        ))
      }
    }

    # Model
    if (!Scenario$Model[Scen_i] %in% c("CropWat", "Aquacrop")) {
      stop(sprintf(
        "Warning, the asked model is not yet parametered in CAWET (Model) provided an already parametered one or/and contact the support for a new adding model interest for Scenario %d in the ordonnanceur",
        Scen_i
      ))
    }

    # Model_param_crop
    Mod_param_path <- check_file_exists(
      Scenario$Model_param_crop[Scen_i],
      Working_path,
      "Model_param_crop",
      Scen_i,
      "model parameters table"
    )

    # Model_link_RPG_croparam
    Mod_RPG_correspondance_path <- check_file_exists(
      Scenario$Model_link_RPG_croparam[Scen_i],
      Working_path,
      "Model_link_RPG_croparam",
      Scen_i,
      "model corresponding parameters table to RPG names"
    )

    # Territorial_validation_link
    if (!is.na(Scenario$Territorial_validation_link[Scen_i])) {
      Terr_val_link <- check_file_exists(
        Scenario$Territorial_validation_link[Scen_i],
        Working_path,
        "Territorial_validation_link",
        Scen_i,
        "territorial comparison table"
      )

      check_file_extension(
        Terr_val_link,
        c(".csv", "xlsx"),
        "Territorial_validation_link",
        Scen_i,
        "territorial comparison table"
      )
    }

    # Cultural_validation_link
    if (!is.na(Scenario$Cultural_validation_link[Scen_i])) {
      Cul_val_link <- check_file_exists(
        Scenario$Cultural_validation_link[Scen_i],
        Working_path,
        "Cultural_validation_link",
        Scen_i,
        "cultural comparison table"
      )

      check_file_extension(
        Cul_val_link,
        c(".csv", "xlsx"),
        "Cultural_validation_link",
        Scen_i,
        "Cultural comparison table"
      )
    }

    # Wanted_output table
    Wanted_output <- check_file_exists(
      Scenario$Wanted_output[Scen_i],
      Working_path,
      "Wanted_output",
      Scen_i,
      "selection of outputs"
    )
  }
  cli_progress_done()
  cli_alert_success("Vérification Ordonnanceur effectuée avec succès.")
}
