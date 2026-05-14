#' Plotting simulation results for a CAWET scenario
#'
#' This script generates graphs based on the results of a CAWET scenario simulation.
#' It reads the simulation results and creates various visualizations such as pie charts for crop distribution,
#' bar charts for annual and monthly irrigation data, and comparison graphs with validation data if provided.
#' It saves the generated graphs in a specified directory for further analysis.
#' @inheritParams run_script2
#' @return This function does not return a value but saves graphs to the specified directory.
#' @export
#'
run_script3_1 <- function(
  cfgRun = get_Run_Config(Working_path = Working_path)
) {
  cli_alert_info(
    "Demarrage run_script3_1 - Graphiques de la simulation des scenarios CAWET"
  )

  # Global variables initialization
  Working_path <- cfgRun$Working_path
  Path_Run <- cfgRun$Path_Run
  Path_Run_chargementdata <- cfgRun$Path_Run_Chargementdata
  Scenario <- cfgRun$Scenario
  utils::data("grille_safran", package = "CAWET")

  # Scen <- Scenario$Scenario[2]
  for (Scen in Scenario$Scenario) {
    Scen_i <- which(Scenario$Scenario == Scen)
    Path_scen <- paste0(Path_Run_chargementdata, '/', Scen_i, '_', Scen)
    Path_scen_meteo <- paste0(Path_scen, '/Meteo')

    Wanted_output <- read.csv2(paste0(Path_scen, '/Wanted_outputs.csv'))

    Param_crop <- read.csv2(paste0(Path_scen, '/Param_crop_to_RPG.csv'))
    Colors_crop <- Param_crop %>%
      dplyr::filter(!is.na(Crop_RPG)) %>%
      dplyr::select(c(Crop_RPG, Color))

    Simulation_run_scen <- paste0(Path_Run, '/Results/', Scen_i, '_', Scen)
    Simulation_run_path_graph <- paste0(Simulation_run_scen, '/', 'Graphs')
    if (!file.exists(Simulation_run_path_graph)) {
      dir.create(Simulation_run_path_graph)
    }

    #polygones
    if (Scenario$Plots_RADIS_RPG[Scen_i] == 'Yes') {
      #Download area location
      shapefile_data_base <- st_read(FpCAWET(
        Working_path,
        Scenario$Shp_link[Scen_i]
      ))
      shapefile_data_base <- st_transform(shapefile_data_base, crs = 2154) #Transformation of the projection to lambert 93
    }

    if (Scenario$Plots_RADIS_RPG[Scen_i] != 'Yes') {
      shapefile_data1 <- data.frame(stringsAsFactors = FALSE)
      empty_geom <- st_sfc(crs = 2154)
      shapefile_data_base <- st_sf(shapefile_data1, geometry = empty_geom)
    }

    #plots
    Surf_tab <- read.csv2(paste0(
      Path_Run_chargementdata,
      "/",
      Scen_i,
      '_',
      Scen,
      '/Surf_table_',
      Scen,
      '.csv'
    ))
    ST <- as.data.table(Surf_tab) %>%
      dplyr::mutate(
        date = ifelse(
          str_detect(date, '/'),
          format(as.Date(date, format = "%d/%m/%Y"), "%Y-%m-%d"),
          format(as.Date(date, format = "%Y-%m-%d"), "%Y-%m-%d")
        )
      )
    ST[, runid := rleidv(.SD, cols = setdiff(names(ST), "date"))]

    multiplicateur <- as.numeric(length(unique(ST$runid)))

    info_surf_tab <- str_split(
      colnames(Surf_tab)[3:length(colnames(Surf_tab))],
      '_'
    )
    info_surf_tab_poly <- rep(
      unlist(info_surf_tab)[which(str_detect(unlist(info_surf_tab), "poly"))],
      each = multiplicateur
    )
    info_surf_tab_mail <- rep(
      unlist(info_surf_tab)[which(str_detect(unlist(info_surf_tab), "mail"))],
      each = multiplicateur
    )
    info_surf_tab_ssMail <- rep(
      paste0(
        unlist(info_surf_tab)[which(str_detect(
          unlist(info_surf_tab),
          "ssmail"
        ))],
        "_",
        unlist(info_surf_tab)[!is.na(as.numeric(unlist(info_surf_tab)))]
      ),
      each = multiplicateur
    )

    #Cration of pie plots for each polygones
    Surf_tab <- ST[,
      c(
        .(first_date = min(date), last_date = max(date)),
        .SD[1, .SDcols = setdiff(names(ST), c("date", "runid"))]
      ),
      by = runid
    ] %>%
      dplyr::select(-c("date", "year", "runid")) %>%
      pivot_longer(
        !c(first_date, last_date),
        names_to = "income",
        values_to = "Surface"
      ) %>%
      dplyr::mutate(Culture = substr(income, 1, 3)) %>%
      dplyr::mutate(Poly = info_surf_tab_poly) %>%
      dplyr::mutate(mail = info_surf_tab_mail) %>%
      dplyr::mutate(ssMail = info_surf_tab_ssMail) %>%
      dplyr::filter(!is.na(Surface))

    # pol <- unique(Surf_tab$Poly)[1]
    for (pol in unique(Surf_tab$Poly)) {
      Surf_tab_pol <- Surf_tab %>% dplyr::filter(Poly == pol)
      # st <- unique(Surf_tab$first_date)[1]
      for (st in unique(Surf_tab$first_date)) {
        Surf_tab_start <- Surf_tab_pol %>%
          dplyr::filter(first_date == st) %>%
          dplyr::group_by(first_date, last_date, Poly, Culture) %>%
          dplyr::summarise(Surface = sum(Surface)) %>%
          dplyr::mutate(Crop_RPG = Culture) %>%
          full_join(Colors_crop, by = "Crop_RPG") %>%
          dplyr::mutate(Color = ifelse(!is.na(Color), Color, "#cccccc")) %>%
          dplyr::filter(!is.na(Culture))

        sto <- Surf_tab_start$last_date[1]

        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Pie plot description of the territory"
          )]) ==
            "YES" &
            toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs == "Detailed graph by polygon"
            )]) ==
              "YES"
        ) {
          if (length(rownames(Surf_tab_start)) < 32) {
            ggplot(
              Surf_tab_start,
              aes(
                x = "",
                y = Surface,
                fill = paste0(Culture, ' - ', round(Surface / 10000, 1), "ha")
              )
            ) +
              geom_bar(width = 1, stat = "identity", color = "black") +
              coord_polar("y") +
              scale_fill_manual(values = Surf_tab_start$Color) +
              labs(
                title = paste0(
                  "Crop repartition in the polygone ",
                  pol,
                  " between ",
                  st,
                  ' and ',
                  sto
                ),
                subtitle = paste0(
                  'total : ',
                  round(sum(Surf_tab_start$Surface, na.rm = T) / 10000),
                  'ha'
                )
              ) +
              theme_void() +
              theme(legend.title = element_blank()) +
              geom_text(
                aes(label = Culture),
                position = position_stack(vjust = 0.5),
                size = 3
              )
            ggsave(
              paste0(
                Simulation_run_path_graph,
                "/Repartition_cultures_",
                pol,
                '_',
                gsub("/", "_", st),
                '_',
                gsub("/", "_", sto),
                '.png'
              ),
              width = 30,
              height = 20,
              units = 'cm'
            )
          } else {
            ggplot(
              Surf_tab_start,
              aes(
                x = "",
                y = Surface,
                fill = paste0(Culture, ' - ', round(Surface / 10000, 1), "ha")
              )
            ) +
              geom_bar(width = 1, stat = "identity", color = "black") +
              coord_polar("y") +
              scale_fill_manual(values = Surf_tab_start$Color) +
              labs(
                title = paste0(
                  "Crop repartition in the polygone ",
                  pol,
                  " between ",
                  st,
                  ' and ',
                  sto
                ),
                subtitle = paste0(
                  'total : ',
                  round(sum(Surf_tab_start$Surface, na.rm = T) / 10000),
                  'ha'
                )
              ) +
              theme_void() +
              theme(legend.title = element_blank()) +
              geom_text(
                aes(label = Culture),
                position = position_stack(vjust = 0.5),
                size = 3
              )
            ggsave(
              paste0(
                Simulation_run_path_graph,
                "/Repartition_cultures_",
                pol,
                '_',
                gsub("/", "_", st),
                '_',
                gsub("/", "_", sto),
                '.png'
              ),
              width = 30,
              height = 20,
              units = 'cm'
            )
          }
        }
      }
    } #end loop culture/polygons

    #Cration of pie plots for each maille
    # maille <- unique(Surf_tab$mail)[1]
    for (maille in unique(Surf_tab$mail)) {
      Surf_tab_mai <- Surf_tab %>% dplyr::filter(mail == maille)
      # st <- unique(Surf_tab$first_date)[1]
      for (st in unique(Surf_tab_mai$first_date)) {
        Surf_tab_start <- Surf_tab_mai %>%
          dplyr::filter(first_date == st) %>%
          dplyr::group_by(first_date, last_date, Culture) %>%
          dplyr::summarise(Surface = sum(Surface)) %>%
          dplyr::mutate(Crop_RPG = Culture) %>%
          full_join(Colors_crop, by = "Crop_RPG") %>%
          dplyr::mutate(Color = ifelse(!is.na(Color), Color, "#cccccc")) %>%
          dplyr::filter(!is.na(Culture))
        sto <- Surf_tab_start$last_date[1]
        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Pie plot description of the territory"
          )]) ==
            "YES" &
            toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs == "Detailed graph by grid"
            )]) ==
              "YES"
        ) {
          if (length(rownames(Surf_tab_start)) < 32) {
            ggplot(
              Surf_tab_start,
              aes(
                x = "",
                y = Surface,
                fill = paste0(Culture, ' - ', round(Surface / 10000, 1), "ha")
              )
            ) +
              geom_bar(width = 1, stat = "identity", color = "black") +
              coord_polar("y") +
              scale_fill_manual(values = Surf_tab_start$Color) +
              labs(
                title = paste0(
                  "Crop repartition in the net ",
                  substr(maille, 7, 100),
                  " between ",
                  st,
                  ' and ',
                  sto
                ),
                subtitle = paste0(
                  'total : ',
                  round(sum(Surf_tab_start$Surface, na.rm = T) / 10000),
                  'ha'
                )
              ) +
              theme_void() +
              theme(legend.title = element_blank()) +
              geom_text(
                aes(label = Culture),
                position = position_stack(vjust = 0.5),
                size = 3
              )
            ggsave(
              paste0(
                Simulation_run_path_graph,
                "/Repartition_cultures_",
                maille,
                '_',
                st,
                '_',
                sto,
                '.png'
              ),
              width = 30,
              height = 20,
              units = 'cm'
            )
          } else {
            ggplot(
              Surf_tab_start,
              aes(
                x = "",
                y = Surface,
                fill = paste0(Culture, ' - ', round(Surface / 10000, 1), "ha")
              )
            ) +
              geom_bar(width = 1, stat = "identity", color = "black") +
              coord_polar("y") +
              scale_fill_manual(values = Surf_tab_start$Color) +
              labs(
                title = paste0(
                  "Crop repartition in the net ",
                  maille,
                  " between ",
                  st,
                  ' and ',
                  sto
                ),
                subtitle = paste0(
                  'total : ',
                  round(sum(Surf_tab_start$Surface, na.rm = T) / 10000),
                  'ha'
                )
              ) +
              theme_void() +
              theme(legend.title = element_blank()) +
              geom_text(
                aes(label = Culture),
                position = position_stack(vjust = 0.5),
                size = 3
              )

            ggsave(
              paste0(
                Simulation_run_path_graph,
                "/Repartition_cultures_",
                maille,
                '_',
                st,
                '_',
                sto,
                '.png'
              ),
              width = 30,
              height = 20,
              units = 'cm'
            )
          }
        }
      }
    } #end loop culture/maille

    Results_Cw <- read.csv2(paste0(
      Simulation_run_scen,
      '/Resultats_Aggreges/Modelisation_for_polygones_and_network.csv'
    )) %>%
      dplyr::mutate(Mail = substr(Mail, 3, 1000)) %>%
      dplyr::mutate(Crop_RPG = CODE_CU) %>%
      full_join(Colors_crop, by = "Crop_RPG") %>%
      dplyr::mutate(Color = ifelse(!is.na(Color), Color, "#cccccc")) %>%
      dplyr::filter(!is.na(CODE_CU))
    Results_Cw_day <- read.csv2(paste0(
      Simulation_run_scen,
      '/Resultats_Aggreges/Modelisation_for_polygones_and_network_perday.csv'
    )) %>%
      dplyr::mutate(Mail = substr(Mail, 3, 1000)) %>%
      dplyr::mutate(Crop_RPG = CODE_CU) %>%
      full_join(Colors_crop) %>%
      dplyr::mutate(Color = ifelse(!is.na(Color), Color, "#cccccc")) %>%
      dplyr::filter(!is.na(CODE_CU))

    #Graph total irrigation par an
    if (
      toupper(Wanted_output$Wanted[which(
        Wanted_output$Outputs == "Annual visualisation of results"
      )]) ==
        "YES" &
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Detailed graph by crops"
        )]) ==
          "YES"
    ) {
      ggplot(data = Results_Cw) +
        geom_bar(
          stat = 'identity',
          aes(x = as.factor(An), y = Irrigation_m3, fill = CODE_CU)
        ) +
        scale_y_continuous(labels = scales::label_number()) +
        scale_x_discrete("An") +
        scale_fill_manual(
          values = setNames(Results_Cw$Color, Results_Cw$CODE_CU)
        ) +
        theme_bw()
      ggsave(
        paste0(
          Simulation_run_path_graph,
          '/Simulation_allyear_parculture_allterritory',
          '.png'
        ),
        width = 30,
        height = 20,
        units = 'cm'
      )
    }
    if (
      toupper(Wanted_output$Wanted[which(
        Wanted_output$Outputs == "Annual visualisation of results"
      )]) ==
        "YES" &
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Detailed graph by grid"
        )]) ==
          "YES"
    ) {
      ggplot(data = Results_Cw) +
        geom_bar(
          stat = 'identity',
          aes(x = as.factor(An), y = Irrigation_m3, fill = Mail)
        ) +
        scale_y_continuous(labels = scales::label_number()) +
        scale_x_discrete("An") +
        theme_bw()
      ggsave(
        paste0(
          Simulation_run_path_graph,
          '/Simulation_allyear_parmaille_allterritory',
          '.png'
        ),
        width = 30,
        height = 20,
        units = 'cm'
      )
    }
    if (
      length(unique(Results_Cw$ssMail)) < 30 &
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Annual visualisation of results"
        )]) ==
          "YES" &
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Detailed graph by subgrid"
        )]) ==
          "YES"
    ) {
      ggplot(data = Results_Cw) +
        geom_bar(
          stat = 'identity',
          aes(x = as.factor(An), y = Irrigation_m3, fill = ssMail)
        ) +
        scale_y_continuous(labels = scales::label_number()) +
        scale_x_discrete("An") +
        theme_bw() +
        theme(legend.position = 'bottom')
      ggsave(
        paste0(
          Simulation_run_path_graph,
          '/Simulation_allyear_parssmaille_allterritory',
          '.png'
        ),
        width = 30,
        height = 20,
        units = 'cm'
      )
    }

    if (
      toupper(Wanted_output$Wanted[which(
        Wanted_output$Outputs == "Annual visualisation of results"
      )]) ==
        "YES" &
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Detailed graph by polygon"
        )]) ==
          "YES"
    ) {
      ggplot(data = Results_Cw) +
        geom_bar(
          stat = 'identity',
          aes(x = as.factor(An), y = Irrigation_m3, fill = Poly)
        ) +
        scale_y_continuous(labels = scales::label_number()) +
        scale_x_discrete("An") +
        theme_bw()
      ggsave(
        paste0(
          Simulation_run_path_graph,
          '/Simulation_allyear_parpolygone_allterritory',
          '.png'
        ),
        width = 30,
        height = 20,
        units = 'cm'
      )
    }

    #Graph total irrigation par an plus comparaison
    if (!is.na(Scenario$Territorial_validation_link[Scen_i])) {
      Validation_territoire <- st_read(FpCAWET(
        Working_path,
        Scenario$Territorial_validation_link[Scen_i]
      ))

      if (length(Validation_territoire$Year) > 0) {
        #Cas ou on a une validation/comparaison chaque annee
        if (length(Validation_territoire$Year) > 0) {
          Year_list_valterr <- Validation_territoire$Year
        } else {
          Year_list_valterr <- ''
        }
        if (length(Validation_territoire$Year) > 0) {
          Connexion <- '_'
        } else {
          Connexion <- ''
        }
        Irrig_val_terr_df <- data.frame(
          Poly = 'Comparison',
          Mail = 'Comparison',
          CODE_CU = 'Comparison',
          Mois = NA,
          An = paste0(Year_list_valterr, Connexion, 'Comparison'),
          Irrigation_m3 = as.numeric(Validation_territoire$Water_quantity_m3),
          ETc_m3 = NA,
          ETc_mean = NA,
          surf_tot = NA
        )
        Results_Cw_val <- Results_Cw %>%
          dplyr::mutate(An = as.character(An)) %>%
          bind_rows(Irrig_val_terr_df) %>%
          dplyr::mutate(Color = ifelse(!is.na(Color), Color, "#cccccc"))
        Results_Cw_val <- arrange(Results_Cw_val, An)

        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Annual visualisation of results"
          )]) ==
            "YES" &
            toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs == "Detailed graph by crops"
            )]) ==
              "YES"
        ) {
          ggplot(data = Results_Cw_val) +
            geom_bar(
              stat = 'identity',
              aes(x = as.factor(An), y = Irrigation_m3, fill = CODE_CU)
            ) +
            scale_y_continuous(labels = scales::label_number()) +
            scale_fill_manual(
              values = setNames(Results_Cw_val$Color, Results_Cw_val$CODE_CU),
              na.value = "darkgrey"
            ) +
            scale_x_discrete("An") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90))
          ggsave(
            paste0(
              Simulation_run_path_graph,
              '/Comparisonvalue_Simulation_allyear_parculture_allterritory',
              '.png'
            ),
            width = 30,
            height = 20,
            units = 'cm'
          )
        }
        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Annual visualisation of results"
          )]) ==
            "YES" &
            toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs == "Detailed graph by grid"
            )]) ==
              "YES"
        ) {
          ggplot(data = Results_Cw_val) +
            geom_bar(
              stat = 'identity',
              aes(x = as.factor(An), y = Irrigation_m3, fill = Mail)
            ) +
            scale_y_continuous(labels = scales::label_number()) +
            scale_x_discrete("An") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90))
          ggsave(
            paste0(
              Simulation_run_path_graph,
              '/Comparisonvalue_Simulation_allyear_parmaille_allterritory',
              '.png'
            ),
            width = 30,
            height = 20,
            units = 'cm'
          )
        }
        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Annual visualisation of results"
          )]) ==
            "YES" &
            toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs == "Detailed graph by polygon"
            )]) ==
              "YES"
        ) {
          ggplot(data = Results_Cw_val) +
            geom_bar(
              stat = 'identity',
              aes(x = An, y = Irrigation_m3, fill = Poly)
            ) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90))
          ggsave(
            paste0(
              Simulation_run_path_graph,
              '/Comparisonvalue_Simulation_allyear_parpolygone_allterritory',
              '.png'
            ),
            width = 30,
            height = 20,
            units = 'cm'
          )
        }

        Irrig_val_terr_df2 <- data.frame(
          An = as.character(Year_list_valterr),
          Irrigation_validation = as.numeric(Validation_territoire$Water_quantity_m3)
        )
        Comparatif_val_calc <- Results_Cw %>%
          dplyr::mutate(An = as.character(An)) %>%
          dplyr::group_by(An) %>%
          dplyr::summarise(
            Irrigation = sum(Irrigation_m3),
            surf = sum(surf_tot)
          ) %>%
          dplyr::full_join(Irrig_val_terr_df2, by = "An")
        ggplot(data = Comparatif_val_calc) +
          geom_point(aes(x = Irrigation_validation, y = Irrigation)) +
          geom_abline(intercept = 0, slope = 1) +
          ggtitle("Comparison graph between calculation and comparison data") +
          scale_y_continuous(
            name = 'Calculated data (m³)',
            limits = c(
              min(
                c(
                  Comparatif_val_calc$Irrigation,
                  Comparatif_val_calc$Irrigation_validation
                ),
                na.rm = T
              ) -
                500,
              max(
                c(
                  Comparatif_val_calc$Irrigation,
                  Comparatif_val_calc$Irrigation_validation
                ),
                na.rm = T
              ) +
                500
            )
          ) +
          scale_x_continuous(
            name = 'Comparison data (m³)',
            limits = c(
              min(
                c(
                  Comparatif_val_calc$Irrigation,
                  Comparatif_val_calc$Irrigation_validation
                ),
                na.rm = T
              ) -
                500,
              max(
                c(
                  Comparatif_val_calc$Irrigation,
                  Comparatif_val_calc$Irrigation_validation
                ),
                na.rm = T
              ) +
                500
            )
          ) +
          theme_bw() +
          theme(
            axis.text = element_text(size = 17),
            axis.title = element_text(size = 20),
            title = element_text(size = 22)
          )
        ggsave(
          paste0(
            Simulation_run_path_graph,
            '/Comparatif_graph_calculated_comparison_data',
            '.png'
          ),
          width = 30,
          height = 30,
          units = 'cm'
        )
      }

      if (length(Validation_territoire$Year) == 0) {
        #Cas ou on a une validation/comparaison unique
        Irrig_val_terr_df <- data.frame(
          Poly = 'Comparison',
          Mail = 'Comparison',
          CODE_CU = 'Validation',
          Mois = NA,
          An = 'Comparison',
          Irrigation_m3 = as.numeric(Validation_territoire$Water_quantity_m3[1]),
          ETc_m3 = NA,
          ETc_mean = NA,
          surf_tot = NA
        )
        Results_Cw_val <- Results_Cw %>%
          dplyr::mutate(An = as.character(An)) %>%
          bind_rows(Irrig_val_terr_df) %>%
          dplyr::mutate(Color = ifelse(!is.na(Color), Color, "#cccccc"))

        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Annual visualisation of results"
          )]) ==
            "YES" &
            toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs == "Detailed graph by crops"
            )]) ==
              "YES"
        ) {
          ggplot(data = Results_Cw_val) +
            geom_bar(
              stat = 'identity',
              aes(x = as.factor(An), y = Irrigation_m3, fill = CODE_CU)
            ) +
            scale_y_continuous(labels = scales::label_number()) +
            scale_fill_manual(
              values = setNames(Results_Cw_val$Color, Results_Cw_val$CODE_CU),
              na.value = "darkgrey"
            ) +
            scale_x_discrete("An") +
            theme_bw()
          ggsave(
            paste0(
              Simulation_run_path_graph,
              '/Comparisonvalue_Simulation_allyear_parculture_allterritory',
              '.png'
            ),
            width = 30,
            height = 20,
            units = 'cm'
          )
        }
        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Annual visualisation of results"
          )]) ==
            "YES" &
            toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs == "Detailed graph by grid"
            )]) ==
              "YES"
        ) {
          ggplot(data = Results_Cw_val) +
            geom_bar(
              stat = 'identity',
              aes(x = as.factor(An), y = Irrigation_m3, fill = Mail)
            ) +
            scale_y_continuous(labels = scales::label_number()) +
            scale_x_discrete("An") +
            theme_bw()
          ggsave(
            paste0(
              Simulation_run_path_graph,
              '/Comparisonvalue_Simulation_allyear_parmaille_allterritory',
              '.png'
            ),
            width = 30,
            height = 20,
            units = 'cm'
          )
        }
        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Annual visualisation of results"
          )]) ==
            "YES" &
            toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs == "Detailed graph by polygon"
            )]) ==
              "YES"
        ) {
          ggplot(data = Results_Cw_val) +
            geom_bar(
              stat = 'identity',
              aes(x = as.factor(An), y = Irrigation_m3, fill = Poly)
            ) +
            scale_y_continuous(labels = scales::label_number()) +
            scale_x_discrete("An") +
            theme_bw()
          ggsave(
            paste0(
              Simulation_run_path_graph,
              '/Comparisonvalue_Simulation_allyear_parpolygone_allterritory',
              '.png'
            ),
            width = 30,
            height = 20,
            units = 'cm'
          )
        }
      }
    } #end validation territorial

    #Graph irrigation annees differenciees
    # year <- unique(Results_Cw$An)[1]
    years <- unique(Results_Cw$An)
    progress_years <- cli_progress_bar(
      format = "{pb_spin} Graphiques des volumes irrigation année {pb_status} ({pb_current}/{pb_total}) | ETA: {pb_eta}",
      format_done = "{col_green(symbol$tick)} Graphiques annuels des volumes irrigation réalisés en {pb_elapsed}",
      total = length(years),
      clear = FALSE
    )
    for (year in years) {
      cli_progress_update(id = progress_years, status = year)
      Results_Cw_y <- Results_Cw %>%
        dplyr::filter(An == year) %>%
        dplyr::mutate(Color = ifelse(!is.na(Color), Color, "#cccccc"))

      if (
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Mensual visualisation of results"
        )]) ==
          "YES" &
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Detailed graph by crops"
          )]) ==
            "YES"
      ) {
        ggplot(Results_Cw_y) +
          geom_bar(
            stat = 'identity',
            aes(x = Mois, y = Irrigation_m3, fill = CODE_CU)
          ) +
          scale_fill_manual(
            values = setNames(Results_Cw_y$Color, Results_Cw_y$CODE_CU),
            na.value = "darkgrey"
          ) +
          scale_x_continuous(
            year,
            breaks = seq(min(Results_Cw_y$Mois), max(Results_Cw_y$Mois), by = 1)
          ) +
          scale_y_continuous(labels = scales::label_number()) +
          ggtitle(year) +
          theme_bw() +
          theme(
            legend.title = element_blank(),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            title = element_text(size = 20),
            legend.text = element_text(size = 15)
          )
        ggsave(
          paste0(
            Simulation_run_path_graph,
            "/Simulation_Cw_",
            year,
            '_parculture',
            '.png'
          ),
          width = 30,
          height = 20,
          units = 'cm'
        )
      }
      if (
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Mensual visualisation of results"
        )]) ==
          "YES" &
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Detailed graph by polygon"
          )]) ==
            "YES"
      ) {
        ggplot(Results_Cw_y) +
          geom_bar(
            stat = 'identity',
            aes(x = Mois, y = Irrigation_m3, fill = Poly)
          ) +
          scale_x_continuous(
            year,
            breaks = seq(min(Results_Cw_y$Mois), max(Results_Cw_y$Mois), by = 1)
          ) +
          scale_y_continuous(labels = scales::label_number()) +
          ggtitle(year) +
          theme_bw() +
          theme(
            legend.title = element_blank(),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            title = element_text(size = 20),
            legend.text = element_text(size = 15)
          )
        ggsave(
          paste0(
            Simulation_run_path_graph,
            "/Simulation_Cw_",
            year,
            '_parpoly',
            '.png'
          ),
          width = 30,
          height = 20,
          units = 'cm'
        )
      }
      if (
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Mensual visualisation of results"
        )]) ==
          "YES" &
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Detailed graph by grid"
          )]) ==
            "YES"
      ) {
        ggplot(Results_Cw_y) +
          geom_bar(
            stat = 'identity',
            aes(x = Mois, y = Irrigation_m3, fill = Mail)
          ) +
          scale_x_continuous(
            year,
            breaks = seq(min(Results_Cw_y$Mois), max(Results_Cw_y$Mois), by = 1)
          ) +
          scale_y_continuous(labels = scales::label_number()) +
          ggtitle(year) +
          theme_bw() +
          theme(
            legend.title = element_blank(),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            title = element_text(size = 20),
            legend.text = element_text(size = 15)
          )
        ggsave(
          paste0(
            Simulation_run_path_graph,
            "/Simulation_Cw_",
            year,
            '_parmail',
            '.png'
          ),
          width = 30,
          height = 20,
          units = 'cm'
        )
      }
      if (
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Mensual visualisation of results"
        )]) ==
          "YES"
      ) {
        ggplot(Results_Cw_y) +
          geom_bar(
            stat = 'identity',
            aes(x = Mois, y = Irrigation_m3),
            fill = 'darkblue'
          ) +
          scale_x_continuous(
            year,
            breaks = seq(min(Results_Cw_y$Mois), max(Results_Cw_y$Mois), by = 1)
          ) +
          scale_y_continuous(labels = scales::label_number()) +
          ggtitle(year) +
          theme_bw() +
          theme(
            legend.title = element_blank(),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            title = element_text(size = 20),
            legend.text = element_text(size = 15)
          )
        ggsave(
          paste0(Simulation_run_path_graph, "/Simulation_Cw_", year, '.png'),
          width = 30,
          height = 20,
          units = 'cm'
        )
      }
    }
    cli_progress_done(id = progress_years)

    # month<-unique(Results_Cw$Mois)[6]
    if (
      toupper(Wanted_output$Wanted[which(
        Wanted_output$Outputs == "Detailed mensual visualisation"
      )]) ==
        "YES"
    ) {
      months <- unique(Results_Cw$Mois)
      progress_months <- cli_progress_bar(
        format = "{pb_spin} Graphiques mensuels : Mois {pb_current}/{pb_total} | ETA: {pb_eta}",
        format_done = "{col_green(symbol$tick)} Graphiques mensuels réalisés en {pb_elapsed}",
        total = length(months),
        clear = FALSE
      )
      for (month in months) {
        cli_progress_update(id = progress_months)
        Results_Cw_m <- Results_Cw %>%
          dplyr::filter(Mois == month) %>%
          dplyr::mutate(Color = ifelse(!is.na(Color), Color, "#cccccc"))

        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Detailed mensual visualisation"
          )]) ==
            "YES" &
            toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs == "Detailed graph by crops"
            )]) ==
              "YES"
        ) {
          ggplot(Results_Cw_m) +
            geom_bar(
              stat = 'identity',
              aes(x = An, y = Irrigation_m3, fill = CODE_CU)
            ) +
            scale_fill_manual(
              values = setNames(Results_Cw_m$Color, Results_Cw_m$CODE_CU),
              na.value = "darkgrey"
            ) +
            scale_x_continuous(
              '',
              breaks = seq(min(Results_Cw_m$An), max(Results_Cw_m$An), by = 1)
            ) +
            scale_y_continuous(labels = scales::label_number()) +
            ggtitle(paste0('Month : ', month)) +
            theme_bw() +
            theme(
              legend.title = element_blank(),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 15),
              title = element_text(size = 20),
              legend.text = element_text(size = 15)
            )
          ggsave(
            paste0(
              Simulation_run_path_graph,
              "/Simulation_Cw_month",
              month,
              '_parculture',
              '.png'
            ),
            width = 30,
            height = 20,
            units = 'cm'
          )
        }
        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Detailed mensual visualisation"
          )]) ==
            "YES" &
            toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs == "Detailed graph by polygon"
            )]) ==
              "YES"
        ) {
          ggplot(Results_Cw_m) +
            geom_bar(
              stat = 'identity',
              aes(x = An, y = Irrigation_m3, fill = Poly)
            ) +
            scale_x_continuous(
              '',
              breaks = seq(min(Results_Cw_m$An), max(Results_Cw_m$An), by = 1)
            ) +
            scale_y_continuous(labels = scales::label_number()) +
            ggtitle(paste0('Month : ', month)) +
            theme_bw() +
            theme(
              legend.title = element_blank(),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 15),
              title = element_text(size = 20),
              legend.text = element_text(size = 15)
            )
          ggsave(
            paste0(
              Simulation_run_path_graph,
              "/Simulation_Cw_month",
              month,
              '_parpoly',
              '.png'
            ),
            width = 30,
            height = 20,
            units = 'cm'
          )
        }
        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Detailed mensual visualisation"
          )]) ==
            "YES" &
            toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs == "Detailed graph by grid"
            )]) ==
              "YES"
        ) {
          ggplot(Results_Cw_m) +
            geom_bar(
              stat = 'identity',
              aes(x = An, y = Irrigation_m3, fill = Mail)
            ) +
            scale_x_continuous(
              '',
              breaks = seq(min(Results_Cw_m$An), max(Results_Cw_m$An), by = 1)
            ) +
            scale_y_continuous(labels = scales::label_number()) +
            ggtitle(paste0('Month : ', month)) +
            theme_bw() +
            theme(
              legend.title = element_blank(),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 15),
              title = element_text(size = 20),
              legend.text = element_text(size = 15)
            )
          ggsave(
            paste0(
              Simulation_run_path_graph,
              "/Simulation_Cw_month",
              month,
              '_parmail',
              '.png'
            ),
            width = 30,
            height = 20,
            units = 'cm'
          )
        }
        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Detailed mensual visualisation"
          )]) ==
            "YES"
        ) {
          ggplot(Results_Cw_m) +
            geom_bar(
              stat = 'identity',
              aes(x = An, y = Irrigation_m3),
              fill = 'cyan2'
            ) +
            scale_x_continuous(
              '',
              breaks = seq(min(Results_Cw_m$An), max(Results_Cw_m$An), by = 1)
            ) +
            scale_y_continuous(labels = scales::label_number()) +
            ggtitle(paste0('Month : ', month)) +
            theme_bw() +
            theme(
              legend.title = element_blank(),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 15),
              title = element_text(size = 20),
              legend.text = element_text(size = 15)
            )
          ggsave(
            paste0(
              Simulation_run_path_graph,
              "/Simulation_Cw_month",
              month,
              '.png'
            ),
            width = 30,
            height = 20,
            units = 'cm'
          )
        }
      }
      cli_progress_done(id = progress_months)
    } #end month results

    if (
      toupper(Wanted_output$Wanted[which(
        Wanted_output$Outputs == "Detailed mensual visualisation"
      )]) ==
        "YES"
    ) {
      Results_Cw_mois <- Results_Cw %>%
        dplyr::group_by(Mois, An) %>%
        dplyr::summarise(Irrigation_m3 = sum(Irrigation_m3))
      Mean_juil <- round(mean(
        (Results_Cw_mois %>% dplyr::filter(Mois == 7))$Irrigation_m3
      ))
      Mean_aout <- round(mean(
        (Results_Cw_mois %>% dplyr::filter(Mois == 8))$Irrigation_m3
      ))
      Res_JuilletAout_graph_year <- Results_Cw_mois %>%
        dplyr::filter(Mois %in% c(7, 8)) %>%
        dplyr::mutate(
          Couleur = ifelse(
            Mois == 7,
            ifelse(Irrigation_m3 > Mean_juil, "red", "green"),
            ifelse(Irrigation_m3 > Mean_aout, "red", "green")
          )
        )
      ggplot(
        Res_JuilletAout_graph_year,
        aes(
          x = factor(paste0(An, ' - ', Mois)),
          y = Irrigation_m3,
          fill = Couleur
        )
      ) +
        geom_bar(stat = "identity", color = 'black') +
        labs(
          title = "Consommation d'eau du territoire simulée en juillet et aout",
          # subtitle=paste0("Surface simulee : ",round(Res_JuilletAout_graph_year$sum_surf_tottraitee[1]),"ha") ,
          x = "Annee - Mois",
          y = "Irrigation simulee (m³)"
        ) +
        geom_hline(aes(yintercept = Mean_juil)) +
        geom_hline(aes(yintercept = Mean_aout)) +
        annotate(
          "text",
          x = 1,
          y = Mean_juil + max(Res_JuilletAout_graph_year$Irrigation_m3) / 10,
          label = "moyenne juillet",
          size = 5,
          hjust = 0
        ) +
        annotate(
          "text",
          x = 1,
          y = Mean_aout + max(Res_JuilletAout_graph_year$Irrigation_m3) / 10,
          label = "moyenne août",
          size = 5,
          hjust = 0
        ) +
        scale_fill_manual(values = c("red" = "red", "green" = "green")) +
        scale_y_continuous(labels = scales::label_number()) +
        theme_minimal() +
        theme(
          legend.position = 'none',
          axis.text.x = element_text(angle = 90),
          title = element_text(size = 12),
          strip.text = element_text(size = 10),
          panel.grid.major.x = element_blank()
        )
      ggsave(
        paste0(
          Simulation_run_path_graph,
          "/Irrigation_territoire_juilletaout",
          '.png'
        ),
        width = 30,
        height = 20,
        units = 'cm'
      )
    } #end summer results

    #Water consumption per day, week and decade
    Results_Cw_day_totterr_day <- Results_Cw_day %>%
      dplyr::group_by(DatesR) %>%
      dplyr::summarise(Irrigation_m3_sumday = sum(Irrigation_m3)) %>%
      dplyr::mutate(
        Irr_roll_mean = slider::slide_dbl(
          Irrigation_m3_sumday,
          mean,
          .before = (as.numeric(Scenario$number_of_sliding_days_for_visualisation[
            Scen_i
          ]) -
            1),
          .complete = TRUE
        ),
        An = lubridate::year(DatesR),
        doy = lubridate::yday(DatesR)
      )
    ggplot(data = Results_Cw_day_totterr_day) +
      geom_line(aes(x = doy, y = Irrigation_m3_sumday, col = as.factor(An))) +
      # geom_line(aes(x=doy,y=Irr_roll_mean,col=as.factor(An)),size=1.2) +
      scale_y_continuous(
        "Irrigation sum (m3)",
        labels = scales::label_number()
      ) +
      scale_x_continuous(breaks = c(1, 50, 100, 150, 200, 250, 300, 350, 365)) +
      labs(col = "Years") +
      theme_bw()
    ggsave(
      paste0(
        Simulation_run_path_graph,
        "/Irrigation_territoire_sumparjour_",
        '.png'
      ),
      width = 30,
      height = 20,
      units = 'cm'
    )

    ggplot(data = Results_Cw_day_totterr_day) +
      geom_line(
        aes(x = doy, y = Irr_roll_mean, col = as.factor(An)),
        size = 1
      ) +
      scale_y_continuous(
        "Irrigation sum (m3)",
        labels = scales::label_number()
      ) +
      scale_x_continuous(breaks = c(1, 50, 100, 150, 200, 250, 300, 350, 365)) +
      labs(col = "Years") +
      theme_bw()
    ggsave(
      paste0(
        Simulation_run_path_graph,
        "/Irrigation_territoire_sumparjour_moyenneglissantesur",
        as.numeric(Scenario$number_of_sliding_days_for_visualisation[Scen_i]),
        'jours',
        '.png'
      ),
      width = 30,
      height = 20,
      units = 'cm'
    )

    if (
      toupper(Wanted_output$Wanted[which(
        Wanted_output$Outputs == "Weekly and decade visualisation"
      )]) ==
        "YES"
    ) {
      Results_Cw_day_totterr_week <- Results_Cw_day %>%
        dplyr::mutate(week = lubridate::isoweek(DatesR)) %>%
        dplyr::group_by(week, An) %>%
        dplyr::summarise(Irrigation_m3_tot = sum(Irrigation_m3))
      Results_Cw_day_totterr_decade <- Results_Cw_day %>%
        dplyr::mutate(
          decade = ifelse(day(DatesR) < 11, 1, ifelse(day(DatesR) < 21, 2, 3))
        ) %>%
        dplyr::group_by(decade, Mois, An) %>%
        dplyr::summarise(Irrigation_m3_tot = sum(Irrigation_m3)) %>%
        dplyr::mutate(
          dec_code = ifelse(decade == 1, 1, ifelse(decade == 2, 4, 7))
        )
      #Weeks
      ggplot(data = Results_Cw_day_totterr_week) +
        geom_point(aes(x = week, y = Irrigation_m3_tot, col = as.factor(An))) +
        scale_y_continuous(
          "Irrigation sum (m3)",
          labels = scales::label_number()
        ) +
        scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
        labs(col = "Years") +
        theme_bw()
      ggsave(
        paste0(
          Simulation_run_path_graph,
          "/Irrigation_territoire_sumparsemaine",
          '.png'
        ),
        width = 30,
        height = 20,
        units = 'cm'
      )
      #decades
      ggplot(data = Results_Cw_day_totterr_decade) +
        geom_point(aes(
          x = as.numeric(paste0(Mois, '.', dec_code)),
          y = Irrigation_m3_tot,
          col = as.factor(An)
        )) +
        scale_y_continuous(
          "Irrigation sum (m3)",
          labels = scales::label_number()
        ) +
        scale_x_continuous(
          "Decades",
          breaks = c(1:12),
          minor_breaks = c(
            1.1,
            1.4,
            1.7,
            2.1,
            2.4,
            2.7,
            3.1,
            3.4,
            3.7,
            4.1,
            4.4,
            4.7,
            5.1,
            5.4,
            5.7,
            6.1,
            6.4,
            6.7,
            7.1,
            7.4,
            7.7,
            8.1,
            8.4,
            8.7,
            9.1,
            9.4,
            9.7,
            10.1,
            10.4,
            10.7,
            11.1,
            11.4,
            11.7,
            12.1,
            12.4,
            12.7
          )
        ) +
        labs(col = "Years") +
        theme_bw() +
        theme(panel.grid.major.x = element_blank())
      ggsave(
        paste0(
          Simulation_run_path_graph,
          "/Irrigation_territoire_sumpardecades",
          '.png'
        ),
        width = 30,
        height = 20,
        units = 'cm'
      )
    }

    Surf_tab_join <- Surf_tab %>%
      dplyr::mutate(Mail = mail) %>%
      dplyr::select(c(Mail, Poly, Surface)) %>%
      dplyr::group_by(Mail, Poly) %>%
      dplyr::summarise(Surface_tot = sum(Surface, na.rm = T)) %>%
      dplyr::mutate(
        Mail = ifelse(substr(Mail, 1, 2) == 'ss', substr(Mail, 3, 30), Mail)
      )
    Surf_tab_join_ssm <- Surf_tab %>%
      dplyr::mutate(Mail = mail) %>%
      dplyr::select(c(ssMail, Poly, Surface)) %>%
      dplyr::group_by(ssMail, Poly) %>%
      dplyr::summarise(Surface_tot = sum(Surface, na.rm = T)) %>%
      dplyr::mutate(ssMail = substr(ssMail, 7, 1000))

    grille_safran_data <- grille_safran %>%
      dplyr::mutate(Mail = paste0('mail', id_maille)) %>%
      dplyr::full_join(Results_Cw, by = "Mail") %>%
      dplyr::full_join(Surf_tab_join, by = c("Mail", "Poly")) %>%
      dplyr::filter(!is.na(CODE_CU)) %>%
      dplyr::group_by(id_maille, An) %>%
      dplyr::summarise(
        Irrigation = sum(Irrigation_m3, na.rm = T),
        Surface_tot = sum(Surface_tot, na.rm = T)
      ) %>%
      dplyr::mutate(Irrigation_mm = (Irrigation * 1000) / Surface_tot)

    val_min <- min(grille_safran_data$Irrigation)
    val_max <- max(grille_safran_data$Irrigation)

    maille_avec_parcelles <- st_read(paste0(
      Path_Run_chargementdata,
      "/",
      Scen_i,
      '_',
      Scen,
      "/Networkplots_",
      year,
      ".shp"
    ))
    grid_all <- st_make_grid(
      maille_avec_parcelles,
      cellsize = 1000,
      square = TRUE
    )
    sous_maillage <- st_intersection(maille_avec_parcelles, grid_all) %>%
      st_as_sf() %>%
      dplyr::group_by(id_maille) %>%
      dplyr::mutate(
        sub_id = dplyr::row_number(),
        ssMail = paste0(id_maille, "_", sub_id)
      ) %>%
      ungroup()

    grille_safran_data_ssmail <- sous_maillage %>%
      dplyr::full_join(Results_Cw, by = "ssMail") %>%
      dplyr::full_join(Surf_tab_join_ssm, by = c("ssMail", "Poly")) %>%
      dplyr::filter(!is.na(CODE_CU)) %>%
      dplyr::group_by(ssMail, An) %>%
      dplyr::summarise(
        Irrigation = sum(Irrigation_m3, na.rm = T),
        Surface_tot = sum(Surface_tot, na.rm = T),
        geometry = st_union(geometry)
      ) %>%
      dplyr::mutate(Irrigation_mm = (Irrigation * 1000) / Surface_tot)

    val_min_ssma <- min(grille_safran_data_ssmail$Irrigation)
    val_max_ssma <- max(grille_safran_data_ssmail$Irrigation)

    # year <- unique(grille_safran_data$An)[1]

    #Cartographies
    if (length(rownames(shapefile_data_base)) > 0) {
      shapefile_data_baseid <- shapefile_data_base
      if (!is.na(Scenario$Colname_idpoly[Scen_i])) {
        names(shapefile_data_baseid)[which(
          names(shapefile_data_baseid) == Scenario$Colname_idpoly[Scen_i]
        )] <- 'id_poly'
      } else {
        shapefile_data_baseid$id_poly <- "polyA"
      }
      shp_poly_data <- shapefile_data_baseid %>%
        dplyr::mutate(
          id = ifelse(
            is.na(Scenario$Colname_idpoly[Scen_i]),
            "A",
            Scenario$Colname_idpoly[Scen_i]
          )
        ) %>%
        dplyr::mutate(Poly = paste0('poly', as.character(id)))
      shp_poly_data <- shp_poly_data %>%
        dplyr::full_join(Results_Cw, by = "Poly") %>%
        dplyr::group_by(Poly, An) %>%
        dplyr::summarise(Irrigation = sum(Irrigation_m3))
      val_min2 <- min(shp_poly_data$Irrigation, na.rm = T)
      val_max2 <- max(shp_poly_data$Irrigation, na.rm = T)
    }

    cli::cli_progress_bar(
      format = "{pb_spin} Graphiques annuels : Year {pb_current}/{pb_total} | ETA: {pb_eta}",
      format_done = "{col_green(symbol$tick)} Graphiques annuels réalisés en {pb_elapsed}",
      total = length(unique(Results_Cw$An)),
      clear = FALSE
    )
    for (year in unique(Results_Cw$An)) {
      cli::cli_progress_update()
      Plots_year <- st_read(paste0(
        Path_Run_chargementdata,
        '/',
        Scen_i,
        '_',
        Scen,
        '/Plots_all_RADIS_information_',
        year,
        '.shp'
      )) #Download plots location for years
      centre <- st_centroid(Plots_year)
      if (Scenario$Plots_RADIS_RPG[Scen_i] == "Non_existant_plots") {
        RadN_illustration <- st_read(paste0(
          Path_Run_chargementdata,
          '/',
          Scen_i,
          '_',
          Scen,
          '/Plots_location_notcenteredforillustration.shp'
        ))
        Plots_year <- RadN_illustration
      }
      if (
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Cartographic description of the territory"
        )]) ==
          "YES"
      ) {
        ggplot() +
          geom_sf(
            data = subset(
              centre,
              Scenario$Plots_RADIS_RPG[Scen_i] == "Non_existant_plots"
            ),
            shape = 3,
            fill = "black"
          ) +
          geom_sf(data = grille_safran_data, fill = NA, col = 'black') +
          geom_sf(
            data = shapefile_data_base,
            fill = alpha("grey", 0.4),
            col = 'black'
          ) +
          geom_sf(
            data = Plots_year,
            aes(fill = as.factor(cod_clt)),
            color = "black",
            size = 0.2
          ) +
          labs(
            title = paste0("Carte du parcellaire ", year),
            fill = "Cultures "
          ) +
          theme_minimal()
        ggsave(file.path(
          Simulation_run_path_graph,
          sprintf("Carte_parcelles_cultures_avec_grille_%d.png", year)
        ))

        ggplot() +
          geom_sf(
            data = subset(
              centre,
              Scenario$Plots_RADIS_RPG[Scen_i] == "Non_existant_plots"
            ),
            shape = 3,
            fill = "black"
          ) +
          geom_sf(
            data = shapefile_data_base,
            fill = alpha("grey", 0.4),
            col = 'black'
          ) +
          geom_sf(
            data = Plots_year,
            aes(fill = as.factor(cod_clt)),
            color = "black",
            size = 0.2
          ) +
          labs(
            title = paste0("Carte du parcellaire ", year),
            fill = "Cultures "
          ) +
          theme_minimal()
        ggsave(file.path(
          Simulation_run_path_graph,
          sprintf("Carte_parcelles_cultures_%d.png", year)
        ))
      }

      #Graphiques cartes mailles, sous maille et polygones
      #maille
      grille_safran_data_y <- grille_safran_data %>%
        dplyr::filter(An == year)
      Plots_year <- st_read(paste0(
        Path_Run_chargementdata,
        '/',
        Scen_i,
        '_',
        Scen,
        '/Plots_all_RADIS_information_',
        year,
        '.shp'
      )) #Download plots location for years
      centre <- st_centroid(Plots_year)
      if (Scenario$Plots_RADIS_RPG[Scen_i] == "Non_existant_plots") {
        RadN_illustration <- st_read(paste0(
          Path_Run_chargementdata,
          '/',
          Scen_i,
          '_',
          Scen,
          '/Plots_location_notcenteredforillustration.shp'
        ))
        Plots_year <- RadN_illustration
      }
      if (
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Cartographic results on the territory"
        )]) ==
          "YES" &
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Detailed graph by grid"
          )]) ==
            "YES"
      ) {
        ggplot() +
          geom_sf(
            data = grille_safran_data_y,
            aes(fill = Irrigation),
            color = "black",
            size = 0.2
          ) +
          geom_sf(data = shapefile_data_base, fill = NA, col = 'black') +
          geom_sf(data = Plots_year, fill = "grey") +
          geom_sf(
            data = subset(
              centre,
              Scenario$Plots_RADIS_RPG[Scen_i] == "Non_existant_plots"
            ),
            shape = 3,
            fill = "black"
          ) +
          scale_fill_gradient(
            low = "chartreuse3",
            limits = c(val_min, val_max),
            high = "cyan3",
            na.value = "transparent"
          ) +
          labs(
            title = paste0("Carte des mailles simulées ", year),
            fill = paste0("Irrigation m³ annuelle - ", year)
          ) +
          theme_minimal()
        ggsave(paste0(Simulation_run_path_graph, "/Map_network", year, '.png'))
      }
      val_min_mm <- min(
        (grille_safran_data_y %>%
          dplyr::filter(Irrigation_mm > 0))$Irrigation_mm,
        na.rm = T
      )
      val_max_mm <- max(
        (grille_safran_data_y %>%
          dplyr::filter(Irrigation_mm > 0))$Irrigation_mm,
        na.rm = T
      )
      if (
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Cartographic results on the territory"
        )]) ==
          "YES" &
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Detailed graph by grid"
          )]) ==
            "YES"
      ) {
        ggplot() +
          geom_sf(
            data = st_as_sf(grille_safran_data_y),
            aes(fill = Irrigation_mm),
            color = "black",
            size = 0.2
          ) +
          geom_sf(data = Plots_year, fill = NA, col = 'black') +
          geom_sf(
            data = subset(
              centre,
              Scenario$Plots_RADIS_RPG[Scen_i] == "Non_existant_plots"
            ),
            shape = 3,
            fill = "black"
          ) +
          # geom_sf(data=Plots_year,fill="grey") +
          scale_fill_gradient(
            low = "chartreuse3",
            limits = c(val_min_mm, val_max_mm),
            high = "cyan3",
            na.value = "transparent"
          ) +
          labs(
            title = paste0("Carte des mailles simulées ", year),
            fill = paste0("Irrigation annuelle mm moyens - ", year)
          ) +
          theme_minimal()
        ggsave(paste0(
          Simulation_run_path_graph,
          "/Map_network_mm_",
          year,
          '.png'
        ))
      }

      #sous maille
      grille_safran_data_y_ssm <- grille_safran_data_ssmail %>%
        dplyr::filter(An == year) %>%
        dplyr::filter(Irrigation > 1)

      if (
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Cartographic results on the territory"
        )]) ==
          "YES" &
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Detailed graph by subgrid"
          )]) ==
            "YES"
      ) {
        ggplot() +
          geom_sf(
            data = st_as_sf(grille_safran_data_y_ssm),
            aes(fill = Irrigation),
            color = "black",
            size = 0.2
          ) +
          geom_sf(data = shapefile_data_base, fill = NA, col = 'black') +
          geom_sf(data = Plots_year, fill = alpha("grey", 0.4)) +
          geom_sf(
            data = subset(
              centre,
              Scenario$Plots_RADIS_RPG[Scen_i] == "Non_existant_plots"
            ),
            shape = 3,
            fill = "black"
          ) +
          scale_fill_gradient(
            low = "chartreuse3",
            limits = c(val_min_ssma, val_max_ssma),
            high = "cyan3",
            na.value = "transparent",
            labels = scales::label_number()
          ) +
          labs(
            title = paste0("Carte des sous-mailles simulées ", year),
            fill = paste0("Irrigation m³ annuelle - ", year)
          ) +
          theme_minimal()
        ggsave(paste0(
          Simulation_run_path_graph,
          "/Map_subnetwork",
          year,
          '.png'
        ))
      }
      val_min_mm_ssma <- min(grille_safran_data_y_ssm$Irrigation_mm, na.rm = T)
      val_max_mm_ssma <- max(grille_safran_data_y_ssm$Irrigation_mm, na.rm = T)
      if (
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Cartographic results on the territory"
        )]) ==
          "YES" &
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Detailed graph by subgrid"
          )]) ==
            "YES"
      ) {
        ggplot() +
          geom_sf(
            data = st_as_sf(grille_safran_data_y_ssm),
            aes(fill = Irrigation_mm),
            color = "black",
            size = 0.2
          ) +
          geom_sf(data = shapefile_data_base, fill = NA, col = 'black') +
          geom_sf(data = Plots_year, fill = alpha("grey", 0.4)) +
          geom_sf(
            data = subset(
              centre,
              Scenario$Plots_RADIS_RPG[Scen_i] == "Non_existant_plots"
            ),
            shape = 3,
            fill = "black"
          ) +
          scale_fill_gradient(
            low = "chartreuse3",
            limits = c(val_min_mm_ssma, val_max_mm_ssma),
            high = "cyan3",
            na.value = "transparent"
          ) +
          labs(
            title = paste0("Carte des sous-mailles simulées ", year),
            fill = paste0("Irrigation annuelle mm moyens - ", year)
          ) +
          theme_minimal()
        ggsave(paste0(
          Simulation_run_path_graph,
          "/Map_network_mm_",
          year,
          '.png'
        ))
      }
      #polygones
      if (
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Cartographic results on the territory"
        )]) ==
          "YES" &
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Detailed graph by polygon"
          )]) ==
            "YES"
      ) {
        if (length(rownames(shapefile_data_base)) > 0) {
          shp_poly_data_y <- shp_poly_data %>% dplyr::filter(An == year)
          ggplot() +
            geom_sf(
              data = shp_poly_data_y,
              aes(fill = Irrigation),
              color = "black",
              size = 0.2
            ) +
            geom_sf(
              data = shapefile_data_base,
              aes(fill = shp_poly_data_y$Irrigation),
              col = 'black'
            ) +
            geom_sf(data = Plots_year, fill = alpha("grey", 0.2)) +
            scale_fill_gradient(
              low = "chartreuse3",
              limits = c(val_min2, val_max2),
              high = "cyan3",
              na.value = "transparent"
            ) +
            labs(
              title = paste0("Carte des mailles simulées ", year),
              fill = paste0("Irrigation m³ annuelle - ", year)
            ) +
            theme_minimal()
          ggsave(paste0(
            Simulation_run_path_graph,
            "/Map_polygons",
            year,
            '.png'
          ))
        }
      }

      #ETc
      Results_Cw_y <- Results_Cw %>%
        dplyr::filter(An == year) %>%
        dplyr::group_by(CODE_CU, Mois, An) %>%
        dplyr::summarise(
          daily_ETc_mean = mean(ETc_mean, na.rm = T),
          daily_ETc_sum = sum(ETc_m3, na.rm = T)
        )

      #crop <- unique(Results_Cw$CODE_CU)[1]
      for (crop in unique(Results_Cw_y$CODE_CU)) {
        Results_Cw_y_c <- Results_Cw_y %>% dplyr::filter(CODE_CU == crop)
        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Mensual visualisation of results"
          )]) ==
            "YES" &
            toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs == "Detailed graph by crops"
            )]) ==
              "YES"
        ) {
          ggplot(Results_Cw_y_c) +
            geom_bar(
              stat = 'identity',
              aes(x = Mois, y = daily_ETc_mean),
              fill = 'darkgreen'
            ) +
            scale_x_continuous(
              year,
              breaks = seq(
                min(Results_Cw_y_c$Mois),
                max(Results_Cw_y_c$Mois),
                by = 1
              )
            ) +
            labs(
              title = paste0(year, ' - ', crop),
              y = "ETc mean (mm/day)",
              x = "Month"
            ) +
            theme_bw() +
            theme(
              legend.title = element_blank(),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 15),
              title = element_text(size = 20),
              legend.text = element_text(size = 15)
            )
          ggsave(
            paste0(
              Simulation_run_path_graph,
              "/Simulation_Cw_ETcmean_",
              year,
              '_',
              crop,
              '.png'
            ),
            width = 30,
            height = 20,
            units = 'cm'
          )

          ggplot(Results_Cw_y_c) +
            geom_bar(
              stat = 'identity',
              aes(x = Mois, y = daily_ETc_sum),
              fill = 'darkgreen'
            ) +
            scale_x_continuous(
              year,
              breaks = seq(
                min(Results_Cw_y_c$Mois),
                max(Results_Cw_y_c$Mois),
                by = 1
              )
            ) +
            labs(
              title = paste0(year, ' - ', crop),
              y = "ETc sum (m3)",
              x = "Month"
            ) +
            theme_bw() +
            theme(
              legend.title = element_blank(),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 15),
              title = element_text(size = 20),
              legend.text = element_text(size = 15)
            )
          ggsave(
            paste0(
              Simulation_run_path_graph,
              "/Simulation_Cw_ETcsum_",
              year,
              '_',
              crop,
              '.png'
            ),
            width = 30,
            height = 20,
            units = 'cm'
          )
        }
      }
    } # year loop end
    cli::cli_progress_done()

    #Irrigation pour chaque culture

    # Crop <- unique(Results_Cw$CODE_CU)[1]
    if (
      toupper(Wanted_output$Wanted[which(
        Wanted_output$Outputs == "Annual visualisation of results"
      )]) ==
        "YES" &
        toupper(Wanted_output$Wanted[which(
          Wanted_output$Outputs == "Detailed graph by crops"
        )]) ==
          "YES"
    ) {
      crops <- unique(Results_Cw$CODE_CU)
      progress_crops <- cli_progress_bar(
        format = "Crop {pb_current}/{pb_total}: {pb_status}",
        total = length(crops),
        clear = FALSE
      )
      for (Crop in crops) {
        cli_progress_update(id = progress_crops, status = Crop)
        Results_Cw_crop <- Results_Cw %>%
          dplyr::filter(CODE_CU == Crop) %>%
          dplyr::group_by(An, Poly, Mail, ssMail) %>%
          dplyr::summarise(Irrigation_mm = sum(irrigation_mm))

        Results_Cw_crop_an <- Results_Cw_crop %>%
          dplyr::group_by(An, Poly) %>%
          dplyr::summarise(Irrigation_mm = mean(Irrigation_mm))

        ggplot(data = Results_Cw_crop_an) +
          geom_bar(
            stat = 'identity',
            aes(x = as.factor(An), y = Irrigation_mm, fill = Poly)
          ) +
          ggtitle(Crop) +
          scale_x_discrete('An') +
          scale_y_continuous('Irrigation mm') +
          theme_bw()
        ggsave(
          paste0(
            Simulation_run_path_graph,
            "/Simulation_Cw_Irrigation_",
            crop,
            '_poly.png'
          ),
          width = 30,
          height = 20,
          units = 'cm'
        )
      }
      cli_progress_done(id = progress_crops)
    }
    #planche de barplots par culture
    Resultats_culture_mm <- Results_Cw %>%
      dplyr::group_by(CODE_CU, An, ssMail) %>%
      dplyr::summarise(
        Irrigation_m3 = sum(Irrigation_m3, na.rm = T),
        Surface = mean(surf_tot, na.rm = T),
        irrigation_mm = sum(irrigation_mm)
      ) %>%
      dplyr::group_by(CODE_CU, An) %>%
      dplyr::summarise(
        Irrigation_m3 = sum(Irrigation_m3, na.rm = T),
        Surface = sum(Surface, na.rm = T),
        Irrigation_mm = mean(irrigation_mm)
      ) %>%
      dplyr::mutate(Index = as.character(An))

    # ggplot(Resultats_culture_mm, aes(x = as.factor(Index), y = Irrigation_mm, fill = as.factor(substr(Index,4,4)))) +
    #   geom_bar(stat = "identity", position = "dodge") +
    #   labs(x = "Culture", y = "Irrigation (mm)", fill = "Année") +
    #   theme_bw() +
    #   facet_wrap(~CODE_CU, scales = "free_x") + theme(axis.text.x=element_text(angle=90),legend.position="none")
    # ggsave(paste0(Simulation_run_path_graph,"/Simulation_Irrigation_planche_parculture_mail.png"))

    if (!is.na(Scenario$Cultural_validation_link[Scen_i])) {
      Liste_c <- unique(Resultats_culture_mm$CODE_CU)

      Validation_crops <- st_read(FpCAWET(
        Working_path,
        Scenario$Cultural_validation_link[Scen_i]
      ))
      if ("Crop" %in% colnames(Validation_crops)) {
        Validation_crops <- Validation_crops %>%
          dplyr::rename('CODE_CU' = 'Crop')
      }

      if (
        (!"CODE_CU" %in% colnames(Validation_crops)) &
          ("Crop" %in% colnames(Validation_crops))
      ) {
        Validation_crops <- Validation_crops %>%
          dplyr::rename("CODE_CU" = "Crop")
      } else {
        if (
          (!"CODE_CU" %in% colnames(Validation_crops)) &
            (!"Crop" %in% colnames(Validation_crops))
        ) {
          message("Warning, wrong colname in Validation table")
        }
      }

      if (length(Liste_c[which(!Liste_c %in% Validation_crops$CODE_CU)]) > 0) {
        Liste_c_nonval <- data.frame(
          CODE_CU = Liste_c[which(!Liste_c %in% Validation_crops$CODE_CU)],
          Irrigation_mm = NA
        )
      } else {
        Liste_c_nonval = data.frame(CODE_CU = NA, Irrigation_mm = NA)
      }
      if (length(Validation_crops$Year) == 0) {
        Validation_crops <- Validation_crops %>%
          dplyr::filter(CODE_CU %in% Liste_c) %>%
          bind_rows(Liste_c_nonval) %>%
          dplyr::mutate(Index = "Validation")
      }
      if (length(Validation_crops$Year) > 0) {
        Validation_crops <- Validation_crops %>%
          dplyr::filter(CODE_CU %in% Liste_c) %>%
          dplyr::mutate('Index' = paste0(Year, '_Validation'))
      }

      Resultats_culture_mm_plots <- Resultats_culture_mm %>%
        bind_rows(Validation_crops) %>%
        dplyr::mutate(
          Irrigation_ref = ifelse(
            str_detect(Index, 'Validation'),
            Irrigation_mm,
            NA
          )
        ) %>%
        dplyr::filter(!is.na(CODE_CU))

      if (sum(Resultats_culture_mm_plots$Irrigation_ref, na.rm = T) > 0) {
        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Annual visualisation of results"
          )]) ==
            "YES" &
            toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs == "Detailed graph by crops"
            )]) ==
              "YES"
        ) {
          ggplot(
            Resultats_culture_mm_plots,
            aes(
              x = as.factor(Index),
              y = Irrigation_mm,
              fill = as.factor(substr(Index, 4, 4))
            )
          ) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_hline(
              aes(yintercept = Irrigation_ref),
              color = "red",
              linetype = "dashed"
            ) +
            labs(x = "Culture", y = "Irrigation (mm)", fill = "Année") +
            theme_minimal() +
            facet_wrap(~CODE_CU, scales = "free_x") +
            theme(
              axis.text.x = element_text(angle = 90),
              legend.position = "none"
            )
          ggsave(paste0(
            Simulation_run_path_graph,
            "/Validation_simulation_Irrigation_planche_parculture_mail.png"
          ))
        }

        if (!"Year" %in% colnames(Validation_crops)) {
          Validation_crops2 <- Validation_crops %>%
            dplyr::select(c(CODE_CU, Irrigation_mm)) %>%
            dplyr::rename("Irrigation_comp" = "Irrigation_mm")
        }
        if ("Year" %in% colnames(Validation_crops)) {
          Validation_crops2 <- Validation_crops %>%
            dplyr::select(c(CODE_CU, Year, Irrigation_mm)) %>%
            dplyr::rename("An" = "Year", "Irrigation_comp" = "Irrigation_mm")
        }
        Res_cult_comp <- Resultats_culture_mm %>%
          dplyr::full_join(Validation_crops2, by = c("CODE_CU", "An")) %>%
          dplyr::mutate(An = as.character(An))

        ggplot(data = Res_cult_comp) +
          geom_point(
            aes(y = Irrigation_mm, x = Irrigation_comp, col = CODE_CU),
            size = 4
          ) +
          geom_abline(slope = 1, intercept = 0) +
          ggtitle("Comparison graph between calculation and comparison data") +
          scale_y_continuous(
            name = 'Calculated data (mm)',
            limits = c(
              min(
                c(Res_cult_comp$Irrigation_mm, Res_cult_comp$Irrigation_comp),
                na.rm = T
              ) -
                1,
              max(
                c(Res_cult_comp$Irrigation_mm, Res_cult_comp$Irrigation_comp),
                na.rm = T
              ) +
                100
            )
          ) +
          scale_x_continuous(
            name = 'Comparison data (mm)',
            limits = c(
              min(
                c(Res_cult_comp$Irrigation_mm, Res_cult_comp$Irrigation_comp),
                na.rm = T
              ) -
                1,
              max(
                c(Res_cult_comp$Irrigation_mm, Res_cult_comp$Irrigation_comp),
                na.rm = T
              ) +
                100
            )
          ) +
          theme_bw() +
          theme(
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 15)
          )
        ggsave(
          paste0(
            Simulation_run_path_graph,
            '/Comparatif_graph_calculated_comparison_data_forcrops_inmm',
            '.png'
          ),
          width = 30,
          height = 30,
          units = 'cm'
        )

        if (
          toupper(Wanted_output$Wanted[which(
            Wanted_output$Outputs == "Annual visualisation of results"
          )]) ==
            "YES"
        ) {
          ggplot(data = Res_cult_comp) +
            geom_point(
              aes(y = Irrigation_mm, x = Irrigation_comp, col = An),
              size = 4
            ) +
            geom_abline(slope = 1, intercept = 0) +
            ggtitle(
              "Comparison graph between calculation and comparison data"
            ) +
            scale_y_continuous(
              name = 'Calculated data (mm)',
              limits = c(
                min(
                  c(Res_cult_comp$Irrigation_mm, Res_cult_comp$Irrigation_comp),
                  na.rm = T
                ) -
                  1,
                max(
                  c(Res_cult_comp$Irrigation_mm, Res_cult_comp$Irrigation_comp),
                  na.rm = T
                ) +
                  100
              )
            ) +
            scale_x_continuous(
              name = 'Comparison data (mm)',
              limits = c(
                min(
                  c(Res_cult_comp$Irrigation_mm, Res_cult_comp$Irrigation_comp),
                  na.rm = T
                ) -
                  1,
                max(
                  c(Res_cult_comp$Irrigation_mm, Res_cult_comp$Irrigation_comp),
                  na.rm = T
                ) +
                  100
              )
            ) +
            theme_bw() +
            theme(
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 15)
            )
          ggsave(
            paste0(
              Simulation_run_path_graph,
              '/Comparatif_graph_calculated_comparison_data_forcropsandyears_inmm',
              '.png'
            ),
            width = 30,
            height = 30,
            units = 'cm'
          )
        }
      }
    }

    #carte resultuat par parcelle
    # year <- unique(Results_Cw$An)[1]
    if (
      toupper(Wanted_output$Wanted[which(
        Wanted_output$Outputs == "Cartographic results on the territory"
      )]) ==
        "YES"
    ) {
      for (year in unique(Results_Cw$An)) {
        message(
          "Carte finale irrigation par culture calquee sur les parcelles - ",
          year
        )
        Tab_res_reduit <- Results_Cw %>%
          dplyr::filter(An == year) %>%
          dplyr::mutate(
            id_poly = as.character(substr(Poly, 5, nchar(Poly))),
            cod_clt = CODE_CU,
            id_mail = substr(Mail, 5, nchar(Mail)),
            year = as.character(An)
          ) %>%
          dplyr::group_by(cod_clt, id_poly, id_mail, year, ssMail) %>%
          dplyr::summarise(
            surf_tot = mean(surf_tot),
            irrigation_m3 = sum(Irrigation_m3),
            irrigation_mm = sum(irrigation_mm)
          ) %>%
          dplyr::filter(surf_tot > 0)
        Plots_year <- st_read(paste0(
          Path_Run_chargementdata,
          '/',
          Scen_i,
          '_',
          Scen,
          '/Plots_all_RADIS_information_',
          year,
          '.shp'
        )) %>% #Download plots location for years
          dplyr::mutate(
            id_poly = as.character(id_poly),
            year = as.character(.env$year)
          )
        centre <- st_centroid(Plots_year)
        if (Scenario$Plots_RADIS_RPG[Scen_i] == "Non_existant_plots") {
          RadN_illustration <- st_read(paste0(
            Path_Run_chargementdata,
            '/',
            Scen_i,
            '_',
            Scen,
            '/Plots_location_notcenteredforillustration.shp'
          ))
          Plots_year <- RadN_illustration %>%
            dplyr::mutate(
              id_poly = as.character(id_poly),
              year = as.character(.env$year)
            )
        }
        Plots_sf_avecres <- Plots_year %>%
          dplyr::full_join(Tab_res_reduit, by = join_by(cod_clt, id_poly, year))
        ggplot(data = Plots_sf_avecres) +
          geom_sf(aes(fill = irrigation_mm), color = NA, size = 0.2) +
          geom_sf(
            data = subset(
              centre,
              Scenario$Plots_RADIS_RPG[Scen_i] == "Non_existant_plots"
            ),
            shape = 3,
            fill = "black"
          ) +
          labs(
            title = paste0(
              "Average irrigation for plots according to crops in ",
              year
            ),
            fill = "Average irrigation for crops (mm)"
          ) +
          scale_fill_gradient(
            low = "cyan",
            high = "darkblue",
            na.value = "lightgrey"
          ) +
          theme_minimal()
        ggsave(
          paste0(
            Simulation_run_path_graph,
            '/Carte parcellaire des consommations ',
            year,
            '.png'
          ),
          width = 30,
          height = 30,
          units = 'cm'
        )
      }
    }
  } #end loop scenarii
  cli_alert_success(
    "run_script3_1 - Graphiques de la simulation des scenarios CAWET - execute avec succes"
  )
}
