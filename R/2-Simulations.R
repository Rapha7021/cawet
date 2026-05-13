#' Run Script 2 - Simulations for crop water management modeling
#'
#' This function runs the simulation (script 2) for crop water management modeling.
#' It performs the following tasks:
#' - Sets up a portable R library for package management.
#' - Loads required R packages.
#' - Reads scenario configurations and input data.
#' Processes simulations for different crops, polygons, and irrigation methods.
#' - Saves detailed output results and generates verification graphs.
#' - Handles missing crop parameter warnings.
#' @inheritParams run
#' @return None. The function saves output files to the specified directories.
#' @export
#'
run_script2 <- function(
  cfgRun = get_Run_Config(Working_path = Working_path)
) {
  cli_alert_info(
    "Demarrage run_script2 - Simulations pour la modelisation de la gestion de l'eau aux cultures"
  )

  Working_path <- cfgRun$Working_path
  Path_Run <- cfgRun$Path_Run
  Path_Run_chargementdata <- cfgRun$Path_Run_Chargementdata
  Scenario <- cfgRun$Scenario

  liste_not_parametredcrops <- c()

  for (Scen in Scenario$Scenario) {
    message("Démarrage simulation Scenario : ", Scen)
    Scen_i <- which(Scenario$Scenario == Scen)
    Path_scen <- paste0(Path_Run_chargementdata, '/', Scen_i, '_', Scen)
    Path_scen_meteo <- paste0(Path_scen, '/Meteo')

    #saving place creation
    Path_Run_results <- paste0(Path_Run, '/Results')
    if (!file.exists(Path_Run_results)) {
      dir.create(Path_Run_results)
    }
    Path_Run_results_scen <- paste0(Path_Run_results, '/', Scen_i, '_', Scen)
    if (!file.exists(Path_Run_results_scen)) {
      dir.create(Path_Run_results_scen)
    }
    Path_Run_results_scen_detail <- paste0(Path_Run_results_scen, '/', 'detail')
    if (!file.exists(Path_Run_results_scen_detail)) {
      dir.create(Path_Run_results_scen_detail)
    }

    Wanted_output <- read_CAWET_data(
      Working_path = Working_path,
      file = paste0(Path_scen, '/Wanted_outputs.csv')
    )

    # Irrigation parameters
    Irrigation_param <- read_CAWET_data(
      Working_path = Working_path,
      file = Scenario$Pratique_irrigation_param[Scen_i]
    )

    # Model crop parameters
    Crops_param <- read_CAWET_data(
      Working_path = Working_path,
      file = Scenario$Model_param_crop[Scen_i]
    )

    # Link RPG to parameters
    Link_RPG_mod <- read_CAWET_data(
      Working_path = Working_path,
      file = Scenario$Model_link_RPG_croparam[Scen_i]
    )
    write.csv2(Link_RPG_mod, paste0(Path_scen, '/Param_crop_to_RPG.csv'))

    Surf_table <- read_CAWET_data(
      Working_path = Working_path,
      file = paste0(Path_scen, '/Surf_table_', Scen, '.csv')
    ) |>
      dplyr::mutate(
        date = ifelse(
          str_detect(date, '/'),
          format(
            as.Date(
              paste0(
                substr(date, 7, 10),
                '-',
                substr(date, 4, 5),
                '-',
                substr(date, 1, 2)
              ),
              format = '%Y-%m-%d'
            ),
            '%Y-%m-%d'
          ),
          format(as.Date(date, fomat = '%Y-%m-%d'), '%Y-%m-%d')
        )
      )
    Prof_table <- read_CAWET_data(
      Working_path = Working_path,
      file = paste0(Path_scen, '/Prof_table_', Scen, '.csv')
    ) |>
      dplyr::mutate(
        date = ifelse(
          str_detect(date, '/'),
          format(
            as.Date(
              paste0(
                substr(date, 7, 10),
                '-',
                substr(date, 4, 5),
                '-',
                substr(date, 1, 2)
              ),
              format = '%Y-%m-%d'
            ),
            '%Y-%m-%d'
          ),
          format(as.Date(date, fomat = '%Y-%m-%d'), '%Y-%m-%d')
        )
      )
    AWC_table <- read_CAWET_data(
      Working_path = Working_path,
      file = paste0(Path_scen, '/AWC_table_', Scen, '.csv')
    ) |>
      dplyr::mutate(
        date = ifelse(
          str_detect(date, '/'),
          format(
            as.Date(
              paste0(
                substr(date, 7, 10),
                '-',
                substr(date, 4, 5),
                '-',
                substr(date, 1, 2)
              ),
              format = '%Y-%m-%d'
            ),
            '%Y-%m-%d'
          ),
          format(as.Date(date, fomat = '%Y-%m-%d'), '%Y-%m-%d')
        )
      )
    if (Scenario$uses_textures[Scen_i]) {
      Arg_mean_table <- read_CAWET_data(
        Working_path = Working_path,
        file = paste0(Path_scen, '/Arg_mean_table_', Scen, '.csv')
      ) |>
        dplyr::mutate(
          date = ifelse(
            str_detect(date, '/'),
            format(
              as.Date(
                paste0(
                  substr(date, 7, 10),
                  '-',
                  substr(date, 4, 5),
                  '-',
                  substr(date, 1, 2)
                ),
                format = '%Y-%m-%d'
              ),
              '%Y-%m-%d'
            ),
            format(as.Date(date, fomat = '%Y-%m-%d'), '%Y-%m-%d')
          )
        )
      Lim_mean_table <- read_CAWET_data(
        Working_path = Working_path,
        file = paste0(Path_scen, '/Lim_mean_table_', Scen, '.csv')
      ) |>
        dplyr::mutate(
          date = ifelse(
            str_detect(date, '/'),
            format(
              as.Date(
                paste0(
                  substr(date, 7, 10),
                  '-',
                  substr(date, 4, 5),
                  '-',
                  substr(date, 1, 2)
                ),
                format = '%Y-%m-%d'
              ),
              '%Y-%m-%d'
            ),
            format(as.Date(date, fomat = '%Y-%m-%d'), '%Y-%m-%d')
          )
        )
      Sab_mean_table <- read_CAWET_data(
        Working_path = Working_path,
        file = paste0(Path_scen, '/Sab_mean_table_', Scen, '.csv')
      ) |>
        dplyr::mutate(
          date = ifelse(
            str_detect(date, '/'),
            format(
              as.Date(
                paste0(
                  substr(date, 7, 10),
                  '-',
                  substr(date, 4, 5),
                  '-',
                  substr(date, 1, 2)
                ),
                format = '%Y-%m-%d'
              ),
              '%Y-%m-%d'
            ),
            format(as.Date(date, fomat = '%Y-%m-%d'), '%Y-%m-%d')
          )
        )
    }
    Liste_simulations <- colnames(Surf_table)[3:length(colnames(Surf_table))]
    progress_simulations <- cli_progress_bar(
      format = "{pb_spin} Simulation {Crop} - {Poly} - {Mail} - {ssMail_num} - {mod_irr} | ETA: {pb_eta}",
      format_done = "{col_green(symbol$tick)} Run {pb_total} simulations in {pb_elapsed}",
      total = length(Liste_simulations) * nrow(Irrigation_param),
      clear = FALSE
    )

    #Simulation <- Liste_simulations[1]
    for (Simulation in Liste_simulations) {
      Crop <- unlist(str_split(Simulation, pattern = '_'))[1]
      Poly <- unlist(str_split(Simulation, pattern = '_'))[2]
      Poly_num <- substr(Poly, 5, 1000)
      Mail <- unlist(str_split(Simulation, pattern = '_'))[3]
      Mail_num <- substr(Mail, 7, 1000)
      ssMail_num <- paste0(
        Mail_num,
        '_',
        unlist(str_split(Simulation, pattern = '_'))[4]
      )

      if (Scenario$Climat_data_unique_or_network[Scen_i] == "Network") {
        Meteo_maille <- read_CAWET_data(
          Working_path = Working_path,
          file = paste0(Path_scen_meteo, '/Meteo_maille', Mail_num, '.csv')
        )
      }
      if (Scenario$Climat_data_unique_or_network[Scen_i] == "Unique") {
        if (Scenario$Climat_data_source[Scen_i] == "Safran") {
          Meteo_maille <- read_CAWET_data(
            Working_path = Working_path,
            file = paste0(Path_scen_meteo, '/Meteo_safran_mean.csv')
          )
        }
        if (Scenario$Climat_data_source[Scen_i] == "Drias") {
          Meteo_maille <- read_CAWET_data(
            Working_path = Working_path,
            file = paste0(Path_scen_meteo, '/Meteo_drias_mean.csv')
          )
        }
        if (Scenario$Climat_data_source[Scen_i] == "Forced_otherclimaticdata") {
          Meteo_maille <- read_CAWET_data(
            Working_path = Working_path,
            file = paste0(Path_scen_meteo, '/Meteo.csv')
          )
        }
      }

      Surf_table_sim <- Surf_table[, c(
        1,
        which(colnames(Surf_table) == Simulation)
      )] %>%
        dplyr::mutate(date = as.Date(date))
      colnames(Surf_table_sim)[2] <- 'Surf'
      Prof_table_sim <- Prof_table[, c(
        1,
        which(colnames(Prof_table) == Simulation)
      )] %>%
        dplyr::mutate(date = as.Date(date))
      colnames(Prof_table_sim)[2] <- 'Prof'
      AWC_table_sim <- AWC_table[, c(
        1,
        which(colnames(AWC_table) == Simulation)
      )] %>%
        dplyr::mutate(date = as.Date(date))
      colnames(AWC_table_sim)[2] <- 'AWC'

      if (Scenario$uses_textures[Scen_i]) {
        Arg_mean_table_sim <- Arg_mean_table[, c(
          1,
          which(colnames(Arg_mean_table) == Simulation)
        )] %>%
          dplyr::mutate(date = as.Date(date))
        colnames(Arg_mean_table_sim)[2] <- 'Arg_mean'
        Sab_mean_table_sim <- Sab_mean_table[, c(
          1,
          which(colnames(Sab_mean_table) == Simulation)
        )] %>%
          dplyr::mutate(date = as.Date(date))
        colnames(Sab_mean_table_sim)[2] <- 'Sab_mean'
        Lim_mean_table_sim <- Lim_mean_table[, c(
          1,
          which(colnames(Lim_mean_table) == Simulation)
        )] %>%
          dplyr::mutate(date = as.Date(date))
        colnames(Lim_mean_table_sim)[2] <- 'Lim_mean'
      }

      Information_table <- Surf_table_sim %>%
        dplyr::full_join(Prof_table_sim, by = "date") %>%
        dplyr::full_join(AWC_table_sim, by = "date") %>%
        dplyr::mutate(
          Surf = ifelse(is.na(Surf), 0, Surf),
          Prof = ifelse(is.na(Prof), 0, Prof),
          AWC = ifelse(is.na(AWC), 1, AWC)
        )
      if (Scenario$uses_textures[Scen_i]) {
        Information_table <- Information_table %>%
          dplyr::full_join(Arg_mean_table_sim, by = "date") %>%
          dplyr::full_join(Sab_mean_table_sim, by = "date") %>%
          dplyr::full_join(Lim_mean_table_sim, by = "date") %>%
          dplyr::mutate(
            Arg_mean = ifelse(is.na(Arg_mean), 33, Arg_mean),
            Sab_mean = ifelse(is.na(Sab_mean), 33, Sab_mean),
            Lim_mean = ifelse(is.na(Lim_mean), 33, Lim_mean)
          )
      }

      Sequences <- Information_table %>%
        arrange(date) %>% # Assurer l'ordre temporel
        mutate(
          change = (Surf != lag(Surf) | Prof != lag(Prof) | AWC != lag(AWC)), # TRUE si un changement #ulterieurement ajouter texture sol
          change = replace_na(change, TRUE), # La 1ère ligne est un nouveau groupe
          group = cumsum(change) # Identifiant de groupe constant entre 2 changements
        ) %>%
        dplyr::group_by(group, Surf, Prof, AWC) %>%
        dplyr::summarise(
          Start = min(date),
          End = max(date),
          # .groups = "drop"
        )

      for (mod_irr in Irrigation_param$Irri) {
        cli_progress_update(id = progress_simulations)

        Outputs_daily <- NULL
        for (Sequ in as.numeric(rownames(Sequences))) {
          #Period of irrigation interdiction
          carac_date_irr_df <- data.frame(
            carac_date_irr = as.Date(c(
              as.Date(
                paste0(Scenario$First_year_simulation[Scen_i], "-01-01"),
                format = "%Y-%m-%d"
              ):as.Date(
                paste0(Scenario$Last_year_simulation[Scen_i], "-12-31"),
                format = "%Y-%m-%d"
              )
            ))
          ) %>%
            dplyr::mutate(doy = as.numeric(lubridate::yday(carac_date_irr)))

          # Add interdiction periods
          if (!is.na(Scenario$Interdiction_usage_eau[Scen_i])) {
            Tab_interd_irr <- read_CAWET_data(
              Working_path = Working_path,
              file = Scenario$Interdiction_usage_eau[Scen_i]
            )

            if (any(Tab_interd_irr$Crop == "Territory")) {
              Tab_interdiction_terr <- Tab_interd_irr %>%
                dplyr::filter(Crop == "Territory")
              for (interd_i in as.numeric(rownames(Tab_interdiction_terr))) {
                if (
                  length(unlist(str_split(
                    Tab_interdiction_terr[interd_i, 2],
                    pattern = "_"
                  ))) ==
                    2 &
                    length(unlist(str_split(
                      Tab_interdiction_terr[interd_i, 3],
                      pattern = "_"
                    ))) ==
                      2
                ) {
                  #Cas d'interdiction sur des plages repetees chaques annees
                  doy_deb <- yday(as.Date(paste0(
                    "2021-",
                    substr(Tab_interdiction_terr[interd_i, 2], 4, 5),
                    "-",
                    substr(Tab_interdiction_terr[interd_i, 2], 1, 2)
                  )))
                  doy_fin <- yday(as.Date(paste0(
                    "2021-",
                    substr(Tab_interdiction_terr[interd_i, 3], 4, 5),
                    "-",
                    substr(Tab_interdiction_terr[interd_i, 3], 1, 2)
                  )))
                  carac_date_irr_df <- carac_date_irr_df %>%
                    dplyr::filter(doy < doy_deb | doy > doy_fin)
                } else {
                  if (
                    length(unlist(str_split(
                      Tab_interdiction_terr[interd_i, 2],
                      pattern = "_"
                    ))) ==
                      3 &
                      length(unlist(str_split(
                        Tab_interdiction_terr[interd_i, 3],
                        pattern = "_"
                      ))) ==
                        3
                  ) {
                    #Cas d'interdiction specifique a une annee
                    day_deb <- as.Date(paste0(
                      substr(Tab_interdiction_terr[interd_i, 2], 7, 10),
                      "-",
                      substr(Tab_interdiction_terr[interd_i, 2], 4, 5),
                      "-",
                      substr(Tab_interdiction_terr[interd_i, 2], 1, 2)
                    ))
                    day_fin <- as.Date(paste0(
                      substr(Tab_interdiction_terr[interd_i, 2], 7, 10),
                      "-",
                      substr(Tab_interdiction_terr[interd_i, 3], 4, 5),
                      "-",
                      substr(Tab_interdiction_terr[interd_i, 3], 1, 2)
                    ))
                    carac_date_irr_df <- carac_date_irr_df %>%
                      dplyr::filter(
                        carac_date_irr < day_deb | carac_date_irr > day_fin
                      )
                  }
                }
              }
            } # end territorial interdiction irrigation

            if (any(Tab_interd_irr$Crop == Crop)) {
              Tab_interdiction_crop <- Tab_interd_irr %>%
                dplyr::filter(Crop == Crop)
              for (interd_i in rownames(Tab_interdiction_crop)) {
                if (
                  length(unlist(str_split(
                    Tab_interdiction_crop[interd_i, 2],
                    pattern = "_"
                  ))) ==
                    2 &
                    length(unlist(str_split(
                      Tab_interdiction_crop[interd_i, 3],
                      pattern = "_"
                    ))) ==
                      2
                ) {
                  #Cas d'interdiction sur des plages repetees chaques annees
                  doy_deb <- yday(as.Date(paste0(
                    "2021-",
                    substr(Tab_interdiction_crop[interd_i, 2], 4, 5),
                    "-",
                    substr(Tab_interdiction_crop[interd_i, 2], 1, 2)
                  )))
                  doy_fin <- yday(as.Date(paste0(
                    "2021-",
                    substr(Tab_interdiction_crop[interd_i, 3], 4, 5),
                    "-",
                    substr(Tab_interdiction_crop[interd_i, 3], 1, 2)
                  )))
                  carac_date_irr_df <- carac_date_irr_df %>%
                    dplyr::filter(doy < doy_deb | doy > doy_fin)
                } else {
                  if (
                    length(unlist(str_split(
                      Tab_interdiction_crop[interd_i, 2],
                      pattern = "_"
                    ))) ==
                      3 &
                      length(unlist(str_split(
                        Tab_interdiction_crop[interd_i, 3],
                        pattern = "_"
                      ))) ==
                        3
                  ) {
                    #Cas d'interdiction specifique a une annee
                    day_deb <- as.Date(paste0(
                      substr(Tab_interdiction_crop[interd_i, 2], 7, 10),
                      "-",
                      substr(Tab_interdiction_crop[interd_i, 2], 4, 5),
                      "-",
                      substr(Tab_interdiction_crop[interd_i, 2], 1, 2)
                    ))
                    day_fin <- as.Date(paste0(
                      substr(Tab_interdiction_crop[interd_i, 2], 7, 10),
                      "-",
                      substr(Tab_interdiction_crop[interd_i, 3], 4, 5),
                      "-",
                      substr(Tab_interdiction_crop[interd_i, 3], 1, 2)
                    ))
                    carac_date_irr_df <- carac_date_irr_df %>%
                      dplyr::filter(
                        carac_date_irr < day_deb | carac_date_irr > day_fin
                      )
                  }
                }
              }
            } # end crop's interdiction irrigation
            carac_date_irr <- carac_date_irr_df$carac_date_irr
          }

          #Cropwat
          if (Scenario$Model[Scen_i] == 'CropWat') {
            cw_output <- Run_Cropwat_CAWET(
              Irrigation_param,
              mod_irr,
              Sequences[Sequ, , drop = FALSE],
              carac_date_irr,
              Link_RPG_mod,
              Crop,
              Crops_param,
              Meteo_maille,
              liste_not_parametredcrops
            )
            if (is.null(cw_output)) {
              next
            }
            cw_output$Surf <- Sequences$Surf[Sequ]
            Outputs_daily <- Outputs_daily %>% bind_rows(cw_output)
          } else if (Scenario$Model[Scen_i] == 'Aquacrop') {
            stop(
              "Modelisation Aquacrop en cours de developpement, non fonctionnelle pour le moment"
            )
            # Run_Aquacrop_CAWET(
            #   Irrigation_param = Irrigation_param,
            #   Scenario = Scenario
            # )
          } else if (Scenario$Model[Scen_i] == 'Optirrig') {
            stop(
              "Modelisation Optirrig en cours de developpement, non fonctionnelle pour le moment"
            )
            param_model <- read_CAWET_data(
              Working_path = Working_path,
              file = Scenario$Model_param_crop[Scen_i]
            )
          } else {
            stop(
              "Modèle ",
              Scenario$Model[Scen_i],
              " non reconnu, vérifier la colonne 'Model' du fichier ordonnanceur"
            )
          } #end model identification
        } #end sequence loop

        # Post-processing and saving results
        if (Scenario$Model[Scen_i] == 'CropWat' && !is.null(Outputs_daily)) {
          write.csv2(
            Outputs_daily,
            paste0(
              Path_Run_results_scen_detail,
              '/',
              Crop,
              '_',
              Poly,
              '_',
              Mail,
              '_',
              ssMail_num,
              '_',
              mod_irr,
              '.csv'
            ),
            row.names = F
          )

          #Saving graph of Kc and climate dynamic
          if (
            !file.exists(paste0(
              Path_Run_results_scen_detail,
              '/Graphical_verification'
            ))
          ) {
            dir.create(paste0(
              Path_Run_results_scen_detail,
              '/Graphical_verification'
            ))
          }
          Outputs_daily_water_graph <- Meteo_maille %>%
            dplyr::mutate(
              DatesR = as.Date(DATE),
              Rain = as.numeric(PRELIQ_Q)
            ) %>%
            dplyr::full_join(Outputs_daily, by = "DatesR") %>%
            dplyr::rename("Irrigation" = "Ir") %>%
            dplyr::select(DatesR, Rain, Irrigation) %>%
            tidyr::pivot_longer(
              !DatesR,
              names_to = "names",
              values_to = "values"
            )

          if (
            (toupper(Wanted_output$Wanted[which(
              Wanted_output$Outputs ==
                "Detailed ET0,Kc,Ks, Irrigation … results for each combination, period/crop/grid/irrigation method"
            )]) ==
              "YES") |
              (toupper(Wanted_output$Wanted[which(
                Wanted_output$Outputs ==
                  "Detailed ET0,Kc,Ks, Irrigation … results only once for each crop/irrigation method"
              )]) ==
                "YES" &
                Simulation %in%
                  Liste_simulations[
                    !duplicated(substr(Liste_simulations, 1, 3))
                  ])
          ) {
            ggplot2::ggplot() +
              ggplot2::geom_line(
                data = Meteo_maille,
                aes(
                  x = as.Date(DATE),
                  y = as.numeric(ETP_Q)
                ),
                col = "orange3"
              ) +
              ggplot2::geom_line(
                data = Outputs_daily,
                aes(x = DatesR, y = ETc_adj),
                col = "blue2"
              ) +
              ggplot2::geom_line(
                data = Outputs_daily,
                aes(x = DatesR, y = Ks * 8),
                col = "violet"
              ) +
              ggplot2::geom_line(
                data = Outputs_daily,
                aes(x = DatesR, y = Kc * 8),
                col = "black"
              ) +
              ggplot2::geom_bar(
                stat = 'identity',
                position = 'stack',
                data = Outputs_daily_water_graph,
                aes(x = DatesR, y = -values / 10, col = as.factor(names))
              ) +
              ggplot2::scale_y_continuous(
                name = "ET0 (orange) | ETc (blue)",
                breaks = seq(0, 100, by = 1),
                sec.axis = sec_axis(
                  trans = ~ . * -10,
                  name = "Water ingress : Rain (cyan) | Irrigation (red)",
                  breaks = seq(0, 1000, by = 20)
                )
              ) +
              ggplot2::scale_x_date(name = '') +
              ggplot2::ggtitle(paste0(
                Crop,
                ' - ',
                Poly,
                ' - ',
                ssMail_num,
                ' - ',
                mod_irr
              )) +
              theme_minimal() +
              theme(
                axis.title.y.left = element_text(
                  color = "black",
                  size = 13
                ),
                axis.title.y.right = element_text(
                  color = "black",
                  size = 13
                ),
                legend.position = 'null'
              ) +
              annotate(
                "segment",
                x = last(as.Date(Meteo_maille$DATE)) + 20,
                xend = last(as.Date(Meteo_maille$DATE)) + 20,
                y = 0,
                yend = 10,
                colour = "black",
                size = 0.8
              ) +
              annotate(
                "text",
                x = last(as.Date(Meteo_maille$DATE)) + 25,
                y = seq(0, 10, by = 2),
                label = c(0, 0.25, 0.5, 0.75, 1, 1.25),
                colour = "black",
                hjust = 0
              ) +
              annotate(
                "text",
                x = last(as.Date(Meteo_maille$DATE)) + 110,
                y = 5,
                label = "Kc (black) | Ks (violet)",
                angle = 90,
                colour = "black"
              )
            ggsave(paste0(
              Path_Run_results_scen_detail,
              '/Graphical_verification/Water_ingress_',
              Crop,
              '_',
              Poly,
              '_',
              Mail,
              '_',
              ssMail_num,
              '_',
              mod_irr,
              '_',
              '.png'
            ))
          }
        }
      } #end mod loop
    } #end simulation loop
    cli_progress_done(id = progress_simulations)

    liste_crops_missing <- unique(liste_not_parametredcrops)
    Read_me_info_miss <- data.frame(Missing_crops = liste_crops_missing)
    if (nrow(Read_me_info_miss) > 0) {
      write.csv2(
        Read_me_info_miss,
        paste0(Path_Run_results_scen_detail, '/Warning_missing_crops.csv'),
        row.names = F
      )
    }

    #Territorial information
    Liste_dir <- dir(Path_Run_results_scen_detail)
    Liste_dir <- Liste_dir[which(
      str_detect(Liste_dir, ".csv") & !str_detect(Liste_dir, "Warning")
    )]

    #Irrigation percentages territory
    Perc_irri <- read_CAWET_data(
      Working_path = Working_path,
      file = Scenario$Pratique_irrigation_table_percentages[Scen_i]
    )

    for (dir_outp in Liste_dir) {
      Cult <- unlist(str_split(dir_outp, '_'))[1]
      Irri_met <- substr(
        paste0(
          unlist(str_split(dir_outp, '_'))[6],
          '_',
          unlist(str_split(dir_outp, '_'))[7]
        ),
        1,
        nchar(paste0(
          unlist(str_split(dir_outp, '_'))[6],
          '_',
          unlist(str_split(dir_outp, '_'))[7]
        )) -
          4
      )
      Pol <- unlist(str_split(dir_outp, "_"))[str_detect(
        unlist(str_split(dir_outp, "_")),
        'poly'
      )]
      Ma <- unlist(str_split(dir_outp, "_"))[str_detect(
        unlist(str_split(dir_outp, "_")),
        'mail'
      )]
      ssMa <- paste0(
        unlist(str_split(dir_outp, '_'))[4],
        "_",
        unlist(str_split(dir_outp, '_'))[5]
      )

      if (
        length((Perc_irri |> dplyr::filter(CODE_CU == Cult))[, which(
          colnames(Perc_irri) == Irri_met
        )]) >
          0
      ) {
        Percentage <- (Perc_irri |> dplyr::filter(CODE_CU == Cult))[, which(
          colnames(Perc_irri) == Irri_met
        )]

        data <- read_CAWET_data(
          Working_path = Working_path,
          file = paste0(Path_Run_results_scen_detail, '/', dir_outp)
        ) |>
          dplyr::mutate(
            Mois = month(DatesR),
            An = year(DatesR),
            CODE_CU = Cult,
            Irrmet = Irri_met,
            Perc = Percentage,
            Poly = Pol,
            Mail = Ma,
            ssMail = ssMa,
            Surf_irrmet = (Perc * Surf) / 100,
            Irr_m3_irrmet = Surf_irrmet * Ir / 1000,
            ETc_m3_irrmet = Surf_irrmet * ETc_adj / 1000
          )

        if (dir_outp == dplyr::first(Liste_dir)) {
          data_join <- data
        }
        if (dir_outp != dplyr::first(Liste_dir)) {
          data_join <- data_join %>% bind_rows(data)
        }
      } else {
        #If non parametred crop
        message(
          "Culture : ",
          Cult,
          " no reference of percentages in the territory"
        )
        if (dir_outp == dplyr::first(Liste_dir)) {
          data_join <- data.frame(
            DatesR = NA,
            Mois = NA,
            An = NA,
            Ks = NA,
            ETc_adj = NA,
            Dr = NA,
            Ir = NA,
            Surf = NA,
            CODE_CU = NA,
            Irrmet = NA,
            Perc = NA,
            Poly = NA,
            Mail = NA,
            ssMail = NA,
            Surf_irrmet = NA,
            Irr_m3_irrmet = NA,
            ETc_m3_irrmet = NA
          )
        }
      }
    } #end daily aggregation data

    #joining data and mensualisation
    Poly_info_plusCCday <- data_join %>%
      dplyr::group_by(Poly, CODE_CU, DatesR, Mois, An) %>%
      dplyr::summarise(
        Irrigation_m3 = sum(Irr_m3_irrmet, na.rm = T),
        ETc_m3 = sum(ETc_m3_irrmet, na.rm = T),
        surf = sum(Surf_irrmet, na.rm = T)
      )
    Poly_info_plusCC <- Poly_info_plusCCday %>%
      dplyr::group_by(Poly, CODE_CU, Mois, An) %>%
      dplyr::summarise(
        Irrigation_m3 = sum(Irrigation_m3, na.rm = T),
        ETc_m3 = sum(ETc_m3, na.rm = T),
        surf_tot = mean(surf, na.rm = T)
      )
    Mail_info_plusCCday <- data_join %>%
      dplyr::group_by(Mail, ssMail, CODE_CU, Mois, An) %>%
      dplyr::summarise(
        Irrigation_m3 = sum(Irr_m3_irrmet, na.rm = T),
        ETc_m3 = sum(ETc_m3_irrmet, na.rm = T),
        surf = sum(Surf_irrmet, na.rm = T)
      )
    Mail_info_plusCC <- Mail_info_plusCCday %>%
      dplyr::group_by(Mail, ssMail, CODE_CU, Mois, An) %>%
      dplyr::summarise(
        Irrigation_m3 = sum(Irrigation_m3, na.rm = T),
        ETc_m3 = sum(ETc_m3, na.rm = T),
        surf_tot = mean(surf, na.rm = T)
      )
    Polymail_info_plusCCday <- data_join %>%
      dplyr::group_by(Poly, Mail, ssMail, CODE_CU, DatesR, Mois, An) %>%
      dplyr::summarise(
        Irrigation_m3 = sum(Irr_m3_irrmet, na.rm = T),
        ETc_m3 = sum(ETc_m3_irrmet, na.rm = T),
        ETc_mean = weighted.mean(ETc_adj, w = Surf_irrmet, na.rm = T),
        surf = sum(Surf_irrmet, na.rm = T),
        irrigation_mm = weighted.mean(Ir, w = Surf_irrmet, na.rm = T)
      )
    Polymail_info_plusCC <- Polymail_info_plusCCday %>%
      dplyr::group_by(Poly, Mail, ssMail, CODE_CU, Mois, An) %>%
      dplyr::summarise(
        Irrigation_m3 = sum(Irrigation_m3, na.rm = T),
        ETc_m3 = sum(ETc_m3, na.rm = T),
        ETc_mean = weighted.mean(ETc_mean, w = surf, na.rm = T),
        surf_tot = mean(surf),
        irrigation_mm = sum(irrigation_mm, na.rm = T)
      )

    Polymail_infoday <- data_join %>%
      dplyr::group_by(Poly, Mail, ssMail, DatesR, Mois, An) %>%
      dplyr::summarise(
        Irrigation_m3 = sum(Irr_m3_irrmet, na.rm = T),
        ETc_m3 = sum(Irr_m3_irrmet, na.rm = T),
        surf = sum(Surf_irrmet)
      )
    Polymail_info <- Polymail_infoday %>%
      dplyr::group_by(Poly, Mail, ssMail, Mois, An) %>%
      dplyr::summarise(
        Irrigation_m3 = sum(Irrigation_m3, na.rm = T),
        ETc_m3 = sum(ETc_m3, na.rm = T),
        surf_tot = sum(surf)
      )

    if (!file.exists(paste0(Path_Run_results_scen, '/Resultats_Aggreges'))) {
      dir.create(paste0(Path_Run_results_scen, '/Resultats_Aggreges'))
    }
    # write.csv2(Poly_info_plusCC,paste0(Path_Run_results_scen,'/Resultats_Aggreges','/Modelisation_for_polygones.csv'),row.names=F)
    # write.csv2(Mail_info_plusCC,paste0(Path_Run_results_scen,'/Resultats_Aggreges','/Modelisation_for_network.csv'),row.names=F)
    write.csv2(
      Polymail_info_plusCC,
      paste0(
        Path_Run_results_scen,
        '/Resultats_Aggreges',
        '/Modelisation_for_polygones_and_network.csv'
      ),
      row.names = F
    )
    # write.csv2(Polymail_info,paste0(Path_Run_results_scen,'/Resultats_Aggreges','/Modelisation_for_polygones_and_network_sanscult.csv'),row.names=F)

    # write.csv2(Poly_info_plusCCday,paste0(Path_Run_results_scen,'/Resultats_Aggreges','/Modelisation_for_polygones_perday.csv'),row.names=F)
    # write.csv2(Mail_info_plusCCday,paste0(Path_Run_results_scen,'/Resultats_Aggreges','/Modelisation_for_network_perday.csv'),row.names=F)
    write.csv2(
      Polymail_info_plusCCday,
      paste0(
        Path_Run_results_scen,
        '/Resultats_Aggreges',
        '/Modelisation_for_polygones_and_network_perday.csv'
      ),
      row.names = F
    )
    # write.csv2(Polymail_infoday,paste0(Path_Run_results_scen,'/Resultats_Aggreges','/Modelisation_for_polygones_and_network_sanscult_perday.csv'),row.names=F)
  } #end loop scenarii
  cli_alert_success(
    "run_script2 - Simulations pour la modelisation de la gestion de l'eau aux cultures - execute avec succes"
  )
}
