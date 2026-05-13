#' Scenario Comparison Script
#'
#' This function runs the script 3.2 for comparing scenarios based on the last run or a chosen run path.
#' It generates comparison plots and CSV files for area, irrigation, and ETc in the Results_comparison folder.
#'
#' @inheritParams run_script2
#' @return This function does not return a value but generates comparison plots and CSV files in the Results_comparison folder.
#' @export
#'
run_script3_2 <- function(
  cfgRun = get_Run_Config(Working_path = Working_path)
) {
  cli_alert_info(
    "Demarrage run_script3_2 - Comparaison des scenarii"
  )

  Working_path <- cfgRun$Working_path
  Path_Run <- cfgRun$Path_Run
  Path_Run_chargementdata <- cfgRun$Path_Run_Chargementdata
  Scenario <- cfgRun$Scenario

  Param_crop <- read.csv2(paste0(
    Path_Run_chargementdata,
    "/1_",
    Scenario$Scenario[1],
    '/Param_crop_to_RPG.csv'
  ))
  Colors_crop <- Param_crop %>%
    dplyr::filter(!is.na(Crop_RPG)) %>%
    dplyr::select(c(Crop_RPG, Color))

  #Creation dossier de resultats comparaison scenarii
  Results_comparison <- paste0(
    Path_Run,
    '/',
    'Results',
    '/',
    'Scenario_comparison'
  )
  if (!file.exists(Results_comparison)) {
    dir.create(Results_comparison)
  }
  if (!file.exists(paste0(Results_comparison, '/Area'))) {
    dir.create(paste0(Results_comparison, '/Area'))
  }
  if (!file.exists(paste0(Results_comparison, '/Irrigation'))) {
    dir.create(paste0(Results_comparison, '/Irrigation'))
  }
  if (!file.exists(paste0(Results_comparison, '/ETc'))) {
    dir.create(paste0(Results_comparison, '/ETc'))
  }
  if (
    !file.exists(paste0(
      Results_comparison,
      '/Irrigation/Comparaison_validation'
    ))
  ) {
    dir.create(paste0(Results_comparison, '/Irrigation/Comparaison_validation'))
  }

  #Aggregation scenarii data
  # Scen <- Scenario$Scenario[1]
  for (Scen in Scenario$Scenario) {
    Scen_i <- which(Scenario$Scenario == Scen)
    Path_scen <- paste0(Path_Run_chargementdata, '/', Scen_i, '_', Scen)
    # Path_scen_meteo <- paste0(Path_scen,'/Meteo')

    #Comparateur de surfaces
    Surf_tab <- read.csv2(paste0(
      Path_Run_chargementdata,
      '/',
      Scen_i,
      '_',
      Scen,
      '/Surf_table_',
      Scen,
      '.csv'
    ))

    #Cration of surface comparison for each polygones
    Surf_tab_grpscen <- Surf_tab %>%
      dplyr::select(-year) %>%
      dplyr::group_by(across(-1)) %>%
      dplyr::summarise(
        first_date = first(date),
        last_date = last(date)
      ) %>%
      pivot_longer(
        !c(first_date, last_date),
        names_to = "income",
        values_to = "Surface"
      ) %>%
      dplyr::mutate(Culture = substr(income, 1, 3)) %>%
      dplyr::mutate(
        Poly = substr(
          income,
          5,
          (regexpr("_", substr(income, 5, 1000))[1] + 5 - 2)
        )
      ) %>%
      dplyr::mutate(
        mail = substr(
          income,
          (regexpr("_", substr(income, 5, 1000))[1] + 5),
          100000
        )
      ) %>%
      dplyr::group_by(Culture, first_date, last_date) %>%
      dplyr::summarise(Surf = sum(Surface, na.rm = T)) %>%
      dplyr::group_by(Culture, Surf) %>%
      dplyr::summarise(
        first_date = first(first_date),
        last_date = last(last_date)
      ) %>%
      dplyr::mutate(Scenario = Scen)

    if (Scen == first(Scenario$Scenario)) {
      Surf_tab_groupe <- Surf_tab_grpscen
    }
    if (Scen != first(Scenario$Scenario)) {
      Surf_tab_groupe <- Surf_tab_groupe %>% bind_rows(Surf_tab_grpscen)
    }
    if (Scen == last(Scenario$Scenario)) {
      write.csv2(
        Surf_tab_groupe,
        paste0(Results_comparison, "/Area/Area_comparison.csv"),
        row.names = F
      )
    }

    #Comparateur irrigation
    Irr_tab <- read.csv2(paste0(
      Path_Run,
      '/Results/',
      Scen_i,
      '_',
      Scen,
      '/Resultats_Aggreges/Modelisation_for_polygones_and_network.csv'
    )) %>%
      dplyr::mutate(Scenario = Scen)

    if (Scen == first(Scenario$Scenario)) {
      Irr_tab_groupe <- Irr_tab
    }
    if (Scen != first(Scenario$Scenario)) {
      Irr_tab_groupe <- Irr_tab_groupe %>% bind_rows(Irr_tab)
    }
    if (Scen == last(Scenario$Scenario)) {
      write.csv2(
        Irr_tab_groupe,
        paste0(Results_comparison, "/Irrigation/Irrigation_comparison.csv"),
        row.names = F
      )
    }

    #Comparateur ETc
    #Comparateur irrigation
    ETc_tab <- read.csv2(paste0(
      Path_Run,
      '/Results/',
      Scen_i,
      '_',
      Scen,
      '/Resultats_Aggreges/Modelisation_for_polygones_and_network_perday.csv'
    )) %>%
      dplyr::mutate(Scenario = Scen)

    if (Scen == first(Scenario$Scenario)) {
      ETc_tab_groupe <- ETc_tab
    }
    if (Scen != first(Scenario$Scenario)) {
      ETc_tab_groupe <- ETc_tab_groupe %>% bind_rows(ETc_tab)
    }
    if (Scen == last(Scenario$Scenario)) {
      write.csv2(
        ETc_tab_groupe,
        paste0(Results_comparison, "/ETc/ETc_comparison.csv"),
        row.names = F
      )
    }
  } #end scenarii loop for aggregate data

  Surf_tab_groupe_2 <- Surf_tab_groupe %>%
    tibble() %>%
    tidyr::complete(
      Culture,
      first_date,
      last_date,
      Scenario,
      fill = list(Surf = 0)
    ) %>%
    dplyr::mutate(Length_period = as.Date(last_date) - as.Date(first_date)) %>%
    dplyr::filter(Length_period > 0)

  # periode_first <- unique(Surf_tab_groupe_2$first_date)[1]
  for (periode_first in unique(Surf_tab_groupe_2$first_date)) {
    per_i <- which(unique(Surf_tab_groupe_2$first_date) == periode_first)
    surf_tab_periode <- Surf_tab_groupe_2 %>%
      dplyr::filter(first_date == periode_first)
    if (
      length(unique(surf_tab_periode$Scenario)) !=
        length(unique(Surf_tab_groupe_2$Scenario))
    ) {
      #cas ou un scenario n'a pas cette date de début
      for (scen_out in unique(Surf_tab_groupe_2$Scenario)[which(
        !unique(Surf_tab_groupe_2$Scenario) %in%
          unique(surf_tab_periode$Scenario)
      )]) {
        #pour chaque scenar non represente
        tab_scen_out <- Surf_tab_groupe_2 %>%
          dplyr::filter(Scenario == scen_out)
        list_first_tso <- unique(tab_scen_out$first_date)
        date_just_av <- max(list_first_tso[which(
          as.Date(list_first_tso) < as.Date(periode_first)
        )])
        if (
          length(list_first_tso[which(
            as.Date(list_first_tso) < as.Date(periode_first)
          )]) ==
            0
        ) {
          tab_scen_out_2 <- surf_tab_periode %>%
            dplyr::mutate(Scenario = scen_out, Surf = 0)
        }
        if (
          length(list_first_tso[which(
            as.Date(list_first_tso) < as.Date(periode_first)
          )]) >
            0
        ) {
          tab_scen_out_2 <- Surf_tab_groupe_2 %>%
            dplyr::filter(first_date == date_just_av)
        }
        surf_tab_periode <- surf_tab_periode %>% bind_rows(tab_scen_out_2)
      } #end remplissage chaque scenario manquant
    } #end loop chaque scenario non reference par la date

    ggplot(data = surf_tab_periode) +
      geom_bar(
        stat = 'identity',
        position = "dodge",
        aes(x = Culture, y = Surf / 10000, fill = Scenario)
      ) +
      scale_y_continuous('Surface (ha)') +
      ggtitle(paste0("Surface comparison for the ", periode_first)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90))
    ggsave(paste0(
      Results_comparison,
      "/Area/Area_comparison_",
      periode_first,
      ".png"
    ))
  }

  #Comparaison des irrigation scenarisees
  #par mois et par culture pour chaque annee
  #year=unique(Irr_tab_groupe$An)[1]
  for (year in unique(Irr_tab_groupe$An)) {
    Irr_tab_plot_y <- Irr_tab_groupe %>%
      dplyr::filter(An == year) %>%
      dplyr::group_by(Mois, CODE_CU, Scenario) %>%
      dplyr::summarise(
        Irrigation_m3 = sum(Irrigation_m3, na.rm = T),
        Surf = sum(surf_tot, na.rm = T),
        Irrigation_mm = weighted.mean(
          irrigation_mm,
          w = surf_tot,
          na.rm = T
        )
      )
    # culture <- unique(Irr_tab_plot_y$CODE_CU)[1]
    for (culture in unique(Irr_tab_plot_y$CODE_CU)) {
      Irr_tab_plot_y_cult <- Irr_tab_plot_y %>%
        dplyr::filter(CODE_CU == culture)
      if (length(unique(Irr_tab_plot_y_cult$Scenario)) > 1) {
        ggplot(
          Irr_tab_plot_y_cult,
          aes(
            x = as.factor(Mois),
            y = Irrigation_mm,
            fill = as.factor(Scenario)
          )
        ) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(x = "Culture", y = "Irrigation (mm)", fill = "Année") +
          ggtitle(paste0(year, ' - ', culture)) +
          theme_bw() +
          facet_wrap(~Scenario, scales = "free_x") +
          theme(
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            strip.text = element_text(size = 15),
            title = element_text(size = 15),
            legend.position = "none"
          )
        ggsave(paste0(
          Results_comparison,
          "/Irrigation/Irrigation_comparison_Scernario_mensual_",
          year,
          '_',
          culture,
          ".png"
        ))
      }
    }
  }

  #par an et par culture
  Irr_tab_plot1 <- Irr_tab_groupe %>%
    dplyr::group_by(An, CODE_CU, Scenario, ssMail) %>%
    dplyr::summarise(
      Irrigation_m3 = sum(Irrigation_m3, na.rm = T),
      surf_tot = mean(surf_tot, na.rm = T),
      irrigation_mm = sum(irrigation_mm)
    ) %>%
    dplyr::group_by(An, CODE_CU, Scenario) %>%
    dplyr::summarise(
      Irrigation_m3 = sum(Irrigation_m3, na.rm = T),
      Surf = sum(surf_tot, na.rm = T),
      Irrigation_mm = weighted.mean(
        irrigation_mm,
        w = surf_tot,
        na.rm = T
      )
    ) %>%
    dplyr::mutate(Crop_RPG = CODE_CU) %>%
    full_join(Colors_crop, by = "Crop_RPG") %>%
    dplyr::mutate(Color = ifelse(is.na(Color), "#cccccc", Color)) %>%
    dplyr::filter(!is.na(CODE_CU)) %>%
    dplyr::filter(!is.na(An)) %>%
    dplyr::filter(!is.na(Scenario))

  # culture <- unique(Irr_tab_plot_y$CODE_CU)[1]
  for (culture in unique(Irr_tab_plot_y$CODE_CU)) {
    Irr_tab_plot_cult <- Irr_tab_plot1 %>% dplyr::filter(CODE_CU == culture)
    if (length(unique(Irr_tab_plot_cult$Scenario)) > 1) {
      ggplot(
        Irr_tab_plot_cult,
        aes(x = as.factor(An), y = Irrigation_mm, fill = as.factor(Scenario))
      ) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Culture", y = "Irrigation (mm)", fill = "Année") +
        ggtitle(culture) +
        theme_bw() +
        facet_wrap(~Scenario, scales = "free_x") +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          strip.text = element_text(size = 15),
          title = element_text(size = 15),
          legend.position = "none"
        )
      ggsave(paste0(
        Results_comparison,
        "/Irrigation/Irrigation_comparison_Scernario_annual_",
        culture,
        ".png"
      ))
    }
  }

  #par an en m3 et en mm moyens
  Irr_tab_plot2 <- Irr_tab_plot1 %>%
    dplyr::filter(Surf > 0) %>%
    dplyr::group_by(An, Scenario) %>%
    dplyr::summarise(
      Irrigation_m3 = sum(Irrigation_m3, na.rm = T),
      Surf = sum(Surf, na.rm = T),
      Irrigation_mm = mean(Irrigation_mm)
    )
  if (length(unique(Irr_tab_plot2$Scenario)) > 1) {
    ggplot(
      Irr_tab_plot2,
      aes(x = as.factor(An), y = Irrigation_mm, fill = as.factor(Scenario))
    ) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Culture", y = "Average irrigation (mm)", fill = "Année") +
      scale_y_continuous(labels = scales::label_number()) +
      theme_bw() +
      facet_wrap(~Scenario, scales = "free_x") +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.position = "none"
      )
    ggsave(paste0(
      Results_comparison,
      "/Irrigation/Irrigation_comparison_Scernario_annual_averagemm.png"
    ))

    ggplot(
      Irr_tab_plot2,
      aes(x = as.factor(An), y = Irrigation_m3, fill = as.factor(Scenario))
    ) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Culture", y = "Irrigation (m3)", fill = "Année") +
      scale_y_continuous(labels = scales::label_number()) +
      theme_bw() +
      facet_wrap(~Scenario, scales = "free_x") +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.position = "none"
      )
    ggsave(paste0(
      Results_comparison,
      "/Irrigation/Irrigation_comparison_Scernario_annual_sum_m3.png"
    ))

    ggplot(
      Irr_tab_plot2,
      aes(
        x = paste0(An, '_', Scenario),
        y = Irrigation_m3,
        fill = as.factor(Scenario)
      )
    ) +
      geom_bar(stat = "identity") +
      labs(x = "", y = "Irrigation (m3)", fill = "Année") +
      scale_y_continuous(labels = scales::label_number()) +
      theme_bw() +
      # facet_wrap(~Scenario, scales = "free_x") +
      theme(
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.position = "none"
      )
    ggsave(paste0(
      Results_comparison,
      "/Irrigation/Irrigation_comparison_Scernarii_annual_sum_m3_onarow.png"
    ))

    ggplot(
      Irr_tab_plot1,
      aes(x = paste0(An, '_', Scenario), y = Irrigation_m3)
    ) +
      geom_bar(stat = "identity", aes(fill = as.factor(CODE_CU))) +
      scale_fill_manual(values = Irr_tab_plot1$Color) +
      labs(x = "", y = "Irrigation (m3)", fill = "Cultures") +
      scale_y_continuous(labels = scales::label_number()) +
      theme_bw() +
      theme(
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15)
      )
    ggsave(paste0(
      Results_comparison,
      "/Irrigation/Irrigation_comparison_Scernarii_annual_sum_m3_onarow_bycrop.png"
    ))
  }

  #ETc comparison
  for (year in unique(ETc_tab_groupe$An)) {
    ETc_tab_y <- ETc_tab_groupe %>% dplyr::filter(An == year)
    for (culture in unique(ETc_tab_y$CODE_CU)) {
      ETc_tab_y_cult <- ETc_tab_y %>% dplyr::filter(CODE_CU == culture)
      if (length(unique(ETc_tab_y_cult$Scenario)) > 1) {
        ggplot(data = ETc_tab_y_cult) +
          geom_line(aes(
            x = as.Date(DatesR),
            y = as.numeric(ETc_mean),
            col = Scenario
          )) +
          labs(x = year, y = "Average ETc (mm)") +
          ggtitle(paste0(year, ' - ', culture)) +
          theme_bw() +
          theme(
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            title = element_text(size = 15),
            legend.text = element_text(size = 10)
          )
        ggsave(paste0(
          Results_comparison,
          "/ETc/ETc_comparison_Scernario_annual_average_mm",
          culture,
          "_",
          year,
          ".png"
        ))
      }
    }
  }

  #Comparaison aux validations
  #somme d'irrigation territoriale
  # Validations <- unique(Scenario$Territorial_validation_link)[2]
  Irr_tab_plot3 <- Irr_tab_plot2 %>%
    dplyr::group_by(An) %>%
    dplyr::summarise(
      Irr_mean = mean(Irrigation_m3),
      Irr_sd = sd(Irrigation_m3)
    ) %>%
    dplyr::mutate(
      Irr_sdmin = min(c(Irr_mean, Irr_mean - Irr_sd), na.rm = TRUE),
      Irr_sdmax = max(c(Irr_mean, Irr_mean + Irr_sd), na.rm = TRUE),
      Irr_sdmin = ifelse(Irr_sdmin < 0, 0, Irr_sdmin)
    )

  ggplot(data = Irr_tab_plot3, aes(x = An)) +
    geom_bar(stat = 'identity', aes(y = Irr_mean), fill = "blue", alpha = 0.7) +
    geom_errorbar(
      data = Irr_tab_plot3,
      aes(ymin = Irr_sdmin, ymax = Irr_sdmax),
      width = 0.4,
      colour = "orange",
      alpha = 0.9,
      size = 1.3
    ) +
    geom_jitter(
      data = Irr_tab_plot2,
      aes(x = An, y = Irrigation_m3),
      width = 0.3,
      size = 3,
      color = "black"
    ) +
    ggtitle("Variabilité multi-scenarii") +
    scale_x_continuous(breaks = unique(Irr_tab_plot3$An)) +
    scale_y_continuous(labels = scales::label_number()) +
    labs(x = "Année", y = "Irrigation (m3)") +
    theme_bw() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 15)
    )
  ggsave(paste0(
    Results_comparison,
    "/Irrigation/Comparaison_validation/Irrigation_comparison_Scernario_annual_sum_m3_allscenarii.png"
  ))

  #Validations <- unique(Scenario$Territorial_validation_link)[!is.na(unique(Scenario$Territorial_validation_link))][1]
  for (Validations in unique(Scenario$Territorial_validation_link)[
    !is.na(unique(Scenario$Territorial_validation_link))
  ]) {
    Validation_i <- which(
      unique(Scenario$Territorial_validation_link)[
        !is.na(unique(Scenario$Territorial_validation_link))
      ] ==
        Validations
    )
    Liste_scenario_consernes <- Scenario$Scenario[which(
      Scenario$Territorial_validation_link == Validations
    )]

    if (length(Liste_scenario_consernes) > 1) {
      Validation_territoire <- st_read(FpCAWET(
        Working_path,
        Validations
      ))

      Validation_territoire <- Validation_territoire %>%
        dplyr::rename('Irr_mean' = 'Water_quantity_m3') %>%
        dplyr::mutate(An = "Validation")

      if (length(Validation_territoire$Year) > 0) {
        Validation_territoire <- Validation_territoire %>%
          dplyr::mutate(
            An = paste0(Year, "-Validation")
          )
      }

      Irr_tab_plot21 <- Irr_tab_plot2 %>%
        dplyr::filter(Scenario %in% Liste_scenario_consernes)
      Irr_tab_plot4 <- Irr_tab_plot21 %>%
        dplyr::group_by(An) %>%
        dplyr::summarise(
          Irr_mean = mean(Irrigation_m3),
          Irr_sd = sd(Irrigation_m3)
        ) %>%
        mutate(Irr_sdmin = Irr_mean - Irr_sd, Irr_sdmax = Irr_mean + Irr_sd) %>%
        dplyr::mutate(An = as.character(An)) %>%
        bind_rows(Validation_territoire) %>%
        dplyr::mutate(
          Irrigation_ref = ifelse(str_detect(An, "Validation"), Irr_mean, NA),
          Coloration = ifelse(str_detect(An, "Validation"), "darkgrey", "blue")
        )
      ggplot(data = Irr_tab_plot4, aes(x = An)) +
        geom_bar(
          stat = 'identity',
          aes(y = Irr_mean),
          fill = Irr_tab_plot4$Coloration,
          alpha = 0.9
        ) +
        geom_errorbar(
          aes(ymin = Irr_sdmin, ymax = Irr_sdmax),
          width = 0.4,
          colour = "orange",
          alpha = 0.9,
          size = 1.3
        ) +
        # geom_hline(aes(yintercept = Irrigation_ref), color = "red", linetype = "dashed") +
        geom_jitter(
          data = Irr_tab_plot21,
          aes(x = as.character(An), y = Irrigation_m3),
          width = 0.3,
          size = 3,
          color = "black"
        ) +
        scale_y_continuous(labels = scales::label_number()) +
        ggtitle(paste(
          "Validation issues des scenarii : ",
          Liste_scenario_consernes,
          collapse = "-"
        )) +
        labs(x = "Année", y = "Irrigation (m3)") +
        theme_bw() +
        theme(
          axis.text.x = element_text(size = 12, angle = 90),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.position = 'none'
        )
      ggsave(paste0(
        Results_comparison,
        "/Irrigation/Comparaison_validation/Irrigation_comparison_Scernario_annual_sum_m3_withvalidation",
        paste(Liste_scenario_consernes, collapse = "_"),
        "_",
        Validation_i,
        ".png"
      ))
    }
  }

  #Irrigation par culture
  Irr_tab_plot11 <- Irr_tab_plot1 %>%
    dplyr::group_by(An, CODE_CU) %>%
    dplyr::summarise(
      Irr_mm_mean = mean(Irrigation_mm, na.rm = T),
      Irr_mm_sd = sd(Irrigation_mm)
    ) %>%
    dplyr::mutate(
      Irr_mm_mean = ifelse(is.nan(Irr_mm_mean), 0, Irr_mm_mean),
      Irr_sdmin = Irr_mm_mean - Irr_mm_sd,
      Irr_sdmax = Irr_mm_mean + Irr_mm_sd
    )

  ggplot(
    Irr_tab_plot11,
    aes(x = as.factor(An), y = Irr_mm_mean, fill = as.factor(substr(An, 4, 4)))
  ) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(
      aes(ymin = Irr_sdmin, ymax = Irr_sdmax),
      width = 0.2,
      colour = "orange",
      alpha = 0.9,
      size = 1.3
    ) +
    geom_jitter(
      data = Irr_tab_plot1,
      aes(x = as.character(An), y = Irrigation_mm),
      width = 0.3,
      size = 2,
      color = "black"
    ) +
    labs(x = "Culture", y = "Irrigation (mm)", fill = "Année") +
    theme_minimal() +
    facet_wrap(~CODE_CU, scales = "free_x") +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.text = element_text(size = 12),
      ,
      strip.text = element_text(size = 15),
      axis.title = element_text(size = 15),
      legend.position = "none"
    )
  ggsave(paste0(
    Results_comparison,
    "/Irrigation/Comparaison_validation/Irrigation_comparison_Scernario_annual_crops_mm_allscenarii.png"
  ))

  # Validations_cult <- unique(Scenario$Cultural_validation_link)[!is.na(unique(Scenario$Cultural_validation_link))][1]
  for (Validations_cult in unique(Scenario$Cultural_validation_link)[
    !is.na(unique(Scenario$Cultural_validation_link))
  ]) {
    Validation_i <- which(
      unique(Scenario$Cultural_validation_link)[
        !is.na(unique(Scenario$Cultural_validation_link))
      ] ==
        Validations_cult
    )
    Liste_scenario_consernes <- Scenario$Scenario[which(
      Scenario$Cultural_validation_link == Validations_cult
    )]

    if (length(Liste_scenario_consernes) > 1) {
      Validation_culture <- st_read(FpCAWET(
        Working_path,
        Validations_cult
      )) %>%
        dplyr::rename(
          'Irr_mm_mean' = 'Irrigation_mm',
          'CODE_CU' = 'Crop'
        ) %>%
        dplyr::mutate(An = "Validation")

      if (length(Validation_culture$Year) > 0) {
        Validation_culture <- Validation_culture %>%
          dplyr::mutate(An = paste0(Year, "-Validation"))
      }

      Irr_tab_plot_cult <- Irr_tab_plot1 %>%
        dplyr::filter(Scenario %in% Liste_scenario_consernes)
      Cultures_ok <- unique(Irr_tab_plot_cult$CODE_CU)
      Irr_tab_plot5 <- Irr_tab_plot_cult %>%
        dplyr::group_by(An, CODE_CU) %>%
        dplyr::summarise(
          Irr_mm_mean = mean(Irrigation_mm, na.rm = T),
          Irr_mm_sd = sd(Irrigation_mm)
        ) %>%
        dplyr::mutate(
          Irr_mm_mean = ifelse(is.nan(Irr_mm_mean), 0, Irr_mm_mean),
          Irr_sdmin = Irr_mm_mean - Irr_mm_sd,
          Irr_sdmax = Irr_mm_mean + Irr_mm_sd
        ) %>%
        dplyr::mutate(An = as.character(An)) %>%
        bind_rows(Validation_culture) %>%
        dplyr::mutate(
          Irrigation_ref = ifelse(
            str_detect(An, "Validation"),
            Irr_mm_mean,
            NA
          ),
          Coloration = ifelse(str_detect(An, "Validation"), "darkgrey", "blue")
        ) %>%
        dplyr::filter(CODE_CU %in% Cultures_ok)

      ggplot(
        Irr_tab_plot5,
        aes(
          x = as.factor(An),
          y = Irr_mm_mean,
          fill = as.factor(substr(An, 4, 4))
        )
      ) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(
          aes(ymin = Irr_sdmin, ymax = Irr_sdmax),
          width = 0.2,
          colour = "orange",
          alpha = 0.9,
          size = 1.3
        ) +
        geom_hline(
          aes(yintercept = Irrigation_ref),
          color = "red",
          linetype = "dashed"
        ) +
        geom_jitter(
          data = Irr_tab_plot_cult,
          aes(x = as.character(An), y = Irrigation_mm),
          width = 0.3,
          size = 2,
          color = "black"
        ) +
        labs(x = "Culture", y = "Irrigation (mm)", fill = "Année") +
        theme_minimal() +
        facet_wrap(~CODE_CU, scales = "free_x") +
        theme(axis.text.x = element_text(angle = 90), legend.position = "none")
      ggsave(paste0(
        Results_comparison,
        "/Irrigation/Comparaison_validation/Irrigation_comparison_Scernario_annual_mmparculture_withvalidation_sceario",
        paste(Liste_scenario_consernes, collapse = "_"),
        ".png"
      ))
    }
  }
  cli_alert_success(
    "run_script3_2 - Comparaison des scenarii - execute avec succes"
  )
}
