#' Climatic indicators calculation and graphs generation
#'
#' This function calculates climatic indicators from meteorological data and generates corresponding graphs.
#'
#' @inheritParams run_script2
#' @return None. The function saves the results and graphs in the specified directories.
#' @export
#'
run_script3_3 <- function(
  cfgRun = get_Run_Config(Working_path = Working_path)
) {
  cli_alert_info(
    "Demarrage run_script3_3 - Indicateurs climatiques"
  )

  Working_path <- cfgRun$Working_path
  Path_Run <- cfgRun$Path_Run
  Path_Run_chargementdata <- cfgRun$Path_Run_Chargementdata
  Scenario <- cfgRun$Scenario

  #Informations necessaires
  Date_deb_ete <- "21-06" #format DD-MM
  Temperature_hot <- 25 #ref : https://paca.chambres-agriculture.fr/fileadmin/user_upload/Provence-Alpes-Cote_d_Azur/020_Inst_Paca/CRA_PACA/Documents/ACTUALITES/2023/page_CLIMAT/FICHES_TECHNIQUES_START_CLIMA_-_Climat.pdf
  Temperature_veryhot <- 30
  Temperature_veryveryhot <- 35
  Temperature_nuittropic <- 20
  mm_fortepluie <- 20
  Jour_gel <- 0
  Seuil_T_base <- 2.5

  #Scen <- Scenario$Scenario[1]
  scenarios <- Scenario$Scenario
  for (Scen in scenarios) {
    cli_alert_info(
      paste0("Traitement du scenario : ", Scen)
    )
    Scen_i <- which(Scenario$Scenario == Scen)
    Path_scen <- paste0(Path_Run_chargementdata, '/', Scen_i, '_', Scen)
    Path_scen_res <- paste0(Path_Run, '/Results/', Scen_i, '_', Scen)
    if (!file.exists(paste0(Path_scen_res, '/Meteo'))) {
      dir.create(paste0(Path_scen_res, '/Meteo'))
    }

    Meteo_dir <- dir(paste0(Path_scen, '/Meteo/'))[str_detect(
      dir(paste0(Path_scen, '/Meteo/')),
      ".csv"
    )]
    #met_path <- Meteo_dir[1]
    for (met_path in Meteo_dir) {
      maille <- unlist(str_split(unlist(str_split(met_path, '_'))[2], '.csv'))[
        1
      ]
      if (met_path != "Meteo.csv") {
        Meteo_maille1 <- read.csv2(paste0(Path_scen, '/Meteo/', met_path))
        if (Scenario$Climat_data_source[Scen_i] == "Safran") {
          Meteo_maille1 <- Meteo_maille1 %>% mutate(across(3:ncol(Meteo_maille1), as.character))
        }
        if (Scenario$Climat_data_source[Scen_i] == "Drias") {
          Meteo_maille1 <- Meteo_maille1 %>% mutate(across(2:12, as.character))
        }
      } else {
        Meteo_maille1 <- read.csv2(paste0(Path_scen, '/Meteo/', met_path)) %>%
          mutate(across(2:3, as.character))
      }

      if ("LAMBX" %in% colnames(Meteo_maille1)) {
        Meteo_maille <- Meteo_maille1 %>%
          dplyr::mutate(Mail = maille) %>%
          pivot_longer(
            !c(LAMBX, LAMBY, DATE, Mail),
            values_to = 'Values',
            names_to = 'Climatic_data'
          ) %>%
          dplyr::mutate(Values = as.numeric(Values)) %>%
          dplyr::filter(!is.na(Values))
      } else {
        if (
          "x" %in% colnames(Meteo_maille1) & "y" %in% colnames(Meteo_maille1)
        ) {
          Meteo_maille <- Meteo_maille1 %>%
            dplyr::mutate(Mail = maille) %>%
            dplyr::select(-c(x, y)) %>%
            pivot_longer(
              !c(DATE, Mail),
              values_to = 'Values',
              names_to = 'Climatic_data'
            ) %>%
            dplyr::mutate(Values = as.numeric(Values)) %>%
            dplyr::filter(!is.na(Values))
        } else {
          Meteo_maille <- Meteo_maille1 %>%
            dplyr::mutate(Mail = maille) %>%
            pivot_longer(
              !c(DATE, Mail),
              values_to = 'Values',
              names_to = 'Climatic_data'
            ) %>%
            dplyr::mutate(Values = as.numeric(Values)) %>%
            dplyr::filter(!is.na(Values))
        }
      }

      if (met_path == first(Meteo_dir)) {
        Meteo_join <- Meteo_maille
      }
      if (met_path != first(Meteo_dir)) {
        Meteo_join <- Meteo_join %>% bind_rows(Meteo_maille)
      }
      if (met_path == last(Meteo_dir)) {
        write.csv2(Meteo_join, paste0(Path_scen_res, '/Meteo/Meteo_data.csv'))
      }
    } #end aggregation meteo data

    if (Scenario$Climat_data_source[Scen_i] %in% c('Safran', 'Drias')) {
      Rain <- Meteo_join %>% dplyr::filter(Climatic_data == 'PRELIQ_Q')
      # Eff_Rain <- Meteo_join %>% dplyr::filter(Climatic_data=='PE_Q')
      Tmin <- Meteo_join %>% dplyr::filter(Climatic_data == 'TINF_H_Q')
      Tmax <- Meteo_join %>% dplyr::filter(Climatic_data == 'TSUP_H_Q')
      Tmean <- Meteo_join %>% dplyr::filter(Climatic_data == 'T_Q')
      Wind <- Meteo_join %>% dplyr::filter(Climatic_data == 'FF_Q')
      ETP <- Meteo_join %>% dplyr::filter(Climatic_data == 'ETP_Q')
    }

    Rain_mean <- Rain %>%
      dplyr::group_by(DATE) %>%
      dplyr::summarise(
        Values = mean(as.numeric(Values)),
        sd_rain = sd(as.numeric(Values))
      )
    # Eff_Rain_mean <- Eff_Rain %>% dplyr::group_by(DATE) %>% dplyr::summarise(Values=mean(as.numeric(Values)),sd_effrain=sd(as.numeric(Values)))
    Tmin_mean <- Tmin %>%
      dplyr::group_by(DATE) %>%
      dplyr::summarise(
        Values = mean(as.numeric(Values)),
        sd_Tmin = sd(as.numeric(Values))
      )
    Tmax_mean <- Tmax %>%
      dplyr::group_by(DATE) %>%
      dplyr::summarise(
        Values = mean(as.numeric(Values)),
        sd_Tmax = sd(as.numeric(Values))
      )
    Tmin_absolu <- Tmin %>%
      dplyr::group_by(DATE) %>%
      dplyr::summarise(
        Values = min(as.numeric(Values)),
        sd_Tminabs = sd(as.numeric(Values))
      )
    Tmax_absolu <- Tmax %>%
      dplyr::group_by(DATE) %>%
      dplyr::summarise(
        Values = max(as.numeric(Values)),
        sd_Tmaxabs = sd(as.numeric(Values))
      )
    Tmean_mean <- Tmean %>%
      dplyr::group_by(DATE) %>%
      dplyr::summarise(
        Values = mean(as.numeric(Values)),
        sd_Tmean = sd(as.numeric(Values))
      )
    Wind_mean <- Wind %>%
      dplyr::group_by(DATE) %>%
      dplyr::summarise(
        Values = mean(as.numeric(Values)),
        sd_wind = sd(as.numeric(Values))
      )
    ETP_mean <- ETP %>%
      dplyr::group_by(DATE) %>%
      dplyr::summarise(
        Values = mean(as.numeric(Values)),
        sd_ETP = sd(as.numeric(Values))
      )

    Meteo_data_daymean <- Rain_mean %>%
      dplyr::rename('Rain' = 'Values') %>%
      # dplyr::full_join(Eff_Rain_mean) %>% dplyr::rename('Rain_eff'='Values') %>%
      dplyr::full_join(Tmin_mean, by = "DATE") %>%
      dplyr::rename('Tmin' = 'Values') %>%
      dplyr::full_join(Tmax_mean, by = "DATE") %>%
      dplyr::rename('Tmax' = 'Values') %>%
      dplyr::full_join(Tmin_absolu, by = "DATE") %>%
      dplyr::rename('Tmin_absolue' = 'Values') %>%
      dplyr::full_join(Tmax_absolu, by = "DATE") %>%
      dplyr::rename('Tmax_absolue' = 'Values') %>%
      dplyr::full_join(Tmean_mean, by = "DATE") %>%
      dplyr::rename('Tmean' = 'Values') %>%
      dplyr::full_join(Wind_mean, by = "DATE") %>%
      dplyr::rename('Wind' = 'Values') %>%
      dplyr::full_join(ETP_mean, by = "DATE") %>%
      dplyr::rename('ETP' = 'Values')

    indics_day <- c(colnames(Meteo_data_daymean)[which(
      !str_detect(colnames(Meteo_data_daymean), 'sd_')
    )])[
      2:length(colnames(Meteo_data_daymean)[which(
        !str_detect(colnames(Meteo_data_daymean), 'sd_')
      )])
    ]
    progress_indics_day <- cli_progress_bar(
      format = "Indic {pb_current}/{pb_total}: {pb_status}",
      total = length(indics_day),
      clear = TRUE
    )
    for (Indic in indics_day) {
      cli_progress_update(id = progress_indics_day, status = Indic)
      Data_meteo_indic <- Meteo_data_daymean
      colnames(Data_meteo_indic)[which(
        colnames(Data_meteo_indic) == Indic
      )] <- 'Indicateur'
      colnames(Data_meteo_indic)[
        which(colnames(Data_meteo_indic) == 'Indicateur') + 1
      ] <- 'Indicateur_sd'

      ggplot(data = Data_meteo_indic) +
        geom_point(aes(
          x = as.Date(DATE),
          y = as.numeric(Indicateur),
          col = as.factor(year(as.Date(DATE)))
        )) +
        scale_y_continuous(Indic) +
        scale_x_date('Dates') +
        theme_bw() +
        theme(
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          legend.position = 'none'
        )
      ggsave(paste0(Path_scen_res, '/Meteo/Day_', Indic, '.png'))

      ggplot(data = Data_meteo_indic) +
        geom_point(aes(x = as.Date(DATE), y = as.numeric(Indicateur))) +
        geom_line(aes(
          x = as.Date(DATE),
          y = as.numeric(Indicateur),
          col = as.factor(year(as.Date(DATE)))
        )) +
        scale_y_continuous(Indic) +
        scale_x_date('Dates') +
        theme_bw() +
        theme(
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          legend.position = 'none'
        )
      ggsave(paste0(Path_scen_res, '/Meteo/Day_', Indic, '_plusline.png'))
    }
    cli_progress_done(id = progress_indics_day)
    #end creation graphs daily

    Rain_annualsum <- Rain_mean %>%
      dplyr::mutate(Year = year(DATE)) %>%
      dplyr::mutate(
        sd = ifelse(!is.na(sd_rain), sd_rain, 0),
        Values_plussd = Values + sd,
        Values_moinssd = Values - sd
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        Rain = sum(Values),
        Rain_plussd = sum(Values_plussd),
        Rain_moinssd = sum(Values_moinssd)
      )
    Nbrfortespluies <- Rain_mean %>%
      dplyr::mutate(
        Year = year(DATE),
        sd = ifelse(!is.na(sd_rain), sd_rain, 0),
        Values_plussd = Values + sd,
        Values_moinssd = Values - sd,
        NJFP = ifelse(Values > mm_fortepluie, 1, 0),
        NJFP_plussd = ifelse(Values_plussd > mm_fortepluie, 1, 0),
        NJFP_moinssd = ifelse(Values_moinssd > mm_fortepluie, 1, 0)
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        Nbr_J_FortePluie = sum(NJFP),
        Nbr_J_FortePluie_plussd = sum(NJFP_plussd),
        Nbr_J_FortePluie_moinssd = sum(NJFP_moinssd)
      )
    # Eff_Rain_annualsum <- Eff_Rain_mean %>% dplyr::mutate(Year=year(DATE)) %>% dplyr::mutate(sd=ifelse(!is.na(sd_effrain),sd_effrain,0),Values_plussd=Values+sd,Values_moinssd=Values-sd) %>% dplyr::group_by(Year) %>% dplyr::summarise(Rain_eff=sum(Values),Raineff_plussd=sum(Values_plussd),Raineff_moinssd=sum(Values_moinssd))
    # Tmin_annualmin <- Tmin_mean %>% dplyr::mutate(Year=year(DATE))  %>% dplyr::mutate(sd=ifelse(!is.na(sd_Tmin),sd_Tmin,0),Values_plussd=Values+sd,Values_moinssd=Values-sd) %>% dplyr::group_by(Year) %>% dplyr::summarise(Tmin_annualmean=min(Values),Tmin_plussd=mean(Values_plussd),Tmin_moinssd=mean(Values_moinssd))
    # Tmax_annualmax <- Tmax_mean %>% dplyr::mutate(Year=year(DATE))  %>% dplyr::mutate(sd=ifelse(!is.na(sd_Tmax),sd_Tmax,0),Values_plussd=Values+sd,Values_moinssd=Values-sd) %>% dplyr::group_by(Year)%>% dplyr::summarise(Tmax_annualmean=mean(Values,Tmax_plussd=mean(Values_plussd),Tmax_moinssd=mean(Values_moinssd)))
    Tmin_annualabsolu <- Tmin_absolu %>%
      dplyr::mutate(Year = year(DATE)) %>%
      dplyr::mutate(
        sd = ifelse(!is.na(sd_Tminabs), sd_Tminabs, 0),
        Values_plussd = Values + sd,
        Values_moinssd = Values - sd
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        Tmin_absolue = min(Values),
        Tminabs_plussd = min(Values_plussd),
        Tminabs_moinssd = min(Values_moinssd)
      )
    Nbr_nuit_trop <- Tmin_absolu %>%
      dplyr::mutate(
        Year = year(DATE),
        sd = ifelse(!is.na(sd_Tminabs), sd_Tminabs, 0),
        Values_plussd = Values + sd,
        Values_moinssd = Values - sd,
        Nbr_Nuit_Trop = ifelse(Values >= Temperature_nuittropic, 1, 0),
        Nbr_Nuit_Trop_plussd = ifelse(
          Values_plussd >= Temperature_nuittropic,
          1,
          0
        ),
        Nbr_Nuit_Trop_moinssd = ifelse(
          Values_moinssd >= Temperature_nuittropic,
          1,
          0
        )
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        Nbr_Nuit_Trop = sum(Nbr_Nuit_Trop),
        Nbr_Nuit_Trop_plussd = sum(Nbr_Nuit_Trop_plussd),
        Nbr_Nuit_Trop_moinssd = sum(Nbr_Nuit_Trop_moinssd)
      )
    Tmax_annualabsolu <- Tmax_absolu %>%
      dplyr::mutate(Year = year(DATE)) %>%
      dplyr::mutate(
        sd = ifelse(!is.na(sd_Tmaxabs), sd_Tmaxabs, 0),
        Values_plussd = Values + sd,
        Values_moinssd = Values - sd
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        Tmax_absolue = max(Values),
        Tmaxabs_plussd = max(Values_plussd),
        Tmaxabs_moinssd = max(Values_moinssd)
      )
    Tmean_annualmean <- Tmean_mean %>%
      dplyr::mutate(Year = year(DATE)) %>%
      dplyr::mutate(
        sd = ifelse(!is.na(sd_Tmean), sd_Tmean, 0),
        Values_plussd = Values + sd,
        Values_moinssd = Values - sd
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        Tmean_day = mean(Values),
        Tmean_plussd = mean(Values_plussd),
        Tmean_moinssd = mean(Values_moinssd)
      )
    Nbr_jour_chaud <- Tmean_mean %>%
      dplyr::mutate(
        Year = year(DATE),
        sd = ifelse(!is.na(sd_Tmean), sd_Tmean, 0),
        Values_plussd = Values + sd,
        Values_moinssd = Values - sd,
        Nbr_J_Chaud = ifelse(Values >= Temperature_hot, 1, 0),
        Nbr_J_Chaud_plussd = ifelse(Values_plussd >= Temperature_hot, 1, 0),
        Nbr_J_Chaud_moinssd = ifelse(Values_moinssd >= Temperature_hot, 1, 0),
        Nbr_J_TresChaud = ifelse(Values >= Temperature_veryhot, 1, 0),
        Nbr_J_TresChaud_plussd = ifelse(
          Values_plussd >= Temperature_veryhot,
          1,
          0
        ),
        Nbr_J_TresChaud_moinssd = ifelse(
          Values_moinssd >= Temperature_veryhot,
          1,
          0
        ),
        Nbr_J_TresTresChaud = ifelse(Values >= Temperature_veryveryhot, 1, 0),
        Nbr_J_TresTresChaud_plussd = ifelse(
          Values_plussd >= Temperature_veryveryhot,
          1,
          0
        ),
        Nbr_J_TresTresChaud_moinssd = ifelse(
          Values_moinssd >= Temperature_veryveryhot,
          1,
          0
        )
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        Nbr_J_Chaud = sum(Nbr_J_Chaud),
        Nbr_J_Chaud_plussd = sum(Nbr_J_Chaud_plussd),
        Nbr_J_Chaud_moinssd = sum(Nbr_J_Chaud_moinssd),
        Nbr_J_TresChaud = sum(Nbr_J_TresChaud),
        Nbr_J_TresChaud_plussd = sum(Nbr_J_TresChaud_plussd),
        Nbr_J_TresChaud_moinssd = sum(Nbr_J_TresChaud_moinssd),
        Nbr_J_TresTresChaud = sum(Nbr_J_TresTresChaud),
        Nbr_J_TresTresChaud_plussd = sum(Nbr_J_TresTresChaud_plussd),
        Nbr_J_TresTresChaud_moinssd = sum(Nbr_J_TresTresChaud_moinssd)
      )
    Wind_annualmean <- Wind_mean %>%
      dplyr::mutate(Year = year(DATE)) %>%
      dplyr::mutate(
        sd = ifelse(!is.na(sd_wind), sd_wind, 0),
        Values_plussd = Values + sd,
        Values_moinssd = Values - sd
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        Wind_mean = mean(Values),
        Wind_plussd = mean(Values_plussd),
        Wind_moinssd = mean(Values_moinssd)
      )
    ETP_annualmean <- ETP_mean %>%
      dplyr::mutate(Year = year(DATE)) %>%
      dplyr::mutate(
        sd = ifelse(!is.na(sd_ETP), sd_ETP, 0),
        Values_plussd = Values + sd,
        Values_moinssd = Values - sd
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        ETP_mean_day = mean(Values),
        ETP_plussd = mean(Values_plussd),
        ETP_moinssd = mean(Values_moinssd)
      )
    ETP_annualsum <- ETP_mean %>%
      dplyr::mutate(Year = year(DATE)) %>%
      dplyr::mutate(
        sd = ifelse(!is.na(sd_ETP), sd_ETP, 0),
        Values_plussd = Values + sd,
        Values_moinssd = Values - sd
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        ETP_sum = sum(Values),
        ETP_sum_plussd = sum(Values_plussd),
        ETP_sum_moinssd = sum(Values_moinssd)
      )
    Nbr_jour_gel <- Tmin_absolu %>%
      dplyr::mutate(
        Year = year(DATE),
        sd = ifelse(!is.na(sd_Tminabs), sd_Tminabs, 0),
        Values_plussd = Values + sd,
        Values_moinssd = Values - sd,
        Nbr_jour_gel = ifelse(Values <= Jour_gel, 1, 0),
        Nbr_jour_gel_plussd = ifelse(Values_plussd <= Jour_gel, 1, 0),
        Nbr_jour_gel_moinssd = ifelse(Values_moinssd <= Jour_gel, 1, 0)
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        Nbr_jour_gel = sum(Nbr_jour_gel),
        Nbr_jour_gel_plussd = sum(Nbr_jour_gel_plussd),
        Nbr_jour_gel_moinssd = sum(Nbr_jour_gel_moinssd)
      )
    Nbr_jours_inf_sueil <- Tmean_mean %>%
      dplyr::mutate(
        Year = year(DATE),
        sd = ifelse(!is.na(sd_Tmean), sd_Tmean, 0),
        Values_plussd = Values + sd,
        Values_moinssd = Values - sd,
        Nbr_jours_inf_sueil = ifelse(Values < Seuil_T_base, 1, 0),
        Nbr_jours_inf_sueil_plussd = ifelse(Values_plussd < Seuil_T_base, 1, 0),
        Nbr_jours_inf_sueil_moinssd = ifelse(
          Values_moinssd < Seuil_T_base,
          1,
          0
        )
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        Nbr_jours_inf_sueil = sum(Nbr_jours_inf_sueil),
        Nbr_jours_inf_sueil_plussd = sum(Nbr_jours_inf_sueil_plussd),
        Nbr_jours_inf_sueil_moinssd = sum(Nbr_jours_inf_sueil_moinssd)
      )
    Nbr_jours_sup_sueil <- Tmean_mean %>%
      dplyr::mutate(
        Year = year(DATE),
        sd = ifelse(!is.na(sd_Tmean), sd_Tmean, 0),
        Values_plussd = Values + sd,
        Values_moinssd = Values - sd,
        Nbr_jours_sup_sueil = ifelse(Values > Seuil_T_base, 1, 0),
        Nbr_jours_sup_sueil_plussd = ifelse(Values_plussd > Seuil_T_base, 1, 0),
        Nbr_jours_sup_sueil_moinssd = ifelse(
          Values_moinssd > Seuil_T_base,
          1,
          0
        )
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        Nbr_jours_sup_sueil = sum(Nbr_jours_sup_sueil),
        Nbr_jours_sup_sueil_plussd = sum(Nbr_jours_sup_sueil_plussd),
        Nbr_jours_sup_sueil_moinssd = sum(Nbr_jours_sup_sueil_moinssd)
      )

    Meteo_data_annualmean <- Rain_annualsum %>%
      dplyr::full_join(Nbrfortespluies, by = "Year") %>%
      # dplyr::full_join(Eff_Rain_annualsum) %>%
      # dplyr::full_join(Tmin_annualmin) %>%
      # dplyr::full_join(Tmax_annualmax) %>%
      dplyr::full_join(Tmin_annualabsolu, by = "Year") %>%
      dplyr::full_join(Nbr_nuit_trop, by = "Year") %>%
      dplyr::full_join(Tmax_annualabsolu, by = "Year") %>%
      dplyr::full_join(Tmean_annualmean, by = "Year") %>%
      dplyr::full_join(Nbr_jour_chaud, by = "Year") %>%
      dplyr::full_join(Wind_annualmean, by = "Year") %>%
      dplyr::full_join(ETP_annualmean, by = "Year") %>%
      dplyr::full_join(ETP_annualsum, by = "Year") %>%
      dplyr::full_join(Nbr_jour_gel, by = "Year") %>%
      dplyr::full_join(Nbr_jours_inf_sueil, by = "Year") %>%
      dplyr::full_join(Nbr_jours_sup_sueil, by = "Year")

    liste <- c(
      "Rain",
      "Nbr_J_FortePluie",
      "Tmin_absolue",
      "Nbr_Nuit_Trop",
      "Tmax_absolue",
      "Tmean_day",
      "Nbr_J_Chaud",
      "Nbr_J_TresChaud",
      "Nbr_J_TresTresChaud",
      "Wind_mean",
      "ETP_mean_day",
      "ETP_sum",
      "Nbr_jour_gel",
      "Nbr_jours_inf_sueil",
      "Nbr_jours_sup_sueil"
    )
    liste_added <- c(
      '',
      paste0(' (>', mm_fortepluie, 'mm)'),
      '',
      paste0(' (>', Temperature_nuittropic, '°C)'),
      '',
      '',
      paste0(' (>', Temperature_hot, '°C)'),
      paste0(' (>', Temperature_veryhot, '°C)'),
      paste0(' (>', Temperature_veryveryhot, '°C)'),
      '',
      '',
      '',
      paste0(' (<', Jour_gel, '°C)'),
      paste0(' (<', Seuil_T_base, '°C)'),
      paste0(' (>', Seuil_T_base, '°C)')
    )

    # Indic_i <- which(colnames(Meteo_data_annualmean)%in%liste)[2]
    indics_year <- colnames(Meteo_data_annualmean)[
      which(colnames(Meteo_data_annualmean) %in% liste)
    ]
    progress_indics_year <- cli_progress_bar(
      format = "Indic {pb_current}/{pb_total}: {pb_status}",
      total = length(indics_year),
      clear = TRUE
    )
    for (Indic in indics_year) {
      cli_progress_update(id = progress_indics_year, status = Indic)
      Data_meteo_indic <- Meteo_data_annualmean
      colnames(Data_meteo_indic)[which(
        colnames(Data_meteo_indic) == Indic
      )] <- 'Indicateur'
      colnames(Data_meteo_indic)[
        which(colnames(Data_meteo_indic) == 'Indicateur') + 1
      ] <- 'Indicateur_plussd'
      colnames(Data_meteo_indic)[
        which(colnames(Data_meteo_indic) == 'Indicateur') + 2
      ] <- 'Indicateur_moinssd'
      added_information <- liste_added[which(liste == Indic)]

      ggplot(data = Data_meteo_indic) +
        geom_bar(
          stat = 'identity',
          aes(
            x = as.numeric(Year),
            y = as.numeric(Indicateur),
            fill = as.factor(Year)
          )
        ) +
        geom_errorbar(
          aes(
            x = as.numeric(Year),
            ymin = Indicateur_moinssd,
            ymax = Indicateur_plussd
          ),
          width = 0.3
        ) +
        scale_y_continuous(paste0(Indic, added_information)) +
        scale_x_continuous('Year') +
        theme_bw() +
        theme(
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          legend.position = 'none'
        )
      ggsave(paste0(Path_scen_res, '/Meteo/', Indic, '.png'))
    }
    cli_progress_done(id = progress_indics_year)
    #end creation graphs yearly
  }
  #end loop scenario

  #comparateur multi sceanri
  if (
    !file.exists(paste0(Path_Run, '/Results/Scenario_comparison', '/Meteo'))
  ) {
    dir.create(paste0(Path_Run, '/Results/Scenario_comparison', '/Meteo'))
  }

  for (Scen in Scenario$Scenario) {
    Scen_i <- which(Scenario$Scenario == Scen)
    Path_scen <- paste0(Path_Run_chargementdata, '/', Scen_i, '_', Scen)
    Path_scen_res <- paste0(Path_Run, '/Results/', Scen_i, '_', Scen)

    Meteo_scen <- read.csv2(paste0(Path_scen_res, '/Meteo/Meteo_data.csv')) %>%
      dplyr::mutate(Scenario = Scen)
    if (Scen == first(Scenario$Scenario)) {
      Meteo_Scen_join <- Meteo_scen
    }
    if (Scen != first(Scenario$Scenario)) {
      Meteo_Scen_join <- Meteo_Scen_join %>% bind_rows(Meteo_scen)
    }
    if (Scen == last(Scenario$Scenario)) {
      write.csv2(
        Meteo_Scen_join,
        paste0(
          Path_Run,
          '/Results/Scenario_comparison',
          '/Meteo/Meteo_Scenari.csv'
        )
      )
    }
  } #end aggregation loop

  # if(Scenario$Climat_data_source[Scen_i]=='Safran'){
  Rain <- Meteo_Scen_join %>% dplyr::filter(Climatic_data == 'PRELIQ_Q')
  # Eff_Rain <- Meteo_Scen_join %>% dplyr::filter(Climatic_data=='PE_Q')
  Tmin <- Meteo_Scen_join %>% dplyr::filter(Climatic_data == 'TINF_H_Q')
  Tmax <- Meteo_Scen_join %>% dplyr::filter(Climatic_data == 'TSUP_H_Q')
  Tmean <- Meteo_Scen_join %>% dplyr::filter(Climatic_data == 'T_Q')
  Wind <- Meteo_Scen_join %>% dplyr::filter(Climatic_data == 'FF_Q')
  ETP <- Meteo_Scen_join %>% dplyr::filter(Climatic_data == 'ETP_Q')
  # }

  Rain_mean <- Rain %>%
    dplyr::group_by(DATE, Scenario) %>%
    dplyr::summarise(Values = mean(Values), sd_rain = sd(Values))
  # Eff_Rain_mean <- Eff_Rain %>% dplyr::group_by(DATE,Scenario) %>% dplyr::summarise(Values=mean(Values),sd_effrain=sd(Values))
  Tmin_mean <- Tmin %>%
    dplyr::group_by(DATE, Scenario) %>%
    dplyr::summarise(Values = mean(Values), sd_Tmin = sd(Values))
  Tmax_mean <- Tmax %>%
    dplyr::group_by(DATE, Scenario) %>%
    dplyr::summarise(Values = mean(Values), sd_Tmax = sd(Values))
  Tmin_absolu <- Tmin %>%
    dplyr::group_by(DATE, Scenario) %>%
    dplyr::summarise(Values = min(Values), sd_Tminabs = sd(Values))
  Tmax_absolu <- Tmax %>%
    dplyr::group_by(DATE, Scenario) %>%
    dplyr::summarise(Values = max(Values), sd_Tmaxabs = sd(Values))
  Tmean_mean <- Tmean %>%
    dplyr::group_by(DATE, Scenario) %>%
    dplyr::summarise(Values = mean(Values), sd_Tmean = sd(Values))
  Wind_mean <- Wind %>%
    dplyr::group_by(DATE, Scenario) %>%
    dplyr::summarise(Values = mean(Values), sd_wind = sd(Values))
  ETP_mean <- ETP %>%
    dplyr::group_by(DATE, Scenario) %>%
    dplyr::summarise(Values = mean(Values), sd_ETP = sd(Values))

  Meteo_data_daymean <- Rain_mean %>%
    dplyr::rename('Rain' = 'Values') %>%
    # dplyr::full_join(Eff_Rain_mean) %>% dplyr::rename('Rain_eff'='Values') %>%
    dplyr::full_join(Tmin_mean, by = c("DATE", "Scenario")) %>%
    dplyr::rename('Tmin' = 'Values') %>%
    dplyr::full_join(Tmax_mean, by = c("DATE", "Scenario")) %>%
    dplyr::rename('Tmax' = 'Values') %>%
    dplyr::full_join(Tmin_absolu, by = c("DATE", "Scenario")) %>%
    dplyr::rename('Tmin_absolue' = 'Values') %>%
    dplyr::full_join(Tmax_absolu, by = c("DATE", "Scenario")) %>%
    dplyr::rename('Tmax_absolue' = 'Values') %>%
    dplyr::full_join(Tmean_mean, by = c("DATE", "Scenario")) %>%
    dplyr::rename('Tmean' = 'Values') %>%
    dplyr::full_join(Wind_mean, by = c("DATE", "Scenario")) %>%
    dplyr::rename('Wind' = 'Values') %>%
    dplyr::full_join(ETP_mean, by = c("DATE", "Scenario")) %>%
    dplyr::rename('ETP' = 'Values')

  for (Indic_i in 3:length(colnames(Meteo_data_daymean)[which(
    !str_detect(colnames(Meteo_data_daymean), 'sd_')
  )])) {
    Indic <- (colnames(Meteo_data_daymean)[which(
      !str_detect(colnames(Meteo_data_daymean), 'sd_')
    )])[Indic_i]
    Data_meteo_indic <- Meteo_data_daymean
    colnames(Data_meteo_indic)[which(
      colnames(Data_meteo_indic) == Indic
    )] <- 'Indicateur'
    colnames(Data_meteo_indic)[
      which(colnames(Data_meteo_indic) == 'Indicateur') + 1
    ] <- 'Indicateur_sd'

    ggplot(data = Data_meteo_indic) +
      geom_point(aes(
        x = as.Date(DATE),
        y = as.numeric(Indicateur),
        col = as.factor(year(as.Date(DATE)))
      )) +
      scale_y_continuous(Indic) +
      scale_x_date('Dates') +
      theme_bw() +
      theme(
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.position = 'none'
      )
    ggsave(paste0(
      Path_Run,
      '/Results/Scenario_comparison',
      '/Meteo/Day_',
      Indic,
      '.png'
    ))

    ggplot(data = Data_meteo_indic) +
      geom_point(aes(
        x = as.Date(DATE),
        y = as.numeric(Indicateur),
        col = Scenario
      )) +
      geom_line(aes(
        x = as.Date(DATE),
        y = as.numeric(Indicateur),
        col = Scenario
      )) +
      scale_y_continuous(Indic) +
      scale_x_date('Dates') +
      theme_bw() +
      theme(
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12)
      )
    ggsave(paste0(
      Path_Run,
      '/Results/Scenario_comparison',
      '/Meteo/Day_',
      Indic,
      '_plusline.png'
    ))
  } #end creation graphs daily

  Rain_annualsum <- Rain_mean %>%
    dplyr::mutate(Year = year(DATE)) %>%
    dplyr::mutate(
      sd = ifelse(!is.na(sd_rain), sd_rain, 0),
      Values_plussd = Values + sd,
      Values_moinssd = Values - sd
    ) %>%
    dplyr::group_by(Year, Scenario) %>%
    dplyr::summarise(
      Rain = sum(Values),
      Rain_plussd = sum(Values_plussd),
      Rain_moinssd = sum(Values_moinssd)
    )
  Nbrfortespluies <- Rain_mean %>%
    dplyr::mutate(
      Year = year(DATE),
      sd = ifelse(!is.na(sd_rain), sd_rain, 0),
      Values_plussd = Values + sd,
      Values_moinssd = Values - sd,
      NJFP = ifelse(Values > mm_fortepluie, 1, 0),
      NJFP_plussd = ifelse(Values_plussd > mm_fortepluie, 1, 0),
      NJFP_moinssd = ifelse(Values_moinssd > mm_fortepluie, 1, 0)
    ) %>%
    dplyr::group_by(Year, Scenario) %>%
    dplyr::summarise(
      Nbr_J_FortePluie = sum(NJFP),
      Nbr_J_FortePluie_plussd = sum(NJFP_plussd),
      Nbr_J_FortePluie_moinssd = sum(NJFP_moinssd)
    )
  # Eff_Rain_annualsum <- Eff_Rain_mean %>% dplyr::mutate(Year=year(DATE)) %>% dplyr::mutate(sd=ifelse(!is.na(sd_effrain),sd_effrain,0),Values_plussd=Values+sd,Values_moinssd=Values-sd) %>% dplyr::group_by(Year,Scenario) %>% dplyr::summarise(Rain_eff=sum(Values),Raineff_plussd=sum(Values_plussd),Raineff_moinssd=sum(Values_moinssd))
  # Tmin_annualmin <- Tmin_mean %>% dplyr::mutate(Year=year(DATE))  %>% dplyr::mutate(sd=ifelse(!is.na(sd_Tmin),sd_Tmin,0),Values_plussd=Values+sd,Values_moinssd=Values-sd) %>% dplyr::group_by(Year,Scenario) %>% dplyr::summarise(Tmin_annualmean=min(Values),Tmin_plussd=mean(Values_plussd),Tmin_moinssd=mean(Values_moinssd))
  # Tmax_annualmax <- Tmax_mean %>% dplyr::mutate(Year=year(DATE))  %>% dplyr::mutate(sd=ifelse(!is.na(sd_Tmax),sd_Tmax,0),Values_plussd=Values+sd,Values_moinssd=Values-sd) %>% dplyr::group_by(Year,Scenario)%>% dplyr::summarise(Tmax_annualmean=mean(Values,Tmax_plussd=mean(Values_plussd),Tmax_moinssd=mean(Values_moinssd)))
  Tmin_annualabsolu <- Tmin_absolu %>%
    dplyr::mutate(Year = year(DATE)) %>%
    dplyr::mutate(
      sd = ifelse(!is.na(sd_Tminabs), sd_Tminabs, 0),
      Values_plussd = Values + sd,
      Values_moinssd = Values - sd
    ) %>%
    dplyr::group_by(Year, Scenario) %>%
    dplyr::summarise(
      Tmin_absolue = min(Values),
      Tminabs_plussd = min(Values_plussd),
      Tminabs_moinssd = min(Values_moinssd)
    )
  Nbr_nuit_trop <- Tmin_absolu %>%
    dplyr::mutate(
      Year = year(DATE),
      sd = ifelse(!is.na(sd_Tminabs), sd_Tminabs, 0),
      Values_plussd = Values + sd,
      Values_moinssd = Values - sd,
      Nbr_Nuit_Trop = ifelse(Values >= Temperature_nuittropic, 1, 0),
      Nbr_Nuit_Trop_plussd = ifelse(
        Values_plussd >= Temperature_nuittropic,
        1,
        0
      ),
      Nbr_Nuit_Trop_moinssd = ifelse(
        Values_moinssd >= Temperature_nuittropic,
        1,
        0
      )
    ) %>%
    dplyr::group_by(Year, Scenario) %>%
    dplyr::summarise(
      Nbr_Nuit_Trop = sum(Nbr_Nuit_Trop),
      Nbr_Nuit_Trop_plussd = sum(Nbr_Nuit_Trop_plussd),
      Nbr_Nuit_Trop_moinssd = sum(Nbr_Nuit_Trop_moinssd)
    )
  Tmax_annualabsolu <- Tmax_absolu %>%
    dplyr::mutate(Year = year(DATE)) %>%
    dplyr::mutate(
      sd = ifelse(!is.na(sd_Tmaxabs), sd_Tmaxabs, 0),
      Values_plussd = Values + sd,
      Values_moinssd = Values - sd
    ) %>%
    dplyr::group_by(Year, Scenario) %>%
    dplyr::summarise(
      Tmax_absolue = max(Values),
      Tmaxabs_plussd = max(Values_plussd),
      Tmaxabs_moinssd = max(Values_moinssd)
    )
  Tmean_annualmean <- Tmean_mean %>%
    dplyr::mutate(Year = year(DATE)) %>%
    dplyr::mutate(
      sd = ifelse(!is.na(sd_Tmean), sd_Tmean, 0),
      Values_plussd = Values + sd,
      Values_moinssd = Values - sd
    ) %>%
    dplyr::group_by(Year, Scenario) %>%
    dplyr::summarise(
      Tmean_day = mean(Values),
      Tmean_plussd = mean(Values_plussd),
      Tmean_moinssd = mean(Values_moinssd)
    )
  Nbr_jour_chaud <- Tmean_mean %>%
    dplyr::mutate(
      Year = year(DATE),
      sd = ifelse(!is.na(sd_Tmean), sd_Tmean, 0),
      Values_plussd = Values + sd,
      Values_moinssd = Values - sd,
      Nbr_J_Chaud = ifelse(Values >= Temperature_hot, 1, 0),
      Nbr_J_Chaud_plussd = ifelse(Values_plussd >= Temperature_hot, 1, 0),
      Nbr_J_Chaud_moinssd = ifelse(Values_moinssd >= Temperature_hot, 1, 0),
      Nbr_J_TresChaud = ifelse(Values >= Temperature_veryhot, 1, 0),
      Nbr_J_TresChaud_plussd = ifelse(
        Values_plussd >= Temperature_veryhot,
        1,
        0
      ),
      Nbr_J_TresChaud_moinssd = ifelse(
        Values_moinssd >= Temperature_veryhot,
        1,
        0
      ),
      Nbr_J_TresTresChaud = ifelse(Values >= Temperature_veryveryhot, 1, 0),
      Nbr_J_TresTresChaud_plussd = ifelse(
        Values_plussd >= Temperature_veryveryhot,
        1,
        0
      ),
      Nbr_J_TresTresChaud_moinssd = ifelse(
        Values_moinssd >= Temperature_veryveryhot,
        1,
        0
      )
    ) %>%
    dplyr::group_by(Year, Scenario) %>%
    dplyr::summarise(
      Nbr_J_Chaud = sum(Nbr_J_Chaud),
      Nbr_J_Chaud_plussd = sum(Nbr_J_Chaud_plussd),
      Nbr_J_Chaud_moinssd = sum(Nbr_J_Chaud_moinssd),
      Nbr_J_TresChaud = sum(Nbr_J_TresChaud),
      Nbr_J_TresChaud_plussd = sum(Nbr_J_TresChaud_plussd),
      Nbr_J_TresChaud_moinssd = sum(Nbr_J_TresChaud_moinssd),
      Nbr_J_TresTresChaud = sum(Nbr_J_TresTresChaud),
      Nbr_J_TresTresChaud_plussd = sum(Nbr_J_TresTresChaud_plussd),
      Nbr_J_TresTresChaud_moinssd = sum(Nbr_J_TresTresChaud_moinssd)
    )
  Wind_annualmean <- Wind_mean %>%
    dplyr::mutate(Year = year(DATE)) %>%
    dplyr::mutate(
      sd = ifelse(!is.na(sd_wind), sd_wind, 0),
      Values_plussd = Values + sd,
      Values_moinssd = Values - sd
    ) %>%
    dplyr::group_by(Year, Scenario) %>%
    dplyr::summarise(
      Wind_mean = mean(Values),
      Wind_plussd = mean(Values_plussd),
      Wind_moinssd = mean(Values_moinssd)
    )
  ETP_annualmean <- ETP_mean %>%
    dplyr::mutate(Year = year(DATE)) %>%
    dplyr::mutate(
      sd = ifelse(!is.na(sd_ETP), sd_ETP, 0),
      Values_plussd = Values + sd,
      Values_moinssd = Values - sd
    ) %>%
    dplyr::group_by(Year, Scenario) %>%
    dplyr::summarise(
      ETP_mean_day = mean(Values),
      ETP_plussd = mean(Values_plussd),
      ETP_moinssd = mean(Values_moinssd)
    )

  Meteo_data_annualmean <- Rain_annualsum %>%
    dplyr::full_join(Nbrfortespluies, by = c("Year", "Scenario")) %>%
    # dplyr::full_join(Eff_Rain_annualsum) %>%
    # dplyr::full_join(Tmin_annualmin) %>%
    # dplyr::full_join(Tmax_annualmax) %>%
    dplyr::full_join(Tmin_annualabsolu, by = c("Year", "Scenario")) %>%
    dplyr::full_join(Nbr_nuit_trop, by = c("Year", "Scenario")) %>%
    dplyr::full_join(Tmax_annualabsolu, by = c("Year", "Scenario")) %>%
    dplyr::full_join(Tmean_annualmean, by = c("Year", "Scenario")) %>%
    dplyr::full_join(Nbr_jour_chaud, by = c("Year", "Scenario")) %>%
    dplyr::full_join(Wind_annualmean, by = c("Year", "Scenario")) %>%
    dplyr::full_join(ETP_annualmean, by = c("Year", "Scenario"))

  for (Indic_i in 3:length(colnames(Meteo_data_annualmean)[which(
    !str_detect(colnames(Meteo_data_annualmean), 'sd')
  )])) {
    Indic <- (colnames(Meteo_data_annualmean)[which(
      !str_detect(colnames(Meteo_data_annualmean), 'sd')
    )])[Indic_i]
    Data_meteo_indic <- Meteo_data_annualmean
    colnames(Data_meteo_indic)[which(
      colnames(Data_meteo_indic) == Indic
    )] <- 'Indicateur'
    colnames(Data_meteo_indic)[
      which(colnames(Data_meteo_indic) == 'Indicateur') + 1
    ] <- 'Indicateur_plussd'
    colnames(Data_meteo_indic)[
      which(colnames(Data_meteo_indic) == 'Indicateur') + 2
    ] <- 'Indicateur_moinssd'

    ggplot(data = Data_meteo_indic, aes(fill = Scenario)) +
      geom_bar(
        stat = 'identity',
        position = 'dodge',
        aes(x = as.factor(Year), y = Indicateur)
      ) +
      geom_errorbar(
        position = position_dodge(0.9),
        aes(
          x = as.factor(Year),
          ymin = Indicateur_moinssd,
          ymax = Indicateur_plussd
        ),
        width = 0.3
      ) +
      scale_y_continuous(Indic) +
      scale_x_discrete('Year') +
      theme_bw() +
      theme(
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12)
      )
    ggsave(paste0(
      Path_Run,
      '/Results/Scenario_comparison',
      '/Meteo/',
      Indic,
      '.png'
    ))
  } #end creation graphs yearly
  cli_alert_success(
    "run_script3_3 - Indicateurs climatiques - execute avec succes"
  )
}
