#' Run Cropwat CAWET simulation for a given crop, irrigation method and sequence
#'
#' This function runs the Cropwat CAWET simulation for a specified crop, irrigation method, and sequence. It takes into account various parameters such as irrigation settings, crop characteristics, meteorological data, and simulation settings to generate detailed output results.
#' @param Irrigation_param A data frame containing irrigation parameters.
#' @param mod_irr A string representing the irrigation method.
#' @param Sequence A data frame containing sequence information.
#' @param carac_date_irr A logical value indicating whether to use specific irrigation dates
#' @param Link_RPG_mod A data frame linking RPG crop names to Cropwat crop names.
#' @param Crop A string representing the crop name.
#' @param Crops_param A data frame containing crop parameters.
#' @param Meteo_maille A data frame containing meteorological data for the grid cell
#' @param liste_not_parametredcrops A vector of strings representing crops that are
#' not parameterized.
#' @return None. The function saves the detailed output results to a CSV file.
#' @export
#'
Run_Cropwat_CAWET <- function(
  Irrigation_param,
  mod_irr,
  Sequence,
  carac_date_irr,
  Link_RPG_mod,
  Crop,
  Crops_param,
  Meteo_maille,
  liste_not_parametredcrops
) {
  #Irrigation_param=Irrigation_param

  #Cropwat

  #Irrigation parameter
  Cw_Raw_rat <- Irrigation_param[which(Irrigation_param$Irri == mod_irr), 2]
  Reffil_RU <- ifelse(
    Irrigation_param[which(Irrigation_param$Irri == mod_irr), 3] == "Yes",
    TRUE,
    FALSE
  )
  # safe numeric parse: check for any digit first, then parse quietly
  Cw_Raw_num <- if (is.na(Cw_Raw_rat) || !grepl("\\d", Cw_Raw_rat)) {
    NA_real_
  } else {
    suppressWarnings(as.numeric(Cw_Raw_rat))
  }

  if (!is.na(Cw_Raw_num)) {
    carac_date_irr <- carac_date_irr
  } else {
    #Si pas de restriction d'irrigation
    carac_date_irr <- TRUE
  }

  #Crop parameter
  Cw_line <- Link_RPG_mod %>% dplyr::filter(str_detect(Crop_RPG, Crop))

  if (length(rownames(Cw_line)) == 1) {
    Cp <- Crops_param %>%
      dplyr::filter(crop == Cw_line$Crop_cw[1]) %>%
      dplyr::mutate(
        sowing_date = ifelse(!is.na(sowing_date), sowing_date, "01_01")
      )

    Start_date <- as.Date(Sequence$Start, format = "%Y-%m-%d")
    Stop_date <- as.Date(Sequence$End, format = "%Y-%m-%d")

    if (all(grepl("^../../....$", Meteo_maille$DATE))) {
      Meteo_maille <- Meteo_maille %>%
        dplyr::mutate(DATE = as.Date(DATE, format = "%d/%m/%Y")) %>%
        dplyr::mutate(time = DATE)
    }

    Meteo_sequ <- Meteo_maille %>%
      dplyr::filter(DATE >= Start_date) %>%
      dplyr::filter(DATE <= Stop_date)

    AWC_valeur <- Sequence$AWC

    cw_input <- CropWat::CW_create_input(
      crop = Crop,
      DatesR = as.Date(c(Start_date:Stop_date), format = "%Y-%m-%d"),
      ETo = as.numeric(Meteo_sequ$ETP_Q),
      P = as.numeric(Meteo_sequ$PRELIQ_Q),
      soil_depth = 1.2,
      AWC = AWC_valeur,
      cp = Cp,
      sowing_date = paste0(
        substr(Cp$sowing_date, 1, 2),
        '-',
        substr(Cp$sowing_date, 4, 5)
      )
    )
    # plot(cw_input)
    # Set up of the initial state of the model
    X <- CropWat::CW_create_state(cw_input = cw_input)

    #Simulation Cropwat
    fun_irrig_Cw <- CropWat::CW_irrig_fun_factory(
      RAW_ratio = as.numeric(Cw_Raw_rat),
      apply_Dr = Reffil_RU,
      dates_irrig = carac_date_irr
    )
    cw_output <- CropWat::CW_run_simulation(
      X,
      cw_input,
      FUN_IRRIG = fun_irrig_Cw
    )
    cw_output <- cw_output %>% dplyr::full_join(cw_input, by = "DatesR")
    return(cw_output)
  } else {
    # end if crop is parametred
    cli_alert_danger("{Crop} is not parametred")
    liste_not_parametredcrops <- c(liste_not_parametredcrops, Crop)
    return(NULL)
  } # end if crop is not parametred
}
