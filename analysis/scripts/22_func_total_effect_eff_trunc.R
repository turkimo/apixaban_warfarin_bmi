# Function to calculate total effects of treatments on stroke/SE using 99% truncated IPWs

func_results_eff_trunc <- function(data,outcome,outcome1, cut_times) {

  # Get IPT weights
ipt_wt_eff(dat_ipweights_eff)

data1 = merge(data,ipt_weights) %>% merge(dat_ipweights_mort)

data1$bmi_groups_n = as.integer(data1$bmi_groups)

# Long data for time-varying IPC weights
  data_event_cens_long <- data1 %>% 
    survSplit(
      cut = cut_times,
      start = "Tstart",
      end = "time_end",
      event = "cens_36")
  
  data_event_long <- data1 %>% 
    survSplit(
      cut = cut_times,
      start = "Tstart",
      end = "time_end",
      event = "event_compet_36") %>% 
    mutate(
      eventCens = data_event_cens_long$cens_36,
      # event indicator
      event_36_long = if_else(event_compet_36 == 1 & eventType == 1, 1, 0),
      # competing events (death) indicator
      compet_36_long = if_else(event_compet_36 == 1 & eventType == 2, 1, 0)
    )
  
  data_event_long %<>% mutate(
    # Assign the outcome for participants censored or died before the outcome as NA 
    event_36_long = if_else(eventCens == 1, NA_real_, event_36_long),
    compet_36_long = if_else(eventCens == 1, NA_real_, compet_36_long),
    event_36_long = if_else(compet_36_long == 1, NA_real_, event_36_long)
  )
  
  
  data_event_long %<>% filter(time_end < length(cut_times))
  
  # interaction of time to allow time dependent baseline hazard
  data_event_long$time_end2 <- data_event_long$time_end * data_event_long$time_end
  data_event_long$time_end3 <- data_event_long$time_end2 * data_event_long$time_end
  
  # time dependent coefficients for the hazard of stroke/SE
  data_event_long$drugsub1 <- data_event_long$time_end * data_event_long$drugsub
  data_event_long$drugsub2 <- data_event_long$time_end2 * data_event_long$drugsub
  data_event_long$drugsub3 <- data_event_long$time_end3 * data_event_long$drugsub
  
  # IPC weighting
  ipc_wt_eff(data_event_long)
  
  
  # Compute survival probability per arm per month 
  
  # Treatment 1 api ####
  hazard_treated_ipcw_cens_stab <- nonParametricCumHaz_all(an_data$ipw_sw_all.trunc, 
                                                           inputdata = an_data, grp = 1, 
                                                           event_36_long = TRUE)
  
  hazardcomp_treated_ipcw_cens_stab <- nonParametricCumHaz_all(an_data$ipw_sw_all.trunc, 
                                                               inputdata = an_data, grp = 1, 
                                                               event_36_long = FALSE)
  
  cuminc_treated_ipcw_cens_stab <- nonParametricCumInc(hazard1 = hazard_treated_ipcw_cens_stab,
                                                       hazardcomp_treated_ipcw_cens_stab)
  
  ipcw_stab_data_treated <- bind_cols("hazard_treated" = hazard_treated_ipcw_cens_stab,
                                      "hazardcompet_treated" = hazardcomp_treated_ipcw_cens_stab, 
                                      "cuminc_treated" = cuminc_treated_ipcw_cens_stab)
  
  # Treatment 1 api  normal weight 
  

  hazard_treated_ipcw_cens_stab_norm <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                            inputdata = an_data, grp = 1, bmi_grp = 1, 
                                                            event_36_long = TRUE)
  
  hazardcomp_treated_ipcw_cens_stab_norm <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                                inputdata = an_data, grp = 1, bmi_grp = 1, 
                                                                event_36_long = FALSE)
  
  cuminc_treated_ipcw_cens_stab_norm <- nonParametricCumInc(hazard1 = hazard_treated_ipcw_cens_stab_norm,
                                                            hazardcomp_treated_ipcw_cens_stab_norm)
  
  ipcw_stab_data_treated_norm <- bind_cols("hazard_treated" = hazard_treated_ipcw_cens_stab_norm,
                                           "hazardcompet_treated" = hazardcomp_treated_ipcw_cens_stab_norm, 
                                           "cuminc_treated" = cuminc_treated_ipcw_cens_stab_norm)
  
  
  
  
  # Treatment 1 api  overweight 
  

  hazard_treated_ipcw_cens_stab_over <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                            inputdata = an_data, grp = 1, bmi_grp = 2, 
                                                            event_36_long = TRUE)
  
  hazardcomp_treated_ipcw_cens_stab_over <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                                inputdata = an_data, grp = 1, bmi_grp = 2, 
                                                                event_36_long = FALSE)
  
  cuminc_treated_ipcw_cens_stab_over <- nonParametricCumInc(hazard1 = hazard_treated_ipcw_cens_stab_over,
                                                            hazardcomp_treated_ipcw_cens_stab_over)
  
  ipcw_stab_data_treated_over <- bind_cols("hazard_treated" = hazard_treated_ipcw_cens_stab_over,
                                           "hazardcompet_treated" = hazardcomp_treated_ipcw_cens_stab_over, 
                                           "cuminc_treated" = cuminc_treated_ipcw_cens_stab_over)
  
  
  
  # Treatment 1 api  obese 
  

  hazard_treated_ipcw_cens_stab_obese <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                             inputdata = an_data, grp = 1, bmi_grp = 3, 
                                                             event_36_long = TRUE)
  
  hazardcomp_treated_ipcw_cens_stab_obese <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                                 inputdata = an_data, grp = 1, bmi_grp = 3, 
                                                                 event_36_long = FALSE)
  
  cuminc_treated_ipcw_cens_stab_obese <- nonParametricCumInc(hazard1 = hazard_treated_ipcw_cens_stab_obese,
                                                             hazardcomp_treated_ipcw_cens_stab_obese)
  
  ipcw_stab_data_treated_obese <- bind_cols("hazard_treated" = hazard_treated_ipcw_cens_stab_obese,
                                            "hazardcompet_treated" = hazardcomp_treated_ipcw_cens_stab_obese, 
                                            "cuminc_treated" = cuminc_treated_ipcw_cens_stab_obese)
  
  
  
  
  
  # Treatment 0 warf ####
  hazard_untreated_ipcw_cens_stab <- nonParametricCumHaz_all(an_data$ipw_sw_all.trunc, 
                                                             inputdata = an_data, grp = 0, 
                                                             event_36_long = TRUE)
  
  hazardcomp_untreated_ipcw_cens_stab <- nonParametricCumHaz_all(an_data$ipw_sw_all.trunc, 
                                                                 inputdata = an_data, grp = 0, 
                                                                 event_36_long = FALSE)
  
  cuminc_untreated_ipcw_cens_stab <- nonParametricCumInc(hazard1 = hazard_untreated_ipcw_cens_stab,
                                                         hazardcomp_untreated_ipcw_cens_stab)
  
  ipcw_stab_data_untreated <- bind_cols("hazard_untreated" = hazard_untreated_ipcw_cens_stab,
                                        "hazardcompet_untreated" = hazardcomp_untreated_ipcw_cens_stab, 
                                        "cuminc_untreated" = cuminc_untreated_ipcw_cens_stab)
  
  
  # Treatment 0 warf  normal weight 
  

  hazard_untreated_ipcw_cens_stab_norm <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                              inputdata = an_data, grp = 0, bmi_grp = 1, 
                                                              event_36_long = TRUE)
  
  hazardcomp_untreated_ipcw_cens_stab_norm <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                                  inputdata = an_data, grp = 0, bmi_grp = 1, 
                                                                  event_36_long = FALSE)
  
  cuminc_untreated_ipcw_cens_stab_norm <- nonParametricCumInc(hazard1 = hazard_untreated_ipcw_cens_stab_norm,
                                                              hazardcomp_untreated_ipcw_cens_stab_norm)
  
  ipcw_stab_data_untreated_norm <- bind_cols("hazard_untreated" = hazard_untreated_ipcw_cens_stab_norm,
                                             "hazardcompet_untreated" = hazardcomp_untreated_ipcw_cens_stab_norm, 
                                             "cuminc_untreated" = cuminc_untreated_ipcw_cens_stab_norm)
  
  
  
  
  # Treatment 0 warf  overweight 
  

  hazard_untreated_ipcw_cens_stab_over <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                              inputdata = an_data, grp = 0, bmi_grp = 2, 
                                                              event_36_long = TRUE)
  
  hazardcomp_untreated_ipcw_cens_stab_over <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                                  inputdata = an_data, grp = 0, bmi_grp = 2, 
                                                                  event_36_long = FALSE)
  
  cuminc_untreated_ipcw_cens_stab_over <- nonParametricCumInc(hazard1 = hazard_untreated_ipcw_cens_stab_over,
                                                              hazardcomp_untreated_ipcw_cens_stab_over)
  
  ipcw_stab_data_untreated_over <- bind_cols("hazard_untreated" = hazard_untreated_ipcw_cens_stab_over,
                                             "hazardcompet_untreated" = hazardcomp_untreated_ipcw_cens_stab_over, 
                                             "cuminc_untreated" = cuminc_untreated_ipcw_cens_stab_over)
  
  
  
  # Treatment 0 warf  obese 
  

  hazard_untreated_ipcw_cens_stab_obese <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                               inputdata = an_data, grp = 0, bmi_grp = 3, 
                                                               event_36_long = TRUE)
  
  hazardcomp_untreated_ipcw_cens_stab_obese <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                                   inputdata = an_data, grp = 0, bmi_grp = 3, 
                                                                   event_36_long = FALSE)
  
  cuminc_untreated_ipcw_cens_stab_obese <- nonParametricCumInc(hazard1 = hazard_untreated_ipcw_cens_stab_obese,
                                                               hazardcomp_untreated_ipcw_cens_stab_obese)
  
  ipcw_stab_data_untreated_obese <- bind_cols("hazard_untreated" = hazard_untreated_ipcw_cens_stab_obese,
                                              "hazardcompet_untreated" = hazardcomp_untreated_ipcw_cens_stab_obese, 
                                              "cuminc_untreated" = cuminc_untreated_ipcw_cens_stab_obese) 
  
  
  ipcw_stab_data0 <- bind_cols(bmi_group = "All", ipcw_stab_data_treated,ipcw_stab_data_untreated)
  
  ipcw_stab_data1 <- bind_cols(bmi_group = "Normal", ipcw_stab_data_treated_norm,ipcw_stab_data_untreated_norm)
  
  ipcw_stab_data2 <- bind_cols(bmi_group = "Overweight", ipcw_stab_data_treated_over,ipcw_stab_data_untreated_over)
  
  ipcw_stab_data3 <- bind_cols(bmi_group = "Obese", ipcw_stab_data_treated_obese,ipcw_stab_data_untreated_obese)
  
  ipcw_stab_data <- bind_rows(ipcw_stab_data0, ipcw_stab_data1,ipcw_stab_data2,ipcw_stab_data3)
  

  
  # Cumulative incidence for overall
  cuminc_0_all <- ipcw_stab_data %>% filter(bmi_group == "All" ) %$% cuminc_untreated[36]
  cuminc_1_all <- ipcw_stab_data %>% filter(bmi_group == "All" ) %$% cuminc_treated[36]
  Y_0_all <- cuminc_0_all
  Y_1_all <- cuminc_1_all
  riskdiff_all <- Y_1_all - Y_0_all
  riskratio_all <- Y_1_all/Y_0_all
  
  # Cumulative incidence for Normal weight
  cuminc_0_norm <- ipcw_stab_data %>% filter(bmi_group == "Normal" ) %$% cuminc_untreated[36]
  cuminc_1_norm <- ipcw_stab_data %>% filter(bmi_group == "Normal" ) %$% cuminc_treated[36]
  Y_0_norm <- cuminc_0_norm
  Y_1_norm <- cuminc_1_norm
  riskdiff_norm <- Y_1_norm - Y_0_norm
  riskratio_norm <- Y_1_norm/Y_0_norm
  
  
  
  # Cumulative incidence for Overweight
  cuminc_0_over <- ipcw_stab_data %>% filter(bmi_group == "Overweight" ) %$% cuminc_untreated[36]
  cuminc_1_over <- ipcw_stab_data %>% filter(bmi_group == "Overweight" ) %$% cuminc_treated[36]
  Y_0_over <- cuminc_0_over
  Y_1_over <- cuminc_1_over
  riskdiff_over <- Y_1_over - Y_0_over
  riskratio_over <- Y_1_over/Y_0_over
  
  
  
  # Cumulative incidence for Obese
  cuminc_0_obes <- ipcw_stab_data %>% filter(bmi_group == "Obese" ) %$% cuminc_untreated[36]
  cuminc_1_obes <- ipcw_stab_data %>% filter(bmi_group == "Obese" ) %$% cuminc_treated[36]
  Y_0_obes <- cuminc_0_obes
  Y_1_obes <- cuminc_1_obes
  riskdiff_obes <- Y_1_obes - Y_0_obes
  riskratio_obes <- Y_1_obes/Y_0_obes



  # Bootstrapping standard errors using parallel clustering ####
  
data_base = merge(data,ipt_weights) %>% merge(dat_ipweights_mort)

data_base$bmi_groups_n = as.integer(data_base$bmi_groups)

tstart <- Sys.time()

set.seed(123)
numboot <- 500

cluster <- makeCluster(5)
registerDoSNOW(cluster)

estimates <-  foreach(i = 1:numboot, .packages = c("tidyverse","magrittr","splitstackshape","survival"), .combine = "rbind") %dopar% {

  source("H:\\My Documents\\GitHub\\apixaban_warfarin_obese\\analysis\\scripts\\08_func_ipcw_eff.R")  
  source("H:\\My Documents\\GitHub\\apixaban_warfarin_obese\\analysis\\scripts\\11_func_nonparm_cumhazard.R")
  
  
  cut_times <- c(0:35)
  
  .GlobalEnv$cut_times <- cut_times
  
  indices <- sample(1:nrow(data_base), nrow(data_base), replace=T)
  
  data <- data_base[indices, ]
  
data_event_cens_long <- data %>% 
  survSplit(
    cut = cut_times,
    start = "Tstart",
    end = "time_end",
    event = "cens_36")

data_event_long <- data %>% 
  survSplit(
    cut = cut_times,
    start = "Tstart",
    end = "time_end",
    event = "event_compet_36") %>% 
  mutate(
    eventCens = data_event_cens_long$cens_36,
    # event indicator
    event_36_long = if_else(event_compet_36 == 1 & eventType == 1, 1, 0),
    # competing events (death) indicator
    compet_36_long = if_else(event_compet_36 == 1 & eventType == 2, 1, 0)
  )

data_event_long %<>% mutate(
  # Assign the outcome for participants censored or died before the outcome as NA 
  event_36_long = if_else(eventCens == 1, NA_real_, event_36_long),
  compet_36_long = if_else(eventCens == 1, NA_real_, compet_36_long),
  event_36_long = if_else(compet_36_long == 1, NA_real_, event_36_long)
)


data_event_long %<>% filter(time_end < length(cut_times))

# interaction of time to allow time dependent baseline hazard
data_event_long$time_end2 <- data_event_long$time_end * data_event_long$time_end
data_event_long$time_end3 <- data_event_long$time_end2 * data_event_long$time_end

# time dependent coefficients for the hazard of stroke/SE
data_event_long$drugsub1 <- data_event_long$time_end * data_event_long$drugsub
data_event_long$drugsub2 <- data_event_long$time_end2 * data_event_long$drugsub
data_event_long$drugsub3 <- data_event_long$time_end3 * data_event_long$drugsub

ipc_wt_eff(data_event_long)


# Compute survival probability per arm per month 

# Treatment 1 api ####
hazard_treated_ipcw_cens_stab <- nonParametricCumHaz_all(an_data$ipw_sw_all.trunc, 
                                                         inputdata = an_data, grp = 1, 
                                                         event_36_long = TRUE)

hazardcomp_treated_ipcw_cens_stab <- nonParametricCumHaz_all(an_data$ipw_sw_all.trunc, 
                                                             inputdata = an_data, grp = 1, 
                                                             event_36_long = FALSE)

cuminc_treated_ipcw_cens_stab <- nonParametricCumInc(hazard1 = hazard_treated_ipcw_cens_stab,
                                                     hazardcomp_treated_ipcw_cens_stab)

ipcw_stab_data_treated <- bind_cols("hazard_treated" = hazard_treated_ipcw_cens_stab,
                                    "hazardcompet_treated" = hazardcomp_treated_ipcw_cens_stab, 
                                    "cuminc_treated" = cuminc_treated_ipcw_cens_stab)


# Treatment 1 api  normal weight 


hazard_treated_ipcw_cens_stab_norm <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                          inputdata = an_data, grp = 1, bmi_grp = 1, 
                                                          event_36_long = TRUE)

hazardcomp_treated_ipcw_cens_stab_norm <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                              inputdata = an_data, grp = 1, bmi_grp = 1, 
                                                              event_36_long = FALSE)

cuminc_treated_ipcw_cens_stab_norm <- nonParametricCumInc(hazard1 = hazard_treated_ipcw_cens_stab_norm,
                                                          hazardcomp_treated_ipcw_cens_stab_norm)

ipcw_stab_data_treated_norm <- bind_cols("hazard_treated" = hazard_treated_ipcw_cens_stab_norm,
                                         "hazardcompet_treated" = hazardcomp_treated_ipcw_cens_stab_norm, 
                                         "cuminc_treated" = cuminc_treated_ipcw_cens_stab_norm)




# Treatment 1 api  overweight 


hazard_treated_ipcw_cens_stab_over <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                          inputdata = an_data, grp = 1, bmi_grp = 2, 
                                                          event_36_long = TRUE)

hazardcomp_treated_ipcw_cens_stab_over <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                              inputdata = an_data, grp = 1, bmi_grp = 2, 
                                                              event_36_long = FALSE)

cuminc_treated_ipcw_cens_stab_over <- nonParametricCumInc(hazard1 = hazard_treated_ipcw_cens_stab_over,
                                                          hazardcomp_treated_ipcw_cens_stab_over)

ipcw_stab_data_treated_over <- bind_cols("hazard_treated" = hazard_treated_ipcw_cens_stab_over,
                                         "hazardcompet_treated" = hazardcomp_treated_ipcw_cens_stab_over, 
                                         "cuminc_treated" = cuminc_treated_ipcw_cens_stab_over)



# Treatment 1 api  obese 


hazard_treated_ipcw_cens_stab_obese <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                           inputdata = an_data, grp = 1, bmi_grp = 3, 
                                                           event_36_long = TRUE)

hazardcomp_treated_ipcw_cens_stab_obese <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                               inputdata = an_data, grp = 1, bmi_grp = 3, 
                                                               event_36_long = FALSE)

cuminc_treated_ipcw_cens_stab_obese <- nonParametricCumInc(hazard1 = hazard_treated_ipcw_cens_stab_obese,
                                                           hazardcomp_treated_ipcw_cens_stab_obese)

ipcw_stab_data_treated_obese <- bind_cols("hazard_treated" = hazard_treated_ipcw_cens_stab_obese,
                                          "hazardcompet_treated" = hazardcomp_treated_ipcw_cens_stab_obese, 
                                          "cuminc_treated" = cuminc_treated_ipcw_cens_stab_obese)





# Treatment 0 warf ####
hazard_untreated_ipcw_cens_stab <- nonParametricCumHaz_all(an_data$ipw_sw_all.trunc, 
                                                           inputdata = an_data, grp = 0, 
                                                           event_36_long = TRUE)

hazardcomp_untreated_ipcw_cens_stab <- nonParametricCumHaz_all(an_data$ipw_sw_all.trunc, 
                                                               inputdata = an_data, grp = 0, 
                                                               event_36_long = FALSE)

cuminc_untreated_ipcw_cens_stab <- nonParametricCumInc(hazard1 = hazard_untreated_ipcw_cens_stab,
                                                       hazardcomp_untreated_ipcw_cens_stab)

ipcw_stab_data_untreated <- bind_cols("hazard_untreated" = hazard_untreated_ipcw_cens_stab,
                                      "hazardcompet_untreated" = hazardcomp_untreated_ipcw_cens_stab, 
                                      "cuminc_untreated" = cuminc_untreated_ipcw_cens_stab)


# Treatment 0 warf  normal weight 


hazard_untreated_ipcw_cens_stab_norm <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                            inputdata = an_data, grp = 0, bmi_grp = 1, 
                                                            event_36_long = TRUE)

hazardcomp_untreated_ipcw_cens_stab_norm <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                                inputdata = an_data, grp = 0, bmi_grp = 1, 
                                                                event_36_long = FALSE)

cuminc_untreated_ipcw_cens_stab_norm <- nonParametricCumInc(hazard1 = hazard_untreated_ipcw_cens_stab_norm,
                                                            hazardcomp_untreated_ipcw_cens_stab_norm)

ipcw_stab_data_untreated_norm <- bind_cols("hazard_untreated" = hazard_untreated_ipcw_cens_stab_norm,
                                           "hazardcompet_untreated" = hazardcomp_untreated_ipcw_cens_stab_norm, 
                                           "cuminc_untreated" = cuminc_untreated_ipcw_cens_stab_norm)




# Treatment 0 warf  overweight 


hazard_untreated_ipcw_cens_stab_over <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                            inputdata = an_data, grp = 0, bmi_grp = 2, 
                                                            event_36_long = TRUE)

hazardcomp_untreated_ipcw_cens_stab_over <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                                inputdata = an_data, grp = 0, bmi_grp = 2, 
                                                                event_36_long = FALSE)

cuminc_untreated_ipcw_cens_stab_over <- nonParametricCumInc(hazard1 = hazard_untreated_ipcw_cens_stab_over,
                                                            hazardcomp_untreated_ipcw_cens_stab_over)

ipcw_stab_data_untreated_over <- bind_cols("hazard_untreated" = hazard_untreated_ipcw_cens_stab_over,
                                           "hazardcompet_untreated" = hazardcomp_untreated_ipcw_cens_stab_over, 
                                           "cuminc_untreated" = cuminc_untreated_ipcw_cens_stab_over)



# Treatment 0 warf  obese 


hazard_untreated_ipcw_cens_stab_obese <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                             inputdata = an_data, grp = 0, bmi_grp = 3, 
                                                             event_36_long = TRUE)

hazardcomp_untreated_ipcw_cens_stab_obese <- nonParametricCumHaz(an_data$ipw_sw.trunc, 
                                                                 inputdata = an_data, grp = 0, bmi_grp = 3, 
                                                                 event_36_long = FALSE)

cuminc_untreated_ipcw_cens_stab_obese <- nonParametricCumInc(hazard1 = hazard_untreated_ipcw_cens_stab_obese,
                                                             hazardcomp_untreated_ipcw_cens_stab_obese)

ipcw_stab_data_untreated_obese <- bind_cols("hazard_untreated" = hazard_untreated_ipcw_cens_stab_obese,
                                            "hazardcompet_untreated" = hazardcomp_untreated_ipcw_cens_stab_obese, 
                                            "cuminc_untreated" = cuminc_untreated_ipcw_cens_stab_obese) 


ipcw_stab_data0 <- bind_cols(bmi_group = "All", ipcw_stab_data_treated,ipcw_stab_data_untreated)

ipcw_stab_data1 <- bind_cols(bmi_group = "Normal", ipcw_stab_data_treated_norm,ipcw_stab_data_untreated_norm)

ipcw_stab_data2 <- bind_cols(bmi_group = "Overweight", ipcw_stab_data_treated_over,ipcw_stab_data_untreated_over)

ipcw_stab_data3 <- bind_cols(bmi_group = "Obese", ipcw_stab_data_treated_obese,ipcw_stab_data_untreated_obese)

ipcw_stab_data4 <- bind_cols(bmi_group = "Over_warf vs Normal_warf", ipcw_stab_data_untreated_over,ipcw_stab_data_untreated_norm)

ipcw_stab_data5 <- bind_cols(bmi_group = "Obese_warf vs Normal_warf", ipcw_stab_data_untreated_obese,ipcw_stab_data_untreated_norm)

ipcw_stab_data6 <- bind_cols(bmi_group = "Normal_api vs Normal_warf", ipcw_stab_data_treated_norm,ipcw_stab_data_untreated_norm)


ipcw_stab_data7 <- bind_cols(bmi_group = "Over_api vs Normal_warf", ipcw_stab_data_treated_over,ipcw_stab_data_untreated_norm)

ipcw_stab_data8 <- bind_cols(bmi_group = "Obese_api vs Normal_warf", ipcw_stab_data_treated_obese ,ipcw_stab_data_untreated_norm)



ipcw_stab_data <- bind_rows(ipcw_stab_data0, ipcw_stab_data1,ipcw_stab_data2,ipcw_stab_data3,ipcw_stab_data4,ipcw_stab_data5,ipcw_stab_data6,ipcw_stab_data7,ipcw_stab_data8)


ipw_bs <- ipcw_stab_data %>%
  filter(row_number() == 36 | row_number() == 72 | row_number() == 108 | row_number() == 144 | row_number() == 180 | row_number() == 216 | row_number() == 252 | row_number() == 288 | row_number() == 324) %>%
  mutate(riskdiff = cuminc_treated-cuminc_untreated) %>%
  mutate(riskdiff_eff = cuminc_untreated...4-cuminc_untreated...7) %>%
  mutate(logriskratio = log(cuminc_treated/cuminc_untreated)) %>%
  mutate(logriskratio_eff = log(cuminc_untreated...4/cuminc_untreated...7)) %>%
  select(bmi_group,cuminc_untreated,cuminc_treated,riskdiff, logriskratio,cuminc_untreated...7,cuminc_untreated...4,riskdiff_eff,logriskratio_eff)


}

stopCluster(cluster)

time_end <- Sys.time()

total_time <- time_end-tstart 

total_time



# Point estimates and CIs using bootstrapped standard errors #### 
estimates

results_36 <- data.frame()



# Point estimates and CIs for overall analysis
res_sd_all <- estimates %>%  filter(bmi_group == "All") %>% apply(2,sd)


lower_ci_Y_0_all <- Y_0_all - 1.96*res_sd_all[2]
upper_ci_Y_0_all <- Y_0_all + 1.96*res_sd_all[2]

lower_ci_Y_1_all <- Y_1_all - 1.96*res_sd_all[3]
upper_ci_Y_1_all <- Y_1_all + 1.96*res_sd_all[3]

lcldiff_all <- riskdiff_all - 1.96*res_sd_all[4]
ucldiff_all <- riskdiff_all + 1.96*res_sd_all[4]

lclratio_all <- exp(log(riskratio_all) - 1.96*res_sd_all[5])
uclratio_all <- exp(log(riskratio_all) + 1.96*res_sd_all[5])

result1_all <- cbind(paste0(outcome1), "All", "Y_1=1", paste0(format(round(Y_1_all*100,1), nsmall=1) ,
                                                              " (" , format(round(lower_ci_Y_1_all*100,1), nsmall=1), "," ,
                                                              format(round(upper_ci_Y_1_all*100,1), nsmall=1), ")" ))



result2_all <- cbind(paste0(outcome1), "All", "Y_0=1", paste0(format(round(Y_0_all*100,1), nsmall=1) ,
                                                              " (" , format(round(lower_ci_Y_0_all*100,1), nsmall=1), "," ,
                                                              format(round(upper_ci_Y_0_all*100,1), nsmall=1), ")" ))  

result3_all <- cbind(paste0(outcome1), "All", "RD", paste0(format(round(riskdiff_all*100,1), nsmall=1), 
                                                           " (" , format(round(lcldiff_all*100,1), nsmall=1), "," ,
                                                           format(round(ucldiff_all*100,1), nsmall=1), ")" ))


result4_all <- cbind(paste0(outcome1), "All", "RR", paste0(format(round(riskratio_all,2), nsmall=2), 
                                                           " (" , format(round(lclratio_all,2), nsmall=2), "," ,
                                                           format(round(uclratio_all,2), nsmall=2), ")" ))


results_all <- rbind(result1_all, result2_all, result3_all, result4_all)
results_36 <- rbind(results_36, results_all)

# Point estimates and CIs Normal weight 
res_sd_normal <- estimates %>%  filter(bmi_group == "Normal") %>% apply(2,sd)


lower_ci_Y_0_normal <- Y_0_norm - 1.96*res_sd_normal[2]
upper_ci_Y_0_normal <- Y_0_norm + 1.96*res_sd_normal[2]

lower_ci_Y_1_normal <- Y_1_norm - 1.96*res_sd_normal[3]
upper_ci_Y_1_normal <- Y_1_norm + 1.96*res_sd_normal[3]

lcldiff_normal <- riskdiff_norm - 1.96*res_sd_normal[4]
ucldiff_normal <- riskdiff_norm + 1.96*res_sd_normal[4]

lclratio_normal <- exp(log(riskratio_norm) - 1.96*res_sd_normal[5])
uclratio_normal <- exp(log(riskratio_norm) + 1.96*res_sd_normal[5])

result1_normal <- cbind(paste0(outcome1), "Normal", "Y_1=1", paste0(format(round(Y_1_norm*100,1), nsmall=1) ,
                                                                           " (" , format(round(lower_ci_Y_1_normal*100,1), nsmall=1), "," ,
                                                                           format(round(upper_ci_Y_1_normal*100,1), nsmall=1), ")" ))



result2_normal <- cbind(paste0(outcome1), "Normal", "Y_0=1", paste0(format(round(Y_0_norm*100,1), nsmall=1) ,
                                                                           " (" , format(round(lower_ci_Y_0_normal*100,1), nsmall=1), "," ,
                                                                           format(round(upper_ci_Y_0_normal*100,1), nsmall=1), ")" ))  

result3_normal <- cbind(paste0(outcome1), "Normal", "RD", paste0(format(round(riskdiff_norm*100,1), nsmall=1), 
                                                                        " (" , format(round(lcldiff_normal*100,1), nsmall=1), "," ,
                                                                        format(round(ucldiff_normal*100,1), nsmall=1), ")" ))


result4_normal <- cbind(paste0(outcome1), "Normal", "RR", paste0(format(round(riskratio_norm,2), nsmall=2), 
                                                                        " (" , format(round(lclratio_normal,2), nsmall=2), "," ,
                                                                        format(round(uclratio_normal,2), nsmall=2), ")" ))


results_normal <- rbind(result1_normal, result2_normal, result3_normal, result4_normal)
results_36 <- rbind(results_36, results_normal)


# Point estimates and CIs Overweight 
res_sd_over <- estimates %>%  filter(bmi_group == "Overweight") %>% apply(2,sd)

lower_ci_Y_0_overweight<- Y_0_over - 1.96*res_sd_over[2]
upper_ci_Y_0_overweight<- Y_0_over + 1.96*res_sd_over[2]

lower_ci_Y_1_overweight<- Y_1_over - 1.96*res_sd_over[3]
upper_ci_Y_1_overweight<- Y_1_over + 1.96*res_sd_over[3]

lcldiff_overweight<- riskdiff_over - 1.96*res_sd_over[4]
ucldiff_overweight<- riskdiff_over + 1.96*res_sd_over[4]

lclratio_overweight<- exp(log(riskratio_over) - 1.96*res_sd_over[5])
uclratio_overweight<- exp(log(riskratio_over) + 1.96*res_sd_over[5])

result1_over <- cbind(paste0(outcome1), "Overweight", "Y_1=1", paste0(format(round(Y_1_over*100,1), nsmall=1) ,
                                                                      " (" , format(round(lower_ci_Y_1_overweight*100,1), nsmall=1), "," ,
                                                                      format(round(upper_ci_Y_1_overweight*100,1), nsmall=1), ")" ))



result2_over <- cbind(paste0(outcome1), "Overweight", "Y_0=1", paste0(format(round(Y_0_over*100,1), nsmall=1) ,
                                                                      " (" , format(round(lower_ci_Y_0_overweight*100,1), nsmall=1), "," ,
                                                                      format(round(upper_ci_Y_0_overweight*100,1), nsmall=1), ")" ))  

result3_over <- cbind(paste0(outcome1), "Overweight", "RD", paste0(format(round(riskdiff_over*100,1), nsmall=1), 
                                                                   " (" , format(round(lcldiff_overweight*100,1), nsmall=1), "," ,
                                                                   format(round(ucldiff_overweight*100,1), nsmall=1), ")" ))


result4_over <- cbind(paste0(outcome1), "Overweight", "RR", paste0(format(round(riskratio_over,2), nsmall=2), 
                                                                   " (" , format(round(lclratio_overweight,2), nsmall=2), "," ,
                                                                   format(round(uclratio_overweight,2), nsmall=2), ")" ))


results_over <- rbind(result1_over, result2_over, result3_over, result4_over)
results_36 <- rbind(results_36, results_over)

# Point estimates and CIs Obese
res_sd_obese <- estimates %>%  filter(bmi_group == "Obese") %>% apply(2,sd)

lower_ci_Y_0_obese<- Y_0_obes - 1.96*res_sd_obese[2]
upper_ci_Y_0_obese<- Y_0_obes + 1.96*res_sd_obese[2]

lower_ci_Y_1_obese<- Y_1_obes - 1.96*res_sd_obese[3]
upper_ci_Y_1_obese<- Y_1_obes + 1.96*res_sd_obese[3]

lcldiff_obese<- riskdiff_obes - 1.96*res_sd_obese[4]
ucldiff_obese<- riskdiff_obes + 1.96*res_sd_obese[4]

lclratio_obese<- exp(log(riskratio_obes) - 1.96*res_sd_obese[5])
uclratio_obese<- exp(log(riskratio_obes) + 1.96*res_sd_obese[5])

result1_obese <- cbind(paste0(outcome1), "Obese", "Y_1=1", paste0(format(round(Y_1_obes*100,1), nsmall=1) ,
                                                                  " (" , format(round(lower_ci_Y_1_obese*100,1), nsmall=1), "," ,
                                                                  format(round(upper_ci_Y_1_obese*100,1), nsmall=1), ")" ))



result2_obese <- cbind(paste0(outcome1), "Obese", "Y_0=1", paste0(format(round(Y_0_obes*100,1), nsmall=1) ,
                                                                  " (" , format(round(lower_ci_Y_0_obese*100,1), nsmall=1), "," ,
                                                                  format(round(upper_ci_Y_0_obese*100,1), nsmall=1), ")" ))  

result3_obese <- cbind(paste0(outcome1), "Obese", "RD", paste0(format(round(riskdiff_obes*100,1), nsmall=1), 
                                                               " (" , format(round(lcldiff_obese*100,1), nsmall=1), "," ,
                                                               format(round(ucldiff_obese*100,1), nsmall=1), ")" ))


result4_obese <- cbind(paste0(outcome1), "Obese", "RR", paste0(format(round(riskratio_obes,2), nsmall=2), 
                                                               " (" , format(round(lclratio_obese,2), nsmall=2), "," ,
                                                               format(round(uclratio_obese,2), nsmall=2), ")" ))


results_obese <- rbind(result1_obese, result2_obese, result3_obese, result4_obese)
results_36 <- rbind(results_36, results_obese)


# Effect modification measures

# Point estimates and CIs Overweight warfarin vs normal weight warfarin 
riskdiff_eff1 <- Y_0_over-Y_0_norm
riskratio_eff1 <- Y_0_over/Y_0_norm

res_sd_eff1 <- estimates %>%  filter(bmi_group == "Over_warf vs Normal_warf") %>% apply(2,sd)


lower_ci_Y_0_eff1 <- Y_0_norm - 1.96*res_sd_eff1[6]
upper_ci_Y_0_eff1 <- Y_0_norm + 1.96*res_sd_eff1[6]

lower_ci_Y_1_eff1 <- Y_0_over - 1.96*res_sd_eff1[7]
upper_ci_Y_1_eff1 <- Y_0_over + 1.96*res_sd_eff1[7]

lcldiff_eff1 <- riskdiff_eff1 - 1.96*res_sd_eff1[8]
ucldiff_eff1 <- riskdiff_eff1 + 1.96*res_sd_eff1[8]

lclratio_eff1 <- exp(log(riskratio_eff1) - 1.96*res_sd_eff1[9])
uclratio_eff1 <- exp(log(riskratio_eff1) + 1.96*res_sd_eff1[9])

result1_eff1 <- cbind(paste0(outcome1), "eff1", "Y_1=1", paste0(format(round(Y_0_over*100,1), nsmall=1) ,
                                                                    " (" , format(round(lower_ci_Y_1_eff1*100,1), nsmall=1), "," ,
                                                                    format(round(upper_ci_Y_1_eff1*100,1), nsmall=1), ")" ))



result2_eff1 <- cbind(paste0(outcome1), "eff1", "Y_0=1", paste0(format(round(Y_0_norm*100,1), nsmall=1) ,
                                                                    " (" , format(round(lower_ci_Y_0_eff1*100,1), nsmall=1), "," ,
                                                                    format(round(upper_ci_Y_0_eff1*100,1), nsmall=1), ")" ))  

result3_eff1 <- cbind(paste0(outcome1), "eff1", "RD", paste0(format(round(riskdiff_eff1*100,1), nsmall=1), 
                                                                 " (" , format(round(lcldiff_eff1*100,1), nsmall=1), "," ,
                                                                 format(round(ucldiff_eff1*100,1), nsmall=1), ")" ))


result4_eff1 <- cbind(paste0(outcome1), "eff1", "RR", paste0(format(round(riskratio_eff1,2), nsmall=2), 
                                                                 " (" , format(round(lclratio_eff1,2), nsmall=2), "," ,
                                                                 format(round(uclratio_eff1,2), nsmall=2), ")" ))


results_eff1 <- rbind(result1_eff1, result2_eff1, result3_eff1, result4_eff1)
results_36 <- rbind(results_36, results_eff1)




# Point estimates and CIs obese warfarin vs normal weight warfarin 
riskdiff_eff2 <- Y_0_obes-Y_0_norm
riskratio_eff2 <- Y_0_obes/Y_0_norm


res_sd_eff2 <- estimates %>%  filter(bmi_group == "Obese_warf vs Normal_warf") %>% apply(2,sd)


lower_ci_Y_0_eff2 <- Y_0_norm - 1.96*res_sd_eff2[6]
upper_ci_Y_0_eff2 <- Y_0_norm + 1.96*res_sd_eff2[6]

lower_ci_Y_1_eff2 <- Y_0_obes - 1.96*res_sd_eff2[7]
upper_ci_Y_1_eff2 <- Y_0_obes + 1.96*res_sd_eff2[7]

lcldiff_eff2 <- riskdiff_eff2 - 1.96*res_sd_eff2[8]
ucldiff_eff2 <- riskdiff_eff2 + 1.96*res_sd_eff2[8]

lclratio_eff2 <- exp(log(riskratio_eff2) - 1.96*res_sd_eff2[9])
uclratio_eff2 <- exp(log(riskratio_eff2) + 1.96*res_sd_eff2[9])

result1_eff2 <- cbind(paste0(outcome1), "eff2", "Y_1=1", paste0(format(round(Y_0_obes*100,1), nsmall=1) ,
                                                                " (" , format(round(lower_ci_Y_1_eff2*100,1), nsmall=1), "," ,
                                                                format(round(upper_ci_Y_1_eff2*100,1), nsmall=1), ")" ))



result2_eff2 <- cbind(paste0(outcome1), "eff2", "Y_0=1", paste0(format(round(Y_0_norm*100,1), nsmall=1) ,
                                                                " (" , format(round(lower_ci_Y_0_eff2*100,1), nsmall=1), "," ,
                                                                format(round(upper_ci_Y_0_eff2*100,1), nsmall=1), ")" ))  

result3_eff2 <- cbind(paste0(outcome1), "eff2", "RD", paste0(format(round(riskdiff_eff2*100,1), nsmall=1), 
                                                             " (" , format(round(lcldiff_eff2*100,1), nsmall=1), "," ,
                                                             format(round(ucldiff_eff2*100,1), nsmall=1), ")" ))


result4_eff2 <- cbind(paste0(outcome1), "eff2", "RR", paste0(format(round(riskratio_eff2,2), nsmall=2), 
                                                             " (" , format(round(lclratio_eff2,2), nsmall=2), "," ,
                                                             format(round(uclratio_eff2,2), nsmall=2), ")" ))


results_eff2 <- rbind(result1_eff2, result2_eff2, result3_eff2, result4_eff2)
results_36 <- rbind(results_36, results_eff2)




# Point estimates and CIs normal weight api vs normal weight warfarin 
riskdiff_eff3 <- Y_1_norm-Y_0_norm
riskratio_eff3 <- Y_1_norm/Y_0_norm


res_sd_eff3 <- estimates %>%  filter(bmi_group == "Normal_api vs Normal_warf") %>% apply(2,sd)


lower_ci_Y_0_eff3 <- Y_0_norm - 1.96*res_sd_eff3[2]
upper_ci_Y_0_eff3 <- Y_0_norm + 1.96*res_sd_eff3[2]

lower_ci_Y_1_eff3 <- Y_1_norm - 1.96*res_sd_eff3[3]
upper_ci_Y_1_eff3 <- Y_1_norm + 1.96*res_sd_eff3[3]

lcldiff_eff3 <- riskdiff_eff3 - 1.96*res_sd_eff3[4]
ucldiff_eff3 <- riskdiff_eff3 + 1.96*res_sd_eff3[4]

lclratio_eff3 <- exp(log(riskratio_eff3) - 1.96*res_sd_eff3[5])
uclratio_eff3 <- exp(log(riskratio_eff3) + 1.96*res_sd_eff3[5])

result1_eff3 <- cbind(paste0(outcome1), "eff3", "Y_1=1", paste0(format(round(Y_1_norm*100,1), nsmall=1) ,
                                                                " (" , format(round(lower_ci_Y_1_eff3*100,1), nsmall=1), "," ,
                                                                format(round(upper_ci_Y_1_eff3*100,1), nsmall=1), ")" ))



result2_eff3 <- cbind(paste0(outcome1), "eff3", "Y_0=1", paste0(format(round(Y_0_norm*100,1), nsmall=1) ,
                                                                " (" , format(round(lower_ci_Y_0_eff3*100,1), nsmall=1), "," ,
                                                                format(round(upper_ci_Y_0_eff3*100,1), nsmall=1), ")" ))  

result3_eff3 <- cbind(paste0(outcome1), "eff3", "RD", paste0(format(round(riskdiff_eff3*100,1), nsmall=1), 
                                                             " (" , format(round(lcldiff_eff3*100,1), nsmall=1), "," ,
                                                             format(round(ucldiff_eff3*100,1), nsmall=1), ")" ))


result4_eff3 <- cbind(paste0(outcome1), "eff3", "RR", paste0(format(round(riskratio_eff3,2), nsmall=2), 
                                                             " (" , format(round(lclratio_eff3,2), nsmall=2), "," ,
                                                             format(round(uclratio_eff3,2), nsmall=2), ")" ))


results_eff3 <- rbind(result1_eff3, result2_eff3, result3_eff3, result4_eff3)
results_36 <- rbind(results_36, results_eff3)




# Point estimates and CIs overweight api vs normal weight warfarin 
riskdiff_eff4 <- Y_1_over-Y_0_norm
riskratio_eff4 <- Y_1_over/Y_0_norm


res_sd_eff4 <- estimates %>%  filter(bmi_group == "Over_api vs Normal_warf") %>% apply(2,sd)


lower_ci_Y_0_eff4 <- Y_0_norm - 1.96*res_sd_eff4[2]
upper_ci_Y_0_eff4 <- Y_0_norm + 1.96*res_sd_eff4[2]

lower_ci_Y_1_eff4 <- Y_1_over - 1.96*res_sd_eff4[3]
upper_ci_Y_1_eff4 <- Y_1_over + 1.96*res_sd_eff4[3]

lcldiff_eff4 <- riskdiff_eff4 - 1.96*res_sd_eff4[4]
ucldiff_eff4 <- riskdiff_eff4 + 1.96*res_sd_eff4[4]

lclratio_eff4 <- exp(log(riskratio_eff4) - 1.96*res_sd_eff4[5])
uclratio_eff4 <- exp(log(riskratio_eff4) + 1.96*res_sd_eff4[5])

result1_eff4 <- cbind(paste0(outcome1), "eff4", "Y_1=1", paste0(format(round(Y_1_over*100,1), nsmall=1) ,
                                                                " (" , format(round(lower_ci_Y_1_eff4*100,1), nsmall=1), "," ,
                                                                format(round(upper_ci_Y_1_eff4*100,1), nsmall=1), ")" ))



result2_eff4 <- cbind(paste0(outcome1), "eff4", "Y_0=1", paste0(format(round(Y_0_norm*100,1), nsmall=1) ,
                                                                " (" , format(round(lower_ci_Y_0_eff4*100,1), nsmall=1), "," ,
                                                                format(round(upper_ci_Y_0_eff4*100,1), nsmall=1), ")" ))  

result3_eff4 <- cbind(paste0(outcome1), "eff4", "RD", paste0(format(round(riskdiff_eff4*100,1), nsmall=1), 
                                                             " (" , format(round(lcldiff_eff4*100,1), nsmall=1), "," ,
                                                             format(round(ucldiff_eff4*100,1), nsmall=1), ")" ))


result4_eff4 <- cbind(paste0(outcome1), "eff4", "RR", paste0(format(round(riskratio_eff4,2), nsmall=2), 
                                                             " (" , format(round(lclratio_eff4,2), nsmall=2), "," ,
                                                             format(round(uclratio_eff4,2), nsmall=2), ")" ))


results_eff4 <- rbind(result1_eff4, result2_eff4, result3_eff4, result4_eff4)
results_36 <- rbind(results_36, results_eff4)



# Point estimates and CIs obese api vs normal weight warfarin 
riskdiff_eff5 <- Y_1_obes-Y_0_norm
riskratio_eff5 <- Y_1_obes/Y_0_norm


res_sd_eff5 <- estimates %>%  filter(bmi_group == "Obese_api vs Normal_warf") %>% apply(2,sd)


lower_ci_Y_0_eff5 <- Y_0_norm - 1.96*res_sd_eff5[2]
upper_ci_Y_0_eff5 <- Y_0_norm + 1.96*res_sd_eff5[2]

lower_ci_Y_1_eff5 <- Y_1_obes - 1.96*res_sd_eff5[3]
upper_ci_Y_1_eff5 <- Y_1_obes + 1.96*res_sd_eff5[3]

lcldiff_eff5 <- riskdiff_eff5 - 1.96*res_sd_eff5[4]
ucldiff_eff5 <- riskdiff_eff5 + 1.96*res_sd_eff5[4]

lclratio_eff5 <- exp(log(riskratio_eff5) - 1.96*res_sd_eff5[5])
uclratio_eff5 <- exp(log(riskratio_eff5) + 1.96*res_sd_eff5[5])

result1_eff5 <- cbind(paste0(outcome1), "eff5", "Y_1=1", paste0(format(round(Y_1_obes*100,1), nsmall=1) ,
                                                                " (" , format(round(lower_ci_Y_1_eff5*100,1), nsmall=1), "," ,
                                                                format(round(upper_ci_Y_1_eff5*100,1), nsmall=1), ")" ))



result2_eff5 <- cbind(paste0(outcome1), "eff5", "Y_0=1", paste0(format(round(Y_0_norm*100,1), nsmall=1) ,
                                                                " (" , format(round(lower_ci_Y_0_eff5*100,1), nsmall=1), "," ,
                                                                format(round(upper_ci_Y_0_eff5*100,1), nsmall=1), ")" ))  

result3_eff5 <- cbind(paste0(outcome1), "eff5", "RD", paste0(format(round(riskdiff_eff5*100,1), nsmall=1), 
                                                             " (" , format(round(lcldiff_eff5*100,1), nsmall=1), "," ,
                                                             format(round(ucldiff_eff5*100,1), nsmall=1), ")" ))


result4_eff5 <- cbind(paste0(outcome1), "eff5", "RR", paste0(format(round(riskratio_eff5,2), nsmall=2), 
                                                             " (" , format(round(lclratio_eff5,2), nsmall=2), "," ,
                                                             format(round(uclratio_eff5,2), nsmall=2), ")" ))


results_eff5 <- rbind(result1_eff5, result2_eff5, result3_eff5, result4_eff5)
results_36 <- rbind(results_36, results_eff5)







# Save the dataframe
results_36 <- results_36 %>%
  rename(outcome=V1, modifier=V2, measure=V3, effect=V4) %>%
  spread(measure, effect) %>%
  select(outcome,modifier, "Y_1=1", "Y_0=1", "RD", "RR") %>%
  rename("Outcome" = "outcome",
         "Apixaban"="Y_1=1", 
         "Warfarin"="Y_0=1", 
         "36-month risk difference (95% CI)"="RD", 
         "36-month risk ratio (95% CI)"="RR")

#write the results to txt file
write.table(results_36, paste0("analysis/output/tables/results_36_ipw_trunc_", outcome), sep="\t", quote=FALSE, row.names=FALSE)


}
