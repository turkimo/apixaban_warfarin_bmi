# Function to graph cumulative incidence for stroke/SE outcomes 


func_cuminc_eff <- function(data,outcome,outcome1, cut_times) {
  
ipt_wt_eff(dat_ipweights_eff)

data1 = merge(data,ipt_weights) %>% merge(dat_ipweights_mort)

data1$bmi_groups_n = as.integer(data1$bmi_groups)


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

ipc_wt_eff(data_event_long)


# Compute survival probability per arm per month 

# Treatment 1 api ####

# Treatment 1 api normal weight 


hazard_treated_ipcw_cens_stab_norm <- nonParametricCumHaz(an_data$ipw_sw, 
                                                          inputdata = an_data, grp = 1, bmi_grp = 1, 
                                                          event_36_long = TRUE)

hazardcomp_treated_ipcw_cens_stab_norm <- nonParametricCumHaz(an_data$ipw_sw, 
                                                              inputdata = an_data, grp = 1, bmi_grp = 1, 
                                                              event_36_long = FALSE)

cuminc_treated_ipcw_cens_stab_norm <- nonParametricCumInc(hazard1 = hazard_treated_ipcw_cens_stab_norm,
                                                          hazardcomp_treated_ipcw_cens_stab_norm)

ipcw_stab_data_treated_norm <- bind_cols("hazard_treated" = hazard_treated_ipcw_cens_stab_norm,
                                         "hazardcompet_treated" = hazardcomp_treated_ipcw_cens_stab_norm, 
                                         "cuminc_treated" = cuminc_treated_ipcw_cens_stab_norm)




# Treatment 1 api overweight 


hazard_treated_ipcw_cens_stab_over <- nonParametricCumHaz(an_data$ipw_sw, 
                                                          inputdata = an_data, grp = 1, bmi_grp = 2, 
                                                          event_36_long = TRUE)

hazardcomp_treated_ipcw_cens_stab_over <- nonParametricCumHaz(an_data$ipw_sw, 
                                                              inputdata = an_data, grp = 1, bmi_grp = 2, 
                                                              event_36_long = FALSE)

cuminc_treated_ipcw_cens_stab_over <- nonParametricCumInc(hazard1 = hazard_treated_ipcw_cens_stab_over,
                                                          hazardcomp_treated_ipcw_cens_stab_over)

ipcw_stab_data_treated_over <- bind_cols("hazard_treated" = hazard_treated_ipcw_cens_stab_over,
                                         "hazardcompet_treated" = hazardcomp_treated_ipcw_cens_stab_over, 
                                         "cuminc_treated" = cuminc_treated_ipcw_cens_stab_over)



# Treatment 1 api  obese 


hazard_treated_ipcw_cens_stab_obese <- nonParametricCumHaz(an_data$ipw_sw, 
                                                           inputdata = an_data, grp = 1, bmi_grp = 3, 
                                                           event_36_long = TRUE)

hazardcomp_treated_ipcw_cens_stab_obese <- nonParametricCumHaz(an_data$ipw_sw, 
                                                               inputdata = an_data, grp = 1, bmi_grp = 3, 
                                                               event_36_long = FALSE)

cuminc_treated_ipcw_cens_stab_obese <- nonParametricCumInc(hazard1 = hazard_treated_ipcw_cens_stab_obese,
                                                           hazardcomp_treated_ipcw_cens_stab_obese)

ipcw_stab_data_treated_obese <- bind_cols("hazard_treated" = hazard_treated_ipcw_cens_stab_obese,
                                          "hazardcompet_treated" = hazardcomp_treated_ipcw_cens_stab_obese, 
                                          "cuminc_treated" = cuminc_treated_ipcw_cens_stab_obese)





# Treatment 0 warf ####


# Treatment 0 warf  normal weight 


hazard_untreated_ipcw_cens_stab_norm <- nonParametricCumHaz(an_data$ipw_sw, 
                                                            inputdata = an_data, grp = 0, bmi_grp = 1, 
                                                            event_36_long = TRUE)

hazardcomp_untreated_ipcw_cens_stab_norm <- nonParametricCumHaz(an_data$ipw_sw, 
                                                                inputdata = an_data, grp = 0, bmi_grp = 1, 
                                                                event_36_long = FALSE)

cuminc_untreated_ipcw_cens_stab_norm <- nonParametricCumInc(hazard1 = hazard_untreated_ipcw_cens_stab_norm,
                                                            hazardcomp_untreated_ipcw_cens_stab_norm)

ipcw_stab_data_untreated_norm <- bind_cols("hazard_untreated" = hazard_untreated_ipcw_cens_stab_norm,
                                           "hazardcompet_untreated" = hazardcomp_untreated_ipcw_cens_stab_norm, 
                                           "cuminc_untreated" = cuminc_untreated_ipcw_cens_stab_norm)




# Treatment 0 warf  overweight 


hazard_untreated_ipcw_cens_stab_over <- nonParametricCumHaz(an_data$ipw_sw, 
                                                            inputdata = an_data, grp = 0, bmi_grp = 2, 
                                                            event_36_long = TRUE)

hazardcomp_untreated_ipcw_cens_stab_over <- nonParametricCumHaz(an_data$ipw_sw, 
                                                                inputdata = an_data, grp = 0, bmi_grp = 2, 
                                                                event_36_long = FALSE)

cuminc_untreated_ipcw_cens_stab_over <- nonParametricCumInc(hazard1 = hazard_untreated_ipcw_cens_stab_over,
                                                            hazardcomp_untreated_ipcw_cens_stab_over)

ipcw_stab_data_untreated_over <- bind_cols("hazard_untreated" = hazard_untreated_ipcw_cens_stab_over,
                                           "hazardcompet_untreated" = hazardcomp_untreated_ipcw_cens_stab_over, 
                                           "cuminc_untreated" = cuminc_untreated_ipcw_cens_stab_over)



# Treatment 0 warf  obese 


hazard_untreated_ipcw_cens_stab_obese <- nonParametricCumHaz(an_data$ipw_sw, 
                                                             inputdata = an_data, grp = 0, bmi_grp = 3, 
                                                             event_36_long = TRUE)

hazardcomp_untreated_ipcw_cens_stab_obese <- nonParametricCumHaz(an_data$ipw_sw, 
                                                                 inputdata = an_data, grp = 0, bmi_grp = 3, 
                                                                 event_36_long = FALSE)

cuminc_untreated_ipcw_cens_stab_obese <- nonParametricCumInc(hazard1 = hazard_untreated_ipcw_cens_stab_obese,
                                                             hazardcomp_untreated_ipcw_cens_stab_obese)

ipcw_stab_data_untreated_obese <- bind_cols("hazard_untreated" = hazard_untreated_ipcw_cens_stab_obese,
                                            "hazardcompet_untreated" = hazardcomp_untreated_ipcw_cens_stab_obese, 
                                            "cuminc_untreated" = cuminc_untreated_ipcw_cens_stab_obese) 


ipcw_stab_data1 <- bind_cols(bmi_group = "Normal", ipcw_stab_data_treated_norm,ipcw_stab_data_untreated_norm)

ipcw_stab_data2 <- bind_cols(bmi_group = "Overweight", ipcw_stab_data_treated_over,ipcw_stab_data_untreated_over)

ipcw_stab_data3 <- bind_cols(bmi_group = "Obese", ipcw_stab_data_treated_obese,ipcw_stab_data_untreated_obese)

ipcw_stab_data <- bind_rows(ipcw_stab_data1,ipcw_stab_data2,ipcw_stab_data3)




# Cumulative incidence for Normal weight
cuminc_0_norm <- ipcw_stab_data %>% filter(bmi_group == "Normal" ) %$% cuminc_untreated[36]
cuminc_1_norm <- ipcw_stab_data %>% filter(bmi_group == "Normal" ) %$% cuminc_treated[36]
Y_0_norm <- cuminc_0_norm
Y_1_norm <- cuminc_1_norm
riskdiff_norm <- Y_1_norm - Y_0_norm
riskratio_norm <- Y_1_norm/Y_0_norm



#Cumulative incidence for Overweight
cuminc_0_over <- ipcw_stab_data %>% filter(bmi_group == "Overweight" ) %$% cuminc_untreated[36]
cuminc_1_over <- ipcw_stab_data %>% filter(bmi_group == "Overweight" ) %$% cuminc_treated[36]
Y_0_over <- cuminc_0_over
Y_1_over <- cuminc_1_over
riskdiff_over <- Y_1_over - Y_0_over
riskratio_over <- Y_1_over/Y_0_over



#Cumulative incidence for Obese
cuminc_0_obes <- ipcw_stab_data %>% filter(bmi_group == "Obese" ) %$% cuminc_untreated[36]
cuminc_1_obes <- ipcw_stab_data %>% filter(bmi_group == "Obese" ) %$% cuminc_treated[36]
Y_0_obes <- cuminc_0_obes
Y_1_obes <- cuminc_1_obes
riskdiff_obes <- Y_1_obes - Y_0_obes
riskratio_obes <- Y_1_obes/Y_0_obes


ipcw_stab_data$bmi_group <- factor(ipcw_stab_data$bmi_group, levels = c("Normal","Overweight", "Obese" ))

# Plots
cuminc_plot <- ipcw_stab_data %>% 
  select(bmi_group, cuminc_untreated, cuminc_treated) %>% 
  group_by(bmi_group) %>% 
  mutate(
    time = row_number() - 1
  ) %>% 
  ungroup() %>% 
  pivot_longer(
    cols = c(cuminc_untreated, cuminc_treated),
    names_to = c("cohort"),
    values_to = "cuminc_value"
  ) %>% 
  ggplot(aes(x = time, y = cuminc_value*100, color = cohort, fill = cohort)) +
  geom_step(stat = "identity") +
  scale_x_continuous(limits = c(0,36),breaks = seq(0,36,1)) +
  scale_y_continuous(limits = c(0,5),breaks = seq(0,5,1)) +
  theme_clean(base_size = 13 , base_family = "serif" ) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), strip.text.x.top = element_text(colour = "black", face = "bold")) +
  scale_color_brewer(palette = "Set1", breaks=c("cuminc_untreated", "cuminc_treated"),
                     labels = c("Warfarin", "Apixaban"), guide = guide_legend(reverse = F)) +
  xlab("Time since first prescription, months") + 
  ylab(paste0("Cumulative incidence of ", outcome1, " outcome, per 100 people")) +
  labs(color = "Treatment arm") +
  facet_grid(rows = vars(bmi_group))+
  theme(
    strip.text.y = element_text(size = 13, color = "black", face = "bold"), 
    strip.background = element_rect( fill="gray78", linetype="solid"))

ggsave(paste0("analysis/output/figures/parametricsurvival_", outcome, "_36.png"), cuminc_plot, width = 3437, height = 1971, units= "px")
cuminc_plot 


}
