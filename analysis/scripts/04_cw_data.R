# Outcome data for effects on stroke/SE (composite)
dat_composite <- exp_out %>%
  rename(date_event = epidt_composite) %>%
  rename(date_compet = dod) %>%
  rename(date_stend = senddt) %>%
  rename(date_lost = regenddt) %>%
  rename(date_admcen = admcen) %>%
  rename(time_end = time_composite_36) %>% 
  mutate(event_36 = ifelse(eventType == 1, 1,0)) %>%
  mutate(compet_36 = ifelse(eventType == 2, 1,0)) %>%
  mutate(cens_36 = ifelse(eventType == 0 & time_end < 36, 1,0)) %>%
  mutate(event_compet_36 = ifelse(eventType == 1 | eventType ==2, 1,0)) %>%
  select(patid, indexdt, drugsub, bmi_groups, date_event,date_compet, date_stend, date_lost,date_admcen, event_36,cens_36, compet_36, event_compet_36, time_end,Tstart,eventType, date_composite)


# Outcome data for total effect on stroke/SE (composite) in patients with BMI < 3 years

dat_composite_bmi_sen <- exp_out_bmi_sse %>%
  rename(date_event = epidt_composite) %>%
  rename(date_compet = dod) %>%
  rename(date_stend = senddt) %>%
  rename(date_lost = regenddt) %>%
  rename(date_admcen = admcen) %>%
  rename(time_end = time_composite_36) %>% 
  mutate(event_36 = ifelse(eventType == 1, 1,0)) %>%
  mutate(compet_36 = ifelse(eventType == 2, 1,0)) %>%
  mutate(cens_36 = ifelse(eventType == 0 & time_end < 36, 1,0)) %>%
  mutate(event_compet_36 = ifelse(eventType == 1 | eventType ==2, 1,0)) %>%
  select(patid, indexdt, drugsub, bmi_groups, bmi_groups_derived, date_event,date_compet, date_stend, date_lost,date_admcen, event_36,cens_36, compet_36, event_compet_36, time_end,Tstart,eventType, date_composite)

# Outcome data for total effect on stroke (any type)

dat_stroke <- exp_out_stroke %>%
  rename(date_event = epidt_stroke) %>%
  rename(date_compet = dod) %>%
  rename(date_stend = senddt) %>%
  rename(date_lost = regenddt) %>%
  rename(date_admcen = admcen) %>%
  rename(time_end = time_stroke_36) %>% 
  mutate(event_36 = ifelse(eventType == 1, 1,0)) %>%
  mutate(compet_36 = ifelse(eventType == 2, 1,0)) %>%
  mutate(cens_36 = ifelse(eventType == 0 & time_end < 36, 1,0)) %>%
  mutate(event_compet_36 = ifelse(eventType == 1 | eventType ==2, 1,0)) %>%
  select(patid, indexdt, drugsub, bmi_groups, date_event,date_compet, date_stend, date_lost,date_admcen, event_36,cens_36, compet_36, event_compet_36, time_end,Tstart,eventType, date_stroke)

# Outcome data for total effect on ischemic stroke
dat_isch_stroke <- exp_out_isch_stroke %>%
  rename(date_event = epidt_isch_stroke) %>%
  rename(date_compet = dod) %>%
  rename(date_stend = senddt) %>%
  rename(date_lost = regenddt) %>%
  rename(date_admcen = admcen) %>%
  rename(time_end = time_isch_stroke_36) %>% 
  mutate(event_36 = ifelse(eventType == 1, 1,0)) %>%
  mutate(compet_36 = ifelse(eventType == 2, 1,0)) %>%
  mutate(cens_36 = ifelse(eventType == 0 & time_end < 36, 1,0)) %>%
  mutate(event_compet_36 = ifelse(eventType == 1 | eventType ==2, 1,0)) %>%
  select(patid, indexdt, drugsub, bmi_groups, date_event,date_compet, date_stend, date_lost,date_admcen, event_36,cens_36, compet_36, event_compet_36, time_end,Tstart,eventType,date_isch_stroke)

# Outcome data for total effect on haemorrghic stroke
dat_hem_stroke <- exp_out_hem_stroke %>%
  rename(date_event = epidt_hem_stroke) %>%
  rename(date_compet = dod) %>%
  rename(date_stend = senddt) %>%
  rename(date_lost = regenddt) %>%
  rename(date_admcen = admcen) %>%
  rename(time_end = time_hem_stroke_36) %>% 
  mutate(event_36 = ifelse(eventType == 1, 1,0)) %>%
  mutate(compet_36 = ifelse(eventType == 2, 1,0)) %>%
  mutate(cens_36 = ifelse(eventType == 0 & time_end < 36, 1,0)) %>%
  mutate(event_compet_36 = ifelse(eventType == 1 | eventType ==2, 1,0)) %>%
  select(patid, indexdt, drugsub, bmi_groups, date_event,date_compet, date_stend, date_lost,date_admcen, event_36,cens_36, compet_36,event_compet_36, time_end,Tstart,eventType, date_hem_stroke)

# Outcome data for total effect on systemic embolism
dat_se <- exp_out_sys_embo %>%
  rename(date_event = epidt_sys_embo) %>%
  rename(date_compet = dod) %>%
  rename(date_stend = senddt) %>%
  rename(date_lost = regenddt) %>%
  rename(date_admcen = admcen) %>%
  rename(time_end = time_se_36) %>% 
  mutate(event_36 = ifelse(eventType == 1, 1,0)) %>%
  mutate(compet_36 = ifelse(eventType == 2, 1,0)) %>%
  mutate(cens_36 = ifelse(eventType == 0 & time_end < 36, 1,0)) %>%
  mutate(event_compet_36 = ifelse(eventType == 1 | eventType ==2, 1,0)) %>%
  select(patid, indexdt, drugsub, bmi_groups, date_event,date_compet, date_stend, date_lost,date_admcen, event_36,cens_36, compet_36,event_compet_36, time_end,Tstart,eventType, date_sys_embo)

# Outcome data for treatment effects on death

dat_death <- exp_out %>%
  rename(date_event = dod) %>%
  rename(date_stend = senddt) %>%
  rename(date_lost = regenddt) %>%
  rename(date_admcen = admcen) %>%
  rename(time_end = time_death_36) %>% 
  mutate(cens_36 = ifelse(eventType_death == 0 , 1,0)) %>%
  mutate(event_36 = ifelse(eventType_death == 1 , 1, 0)) %>%
  select(patid, indexdt, drugsub, bmi_groups, date_event, date_stend, date_lost,date_admcen, event_36, time_end,Tstart,cens_36, eventType_death, date_death)

# Outcome data for total effect on major bleeding

dat_bleed <- exp_out_safe %>%
  rename(date_event = epidt_bleeding) %>%
  rename(date_compet = dod) %>%
  rename(date_stend = senddt) %>%
  rename(date_lost = regenddt) %>%
  rename(date_admcen = admcen) %>%
  rename(time_end = time_bleed_36) %>% 
  mutate(event_36 = ifelse(eventType == 1, 1,0)) %>%
  mutate(compet_36 = ifelse(eventType == 2, 1,0)) %>%
  mutate(cens_36 = ifelse(eventType == 0 & time_end < 36, 1,0)) %>%
  mutate(event_compet_36 = ifelse(eventType == 1 | eventType ==2, 1,0)) %>%
  select(patid, indexdt, drugsub, bmi_groups, date_event,date_compet, date_stend, date_lost,date_admcen, event_36,cens_36, compet_36,event_compet_36, time_end,Tstart,eventType, date_bleeding)

# Outcome data for total effect on major bleeding with BMI < 3 years

dat_bleed_bmi_sen <- exp_out_bmi_bleeding %>%
  rename(date_event = epidt_bleeding) %>%
  rename(date_compet = dod) %>%
  rename(date_stend = senddt) %>%
  rename(date_lost = regenddt) %>%
  rename(date_admcen = admcen) %>%
  rename(time_end = time_bleed_36) %>% 
  mutate(event_36 = ifelse(eventType == 1, 1,0)) %>%
  mutate(compet_36 = ifelse(eventType == 2, 1,0)) %>%
  mutate(cens_36 = ifelse(eventType == 0 & time_end < 36, 1,0)) %>%
  mutate(event_compet_36 = ifelse(eventType == 1 | eventType ==2, 1,0)) %>%
  select(patid, indexdt, drugsub, bmi_groups, bmi_groups_derived, date_event,date_compet, date_stend, date_lost,date_admcen, event_36,cens_36, compet_36,event_compet_36, time_end,Tstart,eventType, date_bleeding)

# Identify variables with missing data preparing for ipw 

colSums(is.na(exp_out))

# Dataset for IPW in stroke/SE (composite) 
dat_ipweights_eff <- exp_out %>%
  select(patid, indexdt, drugsub, age, bmi_groups, alcohol, chf_lvef, diabet, gender, hyperten_req_trt, sbp,
         renal_cat, pstroke, pse, ptia, pad, aortic_plaque, MI, smoke, imd2015_5, e2015_imd_5, cancer, 
         haem_cancer, aspirin, statins, clopidogrel, amiodarone, eth_grp, 
         liver_disease, betabl,acei_arb, indexyear, hgb_low, plt_low, liver_high, severecomorbid) %>%
  #drop missing data
drop_na(alcohol,smoke,bmi_groups,imd2015_5,renal_cat,eth_grp, sbp) 


# Dataset for IPW in stroke/SE (composite) using BMI <= 3 years 
dat_ipweights_eff_bmi_sen <- exp_out_bmi_sse %>%
  select(patid, indexdt, drugsub, age, bmi_groups, alcohol, chf_lvef, diabet, gender, hyperten_req_trt, sbp,
         renal_cat, pstroke, pse, ptia, pad, aortic_plaque, MI, smoke, imd2015_5, e2015_imd_5, cancer, 
         haem_cancer, aspirin, statins, clopidogrel, amiodarone, eth_grp, 
         liver_disease, betabl,acei_arb, indexyear, hgb_low, plt_low, liver_high, severecomorbid, copd, connect_tissue, peptic,hemiplegia, bmi_groups_derived) %>%
  #drop missing data
  drop_na(alcohol,smoke,bmi_groups,bmi_groups_derived,imd2015_5,renal_cat,eth_grp, sbp) 


# Dataset for IPW in mortality 

dat_ipweights_mort <- exp_out %>%
  select(patid, indexdt, drugsub, age, bmi_groups, alcohol, chf_lvef, diabet, gender, hyperten_req_trt, sbp,
         renal_cat, pstroke, pse, ptia, pad, aortic_plaque, MI, smoke, imd2015_5, e2015_imd_5, cancer, 
         haem_cancer, aspirin, statins, clopidogrel, amiodarone, eth_grp, 
         liver_disease, betabl,acei_arb, indexyear, hgb_low, plt_low, liver_high,severecomorbid, copd, connect_tissue, peptic,hemiplegia) %>%

  #drop missing data
  drop_na(alcohol,smoke,bmi_groups,imd2015_5,renal_cat,eth_grp, sbp) 



# Dataset for IPW in major bleeding 
dat_ipweights_safe <- exp_out_safe %>%
  select(patid, indexdt, drugsub, age, bmi_groups, alcohol, chf_lvef, diabet, gender, hyperten_req_trt, sbp,
         renal_cat, pstroke, pse, ptia, pad, aortic_plaque, MI, smoke, imd2015_5, e2015_imd_5, cancer, 
         haem_cancer, aspirin, statins, clopidogrel, eth_grp, 
         liver_disease, betabl,acei_arb, indexyear, hgb_low, plt_low, liver_high, severecomorbid) %>%
  #drop missing data
  drop_na(alcohol,smoke,bmi_groups,imd2015_5,renal_cat,eth_grp, sbp) 



# Dataset for IPW in major bleeding using BMI <= 3 years 
dat_ipweights_safe_bmi_sen <- exp_out_bmi_bleeding %>%
  select(patid, indexdt, drugsub, age, bmi_groups, alcohol, chf_lvef, diabet, gender, hyperten_req_trt, sbp,
         renal_cat, pstroke, pse, ptia, pad, aortic_plaque, MI, smoke, imd2015_5, e2015_imd_5, cancer, 
         haem_cancer, aspirin, statins, clopidogrel, eth_grp, 
         liver_disease, betabl,acei_arb, indexyear, hgb_low, plt_low, liver_high, severecomorbid,bmi_groups_derived) %>%
  #drop missing data
  drop_na(alcohol,smoke,bmi_groups, bmi_groups_derived, imd2015_5,renal_cat,eth_grp, sbp) 

