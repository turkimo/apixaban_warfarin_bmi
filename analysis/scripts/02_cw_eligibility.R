# Eligibility for effectiveness####


# Inclusion criteria according to CHADS2-VASC 

# Congestive heart failure (or Left ventricular systolic dysfunction)
str(trt_dat$chf_lvef)
trt_dat$chf_lvef_incl <- ifelse(trt_dat$chf_lvef == "Y",1,0)
table(trt_dat$chf_lvef,trt_dat$chf_lvef_incl)

#Hypertension: blood pressure consistently above 140/90 mmH (or treated hypertension on medication)
str(trt_dat$hyperten_req_trt)

trt_dat$htn_incl <- ifelse(trt_dat$hyperten_req_trt == "Yes", 1, ifelse(trt_dat$sbp > 140 & !is.na(trt_dat$sbp),1, 0))

table(trt_dat$htn_incl,trt_dat$hyperten_req_trt) %>% addmargins()


#Age >= 75 years
str(trt_dat$age75)
trt_dat$age75_incl <- ifelse(trt_dat$age75 == 1, 2,0)

#Diabetes Mellitus
str(trt_dat$diabet)
levels(trt_dat$diabet)

trt_dat$diabet_incl <- ifelse(trt_dat$diabet == "Diabitec", 1,0)


# Stroke or TIA or thromboembolism in history
str(trt_dat$pstroke)
str(trt_dat$ptia)
str(trt_dat$pse)

trt_dat$pstroke_tia_se_incl <- trt_dat %$% ifelse(pstroke == "Y", 2, 
                                          ifelse(ptia == "Y", 2, ifelse(pse == "Y", 2, 0)))
table(trt_dat$pstroke_tia_se_incl, trt_dat$pstroke)


# Vascular disease (e.g. peripheral artery disease, myocardial infarction, aortic plaque)
str(trt_dat$pad)
str(trt_dat$aortic_plaque)
str(trt_dat$MI)
trt_dat$vasc_dis_incl <- trt_dat %$% ifelse(pad == "Y", 1, 
                                    ifelse(aortic_plaque == "Y", 1, 
                                           ifelse(MI == "Y", 1, 0)))


# Age 65-74 years
trt_dat$age65_74_incl <- trt_dat %$% ifelse(age >= 65 & age <= 74,1,0)

# Sex category I.e. female gender)
str(trt_dat$gender)
trt_dat$gender_incl<- trt_dat %$% ifelse(gender == 2, 1,0)

# CHADS-VASC


trt_dat <- trt_dat %>% mutate(chads2_vac = gender_incl + age65_74_incl + age75_incl + vasc_dis_incl+ htn_incl+ chf_lvef_incl+ diabet_incl + pstroke_tia_se_incl)

summary(trt_dat$chads2_vac)


  
  
trt_dat$eligible <- ifelse(trt_dat$chads2_vac >= 2, "Yes", "No")

table(trt_dat$eligible) 


# Exclusion criteria 

str(trt_dat$rev_afib)
str(trt_dat$mitral_stenosis)
str(trt_dat$incr_bleedrisk)
str(trt_dat$mech_heartvalve)

trt_dat$excl <- trt_dat %$% ifelse(rev_afib == "N" & mitral_stenosis == "N" & incr_bleedrisk == "N" & mech_heartvalve == "N" & valvular_dis == "N" & valve_surgery == "N" & endocarditis == "N" & highdoseasp == "N" & thien_and_asp == "N" & stroke7day == "N" & anticoag_ind == "N" & prior_pe_dvt == "N", "No", "Yes")

table(trt_dat$excl, trt_dat$ARISTOTLE_excl) %>% addmargins()

trt_elig_dat <- trt_dat %>% filter(eligible == "Yes" & excl == "No")


table(trt_elig_dat$drugsub)

rm(trt_dat, trt_dat1)

# Eligibility for safety dataset ####


# Inclusion criteria according to CHADS2-VASC 

# Congestive heart failure (or Left ventricular systolic dysfunction)
str(trt_safe$chf_lvef)
trt_safe$chf_lvef_incl <- ifelse(trt_safe$chf_lvef == "Y",1,0)
table(trt_safe$chf_lvef,trt_safe$chf_lvef_incl)

#Hypertension: blood pressure consistently above 140/90 mmH (or treated hypertension on medication)
str(trt_safe$hyperten_req_trt)

trt_safe$htn_incl <- ifelse(trt_safe$hyperten_req_trt == "Yes", 1, ifelse(trt_safe$sbp > 140 & !is.na(trt_safe$sbp),1, 0))

table(trt_safe$htn_incl,trt_safe$hyperten_req_trt) %>% addmargins()


#Age >= 75 years
str(trt_safe$age75)
trt_safe$age75_incl <- ifelse(trt_safe$age75 == 1, 2,0)

#Diabetes Mellitus
str(trt_safe$diabet)
levels(trt_safe$diabet)

trt_safe$diabet_incl <- ifelse(trt_safe$diabet == "Diabitec", 1,0)


# Stroke or TIA or thromboembolism in history
str(trt_safe$pstroke)
str(trt_safe$ptia)
str(trt_safe$pse)

trt_safe$pstroke_tia_se_incl <- trt_safe %$% ifelse(pstroke == "Y", 2, 
                                                  ifelse(ptia == "Y", 2, ifelse(pse == "Y", 2, 0)))
table(trt_safe$pstroke_tia_se_incl, trt_safe$pstroke)


# Vascular disease (e.g. peripheral artery disease, myocardial infarction, aortic plaque)
str(trt_safe$pad)
str(trt_safe$aortic_plaque)
str(trt_safe$MI)
trt_safe$vasc_dis_incl <- trt_safe %$% ifelse(pad == "Y", 1, 
                                            ifelse(aortic_plaque == "Y", 1, 
                                                   ifelse(MI == "Y", 1, 0)))


# Age 65-74 years
trt_safe$age65_74_incl <- trt_safe %$% ifelse(age >= 65 & age <= 74,1,0)

# Sex category I.e. female gender)
str(trt_safe$gender)
trt_safe$gender_incl<- trt_safe %$% ifelse(gender == 2, 1,0)

# CHADS-VASC


trt_safe <- trt_safe %>% mutate(chads2_vac = gender_incl + age65_74_incl + age75_incl + vasc_dis_incl+ htn_incl+ chf_lvef_incl+ diabet_incl + pstroke_tia_se_incl)

summary(trt_safe$chads2_vac)




trt_safe$eligible <- ifelse(trt_safe$chads2_vac >= 2, "Yes", "No")

table(trt_safe$eligible) 


# Exclusion criteria 

str(trt_safe$rev_afib)
str(trt_safe$mitral_stenosis)
str(trt_safe$incr_bleedrisk)
str(trt_safe$mech_heartvalve)

trt_safe$excl <- trt_safe %$% ifelse(rev_afib == "N" & mitral_stenosis == "N" & incr_bleedrisk == "N" & mech_heartvalve == "N" & valvular_dis == "N" & valve_surgery == "N" & endocarditis == "N" & highdoseasp == "N" & thien_and_asp == "N" & stroke7day == "N" & anticoag_ind == "N" & prior_pe_dvt == "N", "No", "Yes")

table(trt_safe$excl, trt_safe$ARISTOTLE_excl) %>% addmargins()

trt_elig_safet_dat <- trt_safe %>% filter(eligible == "Yes" & excl == "No")


table(trt_elig_safet_dat$drugsub)

rm(trt_safe)

