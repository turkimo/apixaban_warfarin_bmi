# Data management ####


# Treatments ####

# Incident apixaban users 

api <- read.delim("H:\\Desktop\\Jan23_data\\apixaban_patient_jan23.txt",stringsAsFactors = T)

api <- api %>% filter(prior_vka == "N")

# Incident warfarin users 
warf <- read.delim("J:\\EHR-Working\\****\\10_****_Project\\Jan23_data\\warf_patient_new_jan23.txt",stringsAsFactors = T)

# Merge the two treatments datasets 
trt_dat <- full_join(warf, api) %>% select(-prior_vka)

# Read treatment data sets
api_1 <- read.delim("H:\\Desktop\\Jan23_data\\apixaban_patient_jan23.txt",stringsAsFactors = T)

warf_1 <- read.delim("J:\\EHR-Working\\****\\10_****_Project\\Jan23_data\\warf_patient_new_jan23.txt",stringsAsFactors = T)

warf_prev <- read.delim("H:\\Desktop\\Jan23_data\\warf_rx_prev_jan23.txt")

# Merge the two treatments datasets (no exclusions) 
trt.count <- full_join(warf_1, api_1) %>% full_join(warf_prev)

trt.count$indexdt <- as.Date(trt.count$indexdt, "%d/%m/%Y")

trt.count <- trt.count %>% 
  group_by(patid) %>%
  filter(indexdt == min(indexdt))

trt.count1 <- trt.count %>% filter(indexdt >= "2013-01-01" & indexdt <= "2019-07-31") %>% select(patid, prior_vka)

trt.count2 <- trt.count1 %>% filter(patid %in% trt_dat$patid)

trt_dat <- left_join(trt_dat,trt.count2, by= "patid")

# Assume that patient continued their first treatment (i.e. no switching)
# Observational analogue of the ITT

# Index date
trt_dat$indexdt <- as.Date(trt_dat$indexdt,"%d/%m/%Y")

trt_dat <- trt_dat %>% 
  group_by(patid) %>%
  filter(indexdt == min(indexdt))

# Drop "" levels in prior vka 
trt_dat$prior_vka <- as.factor(trt_dat$prior_vka)

trt_dat$prior_vka <- droplevels(trt_dat$prior_vka)

trt_dat <- trt_dat %>% filter(prior_vka == "N")

#Tabulate 
trt_dat %$% table(drugsub,prior_vka) %>% addmargins()


# Data management for dates ####

# Birth date assuming mid year
trt_dat$birthdt <- as.Date(ISOdate(trt_dat$yob, 07, 02))

# Transform dates to date class

# AF date 
trt_dat$afdt <- as.Date(trt_dat$afdt,"%d/%m/%Y")

# Loss to follow-up or transfer out 
trt_dat$regenddt <- as.Date(trt_dat$regenddt,"%d/%m/%Y")

# Death dates 
# Read the death ONS dataset 
death <- read.delim("J:\\EHR-Working\\****\\10_****_Project\\data\\ons_death.txt",stringsAsFactors = T)

# Transform dod to date class
death$dod <- as.Date(death$dod, "%d/%m/%Y")

# Filter the data set to select patid and dod only for merging 
death <- death %>% select(patid, dod)

# Join trt_dat and death datasets  
trt_dat <- left_join(trt_dat,death)

# CPRD death date 
trt_dat$cprd_deathdt <- as.Date(trt_dat$cprd_deathdt,"%d/%m/%Y")

# Add time from AF diagnosis to start AC
trt_dat$time_AF_to_AC <- trt_dat %$% time_length(interval(as.Date(afdt), as.Date(indexdt)), "years")

# Age at AF diagnosis 
trt_dat$age_at_AF <- trt_dat %$% time_length(interval(as.Date(birthdt), as.Date(afdt)), "years")

# Add a variable indicating date at 3 years of follow-up 
trt_dat$admcen <- trt_dat$indexdt + 1095.7

# Create age groups for baseline reporting 
trt_dat$age_groups_base <- trt_dat %$% ifelse(age < 49, "18-49",
                                              ifelse(age < 59, "50-59", 
                                                     ifelse(age < 69, "60-69",
                                                            ifelse(age < 75, "70-74",
                                                                   ifelse(age >74, "75 and above",NA)))))

#Add study end date "2019/07/31"
trt_dat$senddt <- "2019/07/31"
trt_dat$senddt <- as.Date(trt_dat$senddt)

# Remove those with index date after end of study 
trt_dat <- trt_dat %>% filter(indexdt <= "2019-07-31")

# Remove those <18 years of age  
trt_dat <- trt_dat %>% filter(age >= 18)

# Remove those with death date before indexdate 
trt_dat <- trt_dat %>% filter(dod >= indexdt | is.na(dod))


# Check duplicate records 
duplicated_trt_dat <- trt_dat %>% 
  group_by(patid) %>%
  filter(n()>1)

# Remove those with prescription of the two treatments on the same day
#trt_dat <- trt_dat[!(trt_dat$patid %in% 704277420131), ]

# Final count
trt_dat %$% table(drugsub,prior_vka) %>% addmargins()

# Create a new variable by grouping BMI ####

# Check the range and structure for validity 
range(trt_dat$bmi,na.rm = T)
str(trt_dat$bmi)

# BMI groups using NICE cut-off points 
trt_dat <- trt_dat %>% mutate(bmi_groups= as.factor(ifelse(bmi <25, 1, 
                                                           ifelse(bmi >= 25 & bmi <30, 2, 
                                                                  ifelse(bmi >= 30,3)))))

trt_dat$bmi_groups <- factor(trt_dat$bmi_groups, levels = c(1,2,3), 
                              labels = c("Normal weight",
                                         "Overweight","Obese"))

table(trt_dat$bmi_groups, useNA = "always")
summary(trt_dat$bmi)

# Create a new categorical variable for BMI in baseline reporting and sensitivity analysis
trt_dat$bmi_groups_base <- trt_dat %$% as.factor(ifelse(bmi < 18.5, "Underweight",
                                                        ifelse(bmi < 25, "Normal",
                                                               ifelse(bmi < 30, "Overweight", 
                                                                      ifelse(bmi < 35, "Obesity class I",
                                                                             ifelse(bmi < 40, "Obesity class II",
                                                                                    ifelse(bmi > 39.9, "Obesity class III", TRUE)))))))
# BMI groups count 
trt_dat %$% table(drugsub,bmi_groups) %>% addmargins()


# Transform variables to factors ####
# Gender 
trt_dat$gender <- factor(trt_dat$gender, levels = c(1,2), labels = c("male","female"))

# Diabetes
trt_dat$diabet <- factor(trt_dat$diabet, levels = c(0,1), labels = c("Non-aiabitec","Diabitec"))

# HTN
trt_dat$hyperten_req_trt <- factor(trt_dat$hyperten_req_trt, levels = c(0,1), labels = c("No","Yes"))

# Ethnic group 
# Transfer the Native Hawaiian or Other Pacific Islander to Other (only 1 individual)
levels(trt_dat$eth_grp)[levels(trt_dat$eth_grp) == "Native Hawaiian or Other Pacific Islander"] <-"Other"

levels(trt_dat$prior_pe_dvt)[levels(trt_dat$prior_pe_dvt) == ""] <- "N"


# Assign NA values for categorical variables 
trt_dat <- trt_dat %>% replace_with_na(replace= list(smoke="", alcohol=c("","2_3_4_drinker"), renal_cat= "5 Not reported", eth_grp= c("","Unknown" )))

# Drop the unused levels from factors 
trt_dat$smoke <- droplevels(trt_dat$smoke)

trt_dat$alcohol <- droplevels(trt_dat$alcohol)

trt_dat$renal_cat <- droplevels(trt_dat$renal_cat)

trt_dat$eth_grp <- droplevels(trt_dat$eth_grp)


# Index year 
trt_dat$indexyear <- format(trt_dat$indexdt, format="%Y")

# Any history of non-major bleeding (Aurum or HES)
trt_dat <- trt_dat %>% mutate(any_bleed = ifelse(hesbleed == "Y" & aurumbleed == "Y", "Y", 
                                         ifelse(hesbleed == "Y" & (aurumbleed == "N" | is.na(aurumbleed)), "Y", 
                                                ifelse( aurumbleed == "Y" & (hesbleed == "N" | is.na(hesbleed)),"Y", "N"))))

table(trt_dat$aurumbleed,trt_dat$any_bleed)

# ACE and ARBs in normotensive individuals 
trt_dat <- trt_dat %>% mutate(acei_arb_normo = as.factor(
  ifelse(hyperten_req_trt == "Yes" & acei_arb == "Y", "N", 
         ifelse(hyperten_req_trt == "No" & acei_arb == "N", "N", 
                ifelse(acei_arb == "Y" & hyperten_req_trt == "No", "Y","N")))))

# Treatment levels as numeric (warfarin 0; apixaban 1)

trt_dat$drugsub <- trt_dat$drugsub <- relevel(trt_dat$drugsub, ref = "Warfarin")
trt_dat$drugsub <- as.numeric(trt_dat$drugsub)-1


# Create another data set allowing for 2 years of follow-up prior to the end of study
trt_dat1 <- trt_dat %>% filter(indexdt <= "2017-07-31")
trt_dat1 <- trt_dat1 %>% filter(indexdt >= "2014-01-01")

# Remove unnecessary dataframes 
rm(warf,api,death,duplicated_trt_dat)

write_csv(trt_dat,"H:\\Desktop\\Jan23_data\\Datasets_updated\\trt_dat.csv")

write_csv(trt_dat1,"H:\\Desktop\\Jan23_data\\Datasets_updated\\trt_dat_2_yr_before_study_end.csv")


# Safety ####

# Incident apixaban users 


api <- read.delim("H:\\Desktop\\Jan23_data\\apixaban_patient_jan23.txt",stringsAsFactors = T)

api <- api %>% filter(prior_vka == "N")


# Create an index date to use later for censoring those started another treatment 
indexdt_api <- api %>% select(patid,indexdt)

indexdt_api <- rename(indexdt_api, indexdt_api = indexdt)

# Incident warfarin users 
warf <- read.delim("J:\\EHR-Working\\****\\10_****_Project\\Jan23_data\\warf_patient_new_jan23.txt",stringsAsFactors = T)

# Create an index date to use later for censoring those started another treatment 
indexdt_warf <- warf %>% select(patid,indexdt)

indexdt_warf <- rename(indexdt_warf, indexdt_warf = indexdt)

# Last rx date
apx_safe <- read.delim("J:\\EHR-Working\\****\\10_****_Project\\data\\apixaban_lastrx.txt",stringsAsFactors = T)

warf_safe <- read.delim("J:\\EHR-Working\\****\\10_****_Project\\data\\warf_lastrx.txt",stringsAsFactors = T)

# Join Last rx data with the main data 
apx_safe <- left_join(api,apx_safe)

warf_safe <- left_join(warf,warf_safe)

# Add date for treatment switching 
apx_safe <- left_join(apx_safe,indexdt_warf)


warf_safe <- left_join(warf_safe,indexdt_api)



# Merge the two treatments datasets ####

trt_safe <- full_join(apx_safe,warf_safe)

#index date
trt_safe$indexdt <- as.Date(trt_safe$indexdt,"%d/%m/%Y")


# Remove the second record for those switched treatment 
trt_safe <- trt_safe %>% 
  group_by(patid) %>%
  filter(indexdt == min(indexdt))

# Drop level of "" in prior vka 
trt_safe$prior_vka <- droplevels(trt_safe$prior_vka)

#Tabulate 
trt_safe %$% table(drugsub,prior_vka) %>% addmargins()

# Data management for dates ####

# Birth date assuming mid year
trt_safe$birthdt <- as.Date(ISOdate(trt_safe$yob, 07, 02))

# Transform date to date class

#index date for indexdt_api
trt_safe$indexdt_api <- as.Date(trt_safe$indexdt_api,"%d/%m/%Y")

#index date for indexdt_warf
trt_safe$indexdt_warf <- as.Date(trt_safe$indexdt_warf,"%d/%m/%Y")

# Filter indexdt_api and indexdt_warf < indexdt for each treatment 
trt_safe <- trt_safe %>% mutate(indexdt_warf= case_when(
  indexdt_warf > indexdt  ~ indexdt_warf))

trt_safe <- trt_safe %>% mutate(indexdt_api= case_when(
  indexdt_api > indexdt  ~ indexdt_api))

# AF date 
trt_safe$afdt <- as.Date(trt_safe$afdt,"%d/%m/%Y")

# Loss to follow-up or transfer out 
trt_safe$regenddt <- as.Date(trt_safe$regenddt,"%d/%m/%Y")

# Death dates 
# Read the death ons dataset 
death <- read.delim("J:\\EHR-Working\\****\\10_****_Project\\data\\ons_death.txt",stringsAsFactors = T)

# Transform dod as date 
death$dod <- as.Date(death$dod, "%d/%m/%Y")

#filter the data set to select patid and dod only for merging 
death_merge <- death %>% select(patid, dod)

# Join the trt_safe data set with death dates 
trt_safe <- left_join(trt_safe,death_merge)

# CPRD death date 
trt_safe$cprd_deathdt <- as.Date(trt_safe$cprd_deathdt,"%d/%m/%Y")

# Add time from AF diagnosis to start AC
trt_safe$timeAFtoAC <- trt_safe %$% time_length(interval(as.Date(afdt), as.Date(indexdt)), "years")

# Age at AF diagnosis 
trt_safe$ageAF <- trt_safe %$% time_length(interval(as.Date(birthdt), as.Date(afdt)), "years")


# Last Rx date 
trt_safe$last_rxdt <- as.Date(trt_safe$last_rxdt,"%d/%m/%Y")

# Add a variable indicating date at 3 years of follow-up 
trt_safe$admcen <- trt_safe$indexdt + 1095.7

#Add study end date "2019/07/31"
trt_safe$senddt <- "2019/07/31"
trt_safe$senddt <- as.Date(trt_safe$senddt)

# Remove those with index date after end of study 
trt_safe <- trt_safe %>% filter(indexdt < "2019-07-31")

# Remove those <18 years of age  
trt_safe <- trt_safe %>% filter(age >= 18)

# Check duplicate recoreds 
duplicated_trt <- trt_safe %>% 
  group_by(patid) %>%
  filter(n()>1)

# Remove those with prescription of the two treatments on the same day
trt_safe <- trt_safe[!(trt_safe$patid %in% 704277420131), ]


# Final count
trt_safe %$% table(drugsub,prior_vka) %>% addmargins()

# Create a new variable by grouping BMI ####

# Check the range and structure for validity 
range(trt_safe$bmi,na.rm = T)
str(trt_safe$bmi)


# BMI groups using NICE cut-off points 
trt_safe <- trt_safe %>% mutate(bmi_groups= as.factor(ifelse(bmi <25, 1, 
                                                           ifelse(bmi >= 25 & bmi <30, 2, 
                                                                  ifelse(bmi >= 30,3)))))

trt_safe$bmi_groups <- factor(trt_safe$bmi_groups, levels = c(1,2,3), 
                             labels = c("Normal weight",
                                        "Overweight","Obese"))


# BMI groups count 
trt_safe %$% table(drugsub,bmi_groups) %>% addmargins()


# Transform variables to factors ####
# Gender 
trt_safe$gender <- factor(trt_safe$gender, levels = c(1,2), labels = c("male","female"))

# Diabetes
trt_safe$diabet <- factor(trt_safe$diabet, levels = c(0,1), labels = c("Non-aiabitec","Diabitec"))

# HTN
trt_safe$hyperten_req_trt <- factor(trt_safe$hyperten_req_trt, levels = c(0,1), labels = c("No","Yes"))

# Ethnic group 
# Transfer the Native Hawaiian or Other Pacific Islander to Other (only 1 individual)
levels(trt_safe$eth_grp)[levels(trt_safe$eth_grp) == "Native Hawaiian or Other Pacific Islander"] <-"Other"

levels(trt_safe$prior_pe_dvt)[levels(trt_safe$prior_pe_dvt) == ""] <- "N"

# Assign NA values for categorical variables 
trt_safe <- trt_safe %>% replace_with_na(replace= list(smoke="", alcohol=c("","2_3_4_drinker"), renal_cat= "5 Not reported", eth_grp= c("","Unknown" )))

# Drop the unused levels from factors 
trt_safe$smoke <- droplevels(trt_safe$smoke)

trt_safe$alcohol <- droplevels(trt_safe$alcohol)

trt_safe$renal_cat <- droplevels(trt_safe$renal_cat)

trt_safe$eth_grp <- droplevels(trt_safe$eth_grp)


# Index year 
trt_safe$indexyear <- format(trt_safe$indexdt, format="%Y")

# Any history of non-major bleeding (Aurum or HES)
trt_safe <- trt_safe %>% mutate(any_bleed = case_when(
  hesbleed == 'Y' & aurumbleed == 'Y' ~ 'Y', 
  hesbleed == 'Y' & (aurumbleed == 'N' | is.na(aurumbleed)) ~ 'Y',
  aurumbleed == 'Y' & (hesbleed == 'N' | is.na(hesbleed)) ~ 'Y',
  aurumbleed == 'N' & hesbleed == 'N'  ~ 'N',
)) 

# ACE and ARBs in noromtensive individuals 
trt_safe <- trt_safe %>% mutate(acei_arb_normo = as.factor(case_when(
  hyperten_req_trt == 'Yes' & acei_arb == 'Y' ~ 'N', 
  hyperten_req_trt == 'No' & acei_arb == 'N' ~ 'N',
  acei_arb == 'Y' & hyperten_req_trt == 'No' ~ 'Y',
  acei_arb == 'N' & hyperten_req_trt == 'Yes'  ~ 'N',
)))

# Treatment levels as numeric (warfarin 0; apixaban 1)
trt_safe$drugsub <- relevel(trt_safe$drugsub, ref = "Warfarin")
trt_safe$drugsub <- as.numeric(trt_safe$drugsub)-1

# Remove unnecessary dataframes 
rm(warf,api,warf_safe,apx_safe,death,death_merge)








