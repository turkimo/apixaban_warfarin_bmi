# Outcomes#### 

# Composite Primary: stroke or Systemic embolism ####

# Read the outcomes dataset 
composite_dat <- read.delim("H:\\Desktop\\Jan23_data\\hes_stroke_embol.txt",stringsAsFactors = T)

# Transform the episode date to a Date class
composite_dat$epidt <- as.Date(composite_dat$epidt, "%d/%m/%Y")

# Filter the data by First occurrence of an event on or after index date
composite_dat <- composite_dat %>% filter(epidt < "2019-07-31")

# Join treatment data with composite outcome data 
composite_first_dat <- left_join(trt_elig_dat,composite_dat, by="patid")

# Create a time variable for the difference between the index date and event date
composite_first_dat <- composite_first_dat %>% mutate(composite_time= time_length(interval(as.Date(indexdt),as.Date(epidt)),"days"))

# Filter events by those occurring during the follow-up 
composite_first_dat <- composite_first_dat %>% filter(composite_time >= 1 & composite_time < 1095.7)

# Filter by first occurrence of events 
composite_first_dat <- composite_first_dat %>% 
  group_by(patid,indexdt,drugsub) %>%
  filter(epidt == min(epidt))

# List all ICD-10 codes 
levels(composite_first_dat$ICD)

# Interpret ICD-10 codes 
composite_first_dat <- composite_first_dat %>% mutate(composite_icd= as.factor(case_when(
  ICD == "I60.0" ~ "Hemorrhagic stroke",
  ICD == "I60.1" ~ "Hemorrhagic stroke",
  ICD == "I60.2"~ "Hemorrhagic stroke",
  ICD == "I60.3" ~ "Hemorrhagic stroke",
  ICD == "I60.4"~ "Hemorrhagic stroke",
  ICD == "I60.5"~ "Hemorrhagic stroke",
  ICD == "I60.6" ~ "Hemorrhagic stroke",
  ICD == "I60.7"~ "Hemorrhagic stroke",
  ICD == "I60.8"~ "Hemorrhagic stroke",
  ICD == "I60.9"~ "Hemorrhagic stroke",
  ICD == "I61.0"~ "Hemorrhagic stroke",
  ICD == "I61.1"~ "Hemorrhagic stroke",
  ICD == "I61.2" ~ "Hemorrhagic stroke",
  ICD == "I61.3" ~ "Hemorrhagic stroke",
  ICD == "I61.4"~ "Hemorrhagic stroke",
  ICD == "I61.5"~ "Hemorrhagic stroke",
  ICD == "I61.6"~ "Hemorrhagic stroke",
  ICD == "I61.7" ~ "Hemorrhagic stroke",
  ICD == "I61.8" ~ "Hemorrhagic stroke",
  ICD == "I61.9"~ "Hemorrhagic stroke",
  ICD == "I62.0"~ "Hemorrhagic stroke",
  ICD == "I62.1"~ "Hemorrhagic stroke",
  ICD == "I62.9"~ "Hemorrhagic stroke",
  ICD == "I63.0" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.1" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.2"~ "Ischemic or uncertain type of stroke",
  ICD == "I63.3"~ "Ischemic or uncertain type of stroke",
  ICD == "I63.4" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.5" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.6" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.8" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.9" ~ "Ischemic or uncertain type of stroke",
  ICD == "I64" ~ "Ischemic or uncertain type of stroke",
  ICD == "I74.0" ~ "Systmeic embolism",
  ICD == "I74.1" ~ "Systmeic embolism",
  ICD == "I74.2" ~ "Systmeic embolism",
  ICD == "I74.3" ~ "Systmeic embolism",
  ICD == "I74.4" ~ "Systmeic embolism",
  ICD == "I74.5" ~ "Systmeic embolism",
  ICD == "I74.8" ~ "Systmeic embolism",
  ICD ==  "I74.9" ~ "Systmeic embolism"
)))


# Identify duplicates 

sum(duplicated(composite_first_dat$patid))

composite_first_dat <- composite_first_dat[!duplicated(composite_first_dat[c("patid","indexdt","epidt")]),]

# remove duplicates 

duplicated_composite_first <- composite_first_dat %>% 
  group_by(patid) %>%
  filter(n()>1)

# Rename variables
composite_first_dat <- rename(composite_first_dat, epidt_composite = epidt, ICD_composite = ICD)

# Merge the exposure data with the primary outcome data ####

# Merge exposures and primary outcome
exp_out <- left_join(trt_elig_dat,composite_first_dat)


# Indicator variable if the outcome occurred during 36 months 
exp_out %<>% mutate(composite_36= ifelse(!is.na(ICD_composite),1,0))

# Censoring date for composite   
exp_out <- exp_out %>% mutate(date_composite = pmin(dod, regenddt,epidt_composite,admcen,senddt,na.rm = T))

# Time variable for composite outcome
exp_out <- exp_out %>% mutate(time_composite_36 = round(time_length(interval(as.Date(indexdt),as.Date(date_composite)),"month"),0))


# Death ####


# Censoring date for death 
exp_out <- exp_out %>% mutate(date_death = pmin(dod, regenddt,admcen,senddt,na.rm = T))


# Create a time variable for the difference between the index date and event date
exp_out <- exp_out %>% mutate(death_time= time_length(interval(as.Date(indexdt),as.Date(date_death)),"days"))

exp_out <- exp_out %>% mutate(time_death_36 = round(time_length(interval(as.Date(indexdt),as.Date(date_death)),"month"),0))

# Outcome indicator variable for mortality filtering for study time to avoid having events after study    
exp_out <- exp_out %>% mutate(death_36 = ifelse(!is.na(dod) & death_time < 1095.7 & dod < "2019-07-31", 1,0))

exp_out %<>% mutate(
  eventType_death = case_when(
    death_36 == 1 & date_death == dod ~ "death",
    T ~ "survivor"
  ),
  # set factor levels
  eventType_death = factor(eventType_death, levels = c("survivor","death")),
  eventType_death = as.integer(eventType_death) - 1,
  Tstart = -0.01
)


# Create a variable with 3 levels (a level for each event type)
exp_out %<>% mutate(
  eventType = case_when(
    composite_36 == 1 & date_composite == epidt_composite ~ "composite",
    death_36 == 1 & date_composite == dod ~ "death",
    T ~ "survivor"
  ),
  # set factor levels
  eventType = factor(eventType, levels = c("survivor", "composite","death")),
  eventType = as.integer(eventType) - 1,
  Tstart = -0.01
)


# Check the validity 
exp_out %>% select(dod, regenddt,epidt_composite,admcen,senddt,date_composite, date_death, eventType)


# Additional estimand, hypothetical for effectiveness ####

# Composite Primary: stroke or Systemic embolism ####

# Read the outcomes dataset 
composite_dat <- read.delim("H:\\Desktop\\Jan23_data\\hes_stroke_embol.txt",stringsAsFactors = T)

# Transform the episode date to a Date class
composite_dat$epidt <- as.Date(composite_dat$epidt, "%d/%m/%Y")

# Filter the data by First occurrence of an event on or after index date
composite_dat <- composite_dat %>% filter(epidt < "2019-07-31")

# Join treatment data with composite outcome data 
composite_first_dat <- left_join(trt_elig_dat,composite_dat, by="patid")

# Create a time variable for the difference between the index date and event date
composite_first_dat <- composite_first_dat %>% mutate(composite_time= time_length(interval(as.Date(indexdt),as.Date(epidt)),"days"))

# Filter events by those occurring during the follow-up 
composite_first_dat <- composite_first_dat %>% filter(composite_time >= 1 & composite_time < 1095.7)

# Filter by first occurrence of events 
composite_first_dat <- composite_first_dat %>% 
  group_by(patid,indexdt,drugsub) %>%
  filter(epidt == min(epidt))

# List all ICD-10 codes 
levels(composite_first_dat$ICD)

# Interpret ICD-10 codes 
composite_first_dat <- composite_first_dat %>% mutate(composite_icd= as.factor(case_when(
  ICD == "I60.0" ~ "Hemorrhagic stroke",
  ICD == "I60.1" ~ "Hemorrhagic stroke",
  ICD == "I60.2"~ "Hemorrhagic stroke",
  ICD == "I60.3" ~ "Hemorrhagic stroke",
  ICD == "I60.4"~ "Hemorrhagic stroke",
  ICD == "I60.5"~ "Hemorrhagic stroke",
  ICD == "I60.6" ~ "Hemorrhagic stroke",
  ICD == "I60.7"~ "Hemorrhagic stroke",
  ICD == "I60.8"~ "Hemorrhagic stroke",
  ICD == "I60.9"~ "Hemorrhagic stroke",
  ICD == "I61.0"~ "Hemorrhagic stroke",
  ICD == "I61.1"~ "Hemorrhagic stroke",
  ICD == "I61.2" ~ "Hemorrhagic stroke",
  ICD == "I61.3" ~ "Hemorrhagic stroke",
  ICD == "I61.4"~ "Hemorrhagic stroke",
  ICD == "I61.5"~ "Hemorrhagic stroke",
  ICD == "I61.6"~ "Hemorrhagic stroke",
  ICD == "I61.7" ~ "Hemorrhagic stroke",
  ICD == "I61.8" ~ "Hemorrhagic stroke",
  ICD == "I61.9"~ "Hemorrhagic stroke",
  ICD == "I62.0"~ "Hemorrhagic stroke",
  ICD == "I62.1"~ "Hemorrhagic stroke",
  ICD == "I62.9"~ "Hemorrhagic stroke",
  ICD == "I63.0" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.1" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.2"~ "Ischemic or uncertain type of stroke",
  ICD == "I63.3"~ "Ischemic or uncertain type of stroke",
  ICD == "I63.4" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.5" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.6" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.8" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.9" ~ "Ischemic or uncertain type of stroke",
  ICD == "I64" ~ "Ischemic or uncertain type of stroke",
  ICD == "I74.0" ~ "Systmeic embolism",
  ICD == "I74.1" ~ "Systmeic embolism",
  ICD == "I74.2" ~ "Systmeic embolism",
  ICD == "I74.3" ~ "Systmeic embolism",
  ICD == "I74.4" ~ "Systmeic embolism",
  ICD == "I74.5" ~ "Systmeic embolism",
  ICD == "I74.8" ~ "Systmeic embolism",
  ICD ==  "I74.9" ~ "Systmeic embolism"
)))


# Identify duplicates 

sum(duplicated(composite_first_dat$patid))

composite_first_dat <- composite_first_dat[!duplicated(composite_first_dat[c("patid","indexdt","epidt")]),]

# remove duplicates 

duplicated_composite_first <- composite_first_dat %>% 
  group_by(patid) %>%
  filter(n()>1)

# Rename variables
composite_first_dat <- rename(composite_first_dat, epidt_composite = epidt, ICD_composite = ICD)

# Merge the exposure data with the primary outcome data ####

# Merge exposures and primary outcome
exp_out_hypo_eff <- left_join(trt_elig_safet_dat,composite_first_dat)


# Indicator variable if the outcome occurred during 36 months 
exp_out_hypo_eff %<>% mutate(composite_36= ifelse(!is.na(ICD_composite),1,0))

# Add 30 days for last_rxdt to account for stockpiling

exp_out_hypo_eff$last_rxdt_stock <- exp_out_hypo_eff$last_rxdt + 30

exp_out_hypo_eff %>% select(last_rxdt,last_rxdt_stock)

# Censoring date for composite   
exp_out_hypo_eff <- exp_out_hypo_eff %>% mutate(date_composite = pmin(dod, regenddt,epidt_composite,admcen,senddt,last_rxdt_stock,indexdt_api,indexdt_warf,na.rm = T))

# Time variable for composite outcome
exp_out_hypo_eff <- exp_out_hypo_eff %>% mutate(time_composite_36 = round(time_length(interval(as.Date(indexdt),as.Date(date_composite)),"month"),0))



# Death ####


# Censoring date for death 
exp_out_hypo_eff <- exp_out_hypo_eff %>% mutate(date_death = pmin(dod, regenddt,admcen,senddt,na.rm = T))


# Create a time variable for the difference between the index date and event date
exp_out_hypo_eff <- exp_out_hypo_eff %>% mutate(death_time= time_length(interval(as.Date(indexdt),as.Date(date_death)),"days"))

exp_out_hypo_eff <- exp_out_hypo_eff %>% mutate(time_death_36 = round(time_length(interval(as.Date(indexdt),as.Date(date_death)),"month"),0))

# Outcome indicator variable for mortality filtering for study time to avoid having events after study    
exp_out_hypo_eff <- exp_out_hypo_eff %>% mutate(death_36 = ifelse(!is.na(dod) & death_time < 1095.7 & dod < "2019-07-31", 1,0))

# Create a variable with 3 levels (a level for each event type)
exp_out_hypo_eff %<>% mutate(
  eventType = case_when(
    composite_36 == 1 & date_composite == epidt_composite ~ "composite",
    death_36 == 1 & date_composite == dod ~ "death",
    T ~ "survivor"
  ),
  # set factor levels
  eventType = factor(eventType, levels = c("survivor", "composite","death")),
  eventType = as.integer(eventType) - 1,
  Tstart = -0.01
)

# Stroke any type ####

# Filtering the data by the First occurring events on or after index date 

# Join treatment data with outcomes data 
stroke_first_dat <- left_join(trt_elig_dat,composite_dat, by="patid")

# Create a time variable for the difference between the index date and event date
stroke_first_dat %<>% mutate(stroke_time= time_length(interval(as.Date(indexdt),as.Date(epidt)),"days"))

stroke_first_dat %<>%  filter(stroke_time >= 1 & stroke_time < 1095.7)


# Interpret ICD-10 codes 
stroke_first_dat %<>%  mutate(stroke_icd= as.factor(case_when(
  ICD == "I60.1" ~ "Stroke",
  ICD == "I60.2"~ "Stroke",
  ICD == "I60.3" ~ "Stroke",
  ICD == "I60.4"~ "Stroke",
  ICD == "I60.5"~ "Stroke",
  ICD == "I60.6" ~ "Stroke",
  ICD == "I60.7"~ "Stroke",
  ICD == "I60.8"~ "Stroke",
  ICD == "I60.9"~ "Stroke",
  ICD == "I61.0"~ "Stroke",
  ICD == "I61.1"~ "Stroke",
  ICD == "I61.2" ~ "Stroke",
  ICD == "I61.3" ~ "Stroke",
  ICD == "I61.4"~ "Stroke",
  ICD == "I61.5"~ "Stroke",
  ICD == "I61.6"~ "Stroke",
  ICD == "I61.7" ~ "Stroke",
  ICD == "I61.8" ~ "Stroke",
  ICD == "I61.9"~ "Stroke",
  ICD == "I62.0"~ "Stroke",
  ICD == "I62.1"~ "Stroke",
  ICD == "I62.9"~ "Stroke",
  ICD == "I63.0" ~ "Stroke",
  ICD == "I63.1" ~ "Stroke",
  ICD == "I63.2"~ "Stroke",
  ICD == "I63.3"~ "Stroke",
  ICD == "I63.4" ~ "Stroke",
  ICD == "I63.5" ~ "Stroke",
  ICD == "I63.6" ~ "Stroke",
  ICD == "I63.8" ~ "Stroke",
  ICD == "I63.9" ~ "Stroke",
  ICD == "I64" ~ "Stroke",
  ICD == "I74.0" ~ "Systmeic embolism",
  ICD == "I74.1" ~ "Systmeic embolism",
  ICD == "I74.2" ~ "Systmeic embolism",
  ICD == "I74.3" ~ "Systmeic embolism",
  ICD == "I74.4" ~ "Systmeic embolism",
  ICD == "I74.5" ~ "Systmeic embolism",
  ICD == "I74.8" ~ "Systmeic embolism",
  ICD ==  "I74.9" ~ "Systmeic embolism"
)))

# Filter by first events 
stroke_first_dat %<>% 
  group_by(patid,indexdt,drugsub,stroke_icd) %>%
  filter(epidt == min(epidt))

# remove duplicates 
sum(duplicated(stroke_first_dat$patid))
stroke_first_dat <- stroke_first_dat[!duplicated(stroke_first_dat[c("patid","indexdt","epidt","stroke_icd")]),]

# Check that there is no duplication 
duplicated_stroke_first_dat <- stroke_first_dat %>% 
  group_by(patid) %>%
  filter(n()>1)

# Rename variables 
stroke_first_dat <- rename(stroke_first_dat, epidt_stroke = epidt, ICD_stroke = ICD)

# filter the data to include stroke only 
stroke_first_dat %<>%  filter(stroke_icd == "Stroke")

# Merge exposures and primary outcome
exp_out_stroke <- left_join(exp_out,stroke_first_dat)

# Censoring date for stroke 
exp_out_stroke %<>%  mutate(date_stroke = pmin(dod, regenddt,epidt_stroke,admcen,senddt,na.rm = T))

# Outcome indicator variable 
exp_out_stroke %<>%  mutate(stroke_36= ifelse(!is.na(ICD_stroke),1,0))

# Time variable for stroke outcome
exp_out_stroke %<>%  mutate(time_stroke_36 = round(time_length(interval(as.Date(indexdt),as.Date(date_stroke)),"month"),0))

# Create a variable with 3 levels (a level for each event type)
exp_out_stroke %<>% mutate(
  eventType = case_when(
    stroke_36 == 1 & date_stroke == epidt_stroke ~ "stroke",
    death_36 == 1 & date_stroke == dod ~ "death",
    T ~ "survivor"
  ),
  # set factor levels
  eventType = factor(eventType, levels = c("survivor", "stroke","death")),
  eventType = as.integer(eventType) - 1,
  Tstart = -0.01
)


# Check the validity 
exp_out_stroke %>% select(dod, regenddt,epidt_stroke,admcen,senddt,date_stroke)


# Ischemic stroke, heamorrghic and SE separately ####

# Ischemic stroke ####


# Filtering the data by the First occurring events on or after index date 

# Join treatment data with outcome data 
stroke_type_first_dat <- left_join(trt_elig_dat,composite_dat, by="patid")

# Create a time variable for the difference between the index date and event date
stroke_type_first_dat %<>% mutate(stroke_type_time= time_length(interval(as.Date(indexdt),as.Date(epidt)),"days"))

stroke_type_first_dat %<>% filter(stroke_type_time >= 1 & stroke_type_time < 1095.7)


# Interpret ICD-10 codes 
stroke_type_first_dat %<>% mutate(stroke_type_icd= as.factor(case_when(
  ICD == "I60.1" ~ "Hemorrhagic stroke",
  ICD == "I60.2"~ "Hemorrhagic stroke",
  ICD == "I60.3" ~ "Hemorrhagic stroke",
  ICD == "I60.4"~ "Hemorrhagic stroke",
  ICD == "I60.5"~ "Hemorrhagic stroke",
  ICD == "I60.6" ~ "Hemorrhagic stroke",
  ICD == "I60.7"~ "Hemorrhagic stroke",
  ICD == "I60.8"~ "Hemorrhagic stroke",
  ICD == "I60.9"~ "Hemorrhagic stroke",
  ICD == "I61.0"~ "Hemorrhagic stroke",
  ICD == "I61.1"~ "Hemorrhagic stroke",
  ICD == "I61.2" ~ "Hemorrhagic stroke",
  ICD == "I61.3" ~ "Hemorrhagic stroke",
  ICD == "I61.4"~ "Hemorrhagic stroke",
  ICD == "I61.5"~ "Hemorrhagic stroke",
  ICD == "I61.6"~ "Hemorrhagic stroke",
  ICD == "I61.7" ~ "Hemorrhagic stroke",
  ICD == "I61.8" ~ "Hemorrhagic stroke",
  ICD == "I61.9"~ "Hemorrhagic stroke",
  ICD == "I62.0"~ "Hemorrhagic stroke",
  ICD == "I62.1"~ "Hemorrhagic stroke",
  ICD == "I62.9"~ "Hemorrhagic stroke",
  ICD == "I63.0" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.1" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.2"~ "Ischemic or uncertain type of stroke",
  ICD == "I63.3"~ "Ischemic or uncertain type of stroke",
  ICD == "I63.4" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.5" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.6" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.8" ~ "Ischemic or uncertain type of stroke",
  ICD == "I63.9" ~ "Ischemic or uncertain type of stroke",
  ICD == "I64" ~ "Ischemic or uncertain type of stroke",
  ICD == "I74.0" ~ "Systmeic embolism",
  ICD == "I74.1" ~ "Systmeic embolism",
  ICD == "I74.2" ~ "Systmeic embolism",
  ICD == "I74.3" ~ "Systmeic embolism",
  ICD == "I74.4" ~ "Systmeic embolism",
  ICD == "I74.5" ~ "Systmeic embolism",
  ICD == "I74.8" ~ "Systmeic embolism",
  ICD ==  "I74.9" ~ "Systmeic embolism"
)))


# Filter by first occurring events 
stroke_type_first_dat %<>%
  group_by(patid,indexdt,drugsub,stroke_type_icd) %>%
  filter(epidt == min(epidt))

# remove duplicates 
sum(duplicated(stroke_type_first_dat$patid))

stroke_type_first_dat <- stroke_type_first_dat[!duplicated(stroke_type_first_dat[c("patid","indexdt","epidt","stroke_type_icd")]),]


duplicated_stroke_type_first_dat <- stroke_type_first_dat %>% 
  group_by(patid) %>%
  filter(n()>1)


# Filter by Ischemic stroke only 
isch_stroke_first_dat <- stroke_type_first_dat %>% filter(stroke_type_icd == "Ischemic or uncertain type of stroke")

# Filter by Hemorrhagic stroke only 
hem_stroke_first_dat <- stroke_type_first_dat %>% filter(stroke_type_icd == "Hemorrhagic stroke")

# Filter by SE only 
sys_embo_first <- stroke_type_first_dat %>% filter(stroke_type_icd == "Systmeic embolism")


# Rename variables for ischemic stroke 
isch_stroke_first_dat <- rename(isch_stroke_first_dat, epidt_isch_stroke = epidt, ICD_isch_stroke = ICD)

# Rename variables for Hemorrhagic stroke 
hem_stroke_first_dat <- rename(hem_stroke_first_dat, epidt_hem_stroke = epidt, ICD_hem_stroke = ICD)

# Rename variables for SE 
sys_embo_first <- rename(sys_embo_first, epidt_sys_embo = epidt, ICD_sys_embo = ICD)




# Merge the  data with each dataset 

# Ischemic stroke 

# Merge exp_out and ischemic stroke 
exp_out_isch_stroke <- left_join(exp_out,isch_stroke_first_dat)

# Censoring date for ischemic stroke 
exp_out_isch_stroke <- exp_out_isch_stroke %>% mutate(date_isch_stroke = pmin(dod, regenddt,epidt_isch_stroke,admcen,senddt,na.rm = T))

# Outcome indicator variable 
exp_out_isch_stroke <- exp_out_isch_stroke %>% mutate(isch_stroke_36= ifelse(stroke_type_icd == "Ischemic or uncertain type of stroke",1,0))

# Time variable for isch stroke outcome
exp_out_isch_stroke <- exp_out_isch_stroke %>% mutate(time_isch_stroke_36 = round(time_length(interval(as.Date(indexdt),as.Date(date_isch_stroke)),"month"),0))

# Create a variable with 3 levels (a level for each event type)
exp_out_isch_stroke %<>% mutate(
  eventType = case_when(
    isch_stroke_36 == 1 & date_isch_stroke == epidt_isch_stroke ~ "ischstroke",
    death_36 == 1 & date_isch_stroke == dod ~ "death",
    T ~ "survivor"
  ),
  # set factor levels
  eventType = factor(eventType, levels = c("survivor", "ischstroke","death")),
  eventType = as.integer(eventType) - 1,
  Tstart = -0.01
)




#Hemorrhagic stroke ####

# Merge exp_out and Hemorrhagic stroke
exp_out_hem_stroke <- left_join(exp_out,hem_stroke_first_dat)

# Censoring date for Hemorrhagic stroke 
exp_out_hem_stroke %<>% mutate(date_hem_stroke = pmin(dod, regenddt,epidt_hem_stroke,admcen,senddt,na.rm = T))

# Outcome indicator variable 
exp_out_hem_stroke %<>% mutate(hem_stroke_36= ifelse(stroke_type_icd == "Hemorrhagic stroke",1,0))

# Time variable for hem stroke outcome
exp_out_hem_stroke %<>% mutate(time_hem_stroke_36 = round(time_length(interval(as.Date(indexdt),as.Date(date_hem_stroke)),"month"),0))
 
 # Create a variable with 3 levels (a level for each event type)
 exp_out_hem_stroke %<>% mutate(
   eventType = case_when(
     hem_stroke_36 == 1 & date_hem_stroke == epidt_hem_stroke ~ "hemstroke",
     death_36 == 1 & date_hem_stroke == dod ~ "death",
     T ~ "survivor"
   ),
   # set factor levels
   eventType = factor(eventType, levels = c("survivor", "hemstroke","death")),
   eventType = as.integer(eventType) - 1,
   eventCens = if_else(eventType == 0, 1, 0),
   eventoutcome = if_else(eventType == 1, 1, 0),
   Tstart = -0.01
 )
 

 
# SE ####
# Merge exp_out and SE 
exp_out_sys_embo <- left_join(exp_out,sys_embo_first)

# Censoring date for SE 
exp_out_sys_embo %<>% mutate(date_sys_embo = pmin(dod, regenddt,epidt_sys_embo,admcen,senddt,na.rm = T))

# Outcome indicator variable for SE
exp_out_sys_embo %<>% mutate(se_36= ifelse(stroke_type_icd == "Systmeic embolism",1,0))

# Time variable for se outcome
exp_out_sys_embo %<>% mutate(time_se_36 = round(time_length(interval(as.Date(indexdt),as.Date(date_sys_embo)),"month"),0))


# Create a variable with 3 levels (a level for each event type)
exp_out_sys_embo %<>% mutate(
  eventType = case_when(
    se_36 == 1 & date_sys_embo == epidt_sys_embo ~ "se",
    death_36 == 1 & date_sys_embo == dod ~ "death",
    T ~ "survivor"
  ),
  # set factor levels
  eventType = factor(eventType, levels = c("survivor", "se","death")),
  eventType = as.integer(eventType) - 1,
  Tstart = -0.01
)



# Remove those with an outcome before the index date 
exp_out_stroke$validity <- exp_out_stroke$date_stroke- exp_out_stroke$indexdt
exp_out_stroke <- exp_out_stroke %>% filter(validity >= 0)

exp_out_isch_stroke$validity <- exp_out_isch_stroke$date_isch_stroke- exp_out_isch_stroke$indexdt
exp_out_isch_stroke <- exp_out_isch_stroke %>% filter(validity >= 0)

exp_out_hem_stroke$validity <- exp_out_hem_stroke$date_hem_stroke- exp_out_hem_stroke$indexdt
exp_out_hem_stroke <- exp_out_hem_stroke %>% filter(validity >= 0)

exp_out_sys_embo$validity <- exp_out_sys_embo$date_sys_embo- exp_out_sys_embo$indexdt
exp_out_sys_embo <- exp_out_sys_embo %>% filter(validity >= 0)


rm(duplicated_composite_first,duplicated_stroke_first_dat,
   duplicated_stroke_type_first_dat,hem_stroke_first_dat,
   isch_stroke_first_dat,composite_first_dat,sys_embo_first,composite_dat,
   stroke_first_dat, stroke_type_first_dat)





# Bleeding safety
## Data management

# Outcome#### 

bleeding <- read.delim("H:\\Desktop\\Jan23_data\\hes_majorbleed.txt",stringsAsFactors = T)

str(bleeding)

# Transform the episode date to Date class
bleeding$epidt <- as.Date(bleeding$epidt, "%d/%m/%Y")


# Filtering the data by the First events only 
# Only events occurring on or after index date? 
# Join treatment data with sse data 
bleeding_first <- left_join(trt_elig_safet_dat,bleeding, by="patid")

# Create a time variable for the difference between the index date and event date
bleeding_first <- bleeding_first %>% mutate(bleeding_time= time_length(interval(as.Date(indexdt),as.Date(epidt)),"days"))

bleeding_first <- bleeding_first %>% filter(bleeding_time >= 1 & bleeding_time < 1827)

# Filter by first events 
bleeding_first <- bleeding_first %>% 
  group_by(patid,indexdt,drugsub,selection) %>%
  filter(epidt == min(epidt))

levels(bleeding_first$ICD)

# Interpret ICD-10 codes 
bleeding_first <- bleeding_first %>% mutate(bleeding_icd= as.factor(case_when(
  ICD == "D62" ~ "Acute posthemorrhagic anemia",
  ICD == "D68.3"~ "Hemorrhagic disorder due to circulating anticoagulants",
  ICD == "H21.0"~ "Retinal hemorrhage",
  ICD == "H31.3"~ "Retinal hemorrhage",
  ICD == "H35.6"~ "Retinal hemorrhage",
  ICD == "H43.1"~ "Vitreous hemorrhage",
  ICD == "H45.0"~ "Vitreous haemorrhage in diseases classified elsewhere",
  ICD == "I31.2"~ "Hemopericardium",
  ICD == "I60.0"~ "Hemorrhagic stroke",
  ICD == "I60.1"~ "Hemorrhagic stroke",
  ICD == "I60.2"~ "Hemorrhagic stroke",
  ICD == "I60.3"~ "Hemorrhagic stroke",
  ICD == "I60.4"~ "Hemorrhagic stroke",
  ICD == "I60.5"~ "Hemorrhagic stroke",
  ICD == "I60.6" ~ "Hemorrhagic stroke",
  ICD == "I60.7"~ "Hemorrhagic stroke",
  ICD == "I60.8"~ "Hemorrhagic stroke",
  ICD == "I60.9"~ "Hemorrhagic stroke",
  ICD == "I61.0"~ "Hemorrhagic stroke",
  ICD == "I61.1"~ "Hemorrhagic stroke",
  ICD == "I61.2" ~ "Hemorrhagic stroke",
  ICD == "I61.3" ~ "Hemorrhagic stroke",
  ICD == "I61.4"~ "Hemorrhagic stroke",
  ICD == "I61.5"~ "Hemorrhagic stroke",
  ICD == "I61.6"~ "Hemorrhagic stroke",
  ICD == "I61.8" ~ "Hemorrhagic stroke",
  ICD == "I61.9"~ "Hemorrhagic stroke",
  ICD == "I62.0"~ "Hemorrhagic stroke",
  ICD == "I62.1"~ "Hemorrhagic stroke",
  ICD == "I62.9"~ "Hemorrhagic stroke",
  ICD ==  "I85.0" ~ "Esophageal varices with bleeding",
  ICD ==  "I98.3" ~ "Oesophageal varices with bleeding in diseases classified elsewhere",
  ICD ==  "J94.2" ~ "Hemothorax",
  ICD ==  "K22.6" ~ "Gastro-esophageal laceration-hemorrhage syndrome",
  ICD ==  "K22.8" ~ "Other specified diseases of esophagus",
  ICD ==  "K25.0" ~ "Gastric ulcer: acute with haemorrhage",
  ICD ==  "K25.2" ~ "Gastric ulcer: acute with both haemorrhage and perforation",
  ICD ==  "K25.4" ~ "Gastric ulcer: chronic or unspecified with haemorrhage",
  ICD ==  "K25.6" ~ "Gastric ulcer: chronic or unspecified with both haemorrhage and perforation",
  ICD ==  "K26.0" ~ "Duodenal ulcer: acute with haemorrhage",
  ICD ==  "K26.2" ~ "Duodenal ulcer: acute with both haemorrhage and perforation",
  ICD ==  "K26.4" ~ "Duodenal ulcer: chronic or unspecified with haemorrhage",
  ICD ==  "K26.6" ~ "Duodenal ulcer: chronic or unspecified with both haemorrhage and perforation",
  ICD ==  "K27.0" ~ "Peptic ulcer, site unspecified: acute with haemorrhage",
  ICD ==  "K27.4" ~ "Peptic ulcer, site unspecified: chronic or unspecified with haemorrhage",
  ICD ==  "K27.6" ~ "Peptic ulcer, site unspecified: chronic or unspecified with both haemorrhage and perforation",
  ICD ==  "K28.0" ~ "Gastrojejunal ulcer: acute with haemorrhage",
  ICD ==  "K28.2" ~ "Gastrojejunal ulcer: acute with both haemorrhage and perforation",
  ICD ==  "K28.4" ~ "Gastrojejunal ulcer: chronic or unspecified with haemorrhage",
  ICD ==  "K28.6" ~ "Gastrojejunal ulcer: chronic or unspecified with both haemorrhage and perforation",
  ICD ==  "K29.0" ~ "Acute haemorrhagic gastritis",
  ICD ==  "K62.5" ~ "Haemorrhage of anus and rectum",
  ICD ==  "K66.1" ~ "Haemoperitoneum",
  ICD ==  "K92.0" ~ "Haematemesis",
  ICD ==  "K92.1" ~ "Melaena",
  ICD ==  "K92.2" ~ "Gastrointestinal haemorrhage, unspecified",
  ICD ==  "M25.0" ~ "Haemarthrosis",
  ICD ==  "N02.0" ~ "Recurrent and persistent haematuria: minor glomerular abnormality",
  ICD ==  "N02.1" ~ "Recurrent and persistent haematuria: focal and segmental glomerular lesions",
  ICD ==  "N02.2" ~ "Recurrent and persistent haematuria : diffuse membranous glomerulonephritis",
  ICD ==  "N02.3" ~ "Recurrent and persistent haematuria: diffuse mesangial proliferative glomerulonephritis",
  ICD ==  "N02.4" ~ "Recurrent and persistent haematuria: diffuse endocapillary proliferative glomerulonephritis",
  ICD ==  "N02.5" ~ "Recurrent and persistent haematuria: diffuse mesangiocapillary glomerulonephritis",
  ICD ==  "N02.7" ~ "Recurrent and persistent haematuria: diffuse crescentic glomerulonephritis",
  ICD ==  "N02.8" ~ "Recurrent and persistent haematuria: other",
  ICD ==  "N02.9" ~ "Recurrent and persistent haematuria: unspecified",
  ICD ==  "N93.8" ~ "Other specified abnormal uterine and vaginal bleeding",
  ICD ==  "N93.9" ~ "Abnormal uterine and vaginal bleeding, unspecified",
  ICD ==  "N95.0" ~ "Postmenopausal bleeding",
  ICD ==  "R04.1" ~ "Haemorrhage from throat",
  ICD ==  "R04.2" ~ "Haemoptysis",
  ICD ==  "R04.8" ~ "Haemorrhage from other sites in respiratory passages",
  ICD ==  "R04.9" ~ "Haemorrhage from respiratory passages, unspecified",
  ICD ==  "R58" ~ "Haemorrhage, not elsewhere classified",
  ICD ==  "S06.4" ~ "Epidural hemorrhage",
  ICD ==  "S06.5" ~ "Traumatic subdural hemorrhage",
  ICD ==  "S06.6" ~ "Traumatic subarachnoid hemorrhage",
)))


# remove duplicates 
sum(duplicated(bleeding_first$patid))
bleeding_first <- bleeding_first[!duplicated(bleeding_first[c("patid","drugsub","indexdt","selection","epidt")]),]


duplicated_bleeding_first <- bleeding_first %>% 
  group_by(patid) %>%
  filter(n()>1)

# Rename variables 
bleeding_first <- rename(bleeding_first, epidt_bleeding = epidt, ICD_bleeding = ICD)

# Merge the trt_elig_safet_dat data with primary outcome data 

# Merge exposures and primary outcome
exp_out_safe <- left_join(trt_elig_safet_dat,bleeding_first)

# Add 30 days for last_rxdt to account for stockpiling

exp_out_safe$last_rxdt_stock <- exp_out_safe$last_rxdt + 30

exp_out_safe %>% select(last_rxdt,last_rxdt_stock)

# Indicator variable if the outcome occurred during 36 months 
exp_out_safe %<>% mutate(bleeding_36= ifelse(!is.na(ICD_bleeding),1,0))

# Censoring date for bleeding   
exp_out_safe <- exp_out_safe %>% mutate(date_bleeding = pmin(dod, regenddt,epidt_bleeding,admcen,senddt,last_rxdt_stock,indexdt_api,indexdt_warf,na.rm = T))

# Time variable for bleeding outcome
exp_out_safe <- exp_out_safe %>% mutate(time_bleed_36 = round(time_length(interval(as.Date(indexdt),as.Date(date_bleeding)),"month"),0))


# Death ####


# Censoring date for death 
exp_out_safe <- exp_out_safe %>% mutate(date_death = pmin(dod, regenddt,admcen,senddt,na.rm = T))


# Create a time variable for the difference between the index date and event date
exp_out_safe <- exp_out_safe %>% mutate(death_time= time_length(interval(as.Date(indexdt),as.Date(date_death)),"days"))


# Outcome indicator variable for mortality filtering for study time to avoid having events after study    
exp_out_safe <- exp_out_safe %>% mutate(death_36 = ifelse(!is.na(dod) & death_time < 1095.7 & dod < "2019-07-31", 1,0))


# Create a variable with 3 levels (a level for each event type)
exp_out_safe %<>% mutate(
  eventType = case_when(
    bleeding_36 == 1 & date_bleeding == epidt_bleeding ~ "bleeding",
    death_36 == 1 & date_bleeding == dod ~ "death",
    T ~ "survivor"
  ),
  # set factor levels
  eventType = factor(eventType, levels = c("survivor", "bleeding","death")),
  eventType = as.integer(eventType) - 1,
  Tstart = -0.01
)

# Check the validity 
exp_out_safe %>% select(dod, regenddt,epidt_bleeding,admcen,senddt,date_bleeding)


# Remove those with a mortality outcome before the index date 
exp_out_safe$validity <- exp_out_safe$date_bleeding- exp_out_safe$indexdt
exp_out_safe <- exp_out_safe %>% filter(validity > 0)



## Senstivity analysis using BMI 3 years 

# BMI DATA ####
bmiset <- read.delim("J:\\EHR-Working\\****\\10_****_Project\\data\\bmi_data.txt")

# Check data structure 
str(bmiset)

# Transform obsdate and obsdt to date format 
bmiset$obsdate <- as.Date(bmiset$obsdate,"%d/%m/%Y")

bmiset$obsdt <- as.Date(bmiset$obsdt,"%d/%m/%Y")


# Create a dataset with patid,indexdt, afdt and selection ####

trtfilter <- exp_out %>% select(patid,indexdt,afdt,yob)


# Merge bmi and trtfilter 
trtbmi <- left_join(x=trtfilter,y=bmiset,by= "patid")

# Check data structure 
str(trtbmi)

# Create a new variable 3 years prior to index date for Weight and BMI
trtbmi1 <- trtbmi %>% filter(param %in% c("Weight","BMI")) %>% mutate(bmieligibile= indexdt-obsdate)

# Join the main data with the filter 
trtbmi <- left_join(x=trtbmi,y=trtbmi1)

#Apply the 3 years filter 
trtbmi <- trtbmi  %>% mutate(bmicheck= as.factor(case_when(
  bmieligibile < -1 ~ 0,
  bmieligibile >= -1 & bmieligibile <= 1095 ~ 1,
  bmieligibile > 1095 ~ 0)))

# Create a new dataset with eligbile readings only 
trtbmi_3_yrs <- trtbmi%>% filter(is.na(bmicheck)| bmicheck == 1)

# Choose the most recent date
trtbmi_3_yrs <- trtbmi_3_yrs %>% group_by(patid,param) %>%
  filter(obsdate == max(obsdate))

# Join the data again 
trtbmi_3_yrs_c <- left_join(trtfilter,trtbmi_3_yrs)


# Number of individuals in with BMI data
trtbmi_3_yrs_c %>% group_by(patid) %>%count()

# remove duplicates
trtbmi_3_yrs_c <- trtbmi_3_yrs_c[!duplicated(trtbmi_3_yrs_c[c("patid","indexdt","obsdate","param")]),]

# Wide data frame 
trtbmi_3_yrs_c_wide <- pivot_wider(
  trtbmi_3_yrs_c,
  id_cols = patid,
  id_expand = T,
  names_from = param,
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_vary = "fastest",
  names_expand = FALSE,
  names_repair = "check_unique",
  values_from = aval,
  values_fill = NULL,
  values_fn = NULL,
  unused_fn = NULL
)

# Select needed columns 
trtbmi_3_yrs_c_wide <- trtbmi_3_yrs_c_wide %>% select(patid,Height,Weight,BMI)

# IF BMI, weight or height equal to 999 assign as missing 
trtbmi_3_yrs_c_wide <- trtbmi_3_yrs_c_wide %>% replace_with_na(replace= list(BMI=999, Weight= 999, Height=999))

# IF BMI out of 5-200kg/m2, weight < 20 kg or height out of 121-214 equal to 999 assign as missing 
trtbmi_3_yrs_c_wide <- trtbmi_3_yrs_c_wide %>%  
  replace_with_na_at(.vars = "Height",
                     condition = ~.x < 1.21)


# Derive BMI from weight and height 
trtbmi_3_yrs_c_wide <- trtbmi_3_yrs_c_wide %>% mutate(BMI_derived = (Weight/((Height)^2)))

# IF derived BMI is missing, use the one recorded directly 
trtbmi_3_yrs_c_wide <- trtbmi_3_yrs_c_wide%>% 
  mutate(bmi_all = coalesce(BMI_derived,BMI))

#Rename variables to label them with the 3 years 
trtbmi_3_yrs_c_wide <- rename(trtbmi_3_yrs_c_wide, Height_3 = Height, Weight_3= Weight, BMI_3 = bmi_all)


# Create BMI groups 
trtbmi_3_yrs_c_wide <- trtbmi_3_yrs_c_wide %>% mutate(bmi_groups_derived= as.factor(case_when(
  BMI_3 < 25 ~ 1,
  BMI_3 > 24.9 & BMI_3 < 30 ~ 2,
  BMI_3 > 29.9 ~ 3)))


trtbmi_3_yrs_c_wide$bmi_groups <- factor(trtbmi_3_yrs_c_wide$bmi_groups_derived, levels = c(1,2,3), 
                                                 labels = c("Normal weight",
                                                            "Overweight","Obese"))

# Merge bmi with treatment and outcome data 
exp_out_bmi_sse <- left_join(exp_out,trtbmi_3_yrs_c_wide)

exp_out_bmi_bleeding <- left_join(exp_out_safe,trtbmi_3_yrs_c_wide)

# Remove unnecessary dataframes 
rm(bmiset,trtbmi_3_yrs_c_wide,trtbmi_3_yrs_c,trtbmi,trtbmi1,trtfilter)






