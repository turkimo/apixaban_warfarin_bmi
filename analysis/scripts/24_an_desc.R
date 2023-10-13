### Participants ####

# Flow chart of participants #### 
total <- read.delim("J:\\EHR-Working\\****\\10_****_Project\\data\\drugissue_oac.txt")

total$issuedate <- as.Date(total$issuedate, "%d/%m/%Y")

# unique patid
total1 <- total %>% filter(issuedate >= "2013-01-01" & issuedate <= "2019-07-31")

n_distinct(total1$patid)

# Read treatment data sets
api <- read.delim("H:\\Desktop\\Jan23_data\\apixaban_patient_jan23.txt",stringsAsFactors = T)

warf <- read.delim("J:\\EHR-Working\\****\\10_****_Project\\Jan23_data\\warf_patient_new_jan23.txt",stringsAsFactors = T)

warf_prev <- read.delim("H:\\Desktop\\Jan23_data\\warf_rx_prev_jan23.txt")

# Merge the two treatments datasets (no exclusions) 
trt.count <- full_join(warf, api) %>% full_join(warf_prev)

trt.count$indexdt <- as.Date(trt.count$indexdt, "%d/%m/%Y")

trt.count <- trt.count %>% 
  group_by(patid, prior_vka) %>%
  filter(indexdt == min(indexdt))

trt.count1 <- trt.count %>% filter(indexdt >= "2013-01-01" & indexdt <= "2019-07-31")


# Summary of data
n_distinct(trt.count$patid)

n_distinct(total1$patid)-n_distinct(trt.count1$patid)
n_distinct(trt.count1$patid) - n_distinct(trt_dat$patid)
table(trt.count$prior_vka,  useNA = "always")

trt.count %>% filter(prior_vka == "N") %$% table(drugsub,  useNA = "always")

nrow(trt.count1)-nrow(trt_dat)
nrow(trt_dat)


duplicated_trt_dat <- trt.count %>% 
  group_by(patid) %>%
  filter(n()>1)

### Baseline table ####


# Read-in data 
data <- exp_out

# Check skewness of cont. variables 
#Age
hist(data$age)

# Systolic blood pressure
hist(data$sbp)

# Weight 
hist(data$weight)

# BMI
hist(data$bmi)

# Check structure 
str(data)

# Treatments 
data$drugsub <- factor(data$drugsub, levels=c(1,0),
                  labels=c("Apixaban", "Warfarin"))

# Variable list 
vars <- c("age","gender","sbp","weight","MI","any_bleed","fall",
  "age75","pstroke_embol","chf_lvef","diabet",
  "hyperten_req_trt","chads2_vac","acei_arb","amiodarone","betabl",
  "aspirin","clopidogrel","digoxin","ccb","statins",
  "nsaids","antacids","renal_cat","pad","aortic_plaque","smoke",
  "alcohol","imd2015_5","e2015_imd_5","eth_grp","copd","connect_tissue",
  "peptic","liver_disease","hemiplegia","cancer","haem_cancer",
  "bmi","bmi_groups","bmi_groups_base", "acei_arb_normo","age_groups_base","indexyear","apx_dose")

non_normal <- c("age","sbp","weight","bmi")                    
#Create the table 
baseline_tab <- CreateTableOne(vars, c("drugsub"), data,
  c("age75","chads2_vac","imd2015_5","e2015_imd_5"),
  includeNA = TRUE,
  smd = TRUE,
  addOverall = T
)


# Print the table ####

kableone(baseline_tab,booktabs = TRUE, longtable = T,nonnormal = non_normal) %>%
  kable_styling(full_width = NULL,latex_options	= "repeat_header") 

# Save the table 
tab_base <- print(baseline_tab, nonnormal = non_normal, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, test = FALSE, smd = TRUE)

tab_base_smd <- print(baseline_tab, nonnormal = non_normal, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, test = FALSE, smd = TRUE)

## Save to a CSV file
write.csv(tab_base, file = "H:\\My Documents\\GitHub\\apixaban_warfarin_obese\\analysis\\output\\tables\\tab_base.csv")






# Read-in data 
data_smd <- merge(dat_ipweights_eff,exp_out)

# Treatments 
data_smd$drugsub <- factor(data_smd$drugsub, levels=c(1,0),
                       labels=c("Apixaban", "Warfarin"))

#Create the table 
baseline_tab_smd <- CreateTableOne(vars, c("drugsub"), data_smd,
                               c("age75","chads2_vac","imd2015_5","e2015_imd_5"),
                               includeNA = TRUE,
                               smd = TRUE,
                               addOverall = T
)


# Print the table ####

kableone(baseline_tab_smd,booktabs = TRUE, longtable = T,nonnormal = non_normal) %>%
  kable_styling(full_width = NULL,latex_options	= "repeat_header") 

# Save the table 
tab_base_smd <- print(baseline_tab_smd, nonnormal = non_normal, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, test = FALSE, smd = TRUE)

## Save to a CSV file
write.csv(tab_base_smd, file = "H:\\My Documents\\GitHub\\apixaban_warfarin_obese\\analysis\\output\\tables\\tab_base_smd.csv")



# Create table one with SMDs when IP weighted
ipt_wt_eff(dat_ipweights_eff)

data1 = merge(ipt_weights,exp_out)

# Treatments 
data1$drugsub <- factor(data1$drugsub, levels=c(1,0),
                       labels=c("Apixaban", "Warfarin"))

baseline_tab_dat_ipw <- svydesign(ids = ~1, data = data1, weights = ~emod.ipw)

baseline_tab_ipw <- svyCreateTableOne(vars, c("drugsub"), data= baseline_tab_dat_ipw,
               c("age75","chads2_vac","imd2015_5","e2015_imd_5"),
               includeNA = TRUE,
               smd = TRUE,
               addOverall = T)

tab_base_ipw_Print_smd <- print(baseline_tab_ipw, nonnormal = non_normal, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, test = FALSE, smd = TRUE)

write.csv(tab_base_ipw_Print_smd, file= "H:\\My Documents\\GitHub\\apixaban_warfarin_obese\\analysis\\output\\tables\\tab_base_ipw.csv")

# Make side by side unweighted and weighted table with SMDs
baseline_tab_both <- cbind(print(baseline_tab_smd, printToggle = FALSE),
               print(baseline_tab_ipw, printToggle = FALSE))

baseline_tab_both <- cbind(tab_base_smd, tab_base_ipw_Print_smd)

write.csv(baseline_tab_both, file= "H:\\My Documents\\GitHub\\apixaban_warfarin_obese\\analysis\\output\\tables\\tab_base_smd_ipw_both.csv")


