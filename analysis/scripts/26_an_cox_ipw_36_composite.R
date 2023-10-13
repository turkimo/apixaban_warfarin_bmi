# Code for IPW cox models

library(survival)

ipt_wt_eff(dat_ipweights_eff)

dat_model <- dat_composite %>%
  merge(ipt_weights, by="patid") %>%
  merge(exp_out) %>%
  mutate(endoffup = pmin(date_event, date_stend,date_lost, na.rm=TRUE))


# Create a survival object for SSE  
sse_surv_msm <- dat_model %$% 
  Surv(time=as.numeric(indexdt)/365.25,
       time2= as.numeric(endoffup)/365.25, event= event_36)

dat_model$endoffup

# Get point estimates 
coxph(sse_surv_msm~as.factor(drugsub)*as.factor(bmi_groups), data = dat_model, weights = emod.ipw, robust = T, id=patid) %>% summary()

dat_model$bmi_groups <- relevel(dat_model$bmi_groups, ref = "Normal weight")

dat_model$bmi_groups <- relevel(dat_model$bmi_groups, ref = "Overweight")

dat_model$bmi_groups <- relevel(dat_model$bmi_groups, ref = "Obese")


ipt_wt_safe(dat_ipweights_safe)

dat_model2 <- dat_bleed %>%
  merge(ipt_weights, by="patid") %>%
  merge(exp_out_safe) %>%
  mutate(endoffup = pmin(date_event, date_stend,date_lost, na.rm=TRUE))


# Create a survival object for SSE  
bleed_surv_msm <- dat_model2 %$% 
  Surv(time=as.numeric(indexdt)/365.25,
       time2= as.numeric(endoffup)/365.25, event= event_36)


# Get point estimates 
coxph(bleed_surv_msm~as.factor(drugsub)*as.factor(bmi_groups), data = dat_model2, weights = emod.ipw, robust = T, id=patid) %>% summary()

dat_model2$bmi_groups <- relevel(dat_model2$bmi_groups, ref = "Normal weight")

dat_model2$bmi_groups <- relevel(dat_model2$bmi_groups, ref = "Overweight")

dat_model2$bmi_groups <- relevel(dat_model2$bmi_groups, ref = "Obese")
