# Function to create stabilized IPT weights using baseline confounders for mortality  

# Calculating IPT weights ####

ipt_wt_mort <- function(dataset) { 
  
  #denominator of stabilized IPT weights
  p_denom <- glm(drugsub ~ 
                   age+I(age^2)+
                   as.factor(bmi_groups)+
                   as.factor(alcohol)+
                   as.factor(chf_lvef)+
                   as.factor(diabet)+
                   as.factor(gender)+
                   as.factor(hyperten_req_trt)+
                   sbp+I(sbp^2)+
                   as.factor(renal_cat)+
                   as.factor(pstroke)+
                   as.factor(pse)+
                   as.factor(ptia)+
                   as.factor(pad)+
                   as.factor(aortic_plaque)+
                   as.factor(MI)+
                   as.factor(smoke)+
                   as.factor(imd2015_5)+
                   as.factor(e2015_imd_5)+
                   as.factor(cancer)+
                   as.factor(haem_cancer)+
                   as.factor(aspirin)+
                   as.factor(statins)+
                   as.factor(clopidogrel)+
                   as.factor(amiodarone)+
                   as.factor(eth_grp)+
                   as.factor(liver_disease)+
                   as.factor(betabl)+
                   as.factor(acei_arb)+
                   as.factor(indexyear)+
                   as.factor(hgb_low)+
                   as.factor(plt_low)+
                   as.factor(liver_high)+
                   as.factor(severecomorbid)+
                   as.factor(copd)+
                   as.factor(connect_tissue)+
                   as.factor(peptic)+
                   as.factor(hemiplegia),
                 family = binomial(), data = dataset)
  
  dataset$pden_trt <- predict(p_denom, dataset, type="response")
  
  # numerator of stabilized IPT weights for effect modification and overall results
  p_num <- glm(drugsub~as.factor(bmi_groups), family = binomial(), data = dataset)
  p_num_all <- glm(drugsub~1, family = binomial(), data = dataset)
  
  dataset$pnum_trt <- predict(p_num, dataset, type="response")
  dataset$pnum_trt_all <- predict(p_num_all, dataset, type="response")
  
  #Calculate the stabilized IPT weights for effect modification and overall results
  dataset$emod.ipw <- ifelse(dataset$drugsub == 0, ((1-dataset$pnum_trt)/(1-dataset$pden_trt)),
                             (dataset$pnum_trt/dataset$pden_trt))
  
  dataset$emod.ipw_all <- ifelse(dataset$drugsub == 0, ((1-dataset$pnum_trt_all)/(1-dataset$pden_trt)),
                                 (dataset$pnum_trt_all/dataset$pden_trt))
  
  #Summary of the IPT weights
  tapply(dataset$emod.ipw, dataset$drugsub, function(x) format(summary(x)))
  
  #Truncation of extreme weights for sensitivity analysis 
  trunc <- function(x){
    quantiles <- quantile( x, c(.01, .99 ) , na.rm = T)
    x[ x < quantiles[1] ] <- quantiles[1]
    x[ x > quantiles[2] ] <- quantiles[2]
    x
  }
  
  # 99% truncated IPT weights
  dataset$emod.ipw.trunc <- trunc(dataset$emod.ipw)
  
  dataset$emod.ipw_all_trunc <- trunc(dataset$emod.ipw_all)
  
  tapply(dataset$emod.ipw.trunc, dataset$drugsub, function(x) format(summary(x)))
  
  dataset <- dataset %>% select(patid, emod.ipw,emod.ipw.trunc,emod.ipw_all,emod.ipw_all_trunc)
  
  rm(p_denom, p_num, p_num_all)
  
  
  ipt_weights <<- dataset
}
