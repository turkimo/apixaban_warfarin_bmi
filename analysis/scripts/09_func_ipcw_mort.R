# Function to create stabilized time-varying IP censoring weights using baseline confounders for mortality

ipc_wt_mort<- function(data_long){

  #denominator of stabilized IPC weights
  pcens_fit <-
    glm(eventCens ~  drugsub +  
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
        data = data_long,
        family = binomial(link = "logit"))
  
  
  # numerator of stabilized IPC weights
  cens_num_fit <- glm(eventCens ~ drugsub , data = data_long, family = binomial(link = "logit"))
  
  
  
  # Calculate the stabilized IPC weights
  data_long %<>% mutate(
    # denominator
    pred_c = 1 - predict(pcens_fit, ., type = "response"),
    
    # numerator
    pred_c_num = 1-predict(cens_num_fit, ., type = "response")) %>% 
    group_by(patid) %>% 
    mutate(
      # cumulative product per patient over all months
      cum_pred_c = cumprod(pred_c),
      cum_pred_c_num = cumprod(pred_c_num)
    ) %>% 
    ungroup() %>% 
    mutate(
      ipcw_cens_unstab = 1/cum_pred_c,
      ipcw_cens_stab = cum_pred_c_num/cum_pred_c,
      # Combined weight for censoring and IPTW
      ipw_sw = emod.ipw*ipcw_cens_stab,
      ipw_sw_all = emod.ipw_all*ipcw_cens_stab,

    )
  
  
  an_data <<- data_long
  
}





