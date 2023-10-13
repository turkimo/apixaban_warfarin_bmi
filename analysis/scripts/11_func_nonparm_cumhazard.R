#### This was taken from Jessica Young (2020) A causal framework for classical statistical estimands in failure-time settings with competing events with some modifications to incorporate stratified analysis####

# utility functions for IPW estimators using Stratified analysis

nonParametricCumHaz <- function(weightVector, inputdata, grp, bmi_grp, event_36_long=TRUE){
  outputHazards <- rep(NA, length.out=length(cut_times))
  counter <- 1 
  for(i in cut_times){
    if(event_36_long){
      indices <- inputdata$time_end==i & inputdata$drugsub == grp & inputdata$bmi_groups_n == bmi_grp & inputdata$eventCens==0 & inputdata$compet_36_long==0 
      eventIndicator <- indices & inputdata$event_36_long==1 
    }else{
      indices <- inputdata$time_end==i & inputdata$drugsub == grp & inputdata$bmi_groups_n == bmi_grp & inputdata$eventCens==0
      eventIndicator <- indices & inputdata$compet_36_long==1 
    }
    outputHazards[counter] <- sum(weightVector[eventIndicator]) / sum(weightVector[indices])
    counter <- counter+1
  }
  return(outputHazards)
}

# utility functions for IPW estimators using overall analysis

nonParametricCumHaz_all <- function(weightVector, inputdata, grp, event_36_long=TRUE){
  outputHazards <- rep(NA, length.out=length(cut_times))
  counter <- 1 
  for(i in cut_times){
    if(event_36_long){
      indices <- inputdata$time_end==i & inputdata$drugsub == grp &  inputdata$eventCens==0 & inputdata$compet_36_long==0 
      eventIndicator <- indices & inputdata$event_36_long==1 
    }else{
      indices <- inputdata$time_end==i & inputdata$drugsub == grp & inputdata$eventCens==0
      eventIndicator <- indices & inputdata$compet_36_long==1 
    }
    outputHazards[counter] <- sum(weightVector[eventIndicator]) / sum(weightVector[indices])
    counter <- counter+1
  }
  return(outputHazards)
}

# utility functions for cumulative incidence (hazard)
nonParametricCumInc <- function(hazard1,hazard2,competing=FALSE){
  inc <- rep(NA, length.out=length(cut_times))
  cumulativeSurvival <- c(1, cumprod( (1-hazard1) * (1-hazard2) ))
  counter <- 1 
  for(i in 1:length(cut_times)){
    if(!competing){
      inc[i] <- hazard1[i] * (1-hazard2[i]) * cumulativeSurvival[i]
    }else{
      inc[i] <- hazard1[i] * cumulativeSurvival[i]
    }
  }
  cumInc <- cumsum(inc)
  return(cumInc)
}

