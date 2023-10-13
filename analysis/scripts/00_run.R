# Run script for project

# Stop scientific notation
options(scipen=999)

# Columns number 
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

# PACKAGES ------------------------------------------------------------------
library(here)
library(tidyverse)
library(lubridate)
library(magrittr)
library(naniar)
library(splitstackshape)
library(parallel)
# use multicore functions
library(foreach)
library(doParallel)
library(doSNOW)
library(dplyr)
library(gtsummary)
library(tableone)
library(survey)
library(kableExtra)
library(huxtable)
library(survival)
library(ggthemes)

# PRELIMINARIES -------------------------------------------------------------
# Control which scripts run
run_01_cw_dataprep    <- 1
run_02_cw_eligibility <- 1
run_03_cw_outcomes    <- 1
run_04_cw_data        <- 1
run_05_func_ipw_eff   <- 1
run_06_func_ipw_mort   <- 1
run_07_func_ipw_safe   <- 1
run_08_func_ipcw_eff   <- 1
run_09_func_ipcw_mort   <- 1
run_10_func_ipcw_safe   <- 1
run_11_func_nonparm_cumhazard   <- 1
run_12_func_cuminc_plot_eff   <- 1
run_13_func_cuminc_plot_safe   <- 1
run_14_func_cuminc_plot_mort  <- 1
run_15_func_total_effect_eff   <- 1
run_16_func_total_effect_safe   <- 1
run_17_func_total_effect_mort   <- 1
run_18_func_direct_effect_eff <- 1 
run_19_func_direct_effect_safe <- 1 
run_20_func_total_effect_eff_bmi_sen   <- 1
run_21_func_total_effect_safe_bmi_sen   <- 1
run_22_func_total_effect_eff_trunc <- 1 
run_23_func_total_effect_safe_trunc <- 1 



# RUN SCRIPTS ---------------------------------------------------------------
outcomes <- c("composite", "death", "stroke", "istroke", "hstroke", "se")

# Read and clean treatments data 
if (run_01_cw_dataprep) source(here("analysis/scripts", "01_cw_dataprep.R"), encoding = "UTF-8")

# Filter data by eligibility criteria 
if (run_02_cw_eligibility) source(here("analysis/scripts", "02_cw_eligibility.R"), encoding = "UTF-8")

# Read and clean outcomes data
if (run_03_cw_outcomes) source(here("analysis/scripts", "03_cw_outcomes.R"), encoding = "UTF-8")

# Read and clean data 
if (run_04_cw_data) source(here("analysis/scripts", "04_cw_data.R"), encoding = "UTF-8")

# Function for inverse probability treatment weighting of effectiveness outcomes 
if (run_05_func_ipw_eff) source(here("analysis/scripts", "05_func_ipw_eff.R"), encoding = "UTF-8")

# Function for inverse probability treatment weighting of mortality outcome
if (run_06_func_ipw_mort) source(here("analysis/scripts", "06_func_ipw_mort.R"), encoding = "UTF-8")

# Function for inverse probability treatment weighting of safety outcome
if (run_07_func_ipw_safe) source(here("analysis/scripts", "07_func_ipw_safe.R"), encoding = "UTF-8")

# Function for inverse probability censoring weighting of effectiveness outcome
if (run_08_func_ipcw_eff) source(here("analysis/scripts", "08_func_ipcw_eff.R"), encoding = "UTF-8")

# Function for inverse probability censoring weighting of mortality outcome
if (run_09_func_ipcw_mort) source(here("analysis/scripts", "09_func_ipcw_mort.R"), encoding = "UTF-8")

# Function for inverse probability censoring weighting of safety outcome
if (run_10_func_ipcw_safe) source(here("analysis/scripts", "10_func_ipcw_safe.R"), encoding = "UTF-8")

# Function for non-parametric cumulative hazard 
if (run_11_func_nonparm_cumhazard) source(here("analysis/scripts", "11_func_nonparm_cumhazard.R"), encoding = "UTF-8")

# Function for cumulative incidence plot for effectiveness outcomes
if (run_12_func_cuminc_plot_eff) source(here("analysis/scripts", "12_func_cuminc_plot_eff.R"), encoding = "UTF-8")

# Function for cumulative incidence plot for safety outcome
if (run_13_func_cuminc_plot_safe) source(here("analysis/scripts", "13_func_cuminc_plot_safe.R"), encoding = "UTF-8")

# Function for cumulative incidence plot for mortality outcome
if (run_14_func_cuminc_plot_mort) source(here("analysis/scripts", "14_func_cuminc_plot_mort.R"), encoding = "UTF-8")

# Function for estimating the total effect for effectiveness outcomes
if (run_15_func_total_effect_eff) source(here("analysis/scripts", "15_func_total_effect_eff.R"), encoding = "UTF-8")

# Function for estimating the total effect for safety outcome
if (run_16_func_total_effect_safe) source(here("analysis/scripts", "16_func_total_effect_safe.R"), encoding = "UTF-8")

# Function for estimating the total effect for mortality outcome
if (run_17_func_total_effect_mort) source(here("analysis/scripts", "17_func_total_effect_mort.R"), encoding = "UTF-8")

# Function for estimating the direct effect for effectiveness outcomes
if (run_18_func_direct_effect_eff) source(here("analysis/scripts", "18_func_direct_effect_eff.R"), encoding = "UTF-8")

# Function for estimating the direct effect for safety outcome
if (run_19_func_direct_effect_safe) source(here("analysis/scripts", "19_func_direct_effect_safe.R"), encoding = "UTF-8")

# Function for estimating the total effect for effectiveness outcome restricted to BMI measured =< 3 years
if (run_20_func_total_effect_eff_bmi_sen) source(here("analysis/scripts", "20_func_total_effect_eff_bmi_sen.R"), encoding = "UTF-8")

# Function for estimating the total effect for safety outcome restricted to BMI measured =< 3 years
if (run_21_func_total_effect_safe_bmi_sen) source(here("analysis/scripts", "21_func_total_effect_safe_bmi_sen.R"), encoding = "UTF-8")

# Function for estimating the total effect for effectiveness outcome using 99% truncated inverse probability weights 
if (run_22_func_total_effect_eff_trunc) source(here("analysis/scripts", "22_func_total_effect_eff_trunc.R"), encoding = "UTF-8")

# Function for estimating the total effect for safety outcome using 99% truncated inverse probability weights 
if (run_23_func_total_effect_safe_trunc) source(here("analysis/scripts", "23_func_total_effect_safe_trunc.R"), encoding = "UTF-8")



