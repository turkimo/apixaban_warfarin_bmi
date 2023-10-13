# Results
cut_times = c(0:35)

# Composite ####

# Results 

# Total effects
func_results_eff(data=dat_composite, outcome="composite", outcome1="Composite", cut_times = c(0:35))

# Direct effects
func_results_direct_eff(data=dat_composite, outcome="composite", outcome1="Composite", cut_times = c(0:35))


# BMI <= 3 years 
func_results_eff_bmi_sen(data=dat_composite_bmi_sen, outcome="composite_bmi_sen", outcome1="Composite_bmi_sen", cut_times = c(0:35))

# IPW 99% truncated total effects
func_results_eff_trunc(data=dat_composite, outcome="composite", outcome1="Composite", cut_times = c(0:35))

# Plot 
func_cuminc_eff(data=dat_composite, outcome="composite", outcome1="Composite", cut_times = c(0:35))

# Stroke ####

# Results 

# Total effects
func_results_eff(data=dat_stroke, outcome="stroke", outcome1="Stroke", cut_times = c(0:35))

func_results_eff_trunc(data=dat_stroke, outcome="stroke", outcome1="Stroke", cut_times = c(0:35))

# Plot 
func_cuminc_eff(data=dat_stroke, outcome="stroke", outcome1="Stroke", cut_times = c(0:35))

# Ischemic stroke ####

# Results 

# Total effects
func_results_eff(data=dat_isch_stroke, outcome="ish_stroke", outcome1="Ischemic Stroke", cut_times = c(0:35))

func_results_eff_trunc(data=dat_isch_stroke, outcome="ish_stroke", outcome1="Ischemic Stroke", cut_times = c(0:35))

# Plot 
func_cuminc_eff(data=dat_isch_stroke, outcome="ish_stroke", outcome1="Ischemic Stroke", cut_times = c(0:35))


#Hemorrhagic stroke ####

# Results 

# Total effects
func_results_eff(data=dat_hem_stroke, outcome="hem_stroke", outcome1="Hemorrhagic Stroke", cut_times = c(0:35))

func_results_eff_trunc(data=dat_hem_stroke, outcome="hem_stroke", outcome1="Hemorrhagic Stroke", cut_times = c(0:35))


# Plot 
func_cuminc_eff(data=dat_hem_stroke, outcome="hem_stroke", outcome1="Hemorrhagic Stroke", cut_times = c(0:35))

# Systemic embolism ####

# Results 

# Total effects
func_results_eff(data=dat_se, outcome="se", outcome1="Systemic embolism", cut_times = c(0:35))

func_results_eff_trunc(data=dat_se, outcome="se", outcome1="Systemic embolism", cut_times = c(0:35))

# Plot 
func_cuminc_eff(data=dat_se, outcome="se", outcome1="Systemic embolism", cut_times = c(0:35))


# Mortality 
# Results 

# Total effects
func_results_mort(data=dat_death, outcome="death", outcome1="Death", cut_times = c(0:35))

# Plot 
func_cuminc_mort(data=dat_death, outcome="death", outcome1="Death", cut_times = c(0:35))

# Bleeding ####

# Results 

# Total effects
func_results_safe(data=dat_bleed, outcome="bleed", outcome1="Major Bleeding", cut_times = c(0:35))

# Direct effects
func_results_direct_safe(data=dat_bleed, outcome="bleed", outcome1="Major Bleeding", cut_times = c(0:35))

# BMI <= 3 years 
func_results_safe_bmi_sen(data=dat_bleed_bmi_sen, outcome="bleed", outcome1="Major Bleeding", cut_times = c(0:35))

# IPW 99% truncated total effects
func_results_safe_trunc(data=dat_bleed, outcome="bleed", outcome1="Major Bleeding", cut_times = c(0:35))

# Plot 
func_cuminc_safe(data=dat_bleed, outcome="bleed", outcome1="Major Bleeding", cut_times = c(0:35))


