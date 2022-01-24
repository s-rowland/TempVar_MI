# Fit the alternative metric TempVar - MI Health Models
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 06/08/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Assess Nonlinearity 
# 2: Choose df for Natural Splines

####********************
#### 0: Preparation #### 
####********************

# 0a Declare directories
if (!exists('Ran_0_00')){
  here::i_am('README.md')
  source(here::here('TempVar_mi', 'Scripts',
                    '0_00_create_folder_structure.R'))
}

# 0b Load figure sizes, etc
source(paste0(scripts.folder, "G_00_set_figure_size.R"))

####*****************************
#### 1: Assess Nonlinearity  ####
####*****************************

# 1a Fit penalized models 
analyze_TempVar("mi", "MaxAbsFDT_24hr", "psp", "fullpop", "fullpop", "CheckLinearity")
analyze_TempVar("mi", "SDT_24hr", "psp", "fullpop", "fullpop", "CheckLinearity")
analyze_TempVar("mi", "DRT_24hr", "psp", "fullpop", "fullpop", "CheckLinearity")
analyze_TempVar("mi", "SDMinMaxT_48hr", "psp", "fullpop", "fullpop", "CheckLinearity")
analyze_TempVar("mi", "DayDiffT_48hr", "psp", "fullpop", "fullpop", "CheckLinearity")

# 1b Create function to extract edf
get_edf <- function(TempVarMetric, SubPop, CaseType, ERConstraint, SubPopVar, Sensitivity){
  ModelName <- paste0("TempVar", "_",CaseType, "_", TempVarMetric, "_ER", ERConstraint,
                      "_",SubPopVar, "_", SubPop, "_", Sensitivity)
  mod <- readRDS(paste0(models.folder, ModelName, ".RDS"))
  edf <- summary(mod)$coefficients[2,5]
  c(TempVarMetric, SubPop, edf)
}

# 1c Create edf table 
edf_table <- data.frame(
  TempVarMetric = NA, 
  Season = NA, 
  edf = NA
)

# 1d Fill Table 
edf_table[1, ] <- get_edf("MaxAbsFDT_24hr", "fullpop", "mi","psp", "fullpop", "CheckLinearity")
edf_table[2, ] <- get_edf("SDT_24hr", "fullpop", "mi","psp", "fullpop", "CheckLinearity")
edf_table[3, ] <- get_edf("DRT_24hr", "fullpop", "mi","psp", "fullpop", "CheckLinearity")
edf_table[4, ] <- get_edf("SDMinMaxT_48hr", "fullpop", "mi","psp", "fullpop", "CheckLinearity")
edf_table[5, ] <- get_edf("DayDiffT_48hr", "fullpop", "mi","psp", "fullpop", "CheckLinearity")

# 1d Save table 
edf_table %>% write_csv(paste0(tables.folder, "EDF_AlternativeMetric_Models.csv"))

####**************************************
#### 2: Choose df for Natural Splines ####
####**************************************

# 2a 3 df 
analyze_TempVar("mi", "MaxAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "Main")
analyze_TempVar("mi", "SDT_24hr", "3dfevenknots", "fullpop", "fullpop", "Main")
analyze_TempVar("mi", "DRT_24hr", "3dfevenknots", "fullpop", "fullpop", "Main")
analyze_TempVar("mi", "SDMinMaxT_48hr", "3dfevenknots", "fullpop", "fullpop", "Main")
analyze_TempVar("mi", "DayDiffT_48hr", "3dfevenknots", "fullpop", "fullpop", "Main")

# 2b 4 df 
analyze_TempVar("mi", "MaxAbsFDT_24hr", "4dfevenknots", "fullpop", "fullpop", "Main")
analyze_TempVar("mi", "SDT_24hr", "4dfevenknots", "fullpop", "fullpop", "Main")
analyze_TempVar("mi", "DRT_24hr", "4dfevenknots", "fullpop", "fullpop", "Main")
analyze_TempVar("mi", "SDMinMaxT_48hr", "4dfevenknots", "fullpop", "fullpop", "Main")
analyze_TempVar("mi", "DayDiffT_48hr", "4dfevenknots", "fullpop", "fullpop", "Main")

# 2c 5 df 
analyze_TempVar("mi", "MaxAbsFDT_24hr", "5dfevenknots", "fullpop", "fullpop", "Main")
analyze_TempVar("mi", "SDT_24hr", "5dfevenknots", "fullpop", "fullpop", "Main")
analyze_TempVar("mi", "DRT_24hr", "5dfevenknots", "fullpop", "fullpop", "Main")
analyze_TempVar("mi", "SDMinMaxT_48hr", "5dfevenknots", "fullpop", "fullpop", "Main")
analyze_TempVar("mi", "DayDiffT_48hr", "5dfevenknots", "fullpop", "fullpop", "Main")

# 2d Create function to extract AIC
get_aic <- function(TempVarMetric, SubPop, CaseType, ERConstraint, SubPopVar, Sensitivity){
  ModelName <- paste0("TempVar", "_",CaseType, "_", TempVarMetric, "_ER", ERConstraint,
                      "_",SubPopVar, "_", SubPop, "_", Sensitivity)
  mod <- readRDS(paste0(models.folder, ModelName, ".RDS"))
  AIC <- AIC(mod)
  c(TempVarMetric, SubPop, ERConstraint, AIC)
}

# 2e Create aic table 
aic_table <- data.frame(
  TempVarMetric = NA, 
  Season = NA, 
  ERConstraint = NA,
  AIC = NA
)

# 2f Fill Table 
aic_table[1, ] <- get_aic("MaxAbsFDT_24hr", "fullpop", "mi","3dfevenknots", "fullpop", "Main")
aic_table[2, ] <- get_aic("MaxAbsFDT_24hr", "fullpop", "mi","4dfevenknots", "fullpop", "Main")
aic_table[3, ] <- get_aic("MaxAbsFDT_24hr", "fullpop", "mi","5dfevenknots", "fullpop", "Main")
aic_table[4, ] <- get_aic("SDT_24hr", "fullpop", "mi","3dfevenknots", "fullpop", "Main")
aic_table[5, ] <- get_aic("SDT_24hr", "fullpop", "mi","4dfevenknots", "fullpop", "Main")
aic_table[6, ] <- get_aic("SDT_24hr", "fullpop", "mi","5dfevenknots", "fullpop", "Main")
aic_table[7, ] <- get_aic("DRT_24hr", "fullpop", "mi","3dfevenknots", "fullpop", "Main")
aic_table[8, ] <- get_aic("DRT_24hr", "fullpop", "mi","4dfevenknots", "fullpop", "Main")
aic_table[9, ] <- get_aic("DRT_24hr", "fullpop", "mi","5dfevenknots", "fullpop", "Main")
aic_table[10, ] <- get_aic("SDMinMaxT_48hr", "fullpop", "mi","3dfevenknots", "fullpop", "Main")
aic_table[11, ] <- get_aic("SDMinMaxT_48hr", "fullpop", "mi","4dfevenknots", "fullpop", "Main")
aic_table[12, ] <- get_aic("SDMinMaxT_48hr", "fullpop", "mi","5dfevenknots", "fullpop", "Main")
aic_table[13, ] <- get_aic("DayDiffT_48hr", "fullpop", "mi","3dfevenknots", "fullpop", "Main")
aic_table[14, ] <- get_aic("DayDiffT_48hr", "fullpop", "mi","4dfevenknots", "fullpop", "Main")
aic_table[15, ] <- get_aic("DayDiffT_48hr", "fullpop", "mi","5dfevenknots", "fullpop", "Main")

# 2g Find minAIC 
aic_table <- aic_table %>%
  group_by(TempVarMetric, Season) %>% 
  summarize(minAIC = min(AIC)) %>%
  left_join(aic_table , by = c("TempVarMetric", "Season"))

# 2h Calculate deltaAIC 
aic_table <- aic_table  %>% 
  mutate(deltaAIC = as.numeric(AIC) - as.numeric(minAIC))

# 2i Calculate weight denominator
aic_table <- aic_table %>% 
  group_by(TempVarMetric) %>% 
  summarize(denom = sum(exp(-0.5 * deltaAIC))) %>%
  left_join(aic_table, by = "TempVarMetric")

# 2j calculate AIC weights 
aic_table$AkaikeWeight <- exp(-0.5*aic_table$deltaAIC) / aic_table$denom

# 2k Put values in pretty format
aic_table$AkaikeWeight <- round(100*aic_table$AkaikeWeight, 1)

# 2l Save table 
aic_table %>% 
  write_csv(paste0(tables.folder, "AIC_AlternativeMetric_Models.csv"))
