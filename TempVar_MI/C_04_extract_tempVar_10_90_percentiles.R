# Extract the 10th and 90th Percentiles of Variability Metrics
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/14/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Create Metric Percentile Function
# 2: Calculate Percentiles for Metrics

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

####*******************************************
#### 1: Create Metric Percentile Function  ####
####*******************************************

# 1a Name function 
extract_percentiles <- function(TempVarMetric){
  #TempVarMetric <- "SDT_24hr"
  
  # 1b Readin Data
  dta <- fst::read.fst(paste0(final.data.folder, "prepared_cohort", "_", "mi", 
                              "_identified", "_", "Pop", "_", "STEMI2hrDelay", "_TVOnly.fst"))
  
  # 1c Keep only primary MI admissions according to outcome definition
  dta <- dta %>% filter(MI_prim == 1)
  
  # 1d Rename variable
  dta <- dta %>% rename(TempVar:= !!TempVarMetric) 
  
  # 1e Determine various percentilea 
  Percentiles <- data.frame(
             TempVarMetric = TempVarMetric, 
             TempVar.mean = mean(dta$TempVar, na.rm = TRUE),
             TempVar.sd = sd(dta$TempVar, na.rm = TRUE),
             TempVar.1th = quantile(dta$TempVar, 0.01, type = 1, na.rm = TRUE), 
             TempVar.25th = quantile(dta$TempVar, 0.025, type = 1, na.rm = TRUE), 
             TempVar.5th = quantile(dta$TempVar, 0.05, type = 1, na.rm = TRUE),
             TempVar.10th = quantile(dta$TempVar, 0.1, type = 1, na.rm = TRUE), 
             TempVar.15th = quantile(dta$TempVar, 0.15, type = 1, na.rm = TRUE),  
             TempVar.20th = quantile(dta$TempVar, 0.2, type = 1, na.rm = TRUE),  
             TempVar.25th = quantile(dta$TempVar, 0.25, type = 1, na.rm = TRUE),
             
             TempVar.75th = quantile(dta$TempVar, 0.75, type = 1, na.rm = TRUE), 
             TempVar.80th = quantile(dta$TempVar, 0.8, type = 1, na.rm = TRUE), 
             TempVar.85th = quantile(dta$TempVar, 0.85, type = 1, na.rm = TRUE), 
             TempVar.90th = quantile(dta$TempVar, 0.9, type = 1, na.rm = TRUE), 
             TempVar.95th = quantile(dta$TempVar, 0.95, type = 1, na.rm = TRUE), 
             TempVar.975th = quantile(dta$TempVar, 0.975, type = 1, na.rm = TRUE), 
             TempVar.99th = quantile(dta$TempVar, 0.99, type = 1, na.rm = TRUE)) %>% 
    mutate(TempVar.MeanMinusSD = TempVar.mean - TempVar.sd, 
           TempVar.MeanPlusSD = TempVar.mean + TempVar.sd)
  
  # 1f Return output 
  Percentiles
}

####******************************************
#### 2: Calculate Percentiles for Metrics ####
####******************************************

# 2a Calculate Percentiles
# we already have the list of metrics from G_00 
Percentile.List <- map(NamesTVListExpW, extract_percentiles)

# 2b Combine into a single dataframe 
Percentile.Table <- reduce(Percentile.List, rbind)

# 2c Save results 
Percentile.Table %>% write_csv(paste0(tables.folder, "Supp_10_90_Percentiles_MultiMetric.csv"))
