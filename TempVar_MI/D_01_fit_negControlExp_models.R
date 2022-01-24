# Fit Negative Control Models
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/29/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Load Analysis Functions
# 2: Fit Negative Control Exposure Models
# 3: Create Indicator Calculator Function
# 3A: Readin Percentile Table
# 3B: Wrangle Effect Estimates
# 3C: Calculate Indicators
# 4: Calculate Indicators

####********************
#### 0: Preparation #### 
####********************

# 0a Declare directories
if (!exists('Ran_0_00')){
  here::i_am('README.md')
  source(here::here('TempVar_mi', 'Scripts',
                    '0_00_create_folder_structure.R'))
}

####*********************************
#### 1: Load Analysis Functions  ####
####*********************************

# 0a Load functions 
source(paste0(scripts.folder, "D_00_analyze_negativeControlExp_tempVar_function.R"))

####*********************************************
#### 2: Fit Negative Control Exposure Models #### 
####*********************************************

# 2a MeanAbsFDT
analyze_TempVar_NegContExp("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "NegContExp")

# 2b MeanFDT_24hr
analyze_TempVar_NegContExp("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "NegContExp")

####*********************************************
#### 3: Create Indicator Calculator Function ####
####*********************************************

# 3a Name function 
calculate_nce_indicators <- function(CaseType, TempVarMetric,  ERConstraint, SubPopVar, SubPop, Sensitivity){
  #CaseType <- "mi";   TempVarMetric <- "MeanAbsFDT_24hr";   ERConstraint <- "3dfevenknots";  
  #SubPopVar <- "fullpop"; SubPop <- "fullpop"; Sensitivity <- "NegContExp"
  
  ####*********************************
  #### 3A: Readin Percentile Table ####
  ####*********************************
  
  # 3A.a Readin Percentile Table
  Percentile.Table <- read_csv(paste0(tables.folder, "Supp_10_90_Percentiles_MultiMetric.csv"))
  
  # 3A.b Determine the percentiles 
  TempVar.10th <- Percentile.Table$TempVar.10th[Percentile.Table$TempVarMetric == TempVarMetric]
  TempVar.90th <- Percentile.Table$TempVar.90th[Percentile.Table$TempVarMetric == TempVarMetric]
  
  ####**********************************
  #### 3B: Wrangle Effect Estimates ####
  ####**********************************
  
  # 3B.a Declare ModelName
  LeadModelName <- paste0("TempVar", "_",CaseType, "_", TempVarMetric, "_ER", ERConstraint,
                          "_",SubPopVar, "_", SubPop, "_", Sensitivity)
  
  # 3B.b Readin lead exposure estimates
  # B.b.i Readin model predictions 
  pred.table.Lead <- read.csv(paste0(estimates.folder, LeadModelName, "_TempVarLead.csv"))
  # 3B.b.ii Convert back to coefficient units 
  pred.table.Lead <- pred.table.Lead %>%
    mutate(fit.beta = log(fit.or), lci.beta = log(lci.or), uci.beta = log(uci.or)) %>% 
    dplyr::select(-fit.or, -lci.or, -uci.or) 
  
  # 3B.c Extract effect estimates for mean to 90th percentile 
  # 3B.c.i determine values closest to percentile 
  pred.table.Lead <- pred.table.Lead %>% 
    mutate(Diff90 = abs(TempVar - TempVar.90th))
  minDiff90 <- min(pred.table.Lead$Diff90)
  # 3B.c.ii Extract estimates
  fit.beta90 <- pred.table.Lead$fit.beta[pred.table.Lead$Diff90 == minDiff90]
  lci.beta90 <- pred.table.Lead$lci.beta[pred.table.Lead$Diff90 == minDiff90]
  uci.beta90 <- pred.table.Lead$uci.beta[pred.table.Lead$Diff90 == minDiff90]
  
  ####******************************
  #### 3C: Calculate Indicators ####
  ####******************************
  
  # 3C.a Calculate Indicator 1 for mean to 90th percentile
  # 3C.a.i determine values closest to percentile 
  pred.table.Lead <- pred.table.Lead %>% 
    mutate(Diff90 = abs(TempVar - TempVar.90th))
  minDiff90 <- min(pred.table.Lead$Diff90)
  # 3C.a.ii Extract estimates
  fit.beta90.Lead <- pred.table.Lead$fit.beta[pred.table.Lead$Diff90 == minDiff90]
  lci.beta90.Lead <- pred.table.Lead$lci.beta[pred.table.Lead$Diff90 == minDiff90]
  uci.beta90.Lead <- pred.table.Lead$uci.beta[pred.table.Lead$Diff90 == minDiff90]
  # 3C.a.iii Back-calculate se 
  se.Lead <- (1/1.96) * (uci.beta90.Lead - lci.beta90.Lead)
  # 3C.a.iv Calculate indicator 
  # I* = B*/sigma* 
  # This is "an approximate z-score" 
  Indicator.90 <- fit.beta90.Lead/se.Lead
  # 3C.a.v Calculate p-value for indicator 
  p.Indicator.90 <- 2 * (1-pnorm(abs(Indicator.90)))
  # 3C.a.vi Put in nice format 
  p.Indicator.90 <- round(p.Indicator.90, 3)
  
  # 3C.b Output indicator
  Output <- c(TempVarMetric, p.Indicator.90)
}

####*****************************
#### 4: Calculate Indicators ####
####*****************************

# 4a Create table 
Indicator.Table <- data.frame(
  TempVarMetric = NA, 
  Indicator = NA)

# 4b Calculate indicators
Indicator.Table[1, ] <- calculate_nce_indicators("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "NegContExp")
Indicator.Table[2, ] <- calculate_nce_indicators("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "NegContExp")

# 4c Save results 
Indicator.Table %>% write_csv(paste0(tables.folder, "NegControlExposure_Indicator_Table.csv"))