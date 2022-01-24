# Create eTable 4: EffectEstimates of alternative variability metrics
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 08/18/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Set Up Plotting Objects 
# 2: Create Extract Estimates Function
# 3: Extract Estimates

####********************
#### 0: Preparation #### 
####********************

# 0a Load packages
library(readr)
library(dplyr)
library(stringr)

####***********************
#### 1: Set Up Objects #### 
####***********************

# 1a Declare convert to precent function 
convert_to_percent <- function(x){
  100 * (x - 1)
}

# 1b Readin Percentile Table
Percentile.Table <- read_csv(paste0(tables.folder, "Supp_10_90_Percentiles_MultiMetric.csv"))

####*******************************************
#### 2: Create Extract Estimates Function  ####
####*******************************************

# 2a Name function 
extract_effect_estimates <- function(CaseType, TempVarMetric,  ERConstraint, SubPopVar, SubPop, Sensitivity){
  #CaseType <- "mi";   TempVarMetric <- "MeanAbsFDT_24hr";   ERConstraint <- "3dfevenknots";  
  #SubPopVar <- "fullpop"; SubPop <- "fullpop"; Sensitivity <- "Main"
  
  # Determine the percentile 
  TempVar.10th <- Percentile.Table$TempVar.10th[Percentile.Table$TempVarMetric == TempVarMetric]
  TempVar.90th <- Percentile.Table$TempVar.90th[Percentile.Table$TempVarMetric == TempVarMetric]
  
  ####*************************
  #### 3: Readin Estimates ####
  ####*************************
  
  # 3a Declare ModelName
  ModelName <- paste0("TempVar", "_",CaseType, "_", TempVarMetric, "_ER", ERConstraint,
                      "_",SubPopVar, "_", SubPop, "_", Sensitivity)
  
  # 3b Readin lag exposure estimates from main model
  # 3b.i Readin main model estimates
  pred.table <- read.csv(paste0(estimates.folder, ModelName, ".csv"))
  # 3b.ii Rename variables 
  pred.table <- pred.table %>% 
    mutate(fit.pc = convert_to_percent(fit.or), 
           lci.pc = convert_to_percent(lci.or),
           uci.pc = convert_to_percent(uci.or)) 
  
  ####**************************
  #### 4: Extract Estimates ####
  ####**************************
  
  # 4b Extract Beta's from the two models 
  # 4b.i determine values closest to percentile 
  pred.table <- pred.table %>% 
    mutate(Diff10 = abs(TempVar - TempVar.10th), 
           Diff90 = abs(TempVar - TempVar.90th))
  
  minDiff10 <- min(pred.table$Diff10)
  minDiff90 <- min(pred.table$Diff90)
  # 4b.ii Extract fit.pc 
  fit.or10 <- pred.table$fit.or[pred.table$Diff10 == minDiff10]
  fit.or90 <- pred.table$fit.or[pred.table$Diff90 == minDiff90]
  fit.pc10 <- pred.table$fit.pc[pred.table$Diff10 == minDiff10]
  fit.pc90 <- pred.table$fit.pc[pred.table$Diff90 == minDiff90]
  # 4b.ii Extract LCI
  lci.or10 <- pred.table$lci.or[pred.table$Diff10 == minDiff10]
  lci.or90 <- pred.table$lci.or[pred.table$Diff90 == minDiff90]
  lci.pc10 <- pred.table$lci.pc[pred.table$Diff10 == minDiff10]
  lci.pc90 <- pred.table$lci.pc[pred.table$Diff90 == minDiff90]
  # 4b.ii Extract LCI
  uci.or10 <- pred.table$uci.or[pred.table$Diff10 == minDiff10]
  uci.or90 <- pred.table$uci.or[pred.table$Diff90 == minDiff90]
  uci.pc10 <- pred.table$uci.pc[pred.table$Diff10 == minDiff10]
  uci.pc90 <- pred.table$uci.pc[pred.table$Diff90 == minDiff90]
  
  # 4c Output all estimates
  Output <- c(TempVarMetric, 
              fit.or10, lci.or10, uci.or10,
              fit.or90, lci.or90, uci.or90, 
              fit.pc10, lci.pc10, uci.pc10,
              fit.pc90, lci.pc90, uci.pc90, 
              Sensitivity)
}


####***********************
#### 5: Create eTable  ####
####***********************

# 5a Create table 
Estimate.Table <- data.frame(
  TempVarMetric = NA, 
  fit.or10 = NA, 
  lci.or10 = NA, 
  uci.or10 = NA, 
  fit.or90 = NA, 
  lci.or90 = NA, 
  uci.or90 = NA, 
  
  fit.pc10 = NA, 
  lci.pc10 = NA, 
  uci.pc10 = NA, 
  fit.pc90 = NA, 
  lci.pc90 = NA, 
  uci.pc90 = NA, 
  Sensitivity = NA)

# 5b Calculate indicators
Estimate.Table[1, ] <- extract_effect_estimates("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "Main")
Estimate.Table[2, ] <- extract_effect_estimates("mi", "MaxAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "Main")
Estimate.Table[3, ] <- extract_effect_estimates("mi", "SDT_24hr", "3dfevenknots", "fullpop", "fullpop", "Main")
Estimate.Table[4, ] <- extract_effect_estimates("mi", "DRT_24hr", "3dfevenknots", "fullpop", "fullpop", "Main")
Estimate.Table[5, ] <- extract_effect_estimates("mi", "SDMinMaxT_48hr", "4dfevenknots", "fullpop", "fullpop", "Main")
Estimate.Table[6, ] <- extract_effect_estimates("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "Main")
Estimate.Table[7, ] <- extract_effect_estimates("mi", "DayDiffT_48hr", "4dfevenknots", "fullpop", "fullpop", "Main")

# 5c Rearrange data so that we have a column for 10th percentile and one for 90th
# replicates the structure of the table in the supplemental
Estimate.Table <- Estimate.Table %>% 
  mutate(fit.pc10 = as.numeric(fit.pc10), 
         lci.pc10 = as.numeric(lci.pc10), uci.pc10 = as.numeric(uci.pc10), 
         fit.pc90 = as.numeric(fit.pc90), 
         lci.pc90 = as.numeric(lci.pc90), uci.pc90 = as.numeric(uci.pc90)) %>%
  mutate(EE10 = paste0(format(round(fit.pc10, 2), nsmall = 2), " (", 
                       format(round(lci.pc10, 2), nsmall = 2), ", ", 
                       format(round(uci.pc10, 2), nsmall = 2), ")"), 
         EE90 = paste0(format(round(fit.pc90, 2), nsmall = 2), " (", 
                       format(round(lci.pc90, 2), nsmall = 2), ", ", 
                       format(round(uci.pc90, 2), nsmall = 2), ")"))


# 5d Save results 
Estimate.Table %>% 
  write_csv(paste0(tables.folder, "EE_AlternativeMetrics.csv"))
