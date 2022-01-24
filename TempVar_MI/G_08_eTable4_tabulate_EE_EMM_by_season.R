# Create eTable 2 
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 06/04/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Set Up Objects 
# 2: Create Function to Extract EE and CI
  # 2A: Wrangle Estimates
  # 2B: Extract Estimates
# 3: Create eTable 
# 4: Statistically Compare Effect Estimates

####********************
#### 0: Preparation #### 
####********************

# 0a Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(dlnm)
library(stringr)
library(egg)
library(tidyr)

####***********************
#### 1: Set Up Objects #### 
####***********************

# 1a Declare convert to precent function 
convert_to_percent <- function(x){
  100 * (x - 1)
}

# 1b Readin percentile table
Percentile.Table <- read_csv(paste0(tables.folder, "Supp_10_90_Percentiles_MultiMetric.csv"))

####*********************************************
#### 2: Create Function to Extract EE and CI #### 
####*********************************************

# 2a Name function 
extract_effect_estimates <- function(CaseType, TempVarMetric,  ERConstraint, SubPopVar, SubPop, Sensitivity){
  #CaseType <- "mi";   TempVarMetric <- "MeanAbsFDT_24hr";   ERConstraint <- "3dfevenknots";  
  #SubPopVar <- "fullpop"; SubPop <- "fullpop"; Sensitivity <- "Main"
  
  ####***************************
  #### 2A: Wrangle Estimates ####
  ####***************************
  
  # 2A.a Declare ModelName
  ModelName <- paste0("TempVar", "_",CaseType, "_", TempVarMetric, "_ER", ERConstraint,
                      "_",SubPopVar, "_", SubPop, "_", Sensitivity)
  
  # 2A.b Readin lag exposure estimates from main model
  # 2A.b.i Readin main model estimates
  pred.table <- read.csv(paste0(estimates.folder, ModelName, ".csv"))
  # 2A.b.ii Keep only the exposure contrasts of interest
  pred.table10 <- pred.table %>% 
    filter(Labels == "per10") %>% 
    mutate(fit.pc10 = convert_to_percent(fit.or), 
           lci.pc10 = convert_to_percent(lci.or),
           uci.pc10 = convert_to_percent(uci.or)) %>%
    rename(fit.or10 = fit.or, lci.or10 = lci.or, uci.or10 = uci.or) %>%
    mutate(Season = SubPop) %>%
    dplyr::select(TVMetric, Season, fit.or10, lci.or10, uci.or10, fit.pc10, lci.pc10, uci.pc10)
 pred.table90 <- pred.table %>% 
  filter(Labels == "per90") %>% 
  mutate(fit.pc90 = convert_to_percent(fit.or), 
         lci.pc90 = convert_to_percent(lci.or),
         uci.pc90 = convert_to_percent(uci.or)) %>%
   rename(fit.or90 = fit.or, lci.or90 = lci.or, uci.or90 = uci.or) %>%
  dplyr::select(fit.or90, lci.or90, uci.or90, fit.pc90, lci.pc90, uci.pc90)
  
  # 2B.c Output all estimates
  Output <- bind_cols(pred.table10, pred.table90)
}


####**********************
#### 3: Create eTable ####
####**********************

# 3a Create table 
est.table <- data.frame(
  TempVarMetric = NA, 
  Season = NA,
  fit.or10 = NA, lci.or10 = NA, uci.or10 = NA,
  fit.pc10 = NA, lci.pc10 = NA, uci.pc10 = NA, 
  fit.or90 = NA, lci.or90 = NA, uci.or90 = NA, 
  fit.pc90 = NA, lci.pc90 = NA, uci.pc90 = NA)

# 3b Calculate indicators
est.table[1, ] <- extract_effect_estimates("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "Main")
est.table[2, ] <- extract_effect_estimates("mi", "MeanAbsFDT_24hr", "3dfevenknots", "Season4mo", "Hottest4", "Main")
est.table[3, ] <- extract_effect_estimates("mi", "MeanAbsFDT_24hr", "3dfevenknots", "Season4mo", "Coldest4", "Main")
est.table[4, ] <- extract_effect_estimates("mi", "MeanAbsFDT_24hr", "4dfevenknots", "Season4mo", "Moderate4", "Main")
est.table[5, ] <- extract_effect_estimates("mi", "MeanFDT_24hr", "4dfevenknots", "fullpop", "fullpop", "Main")
est.table[6, ] <- extract_effect_estimates("mi", "MeanFDT_24hr", "3dfevenknots", "Season4mo", "Hottest4", "Main")
est.table[7, ] <- extract_effect_estimates("mi", "MeanFDT_24hr", "3dfevenknots", "Season4mo", "Coldest4", "Main")
est.table[8, ] <- extract_effect_estimates("mi", "MeanFDT_24hr", "3dfevenknots", "Season4mo", "Moderate4", "Main")

# 3c Rearrange data so that we have a column for 10th percentile and one for 90th
# replicates the structure of the table in the supplemental
est.table <- est.table %>% 
  mutate(fit.pc10 = as.numeric(fit.pc10), 
         lci.pc10 = as.numeric(lci.pc10), uci.pc10 = as.numeric(uci.pc10), 
         fit.pc90 = as.numeric(fit.pc90), 
         lci.pc90 = as.numeric(lci.pc90), uci.pc90 = as.numeric(uci.pc90), 
         fit.or10 = as.numeric(fit.or10), 
         lci.or10 = as.numeric(lci.or10), uci.or10 = as.numeric(uci.or10), 
         fit.or90 = as.numeric(fit.or90), 
         lci.or90 = as.numeric(lci.or90), uci.or90 = as.numeric(uci.or90)) %>%
  mutate(EE10 = paste0(format(round(fit.pc10, 2), nsmall = 2), " (", 
                       format(round(lci.pc10, 2), nsmall = 2), ", ", 
                       format(round(uci.pc10, 2), nsmall = 2), ")"), 
         EE90 = paste0(format(round(fit.pc90, 2), nsmall = 2), " (", 
                       format(round(lci.pc90, 2), nsmall = 2), ", ", 
                       format(round(uci.pc90, 2), nsmall = 2), ")"))
 
# 3e Save table 
est.table %>% 
  dplyr::select(TempVarMetric, Season, EE10, EE90) %>%
  write_csv(paste0(manuscript.folder, "eTable4_EE_SeasonalStratified.csv"))

####***********************************************
#### 4: Statistically Compare Effect Estimates #### 
####***********************************************

# 4a Wrangle columns 
# we take the logs to return to the coefficient scale
est.table10 <- est.table %>% 
  mutate(Percentile = 10) %>% 
  mutate(fit.beta = log(fit.or10), lci.beta = log(lci.or10), uci.beta = log(uci.or10)) %>% 
  dplyr::select(TempVarMetric, Season, Percentile, fit.beta, lci.beta, uci.beta) 
est.table90 <- est.table %>% 
  mutate(Percentile = 90) %>% 
  mutate(fit.beta = log(fit.or90), lci.beta = log(lci.or90), uci.beta = log(uci.or90)) %>% 
  dplyr::select(TempVarMetric, Season, Percentile, fit.beta, lci.beta, uci.beta)   
ee_table <- bind_rows(est.table10, est.table90)

# 4b Define Comparison Function  
compare_stratified_effect_estimates <- function(TempVarMetric0, Percentile0, SubGroup1, SubGroup2){
  # TempVarMetric0 <- "MeanAbsFDT_24hr"; Percentile0 <- 10
  # SubGroup1 <- "Hottest4"; SubGroup2 <- "Coldest4"
  # 4b.i Keep the right percentile and tempvarmetric
  ee_table_per <- ee_table %>% 
    filter(TempVarMetric == TempVarMetric0, Percentile == Percentile0)
  # 4b.ii Identify corresponding ORs
  beta1 <- ee_table_per$fit.beta[ee_table_per$Season == SubGroup1]
  beta2 <- ee_table_per$fit.beta[ee_table_per$Season == SubGroup2]
  # 4b.iii Calculate difference of the logRR 
  diff.beta <- beta1 - beta2 
  # 4b.iv Get the se
  lci.beta1 <- ee_table_per$lci.beta[ee_table_per$Season == SubGroup1]
  uci.beta1 <- ee_table_per$uci.beta[ee_table_per$Season == SubGroup1]
  se1 <- (uci.beta1 - lci.beta1 ) / (2 * 1.96)
  lci.beta2 <- ee_table_per$lci.beta[ee_table_per$Season == SubGroup2]
  uci.beta2 <- ee_table_per$uci.beta[ee_table_per$Season == SubGroup2]
  se2 <- (uci.beta2 - lci.beta2 ) / (2 * 1.96)
  # 4b.v Calculate the joint se 
  se.diff <- sqrt(se1^2 + se2^2)
  # 4b.vi Calculate the z score 
  z.score <- diff.beta / se.diff
  # 4b.vii Calculate P value 
  p.value <- 2 * (1 - pnorm(abs(z.score)))
  # 4b.viii Return value 
  round(p.value, 4)
}

# 4c Create table 
emm_table <- data.frame(
  TempVarMetric = c(rep("MeanAbsFDT_24hr", 6), rep("MeanFDT_24hr", 6)),
    Percentile = c(rep(c(10, 10, 10, 90, 90, 90), 2)),
    Season1 = c(rep(c("Hottest4", "Hottest4", "Coldest4"), 4)),
    Season2 = c(rep(c("Coldest4", "Moderate4", "Moderate4"), 4)),
    PValue = rep(NA, 12)
)

# 4d  Fill table 
for (i in 1: nrow(emm_table)){
  emm_table[i, 5] <- compare_stratified_effect_estimates(emm_table$TempVarMetric[i], 
                                                        emm_table$Percentile[i],
                                                        emm_table$Season1[i],
                                                        emm_table$Season2[i]) 
}

# 4e Save table
emm_table %>% write_csv(paste0(manuscript.folder, "XTable1_PVal_SeasonalStratified.csv"))
