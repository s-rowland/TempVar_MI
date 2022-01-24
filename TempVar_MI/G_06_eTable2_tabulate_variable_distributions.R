# Create TableOne 
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 11/24/2020

####***********************
#### Table of Contents #### 
####***********************

# D: Description
# 0: Preparation 
# 1: Readin and Organize Case Data
# 2: Create Table One

####********************
#### D: Description ####
####********************

# This script applies the same data processing steps as the 
# analysis function to ensure TableOne reflects the study population

####********************
#### 0: Preparation ####
####********************

# 0a Load Packages
library(readr)
library(dplyr)
library(lubridate)
library(magrittr)
library(tidyr)
library(stringr)
library(purrr)
library(furrr)

####**************************************
#### 1: Readin and Organize Case Data ####
####**************************************
  
# 1a Readin data
# 4518527 obs of 71 variables 
dta <- fst::read.fst(paste0(final.data.folder, "prepared_cohort", "_", "mi",
                            "_identified", "_", "Pop", "_", "STEMI2hrDelay", "_TVOnly.fst"))

# 1b Keep only primary MI admissions according to main outcome definition
dta <- dta %>% filter(MI_prim == 1)

# 1c Break up data into cases and controls 
cases <- dta %>% filter(Case == 1)
controls <- dta %>% filter(Case == 0)

####*************************
#### 2: Create Table One ####
####*************************

# 2a Create function to extract distribution 
extract_dist <- function(data, VarName){
  dgk <- data  %>% rename(Exp := !!VarName)
  
  c(round(mean(dgk$Exp), 2),
    round(sd(dgk$Exp),2), 
    round(quantile(dgk$Exp, 0.1, type = 1),2), 
    round(quantile(dgk$Exp, 0.9, type = 1),2))
}

# 2b Extract function to combine distributions 
combine_dist <- function(VarName){
  c(VarName, extract_dist(dta, VarName),extract_dist(cases, VarName),extract_dist(controls, VarName))
}

# 2c Create list of variables to get distributions
# these are just the variables used in the main models
VarList <- c(NamesTVListExpW, 
             "MeanT_24hr", "MeanR_24hr",
             "MeanAbsFDR_24hr", "MaxAbsFDR_24hr", 
             "SDR_24hr", "DRR_24hr", "SDMinMaxR_48hr",
             "MeanFDR_24hr", "DayDiffR_48hr")

# 2d Extract distributions
Dist.List <- map(VarList, combine_dist)

# 2e Combine into a single dataframe 
Dist.Table <- reduce(Dist.List, rbind) %>% as.data.frame()

# 2f Rename columns 
colnames(Dist.Table) <- c("VarName", "Dta_Mean","Dta_SD", "Dta_10th", "Dta_90th",
"Case_Mean", "Case_SD", "Case_10th", "Case_90th", 
"Control_Mean","Control_SD",  "Control_10th", "Control_90th")
# 2g Save results 
Dist.Table %>% 
  write_csv(paste0(manuscript.folder, "eTable2_exposure_distributions.csv"))
