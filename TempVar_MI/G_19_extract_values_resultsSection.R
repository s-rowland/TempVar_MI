# Calculate Summary Metrics 
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/22/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Generate Numbers for Abstract
# 2: Generate Numbers for Methods
# 3: Generate Numbers for Study Participants
# 4: Generate Numbers for Weather Conditions
# 5: Generate Numbers for MeanAbsFDT and MI 
# 6: Generate Numbers for MeanFDT and MI
# 7: Generate Numbers for Sensitivity Analyses

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

# 0b  Set study parameters
CaseType <- "mi"
NLDASWeight <- "Pop"
Timing <- "STEMI2hrDelay"

####**************************************
#### 1: Generate Numbers for Abstract ####
####**************************************

# 1a Total N 
TableOne <- read_csv(paste0(manuscript.folder, "Table1_TableOne.csv"))
r.1a <- TableOne$NCases[1]
# 1b EE for inc of MeanAbsFDT 
EETable <- read_csv(paste0(manuscript.folder, "eTable5_EE_AlternativeMetrics.csv"))
r.1b <- EETable$EE90[1]
# 1c EE for inc Mean FDT 
r.1c <- EETable$EE90[6]

####*************************************
#### 2: Generate Numbers for Methods ####
####*************************************

# 2a Percent removed due to missingness 
Miss <- read_csv(paste0(final.data.folder, "PercentMissing_mi.csv"))
r.2a <- Miss$PercentMissing[1]
####************************************************
#### 3: Generate Numbers for Study Participants ####
####************************************************

# 3a Total N 
r.3a <- TableOne$NCases[1]
# 3b Unique Individuals 
r.3b <- Miss$NInd[1]
# 3c Number periods with bad temp data
r.3c <- Miss$NLDASErr[1]
# 3d mean age 
r.3d <- Miss$AgeMean[1]
# 3e sd age 
r.3e <- Miss$AgeSD[1]
# 3f percent on public insurance 
r.3f <- Miss$PPubIns[1]
# 3g percent in NYC 
r.3g <- Miss$PNYC[1]
# 3h Average MeanT_24hr 
TableTwo <- read_csv(paste0(manuscript.folder, "eTable2_exposure_distributions.csv"))
dta <- fst::read.fst(paste0(final.data.folder, "prepared_cohort", "_", "mi",
                            "_identified", "_", "Pop", "_", "STEMI2hrDelay", "_TVOnly.fst"))
dta <- dta %>%filter(MI_prim == 1)
cases <- dta %>% filter(Case == 1)
controls <- dta %>% filter(Case == 0)

r.3h <- round(mean(dta$MeanT_24hr),2)
# 3i SD MeanT_24hr
r.3i <- sd(dta$MeanT_24hr)
# 3j Min MeanT_24hr 
r.3j <- min(dta$MeanT_24hr)
# 3k Max MeanT_24hr 
r.3k <- max(dta$MeanT_24hr)

####************************************************
#### 4: Generate Numbers for Weather Conditions ####
####************************************************

# 4a Average MeanAbsFDT_24hr 
r.4a <- mean(dta$MeanAbsFDT_24hr)
# 4b SD MeanAbsFDT_24hr 
r.4b <- sd(dta$MeanAbsFDT_24hr)
# 4c Min MeanAbsFDT_24hr 
r.4c <- min(dta$MeanAbsFDT_24hr)
# 4d Max MeanAbsFDT_24hr 
r.4d <- max(dta$MeanAbsFDT_24hr)
# 4e 10th percentile MeanAbsFDT_24hr 
r.4e <- quantile(dta$MeanAbsFDT_24hr, 0.1)
# 4f 90th percentile MeanAbsFDT_24hr 
r.4f <- quantile(dta$MeanAbsFDT_24hr, 0.9)
# 4g 99.5th percentile MeanAbsFDT_24hr 
r.4g <- quantile(dta$MeanAbsFDT_24hr, 0.995)
# 4h Average MeanAbsFDT_24hramong controls
r.4h <- mean(controls$MeanAbsFDT_24hr)
# 4i Average MeanAbsFDT_24hr among cases
r.4i <- mean(cases$MeanAbsFDT_24hr, na.rm = TRUE)

# 4j Average MeanFDT_24hr 
r.4j <- mean(dta$MeanFDT_24hr)
# 4k SD MeanFDT_24hr 
r.4k <- sd(dta$MeanFDT_24hr)
# 4l Min MeanFDT_24hr 
r.4l <- min(dta$MeanFDT_24hr)
# 4m Max MeanFDT_24hr 
r.4m <- max(dta$MeanFDT_24hr)
# 4n 10th percentile MeanFDT_24hr 
r.4n <- quantile(dta$MeanFDT_24hr, 0.1)
# 4o 90th percentile MeanFDT_24hr 
r.4o <- quantile(dta$MeanFDT_24hr, 0.9)
# 4p Average MeanFDT_24hr among controls
r.4r <- mean(controls$MeanFDT_24hr)
# 4qs Average MeanFDT_24hr among cases
r.4s <- mean(cases$MeanFDT_24hr)

####***********************************************
#### 5: Generate Numbers for MeanAbsFDT and MI ####
####***********************************************

# 5a Average MeanAbsFDT_24hr 
r.5a <- mean(dta$MeanAbsFDT_24hr)
# 5b 90th percentile MeanAbsFDT_24hr 
r.5b <- quantile(dta$MeanAbsFDT_24hr, 0.9)
# 5c fit for mean to 90th for MeanAbsFD _24hr 
r.5c <- EETable$EE90[1]

####********************************************
#### 6: Generate Numbers for MeanFDT and MI ####
####********************************************

# 6a Average MeanFDT_24hr 
r.6a <- round(mean(dta$MeanFDT_24hr),2)
# 6b 90th percentile MeanFDT_24hr 
r.6b <- round(quantile(dta$MeanFDT_24hr, 0.9),2)
# 6c fit for mean to 90th for MeanFDT_24hr  
r.6c <- EETable$EE90[6]

####**************************************************
#### 7: Generate Numbers for Sensitivity Analyses ####
####**************************************************

# 7a Percentage reduction of MeanFDT from Main analysis to 23:00 exposure window analysis
EETable.23 <- read_csv(paste0(manuscript.folder, "eTable7_EE_AlternativeWindow.csv")) %>% 
  filter(TVMetric == "MeanFDT")
r.7a <- 100* (1-EETable.23$fit.pc90[EETable.23$Sensitivity == "All23"]/
                EETable.23$fit.pc90[EETable.23$Sensitivity == "Main"])




