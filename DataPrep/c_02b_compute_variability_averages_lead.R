# Compute Temperature Variability and Averages for Lagged Exposure 
# Data Prep
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/03/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Define Function to Compute Variability of One Variable
# 2: Define Function to Compute Variability and Averages for a Year
# 3: Compute Variability and Averages for Each Year

####**************
#### N: Notes #### 
####**************

# Na Description
# In this script we compute various variability metrics and some averages 
# Some of these metrics are not included in the manuscript, 
# but were used in some initial analyses 
# We also calculate the metrics for 48- and 72- hour exposure windows 
# Though these are not used in the manuscript. 
# everything we compute for temperature we also compute for RH
# this script takes > 4 hours to run

# Nb Break up by year
# This step is broken up by year because it is computationally intensive

####********************
#### 0: Preparation #### 
####********************

# 0a Load packages
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(lubridate)
library(magrittr)
library(tidyr)
library(ggplot2)
library(purrr)
library(furrr)

####***********************************************************************
#### 1: Define Function to Compute Variability and Averages for a Year ####
####***********************************************************************

# 1a Declare function
make_var_ave_data <- function(dta, VarInitial){
  
  dta.e.lag <- dta %>% 
    dplyr::select(contains(paste0(VarInitial, "Lead_")))
  dta.e.lag.24 <- dta.e.lag[,1:24]
  dta.e.lag.48 <- dta.e.lag[,1:48]
  dta.e.lag.24_48 <- dta.e.lag[,25:48]
  # Compute first differences 
  dta.e.fd.24 <- dta %>% dplyr::select("HourName")
  #The first differences should go in the opposite direction because 
  # now the numbers refer to future hours, not lags. 
  for(i in 1:24){
    dta.e.fd.24[,i] <- dta.e.lag[,(i+1)] - dta.e.lag[,i]
  }
  
  # Create dataframe to fill
  dta.e.MinMax <- data.frame(a = rep(NA, nrow(dta)))
  dta.e.MinMax[,1] <- apply(dta.e.lag.24, 1, function(x) min(x))
  dta.e.MinMax[,2] <- apply(dta.e.lag.24, 1, function(x) max(x))
  dta.e.MinMax[,3] <- apply(dta.e.lag.24_48, 1, function(x) min(x))
  dta.e.MinMax[,4] <- apply(dta.e.lag.24_48, 1, function(x) max(x))
  
  dta.e.DayMean <- data.frame(a = rep(NA, nrow(dta)))
  dta.e.DayMean$Mean24hr <- apply(dta.e.lag.24, 1, function(x) mean(x))
  dta.e.DayMean$Mean24_48hr <- apply(dta.e.lag.24_48, 1, function(x) mean(x))
  
  # Create dataframe to fill
  dta.var <- dta %>% dplyr::select("HourName0")
  dta.var[,2] <- apply(dta.e.fd.24, 1, function(x) mean(abs(x)))
  dta.var[,3] <- apply(dta.e.fd.24, 1, function(x) max(abs(x)))
  dta.var[,4] <- apply(dta.e.lag.24, 1, function(x) sd(x))
  dta.var[,5] <- apply(dta.e.lag.24, 1, function(x) max(x) - min(x))
  dta.var[,6] <- apply(dta.e.MinMax, 1, function(x) sd(x))
  dta.var[,7] <- apply(dta.e.fd.24, 1, function(x) mean(x))
  dta.var[,8] <- dta.e.DayMean$Mean24hr - dta.e.DayMean$Mean24_48hr 
  dta.var[,9] <- apply(dta.e.lag.24, 1, function(x) mean(x))
  dta.var[,10] <- apply(dta.e.lag.48, 1, function(x) mean(x))
  dta.var[,11] <- apply(dta.e.lag.24, 1, function(x) min(x))
  dta.var[,12] <- apply(dta.e.lag.24, 1, function(x) max(x))
  dta.var[,13] <- apply(dta.e.lag.48, 1, function(x) min(x))
  dta.var[,14] <- apply(dta.e.lag.48, 1, function(x) max(x))
  
  if(VarInitial == "t"){
    colnames(dta.var) <- c("HourName0", 
                           "MeanAbsFDT_24hrLead", "MaxAbsFDT_24hrLead",
                           "SDT_24hrLead", "DRT_24hrLead", "SDMinMaxT_48hrLead",
                           "MeanFDT_24hrLead", "DayDiffT_48hrLead", 
                           "MeanT_24hrLead", "MeanT_48hrLead",
                           "MinT_24hrLead", "MaxT_24hrLead", "MinT_48hrLead", "MaxT_48hrLead")
  }
  if(VarInitial == "r"){
    colnames(dta.var) <- c("HourName0", 
                           "MeanAbsFDR_24hrLead", "MaxAbsFDR_24hrLead",
                           "SDR_24hrLead", "DRR_24hrLead", "SDMinMaxR_48hrLead",
                           "MeanFDR_24hrLead", "DayDiffR_48hrLead", 
                           "MeanR_24hrLead", "MeanR_48hrLead",
                           "MinR_24hrLead", "MaxR_24hrLead", "MinR_48hrLead", "MaxR_48hrLead")
  }
  dta.var
  
}

####*******************************************************
#### 2: Compute Variability and Averages for Each Year ####
####*******************************************************

calculate_variability <- function(YYYY){
  DGK.temp <- fst::read_fst(paste0(intermediate.data.folder,
                                   "c_6_casecontrolhours_assigned_hourly_", CaseType,"_", 
                                   YYYY,"_","temp", "_", NLDASWeight, "_", Timing, "_Lead.fst"))
  DGK.rh <- fst::read_fst(paste0(intermediate.data.folder, 
                                 "c_6_casecontrolhours_assigned_hourly_", CaseType,"_", 
                                 YYYY,"_","rh", "_", NLDASWeight, "_", Timing, "_Lead.fst"))
  dgk.var.temp <- make_var_ave_data(DGK.temp, "t")
  dgk.var.rh <- make_var_ave_data(DGK.rh, "r")
  
  dta <- DGK.temp %>% 
    dplyr::select(HourName0, contains("tLag_")) %>% 
    inner_join(DGK.rh, by = "HourName0") %>% 
    inner_join(dgk.var.temp, by = "HourName0") %>%
    inner_join(dgk.var.rh, by = "HourName0") 
  # 2h Save results
  dta %>% fst::write_fst(paste0(intermediate.data.folder, "c_7_casecontrolhours_assigned_variability_",
                                CaseType, "_", YYYY, "_", NLDASWeight, "_", Timing , "_Lead.fst"))
}

# 3a Set up mapping
future::plan(multiprocess)

YYYY.listA <- c("2000", "2001", "2002", "2003")
YYYY.listB <- c("2004", "2005", "2006", "2007") 
YYYY.listC <- c("2008", "2009", "2010", "2011") 
YYYY.listD <- c("2012", "2013", "2014", "2015")

# 3a Calculate corr
a <- future_map(YYYY.listA, calculate_variability)
a <- future_map(YYYY.listB, calculate_variability)
a <- future_map(YYYY.listC, calculate_variability)
a <- future_map(YYYY.listD, calculate_variability)


