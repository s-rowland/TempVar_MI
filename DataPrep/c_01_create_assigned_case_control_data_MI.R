# Prepare identified cases 
# Data Prep
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/02/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create Assigned Case Control Data for TV_MI Main Analysis
# 2: Create Assigned Case Control Data with Lead Exposures
# 3: Create Assigned Case Control Data for Alt Window Sensitivity Analysis

####**************
#### N: Notes #### 
####**************

# Na Description
# The goal of this script is to process the case data 
# from step b_1 
# We need to do data cleaning, especially duplicate entries.
# We also apply most of the exclusion criteria in this script. 
# Right now the code has specifications for mi, 
# but could accomodate stroke in the future. 
# We include all the potential MI cases, not just primary mi 
# so that we can easily apply alternative case definitions in sensitivity analyses 

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

####******************************************************************
#### 1: Create Assigned Case Control Data for TV_MI Main Analysis ####
####******************************************************************

# 1a Set CaseType 
CaseType <- "mi"

# 1b Set NLDASWeight
NLDASWeight <- "Pop"

# 1c Set timing assumption
# for one sensitivity analysis we try alternative exposure windows 
# in the manuscript we only use "All23" - 
# meaning all exposure windows start at the 23rd hour of the day 
# STEMI3hrDelay is used in HourlyTemp-MI main analysis 
# STEMI2hrDelay is used in HourlyTemp-MI sensitivity analysis
# STEMI2hrDelay is used for TempVar-MI
Timing <-    "STEMI2hrDelay"

# 1d Prepare case data 
#source(paste0(dataprep.scripts.folder, "c_01a_prepare_cases.R"))

# 1e Create matching control periods 
#source(paste0(dataprep.scripts.folder, "c_01b_create_control_hours.R"))

# 1f Assign lagged hourly weather exposures
#source(paste0(dataprep.scripts.folder, "c_01c_assign_hourly_weather_lag.R"))

# 1g Compute variability and mean metrics
#source(paste0(dataprep.scripts.folder, "c_01d_compute_variability_averages_lag.R"))

# 1h Combine annual data and curate variables
#source(paste0(dataprep.scripts.folder, "c_01e_combine_data.R"))

####**************************************************************
#### 2: Create Assigned Case Control Data with Lead Exposures ####
####**************************************************************

# 2a Assign lead hourly weather exposure 
#source(paste0(dataprep.scripts.folder, "c_02a_assign_hourly_weather_lead.R"))

# 2b Calculate variability 
#source(paste0(dataprep.scripts.folder, "c_02b_compute_variability_averages_lead.R"))

# 2c Combine with lag-exposured data  
#source(paste0(dataprep.scripts.folder, "c_02c_combine_data_lagLeadExposure.R"))

####******************************************************************************
#### 3: Create Assigned Case Control Data for Alt Window Sensitivity Analysis ####
####******************************************************************************

# 3a Set CaseType 
CaseType <- "mi"

# 3b Set NLDASWeight
NLDASWeight <- "Pop"

# 3c Set timing assumption
# for one sensitivity analysis we try alternative exposure windows 
# in the manuscript we only use "All23" - 
# meaning all exposure windows start at the 23rd hour of the day 
# STEMI3hrDelay is used in HourlyTemp-MI main analysis 
# STEMI2hrDelay is used in HourlyTemp-MI sensitivity analysis
# STEMI2hrDelay is used for TempVar-MI
Timing <-    "All23"

# 3d Prepare case data 
#source(paste0(dataprep.scripts.folder, "c_01a_prepare_cases.R"))

# 3e Create matching control periods 
#source(paste0(dataprep.scripts.folder, "c_01b_create_control_hours.R"))

# 3f Assign lagged hourly weather exposures
source(paste0(dataprep.scripts.folder, "c_01c_assign_hourly_weather_lag.R"))

# 3g Compute variability and mean metrics
source(paste0(dataprep.scripts.folder, "c_01d_compute_variability_averages_lag.R"))

# 3h Combine annual data and curate variables
source(paste0(dataprep.scripts.folder, "c_01e_combine_data.R"))
