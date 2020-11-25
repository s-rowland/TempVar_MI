# Create Control Hours
# Data Prep
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/02/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Readin Data
# 2: Define Function to Create Control Days
# 3: Create Control Hours

####**************
#### N: Notes #### 
####**************

# Na Description
# We choose control hours via time-stratified bidirectional matching 
# We match by year, month, day of the week, and hour of the day 
# We also use the function to generate the case hour, 
# so that all of the hours have parallel format.
# This step is pretty fast < 10 min

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

####********************
#### 1: Readin Data ####
####********************

# 1a Readin data 
# this dataset shuld have 1023730 obs and 46 variables
cases <- fst::read_fst(paste0(intermediate.data.folder, 
                              "c_1_all_cases_unassigned_", CaseType, "_", Timing, ".fst"))

# 1b Recreate date variable
cases <- cases %>% 
  mutate(CaseDateTime = parse_date_time(CaseDateRaw, "ymd H", tz="America/New_York")) 

# 1c Split data by years
# Splitting by year speads up the matching. 
# we could alternatively split by MM-YYYY 
# but the current version is sufficient.
years.list <- cases %>% split(cases$YYYY)

####***********************************************
#### 2: Define Function to Create Control Days ####
####***********************************************

# 2a Define function to create datehours
make_control_hr <- function(hours1, BeforeAfter, WK){ 
  # The name of the hour; accounts for if control or case
  VarName <- paste0(BeforeAfter, "_", str_trunc(WK, 1, "left", ""))    
  # adds WKs number of weeks, preserves hour of day even in daylight saving
  hours1 %>% mutate(!!VarName := CaseDateTime + as.period(7 * WK, "day"))  
}

# 2b Define function to create control hours
create_control_hours_by_year <- function(df.YYYY){
  #df.YYYY <- years.list[[2]]
  # 2c Use function to create bidirectionally symmetric datehours 
  hours1 <-  df.YYYY
  hours1 <- hours1 %>% 
    mutate(CaseDateTime = parse_date_time(CaseDateRaw, "ymd H", tz = "America/New_York")) 
  ActiveYYYY <- hours1$YYYY[1]
  hours1 <- make_control_hr(hours1, "Before", -4)
  hours1 <- make_control_hr(hours1, "Before", -3)
  hours1 <- make_control_hr(hours1, "Before", -2)
  hours1 <- make_control_hr(hours1, "Before", -1)
  hours1 <- make_control_hr(hours1, "CaseHour", 0)
  hours1 <- make_control_hr(hours1, "After", 1)
  hours1 <- make_control_hr(hours1, "After", 2)
  hours1 <- make_control_hr(hours1, "After", 3)
  hours1 <- make_control_hr(hours1, "After", 4)

  # 2d Put in long format by HourName
  hours2 <- hours1 %>% 
    gather("HourName", "DayDateTime", contains("CaseHour_"),
           contains("Before_"), contains("After_") ) 
  
  # 2e Stratify by month of event 
  hours3 <- hours2 %>% filter(month(CaseDateTime) == month(DayDateTime))

  # 2f Convert times to HourIndex
  hours4 <- hours3 %>% 
    mutate(HourIndex0 = 
           as.duration(interval(parse_date_time("1990/01/01 00:00:00", "ymd HMS", 
                                tz="America/New_York"), DayDateTime))) %>% 
    mutate(DayHourIndex = as.numeric(HourIndex0, "hours")) %>% 
    dplyr::select(-HourIndex0) %>% 
    mutate(HourIndex0 = 
             as.duration(interval(parse_date_time("1990/01/01 00:00:00", "ymd HMS", 
                                  tz="America/New_York"), CaseDateTime))) %>% 
    mutate(CaseHourIndex = as.numeric(HourIndex0, "hours")) %>% 
    dplyr::select(-HourIndex0) 

  # 2g Check timezone
  #tz(days4$DayDateTime[1])

  # 2h Save results 
  hours4 %>% fst::write_fst(paste0(intermediate.data.folder, 
                           "c_2_casecontrolhours_unassigned_", CaseType, "_", 
                           ActiveYYYY, "_", Timing, ".fst"))
}

####*****************************
#### 3: Create Control Hours ####
####*****************************

# test with a single year
#create_control_hours_by_year(cases.list[[1]])

# 3a Create control hours for all active years
purrr::map(years.list, create_control_hours_by_year)
