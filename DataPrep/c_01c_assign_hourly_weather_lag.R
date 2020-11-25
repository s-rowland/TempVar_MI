# Assign Lagged Weather Variabless
# Data Prep
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/02/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Define Function to Create Lag Hours
# 2: Define Function to Process Lagged Case and Weather Data
 # 2A: Process Outcome Timing Data
 # 2B: Process Weather Data
 # 2C: Split Data
# 3: Define Join Function
# 4: Assign Lagged Hourly Temperature and RH

####**************
#### N: Notes #### 
####**************

# Na Description
# We first assign hourly exposures, 
# up to 72 hours prior to the time of event 
# and then we calculate variability and averages 
# from that hourly data 
# At this step we make the assumption of the timing of the event 
# although we could alternatively modify the datetime of admission in c_1

# Nb Break up by year
# This step is broken up by year because it is computationally intensive
# it is still faster to split by year and then furrr() within year

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
library(furrr)
library(extraDistr)

####********************************************
#### 1: Define Function to Create Lag Hours ####
####********************************************

# 1a Define function to create lag hours
make_lag <- function(hours, HR, weather.variable){ 
  VarName <- paste0(substr(weather.variable, 0,1), "Lag", "_", str_pad(HR,2, pad = "0"))
  hours %>% mutate(!!VarName := DayHourIndex - HR)
} 

####****************************************************************
#### 2: Define Function to Process Lagged Case and Weather Data ####
####****************************************************************

# 2a Define process lags function
process_lags_cases_weather <- function(ActiveYYYY, weather.variable){
  #ActiveYYYY <- "2000"; weather.variable <- "temp"
  
  ####************************************
  #### 2A: Process Outcome Timing Data ####
  ####************************************
  
  # 2A.a Readin casecontrol data 
  hours <- fst::read_fst(paste0(intermediate.data.folder, 
                                "c_2_casecontrolhours_unassigned_", CaseType, "_", 
                                ActiveYYYY, "_", Timing, ".fst"))

  # 2A.b Keep only variables of interest
  hours <- hours %>% 
    mutate(DayHourIndex = as.numeric(DayHourIndex))%>% 
    dplyr::select(contains("DX"), contains("mi"), contains("stroke"), contains("pr"), TPADM, UPID, zcta,
                  sex, age, race, htn, wait,STEMI, CaseDateRawOrig,
                  InOut, CaseDateRaw, 
                  CaseHourIndex, HourName, DayHourIndex)
  
  # 2A.c Replicate dataset
  # we will use this dataset to remember the ICD & demographic info of each patient
  hours0 <- hours
  
  # 2A.d Create lags for 72 hours (3 days)
  for(i in 0:71){ 
    hours <- make_lag(hours, i, weather.variable)
  }
  
  # 2Ae. Put data in long format via lags 
  hours2 <- hours %>%
    dplyr::select(-DayHourIndex) %>% 
    gather("LagName", "LagHourIndex", contains("Lag_")) 
  
  # 2A.h Make list of active hourindexes for that year 
  # we will use this to help curate the previous and next year data
  ActiveHI <- hours2 %>% 
    dplyr::select(LagHourIndex) %>% 
    distinct() %>% 
    arrange(LagHourIndex)
  ActiveHI.list <- as.list(ActiveHI$LagHourIndex)
  Hours <- hours2 
  
  # 2A.g Clean environment
  rm(hours2)
  
  ####******************************
  #### 2B: Process Weather Data ####
  ####******************************
  
  # 2B.a Readin current year's weather data 
  wea1 <-  fst::read_fst(paste0(intermediate.data.folder, 
                                "weather_Long_", NLDASWeight, "_", ActiveYYYY, ".fst")) %>% 
    as.data.frame() %>%
    rename(wea := weather.variable) %>% 
    filter(HourIndex %in% ActiveHI.list)%>% 
    dplyr::select(zcta, HourIndex, wea)
  
  # 2B.b Readin previous year's data 
  # 2B.b.i Identify previous year
  PrevYYYY <- as.character(as.numeric(ActiveYYYY) - 1)
  # 2B.b.ii Readin previous years weather
  wea0 <-  fst::read_fst(paste0(intermediate.data.folder, 
                                "weather_Long_", NLDASWeight, "_", PrevYYYY, ".fst")) %>% 
    as.data.frame() %>%
    rename(wea := weather.variable) %>% 
    filter(HourIndex %in% ActiveHI.list) %>% 
    dplyr::select(zcta, HourIndex, wea)
  
  # 2B.c Readin subsequent year's data 
  # due to the difference in timezones, NLDAS is in UTC, which ends before EST
  # so you need the first 4-6 hours of the subsequent year to capture expsoure
  # for the veyr last days of december
  # 2B.c.i Identify previous year
  NextYYYY <- as.character(as.numeric(ActiveYYYY) + 1 )
  # 2B.c.ii Readin previous years exposure
  wea2 <-  fst::read_fst(paste0(intermediate.data.folder,
                                "weather_Long_", NLDASWeight, "_", NextYYYY, ".fst")) %>% 
    as.data.frame() %>%
    rename(wea := weather.variable) %>% 
    filter(HourIndex %in% ActiveHI.list) %>% 
    dplyr::select(zcta, HourIndex, wea)
  
  # 2B.d Keep only the active climate variable 
  wea.L <- bind_rows(wea1, wea0, wea2)
  
  ####********************
  #### 2C: Split Data ####
  ####********************
  
  # 2C.a Clear the environment
  rm(wea0, wea1, wea2)
  
  # 2C.b Prepare hours data 
  h <- Hours %>% 
    mutate(UPID_event_lag = paste0(UPID,"_", CaseHourIndex, ".", HourName, ".", LagName)) %>% 
    dplyr::select(zcta, UPID_event_lag, LagHourIndex) %>% 
    arrange(LagHourIndex)
  
  # 2C.c Prepare weather data
  w <- wea.L %>% 
    mutate(zcta = as.character(zcta)) %>% 
    arrange(HourIndex) 
  
  # 2C.d Split data into list 
  Hlist <- split(h, h$LagHourIndex)
  Wlist <- split(w, w$HourIndex)
  
  # 2C.e Double check that split was successful
  if (length(Hlist) != length(Wlist))  stop("check w and h size")
  
  # 2C.f Output the three lists
  list(Hlist, Wlist, hours0)
}

####*****************************
#### 3: Define Join Function ####
####*****************************

# 3a Define our function to join by zcta 
# the special function drops variables we do not need 
# and by making it a function we can do it with future_map2
join_by_zcta <- function(hours.df, exp.df) { 
  hours.df %>% 
    dplyr::select(-LagHourIndex) %>%
    left_join(exp.df, by = "zcta") %>%
    dplyr::select(-HourIndex) 
}

####************************************************
#### 4: Assign Lagged Hourly Temperature and RH ####
####************************************************

# 4a Tell furrr how to parallelize
future::plan(multiprocess)

# 4b Create list of years 
YearList <- c("2000", "2001", "2002","2003","2004", "2005", "2006", "2007", 
              "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")

# 4c Create function to assign lagged weather
assign_lagged_weather <- function(i, weather.variable){
  #i <- 1; weather.variable <- "temp"
  # 4c.i Create the lags for each hour
  a.ls <- process_lags_cases_weather(YearList[i], weather.variable)
  Hlist <- a.ls[[1]]
  Wlist <- a.ls[[2]]
  hours0 <- a.ls[[3]]
  # 4c.ii Join by zcta for the active Year
  DGK <- future_map2(Hlist, Wlist, join_by_zcta)
  # 4c.iii Combine the hour-specific lists into a year-long list
  dta <- do.call("rbind", DGK)
  # 4c.iv Clean dataframe
  dta1 <- dta %>% 
    separate(UPID_event_lag, sep = "\\.", c("UPID_event", "HourName", "LagName")) %>%
    mutate(HourName0 = paste0(UPID_event, "_", HourName)) %>%
    dplyr::select(-UPID_event)
  DGK.wide <- tidyr::spread(dta1, LagName, wea)
  # 4c.v Combine weather with demographic data 
  # and save results
  hours0 <- hours0 %>% 
    mutate(HourName0 = paste0(UPID,"_", CaseHourIndex, "_", HourName)) %>% 
    dplyr::select(-zcta, -HourName) %>%
    inner_join(DGK.wide, by = "HourName0") %>% 
    fst::write_fst(paste0(intermediate.data.folder, 
                          "c_3_casecontrolhours_assigned_hourly_", CaseType,"_", YearList[i],"_",weather.variable, "_", 
                          NLDASWeight, "_", Timing, ".fst" ))
}

# 4d Assign temp across years
# It is computationally much faster to run these separately 
# rather than in parallel
# this step eats a lot of memory. 
assign_lagged_weather(1, "temp")  
assign_lagged_weather(2, "temp") 
assign_lagged_weather(3, "temp") 
assign_lagged_weather(4, "temp") 
assign_lagged_weather(5, "temp") 
assign_lagged_weather(6, "temp") 
assign_lagged_weather(7, "temp") 
assign_lagged_weather(8, "temp") 
assign_lagged_weather(9, "temp") 
assign_lagged_weather(10, "temp") 
assign_lagged_weather(11, "temp")   
assign_lagged_weather(12, "temp") 
assign_lagged_weather(13, "temp") 
assign_lagged_weather(14, "temp") 
assign_lagged_weather(15, "temp") 
assign_lagged_weather(16, "temp") 

# 4e Assign rh across years
assign_lagged_weather(1, "rh")  
assign_lagged_weather(2, "rh")   
assign_lagged_weather(3, "rh")   
assign_lagged_weather(4, "rh")   
assign_lagged_weather(5, "rh")  
assign_lagged_weather(6, "rh")   
assign_lagged_weather(7, "rh")   
assign_lagged_weather(8, "rh")   
assign_lagged_weather(9, "rh")  
assign_lagged_weather(10, "rh")   
assign_lagged_weather(11, "rh")    
assign_lagged_weather(12, "rh")   
assign_lagged_weather(13, "rh")  
assign_lagged_weather(14, "rh")   
assign_lagged_weather(15, "rh")   
assign_lagged_weather(16, "rh")   