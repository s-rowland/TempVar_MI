# Assign Hourly Weather and Averages for Lead 
# Data Prep
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/04/2020

####***********************
#### Table of Contents #### 
####***********************

# D: Description
# N: Notes
# 0: Preparation 
# 1: Organize
# 2: Process Outcome Timing Data
# 3: Process Weather Data
# 4: Split Data
# 5: Define Join Function
# 6: Run Join Function Over Years and Variables

####********************
#### D: Description ####
####********************

# This script is almost exactly the same as c_03
# the key difference is in the make_lead() function
# Where we add to the HourIndex (moving forward in time)
# rather than subtracting (moving backwards in time)

####**************
#### N: Notes #### 
####**************

# Na
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
library(furrr)

####*****************
#### 1: Organize ####
####*****************

# 1a Define function to create lead hours
make_lead <- function(hours, HR, weather.variable){ 
  VarName <- paste0(substr(weather.variable, 0,1), "Lead", "_", str_pad(HR,2, pad = "0"))
  hours %>% mutate(!!VarName := DayHourIndex +  HR)
} 


# 1b Define make leads function
make_leads_assign_weather <- function(ActiveYYYY, weather.variable){
  #ActiveYYYY <- "2015"; weather.variable <- "temp"
  
  ####************************************
  #### 2: Process Outcome Timing Data ####
  ####************************************
  
  # 2a Readin casecontrol data 
  hours <- fst::read_fst(paste0(intermediate.data.folder, 
                                "c_2_casecontrolhours_unassigned_", CaseType, "_", 
                                ActiveYYYY, "_", Timing, ".fst"))  

  
  # 2b Keep only variables of interest
  hours <- hours %>% 
    mutate(DayHourIndex = as.numeric(DayHourIndex))%>% 
    dplyr::select(contains("DX"), contains("pr"), TPADM, UPID, zcta,
                  sex, age, race, wait,STEMI, CaseDateRawOrig,
                  InOut, CaseDateRaw, 
                  CaseHourIndex, HourName, DayHourIndex)
  
  # 2c Replicate dataset
  hours0 <- hours
  
  # 2d Create lags for 72 hours (3 days)
  for(i in 1:72){ 
    hours <- make_lead(hours, i, weather.variable)
  }
  
  # 2e Put data in long format via lags 
  hours2 <- hours %>%
    dplyr::select(-DayHourIndex) %>% 
    gather("LeadName", "LeadHourIndex", contains("Lead_")) 
  
  # 2f Make list of active hourindexes for that year 
  ActiveHI <- hours2 %>% 
    dplyr::select(LeadHourIndex) %>% 
    distinct() %>% 
    arrange(LeadHourIndex)
  ActiveHI.list <- as.list(ActiveHI$LeadHourIndex)
  Hours <- hours2 
  
  # 2g Clean environment
  rm(hours2)
  
  ####*****************************
  #### 3: Process Weather Data ####
  ####*****************************
  
  # 3a Readin current year's weather data 
  wea1 <-  fst::read_fst(paste0(intermediate.data.folder, "weather_Long_", 
                                NLDASWeight, "_", ActiveYYYY, ".fst")) %>% 
    as.data.frame() %>%
    rename(wea := weather.variable) %>% 
    filter(HourIndex %in% ActiveHI.list)%>% 
    dplyr::select(zcta, HourIndex, wea)
  
  # 3b Readin previous year's data 
  # 3b.i identify previous year
  PrevYYYY <- as.character(as.numeric(ActiveYYYY) - 1 )
  # 3b.ii readin previous years weather
  wea0 <-  fst::read_fst(paste0(intermediate.data.folder, "weather_Long_", 
                                NLDASWeight, "_", PrevYYYY, ".fst")) %>% 
    as.data.frame() %>%
    rename(wea := weather.variable) %>% 
    filter(HourIndex %in% ActiveHI.list) %>% 
    dplyr::select(zcta, HourIndex, wea)
  
  # 3c Readin subsequent year's data 
  #due to the difference in timezones, NLDAS is in UTC, which ends before EST
  # 3c.i identify previous year
  NextYYYY <- as.character(as.numeric(ActiveYYYY) + 1 )
  # 3c.ii readin previous years exposure
  wea2 <-  fst::read_fst(paste0(intermediate.data.folder, "weather_Long_", 
                                NLDASWeight, "_", NextYYYY, ".fst")) %>% 
    as.data.frame() %>%
    rename(wea := weather.variable) %>% 
    filter(HourIndex %in% ActiveHI.list) %>% 
    dplyr::select(zcta, HourIndex, wea)
  
  # 3d Keep only the active climate variable 
  wea.L <- bind_rows(wea1, wea0, wea2)
  
  ####*******************
  #### 4: Split Data ####
  ####*******************
  
  # 4a Clear the environment
  rm(wea0, wea1, wea2)
  
  # 4b Prepare hourss data 
  h <- Hours %>% 
    mutate(UPID_event_lead = paste0(UPID,"_", CaseHourIndex, ".", HourName, ".", LeadName)) %>% 
    dplyr::select(zcta, UPID_event_lead, LeadHourIndex) %>% arrange(LeadHourIndex)
  
  # 4c Prepare weather data
  w <- wea.L %>% 
    mutate(zcta = as.character(zcta)) %>% 
    arrange(HourIndex) 
  
  # 4d Split data into list 
  Hlist <- split(h, h$LeadHourIndex)
  Wlist <- split(w, w$HourIndex)
  
  # 4e Double check 
  if (length(Hlist) != length(Wlist))  stop("check w and h size")
  
  # 4f Output the three lists
  list(Hlist, Wlist, hours0)
}


####*****************************
#### 5: Define Join Function ####
####*****************************

# 5a Define our join function 
join_by_zcta <- function(hour.df, exp.df) { 
  hour.df %>% 
    dplyr::select(-LeadHourIndex) %>%
    left_join(exp.df, by = "zcta") %>%
    dplyr::select(-HourIndex) 
}

####***************************************************
#### 6: Run Join Function Over Years and Variables ####
####***************************************************

# 6a Tell furrr how to parallelize
future::plan(multiprocess)

# 6b Create list of years 
YearList <- c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", 
              "2009", "2010", "2011", "2012", "2013", "2014", "2015")

# 6c Create function to assign lagged weather
assign_lead_weather <- function(i, weather.variable){
  #i <- 1; weather.variable <- "rh"
  # 6c.i Create the leads for each hour
  a.ls <- make_leads_assign_weather(YearList[i], weather.variable)
  Hlist <- a.ls[[1]]
  Wlist <- a.ls[[2]]
  hours0 <- a.ls[[3]]
  # 6c.ii Join by zcta for the active Year
  DGK <- future_map2(Hlist, Wlist, join_by_zcta)
  # 6c.iii Combine the hour-specific lists into a year-long list
  dta <- do.call("rbind", DGK)
  # 6c.iv Clean dataframe
  dta1 <- dta %>% 
    separate(UPID_event_lead, sep = "\\.", c("UPID_event", "HourName", "LeadName")) %>%
    mutate(HourName0 = paste0(UPID_event, "_", HourName)) %>% 
    dplyr::select(-UPID_event)
  DGK.wide <- tidyr::spread(dta1, LeadName, wea)
  # 6e Combine weather with demographic data 
  hours0 <- hours0 %>% mutate(HourName0 = paste0(UPID,"_", CaseHourIndex, "_", HourName))
  hours0 %>% 
    dplyr::select(-zcta, -HourName) %>%
    inner_join(DGK.wide, by = "HourName0") %>% 
    fst::write_fst(paste0(intermediate.data.folder, 
                          "c_6_casecontrolhours_assigned_hourly_", CaseType,"_", 
                          YearList[i],"_",weather.variable, "_", NLDASWeight, "_", Timing, "_Lead.fst"))
}

# 6d Assign temp across years
#assign_lead_weather(1, "temp")  
#assign_lead_weather(2, "temp") 
#assign_lead_weather(3, "temp") 
#assign_lead_weather(4, "temp") 
#assign_lead_weather(5, "temp") 
#assign_lead_weather(6, "temp") 
#assign_lead_weather(7, "temp") 
#assign_lead_weather(8, "temp") 
#assign_lead_weather(9, "temp") 
#assign_lead_weather(10, "temp") 
#assign_lead_weather(11, "temp")   
#assign_lead_weather(12, "temp") 
assign_lead_weather(13, "temp") 
assign_lead_weather(14, "temp") 
assign_lead_weather(15, "temp") 
assign_lead_weather(16, "temp") 

# 6e Assign rh across years
assign_lead_weather(1, "rh")  
assign_lead_weather(2, "rh")   
assign_lead_weather(3, "rh")   
assign_lead_weather(4, "rh")   
assign_lead_weather(5, "rh")  
assign_lead_weather(6, "rh")   
assign_lead_weather(7, "rh")   
assign_lead_weather(8, "rh")   
assign_lead_weather(9, "rh")  
assign_lead_weather(10, "rh")   
assign_lead_weather(11, "rh")    
assign_lead_weather(12, "rh")   
assign_lead_weather(13, "rh")  
assign_lead_weather(14, "rh")   
assign_lead_weather(15, "rh")   
assign_lead_weather(16, "rh") 