# Compute Weather Variables
# Data Prep
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/02/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Organize
# 2: Declare Function to Compute Weather Variables
# 3: Compute Weather Variables

####**************
#### N: Notes #### 
####**************

# Na Description
# NLDAS does not report relative humidity
# so we must calculate it from the temperature, pressure and specific humidity
# Additionally, temperature is reported in Kelvin. 
# This script calculates the weather parameters for each zcta-hour combination. 
# After this step, the weather data is ready to be joined to the health data
# approximate runtime: <30 min 

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

# 0b Declare directories
project.folder <- "H:/Temp_CVD_Analysis/"
analysis.folder <- paste0(project.folder,"DataPrep/")
data.folder <- paste0(analysis.folder, "Data/")
raw.data.folder <- paste0(data.folder,"Raw_Data/")
raw.outcome.folder <- paste0(raw.data.folder, "Outcome_Data/")
nldas.folder.areaw <- paste0(intermediate.data.folder, "NLDAS_ZCTA_Data_AreaWeighted/")
nldas.folder.popw <- paste0(intermediate.data.folder, "NLDAS_ZCTA_Data_PopWeighted/")
intermediate.data.folder <- paste0(data.folder, "Intermediate_Data/")
final.data.folder <- paste0(data.folder, "Final_Data/")

# 0c Set type of averaging
# Options are "Pop" and "Area"
# For HourlyTemp-MI analysis, all analyses used area-weighted averaging 
# all subsequent analyses, including TempVar_MI, use population-weighted averaging
NLDASWeight <- "Pop" # "Area"

# 0d Set raw nldas folder  
if(NLDASWeight == "Pop"){raw.nldas.folder <- raw.nldas.folder.popw}
if(NLDASWeight == "Area"){raw.nldas.folder <- raw.nldas.folder.areaw}

####*****************
#### 1: Organize #### 
####*****************

# 1a Set active years 
ActiveYearList <- c(1999, 2000, 2001, 2002, 2003, 2004, 2005, 
                    2006,2007, 2008, 2009, 2010, 2011, 2012,
                    2013, 2014, 2015, 2016)

# 1b Declare RH calculator function
##' Convert specific humidity to relative humidity
##'
##' converting specific humidity into relative humidity
##' NCEP surface flux data does not have RH
##' from Bolton 1980 The computation of Equivalent Potential Temperature 
##' \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
##' @title qair2rh
##' @param qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
##' @param temp degrees C
##' @param press pressure in mb
##' @return rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
##' @export
##' @author David LeBauer
qair2rh <- function(qair, temp, press = 1013.25){
  es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
}

####******************************************************
#### 2: Declare Function to Compute Weather Variables #### 
####******************************************************

# 2a Declare function
compute_wea <- function(ActiveYearNumber){

  # 2b Set the ActiveYYY
  ActiveYYYY <- ActiveYearNumber
  
  # 2c Readin weatherdata
  # this should have 1794 obs by 8761 variables 
  # If ActiveYYYY is a leap year, dim are 1794 by 8785 (24 more hours)
  ActiveWeaVar <- "PRES"
  presdf <- fst::read_fst(paste0(nldas.folder.popw, "zcta_", ActiveYYYY, "_", ActiveWeaVar, ".fst"))
  ActiveWeaVar <- "SPFH"
  spfhdf <- fst::read_fst(paste0(nldas.folder.popw, "zcta_", ActiveYYYY, "_", ActiveWeaVar, ".fst"))
  ActiveWeaVar <- "TMP"
  tempdf <- fst::read_fst(paste0(nldas.folder.popw, "zcta_", ActiveYYYY, "_", ActiveWeaVar, ".fst"))

  # 2d Put data in long format 
  presdf.L <- presdf %>% 
    dplyr::select(zcta, contains("0")) %>% 
    gather("DateHour", "pres", 2:ncol(presdf))
  spfhdf.L <- spfhdf %>% 
    dplyr::select(zcta, contains("0")) %>% 
    gather("DateHour", "spfh", 2:ncol(spfhdf)) %>% 
    dplyr::select(-zcta, -DateHour)
  tempdf.L <- tempdf %>% 
    dplyr::select(zcta, contains("0")) %>% 
    gather("DateHour", "temp", 2:ncol(tempdf)) %>% 
    dplyr::select(-zcta, -DateHour)
  
  # 2e Combine weather variables
  wea.L <- bind_cols(presdf.L, spfhdf.L, tempdf.L) %>% 
                 dplyr::select(zcta, DateHour, pres, spfh, temp)

  # 2f Convert units
  wea.L$temp_c <- wea.L$temp - 273.15
  wea.L$pres_mb <- wea.L$pres * 0.01
  
  # 2g Calculate relative humidity 
  wea.L <- wea.L %>% mutate(rh = qair2rh(spfh, temp_c, pres_mb))
  
  # 2h Keep only variables of interest 
  wea.L <- wea.L %>% 
    dplyr::select(zcta, DateHour, rh, temp_c) %>% 
    rename(temp = temp_c)

  # 2i Compute HourIndex 
  wea.L <- wea.L %>% 
    mutate(DateHour = str_sub(DateHour, 0, 11)) %>%
    mutate(DateHourTime = parse_date_time(DateHour, "ymd H", tz = "UTC")) %>% 
    mutate(EHourIndex0 = as.duration(interval(parse_date_time("1990/01/01 00:00:00", 
                                "ymd HMS", tz="America/New_York"), DateHourTime))) %>%
    mutate(HourIndex = as.numeric(EHourIndex0, "hours")) %>% 
    dplyr::select(zcta, HourIndex, rh, temp)

  # 2j Write final dataset
  # this dataset should have 15715440 obs of 4 var
  # for a leap year, dim should be 15758496 obs of 4 var
  wea.L %>% fst::write_fst(paste0(intermediate.data.folder, "weather_long_", NLDASWeight, "_", ActiveYYYY, ".fst"))
  
  # 2i Clean environment
  rm(list = ls(pattern = "^pres"))
  rm(list = ls(pattern = "^spfh"))
  rm(list = ls(pattern = "^temp"))
}

####**********************************
#### 3: Compute Weather Variables ####
####**********************************
 
# 3a Compute weather variables for all active years
purrr::map(ActiveYearList, compute_wea)
