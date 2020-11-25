# Combine Years of Exposure + Outcome Data
# Data Prep
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/07/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Readin Data
# 2: Apply Exclusion Criteria
# 3: Create Data-Derived Variables
# 4: Save Data

####**************
#### N: Notes #### 
####**************

# Na Description
# Here we combine all the data, 
# do some additional manipulations 
# and then save 3 datasets: full, TV only, and deidentified. 
# this script takes ~ 15 min to run 

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
library(sf)
library(purrr)
library(furrr)

####********************
#### 1: Readin Data ####
####********************

# 3a Set up mapping
future::plan(multiprocess)

# 1a Declare active year list
YYYY.list <-  c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                "2009", "2010","2011", "2012", "2013", "2014", "2015")

readin_data_lagLead <- function(YYYY){
  dta.Lag <- fst::read_fst(paste0(intermediate.data.folder, "c_4_casecontrolhours_assigned_variability_",
                              CaseType, "_", YYYY, "_", NLDASWeight, "_", Timing , ".fst"))
  dta.Lead <- fst::read_fst(paste0(intermediate.data.folder, "c_7_casecontrolhours_assigned_variability_",
                                   CaseType, "_", YYYY, "_", NLDASWeight, "_", Timing , "_Lead.fst")) %>% 
    dplyr::select(HourName0, contains("24"), contains("48"))
  dta.tot <- inner_join(dta.Lag, dta.Lead, by = "HourName0")
}

dta.list <- future_map(YYYY.list, readin_data_lagLead)
dta <- bind_rows(dta.list)

####*********************************
#### 2: Apply Exclusion Criteria ####
####*********************************

# 2a Remove cases that are missing meanr or temp 
#none have missing data 

# 2b Filter by admission type
# 4 = Newbron; 5 = Trauma
if(CaseType == "mi") {dta <- dta %>% filter(TPADM !="4" & TPADM != "5")}

# 2c Remove obs with incorrect hourly temp for 48 hour lag
lagT <- dta %>% dplyr::select(contains("tLag_"))
lagT <- lagT[,1:48]
MinTLag <- apply(lagT, 1, function(x) min(x))
# check the number obs to remove
#head(sort(a), 200)
dta <- dta %>% 
  mutate(MinTLag = MinTLag) %>% 
  filter(MinTLag > -100) %>% 
  filter(MeanAbsFDT_24hrLead < 11)

####**************************************
#### 3: Create Data-Derived Variables ####
####**************************************

# 3a Create AgeGroup Variable
dta <- dta %>% 
  mutate(age = as.numeric(age)) %>%
  mutate(ageGroup = if_else(age >= 65, "AgeGTE65", "AgeLT65"))

# 3b Create Case and TimetoEvent variables 
# These variables are used in the coxph arguement
# by making the data structure parallel to a survival dataset
dta <- dta %>% 
  mutate(Case = if_else(str_detect(HourName0, "Case"), 1, 0),
         TimetoEvent = if_else(str_detect(HourName0, "Case"), 1, 2))

# 3c Create identified stratum identifier
# this variable identifies our case-control sets
dta <- dta %>% 
  mutate(CaseHourIndex2 = str_pad(CaseHourIndex, 7,"left", "T"))%>% 
  mutate(EventID = paste0(UPID, "_", CaseHourIndex2))

# 3d Create Stroke subtype variable 
if(CaseType == "stroke"){
  dta <- dta %>% 
    mutate(
    StrokeSubType  = case_when(
      hem_stroke_prim == "1" & isc_stroke_prim != "1" ~ "hem",
      isc_stroke_prim == "1" & hem_stroke_prim != "1" ~ "isc", 
      isc_strokeA == "1" ~ "isc", 
      hem_strokeA == "1" ~ "hem",
      PR01 == "9910" | PR02 == "9910" | PR03 =="9910" | PR04 =="9910" | PR05 == "9910" ~ "isc",
      TRUE  ~ "unkn"
    ))
}

# 3e Create MI outcome variables
# alternative outcome definitions
# (a) 410.x1 in the primary diagnosis
# (b) 410.x1 or 410.x0 in the four diagnostic positions,
# (c) 410.xx in the primary diagnosis
# (d) 410.x1 in the primary diagnosis; excluded if reccurent by 6 mo or less
# (e) 410.x1 in the four diagnostic positions, drop reinfarctions

if( CaseType == "mi") {
  # 3e.i Create MIStatus variable 
  dta <- dta %>% 
    mutate(First = if_else(is.na(wait), "First", "Recurrent" )) %>% 
    mutate(MIStatus = if_else(wait <= 29, "Reinfarction", "Recurrent")) %>% 
    mutate(MIStatus = if_else(is.na(wait), "First", MIStatus))
  
  # 3e.ii Apply alternative outcome definitions
  # Converting the NA to character allows us to do boolean evaluations. 
  # since now if that dx is missing the boolean will yield 0 rather than na. 
  dta <- dta %>% 
    mutate(DXA = if_else(is.na(DXA), "NA", DXA), 
           DX01 = if_else(is.na(DX01), "NA", DX01),
           DX02 = if_else(is.na(DX02), "NA", DX02),
           DX03 = if_else(is.na(DX03), "NA", DX03)) %>% 
    mutate(DXA_410x1 = str_sub(DXA, 1, 3) == "410" & str_sub(DXA,5,5) == "1") %>% 
    mutate(DXA_I21 = str_detect(DXA, "I21")) %>% 
    mutate(DX01_410x1 = str_sub(DX01, 1, 3) == "410" & str_sub(DX01,5,5) == "1") %>% 
    mutate(DX01_I21 = str_detect(DX01, "I21")) %>% 
    mutate(DX02_410x1 = str_sub(DX02, 1, 3) == "410" & str_sub(DX02,5,5) == "1") %>% 
    mutate(DX02_I21 = str_detect(DX02, "I21")) %>% 
    mutate(DX03_410x1 = str_sub(DX03, 1, 3) == "410" & str_sub(DX03,5,5) == "1") %>% 
    mutate(DX03_I21 = str_detect(DX03, "I21")) %>% 
    mutate(MI_prim_tf = as.numeric(DXA_410x1) + as.numeric(DX01_410x1) + as.numeric(DX02_410x1) + as.numeric(DX03_410x1) + 
             as.numeric(DXA_I21) + as.numeric(DX01_I21) + as.numeric(DX02_I21) + as.numeric(DX03_I21)) %>% 
    mutate(MI_prim = as.numeric(MI_prim_tf > 0)) 
  
  dta <- dta %>% 
    mutate(prim_alt_410x1_A_tf = as.numeric(DXA_410x1) + as.numeric(DX01_I21) ) %>% 
    mutate(prim_alt_410x1_A = as.numeric(prim_alt_410x1_A_tf > 0))  
  
  dta <- dta %>% 
    mutate(DXA_410xx = str_sub(DXA, 1, 3) == "410") %>% 
    mutate(prim_alt_410xx_A_tf = as.numeric(DXA_410xx) + as.numeric(DX01_I21) ) %>% 
    mutate(prim_alt_410xx_A = as.numeric(prim_alt_410xx_A_tf > 0))
  
  dta <- dta %>% 
    mutate(DXA_410x0 = str_sub(DXA, 1, 3) == "410" & str_sub(DXA,5,5) == "0") %>% 
    mutate(DX01_410x0 = str_sub(DX01, 1, 3) == "410" & str_sub(DX01,5,5) == "0") %>% 
    mutate(DX02_410x0 = str_sub(DX02, 1, 3) == "410" & str_sub(DX02,5,5) == "0") %>% 
    mutate(DX03_410x0 = str_sub(DX03, 1, 3) == "410" & str_sub(DX03,5,5) == "0") %>% 
    mutate(prim_alt_410x01_A4_tf = 
             as.numeric(DXA_410x1) + as.numeric(DX01_410x1) + as.numeric(DX02_410x1) + as.numeric(DX03_410x1) + 
             as.numeric(DXA_410x0) + as.numeric(DX01_410x0) + as.numeric(DX02_410x0) + as.numeric(DX03_410x0) + 
             as.numeric(DXA_I21) + as.numeric(DX01_I21) + as.numeric(DX02_I21) + as.numeric(DX03_I21)) %>% 
    mutate(prim_alt_410x01_A4 = as.numeric(prim_alt_410x01_A4_tf > 0)) 
 
   # no reinfarctions
  dta <- dta %>% 
    mutate(prim_alt_NoReinfarction = if_else(MIStatus != "Reinfarction", 1, 0))
  
  # 3e.iii Clean up
  dta <- dta %>% select(-contains("tf"))
 }

####******************
#### 4: Save Data ####
####******************

# 4a Keep only variables of interest 
dta <- dta %>% 
  dplyr::select(contains("prim"), EventID, HourName0,# Case features / strata identifiers
               TimetoEvent, Case,                        # coxph variables
               contains("tLag"), contains("rLag"),       # hourly weather
               contains("24"), contains("48"), contains("72"), contains("DayDiff"), 
               CaseDateRaw, CaseHourIndex, UPID, zcta, 
               sex, race, ageGroup, wait, First, MIStatus, contains("DX"), STEMI, CaseDateRawOrig)

# 4b Save identified dataset
dta %>% fst::write_fst(paste0(final.data.folder, "prepared_cohort", "_", CaseType,
                              "_identified", "_", NLDASWeight, "_", Timing, "_Lead.fst"))

# 4c Save variability-only exposures
dta %>% 
  dplyr::select(-contains("tLag"), -contains("rLag")) %>%
  fst::write_fst(paste0(final.data.folder, "prepared_cohort", "_", CaseType, 
                        "_identified", "_", NLDASWeight, "_", Timing, "_TVOnly_Lead.fst"))

# 4d Deidentify cases
# 4d.i Deidentify the EventID variable - replace idenityfing numbers with random letters
dta.deid <- dta %>% 
  mutate(CaseHourIndex2 = as.numeric(as.factor(CaseHourIndex)))%>% 
  mutate(CaseHourIndex2 = str_pad(CaseHourIndex2, 7,"left", "T"))%>% 
  mutate(ID = as.numeric(as.factor(UPID)))%>% 
  mutate(ID = str_pad(ID, 6,"left", "A"))%>% 
  mutate(EventID = paste0(ID, "_", CaseHourIndex2))

# 4d.ii Deidentify cases: keep minimum set of variables 
dta.deid <- dta.deid %>% 
  filter(MI_prim == 1) %>% 
  dplyr::select(EventID, # Case features / strata identifiers
                TimetoEvent, Case,                        # coxph variables
                contains("tLag"),# contains("rLag"),       # hourly weather
                contains("24"), contains("48"), contains("72")) # variability metrics and means

# 4e Save deidentified dataset
dta.deid %>% fst::write_fst(paste0(final.data.folder, "prepared_cohort", "_",CaseType,
                                   "_deidentified",  "_", NLDASWeight, "_", Timing, "_Lead.fst"))
dta.deid %>%  
  dplyr::select(EventID, # Case features / strata identifiers
TimetoEvent, Case, contains("Mean")) 
  fst::write_fst(paste0(final.data.folder, "prepared_cohort", "_", CaseType, 
                        "_deidentified", "_", NLDASWeight, "_", Timing, "_TVOnly_Lead.fst"))

# check for extremes
c(head(sort(dta$MeanAbsFDT_24hr, decreasing = TRUE)), 
  quantile(dta$MeanAbsFDT_24hr, 0.99), quantile(dta$MeanAbsFDT_24hr, 0.95))
c(head(sort(dta$MaxAbsFDT_24hr, decreasing = TRUE)), 
  quantile(dta$MaxAbsFDT_24hr, 0.99), quantile(dta$MaxAbsFDT_24hr, 0.95))
c(head(sort(dta$SDT_24hr, decreasing = TRUE)), 
  quantile(dta$SDT_24hr, 0.99), quantile(dta$SDT_24hr, 0.95))
c(head(sort(dta$DRT_24hr, decreasing = TRUE)), 
  quantile(dta$DRT_24hr, 0.99), quantile(dta$DRT_24hr, 0.95))
c(head(sort(dta$SDMinMaxT_48hr, decreasing = TRUE)), 
  quantile(dta$SDMinMaxT_48hr, 0.99), quantile(dta$SDMinMaxT_48hr, 0.95))
c(head(sort(dta$MeanFDT_24hr, decreasing = TRUE)), 
  quantile(dta$MeanFDT_24hr, 0.99), quantile(dta$MeanFDT_24hr, 0.95))
c(head(sort(dta$DayDiffT_48hr, decreasing = TRUE)), 
  quantile(dta$DayDiffT_48hr, 0.99), quantile(dta$DayDiffT_48hr, 0.95))

  