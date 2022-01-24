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

# 0b Add raw data location 
raw.data.folder <- paste0(project.folder, "DataPrep/Data/Raw_Data/")

####**************************************
#### 1: Readin and Organize Case Data ####
####**************************************
  
# 1a Readin data
# 4504028 obs of 66 variables 
dta <- fst::read.fst(paste0(final.data.folder, "prepared_cohort", "_", "mi",
                            "_identified", "_", "Pop", "_", "STEMI2hrDelay", "_TVOnly.fst"))

# 1b Keep only cases 
cases <- dta %>% filter(Case == 1)

# 1c Keep only primary MI admissions according to main outcome definition
# we should have 805,745 primary cases
cases <- cases %>% filter(MI_prim == 1)

# 1d Convert zcta to numeric 
cases <- cases %>% mutate(zcta = as.numeric(zcta))

####*************************
#### 2: Create Table One ####
####*************************

# 2a Create count variable 
TableOne <- cases %>% 
  summarize(NCases = n()) %>% 
  mutate(Variable = "Count")
  
# 2b Tabulation by sex 
TableOne <- cases %>% 
  group_by(sex) %>% 
  summarize(NCases = n()) %>% 
  rename(Variable = sex) %>% 
  bind_rows(TableOne,.)
  
# 2c Tabulation by AgeGroup 
TableOne <- cases %>% 
  group_by(ageGroup) %>% 
  summarize(NCases = n()) %>% 
  rename(Variable = ageGroup) %>% 
    bind_rows(TableOne,.)
  
# 2d Tabulation by simplified race 
TableOne <- cases %>%
    mutate(raceGP = if_else(race == "PacificIslander" | race == "Asian", "Asian American", race)) %>% 
    group_by(raceGP) %>% 
  summarize(NCases = n()) %>% 
  rename(Variable = raceGP) %>% 
    bind_rows(TableOne, .)
  
# 2e Tabulation by NYC residence 
# 4d Percent from NYC 
# csv file taken from 
# NYC Open Data 
# at https://data.cityofnewyork.us/Health/Modified-Zip-Code-Tabulation-Areas-MODZCTA-Map/5fzm-kpwv
# on 11/17/2020s
# 4d.i Readin and wrangle data 
nyc <- read_csv(paste0(raw.data.folder, "Modified_Zip_Code_Tabulation_Areas__MODZCTA_.csv"))
nyc <- nyc %>% 
  separate(ZCTA, c("z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8", "z9", "z10", "z11", "z12"), 
           extra = "merge")
# 4d.ii Extract list of zcta in NYC
nyc.zcta <- c(nyc$z1, nyc$z2, nyc$z3, nyc$z4, nyc$z5, nyc$z6, nyc$z7, nyc$z8, 
              nyc$z9, nyc$z10, nyc$z11, nyc$z12)
# 4d.iii Isolate cases in nyc
cases.nyc <- cases %>% 
  filter(zcta %in% nyc.zcta) %>% 
  select(EventID) %>% 
  distinct() %>% 
  mutate(nyc = 1)

TableOne <- cases %>% 
  left_join(cases.nyc, by = "EventID") %>%
  group_by(nyc) %>% 
  summarize(NCases = n()) %>% 
  mutate(nyc = if_else(is.na(nyc), "NotNYC", "NYC")) %>%
  rename(Variable = nyc) %>% 
  bind_rows(TableOne,.)

# 2f Tabulation by First/Recurrent Status 
TableOne <- cases %>% 
  group_by(MIStatus) %>% 
  summarize(NCases = n()) %>% 
  rename(Variable = MIStatus)%>% 
  bind_rows(TableOne,.)
  
# 2g Compute percentages 
TableOne <-  TableOne %>% 
  mutate(Percentage = format(round(100 * NCases/ nrow(cases) , 2), nsmall = 2))
  
# 2h Rearrange columns 
TableOne <- TableOne %>% select(Variable, NCases, Percentage)
  
# 2i Save TableOne
TableOne %>% write_csv(paste0(manuscript.folder, "Table1_TableOne.csv"))
