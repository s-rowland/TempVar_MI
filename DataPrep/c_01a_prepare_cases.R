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
# 1: Readin Data 
# 2: Organize Data
# 3: Apply Exclusion Criteria
# 4: Data Checks

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

####********************
#### 1: Readin Data ####
####********************

# 1a Create readin function, which includes some cleaning
ReadOutcomeData <- function(yy, pp, casetype){
  cases <- read_csv(paste0(raw.outcome.folder, "raw_", yy, "_", pp, "_", casetype, ".csv"), col_types = cols(.default = "c") ) %>% 
    mutate(InOut = !!pp, EventType = !!casetype, Year = str_sub(ADMDT, 0,4)) 
  if(pp == "op"){cases <- cases %>% mutate(TPADM = "op")}
cases
}

# 1b Setup empty dataframe to fill
cases <- ReadOutcomeData("00", "op", CaseType) %>% sample_frac(0)

# 1c Declare datasets we will readin
# we use 95-99 only to hep identify subsequent cases
YYList <- c("95", "96", "97", "98", "99","00", "01", "02", "03","04", "05", "06",
            "07", "08", "09", "10", "11", "12", "13", "14", "15")
PPList <- c("ip", "op")

# 1d Readin and combine datasets
for(i in 1:length(YYList)){
  for(j in 1:length(PPList)){
    cases <- ReadOutcomeData(YYList[i], PPList[j], CaseType) %>%
      bind_rows(cases)
  }
}
# the resulting dataframe for MI should have 1609158 obs of 28 variables. 
# it will shrink quickly

# 1e Drop cases with missing datetime information 
# we have to drop these cases here because otherwise
# they will interfere with determining first/last status
# after we run this line we should have 1609158 obs of 28 variables for MI 
cases <- cases %>% 
  filter(!is.na(ADMDT))

####**************************
#### 2: Process Variables ####
####**************************

# 2a Rename some variables that are all caps, but not acronyms
# we also have a lowercase zcta
cases <- cases %>% 
  rename(sex = SEX, age = AGE, race = RACE, zcta = ZIP)

# 2b Create proper datetime variable
cases <- cases %>% 
  mutate(CaseDateRawOrig = paste0(ADMDT, " ", ADMHR)) %>%
  mutate(CaseDateTime = parse_date_time(CaseDateRawOrig, "ymd H", tz = "America/New_York")) 

# 2c Adjust CaseDateTime for prehospital delay 
# 2c.i Identify ICD codes that correspond to STEMI 
STEMIICDList <- c('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', 
                  '41030', '41031', '41032','41040', '41041', '41042', '41050', '41051', '41052', 
                  '41060', '41061', '41062', '41080', '41081', '41082', '41090', '41091', '41092', 
                  'I21', 'I210', 'I2100', 'I2101', 'I2102', 'I2109','I211', 'I2110', 'I2111', 'I2119',
                  'I212', 'I2120','I2121', 'I2129', 'I213', 'I2130', 'I21A', 'I21A0', 'I21A1', 'I21A9')
# 2c.ii Identify STEMI cases
cases <- cases %>% 
  mutate(STEMI = if_else(DXA %in% STEMIICDList | DX01 %in% STEMIICDList |
                         DX02 %in% STEMIICDList | DX03 %in% STEMIICDList, 1, 0)) 
# 2c.iii Determine assumed prehospital delay time 
# delay was set to 3 for all MI in an earlier analysis (HourlyTemp_MI)
if(CaseType == "mi" & Timing == "STEMI3hrDelay"){cases <- cases %>% mutate(delay = 3)}
if(CaseType == "mi" & (Timing == "STEMI2hrDelay"| Timing == "All23")){
  cases <- cases %>% mutate(delay = if_else(STEMI == 1, 2, 3))
}
if(CaseType == "stroke"){cases <- cases %>% mutate(delay = 3)}
# 2c.iv Apply assumed prehospital delay time 
cases <- cases %>% 
  mutate(CaseDateTime = CaseDateTime - 3600*delay) 

# 2d Compute datetime variables 
cases <- cases %>% 
  mutate(YYYY = year(CaseDateTime), 
         MM = month(CaseDateTime),
         DD = day(CaseDateTime),
         HH = hour(CaseDateTime), 
         YDay = yday(CaseDateTime)) %>% 
  mutate(YYYY = if_else(is.na(YYYY), as.numeric(str_sub(ADMDT, 0, 4)), YYYY)) %>% 
  mutate(SecularDay = YDay + 365 * (as.numeric(YYYY) - 1995)) %>% 
  mutate(CaseDateRaw = paste0(YYYY, str_pad(MM, 2, "left", "0"),
                              str_pad(DD, 2, "left", "0"), " ", str_pad(HH, 2, "left", "0")))

# 2e Identify primary cases
if(CaseType == "mi"){
  cases <- cases %>% 
    mutate(DXA  = if_else(is.na(DXA), "NA", DXA), 
           DX01 = if_else(is.na(DX01), "NA", DX01),
           DX02 = if_else(is.na(DX02), "NA", DX02),
           DX03 = if_else(is.na(DX03), "NA", DX03)) %>% 
    mutate(DXA_410x1 = str_sub(DXA, 1, 3) == "410" & str_sub(DXA, 5, 5) == "1") %>% 
    mutate(DXA_410x0 = str_sub(DXA, 1, 3) == "410" & str_sub(DXA, 5, 5) == "0") %>% 
    mutate(DXA_I21 = str_detect(DXA, "I21")) %>% 
    mutate(DX01_410x1 = str_sub(DX01, 1, 3) == "410" & str_sub(DX01, 5, 5) == "1") %>% 
    mutate(DX01_410x0 = str_sub(DX01, 1, 3) == "410" & str_sub(DX01, 5, 5) == "0") %>% 
    mutate(DX01_I21 = str_detect(DX01, "I21")) %>% 
    mutate(DX02_410x1 = str_sub(DX02, 1, 3) == "410" & str_sub(DX02, 5, 5) == "1") %>% 
    mutate(DX02_410x0 = str_sub(DX02, 1, 3) == "410" & str_sub(DX02, 5, 5) == "0") %>% 
    mutate(DX02_I21 = str_detect(DX02, "I21")) %>% 
    mutate(DX03_410x1 = str_sub(DX03, 1, 3) == "410" & str_sub(DX03, 5, 5) == "1") %>% 
    mutate(DX03_410x0 = str_sub(DX03, 1, 3) == "410" & str_sub(DX03, 5, 5) == "0") %>% 
    mutate(DX03_I21 = str_detect(DX03, "I21")) %>% 
    mutate(MI_prim_tf = 
             as.numeric(DXA_410x1) + as.numeric(DX01_410x1) + as.numeric(DX02_410x1) + 
             as.numeric(DX03_410x1) + as.numeric(DXA_410x0) + as.numeric(DX01_410x0) +
             as.numeric(DX02_410x0) + as.numeric(DX03_410x0) +as.numeric(DXA_I21) + 
             as.numeric(DX01_I21) +  as.numeric(DX02_I21) + as.numeric(DX03_I21)) %>% 
    mutate(MI_prim = as.numeric(MI_prim_tf > 0))  %>% 
    mutate(outcome_prim = MI_prim)
}
if(CaseType == "stroke"){
  cases <- cases %>% mutate(outcome_prim = if_else(isc_stroke_prim==1|hem_stroke_prim==1, 1, 0))
}

# 2f Caculate recurrence time and subsequent status
# This step must be done at this point in the code 
# before we have excuded any cases based on data availability, etc
# note that subsequnet status is based on time since primary MI admission,
# not any MI admission
cases <- cases %>%  
  group_by(UPID) %>% 
  arrange(SecularDay) %>% 
  mutate(wait = SecularDay-lag(SecularDay) * as.numeric(lag(outcome_prim))) %>%
  ungroup()

# 2g Organize race variable
# Collapse race categories in order to have sufficiently large racial groups to assess
# 2f.i Read in the simplified race codebook
RaceCodeBook <- read_csv(paste0(raw.data.folder, "RaceCodeBook.csv"), 
                         col_types = cols(race  = col_character(),
                                          Race1 = col_character(),
                                          Race2 = col_character()))
RaceCodeBook <- RaceCodeBook %>% mutate(race = str_pad(race, 2, "left", "0"))
# 2g.ii Assign simplified Race categories + hispanic 
cases1 <- cases %>% 
  left_join(RaceCodeBook, by = "race") %>%
  mutate(RaceF = if_else(ETHNIC == "1", "Hispanic", Race2)) 
# 2g.iii Keep only the simplified race categories
cases2 <- cases1 %>% 
  dplyr::select(-ETHNIC, -race, -Race1, -Race2) %>% 
  rename(race = RaceF) 
# 2g.iv Identify the patients who are inconsistently assigned two races 
# We assume that these subjects are multiracial, and it is not an error in coding. 
TwoRace <- cases2 %>%  
  group_by(UPID) %>%
  dplyr::select(UPID, race) %>%
  distinct() %>% 
  summarize(NumRace = n()) %>%
  filter(NumRace !=1) %>% 
  mutate(MR = "Multi-Racial")
# 2g.v Assign multiracial category to the subjects that were assigned more than 1 race
cases3 <- cases2 %>% 
  left_join(TwoRace, by = "UPID")  %>% 
  mutate(raceZ = if_else(is.na(MR), race, "Multi-Racial")) %>% 
  dplyr::select(-race, -MR, -NumRace) %>% 
  rename(race = raceZ)  %>% 
  mutate(race = if_else(race == "MultiRacial", "Multi-Racial", race)) %>% 
  mutate(race = if_else(is.na(race), "OtherRace", race))
# 2g.vi Remove any remaining duplicates by race
# a very small number of rows are duplicates, where all the information is the same except for patient race
cases <- cases3 %>% distinct()

# 2h Determine insurance type
PubInsKey <- c("C", "D", "E", "J", "K", "H")
cases <- cases %>% 
  mutate(PublicIns = if_else(SOURCE1 %in%(PubInsKey), "Public Insurance", "Not Public Insurance")) 

####*********************************
#### 3: Apply Exclusion Criteria ####
####*********************************

# Note that we first apply exclusion criteria based on study population 
# and then based on data availability/ accuracy 
cases.base <- cases
cases <- cases.base

# 3b Remove admissions on the same day as another MI admission 
# we assume that these are the same MI event. 
# we remove all subsequent admissions, but for the first event of the day,
# wait will not be zero
cases <- cases %>% 
  filter(wait != 0 | is.na(wait)) %>% 
  mutate(HH2 = if_else(is.na(HH), 23, as.numeric(HH))) %>%
  mutate(CaseDateRaw2 = str_sub(CaseDateRawOrig, 1, 9)) %>% 
  group_by(UPID, CaseDateRaw2) %>% 
  arrange(desc(HH2)) %>% 
  arrange(desc(MI_prim)) %>% 
  slice(0:1) %>% 
  ungroup()

# 3a Remove admissions before 2000 or after 2015
cases <- cases %>% 
  filter(YYYY > 1999) %>%
  filter(YYYY < 2016)

a <- cases %>% filter(is.na(ADMHR))
casesTot <- cases
# 3e Remove those with missing admit hour or ZIP
# or invalid ZIP (eg contains letters)
cases <- cases %>% 
  filter(!is.na(ADMHR)) %>% 
  filter(!is.na(CaseDateTime)) %>%
  mutate(zcta = as.character(zcta)) %>%
  filter(str_length(zcta) == 5) %>% 
  filter(!str_detect(zcta, "[:alpha:]")) %>% 
  filter(!str_detect(zcta, "[:ALPHA:]"))
# record the number of missing 
PercentMissing <- 100*(1-(nrow(cases) / nrow(casesTot)))
# 3c Remove entries with residence outside of NY zipcodes 
ZCTA5CE10 <- sf::st_read(paste0(raw.data.folder, "tl_2010_36_zcta510/tl_2010_36_zcta510.shp"))

ZCTA5CE10 <- ZCTA5CE10 %>% 
  rename(zcta = ZCTA5CE10) %>% 
  dplyr::select(zcta) %>% 
  as.data.frame() %>% 
  dplyr::select(zcta)
cases <- cases %>% right_join(ZCTA5CE10, by = "zcta")

# 3d Remove subjects under 18 
cases <- cases %>% filter(as.numeric(age) > 17)

# 3e Remove events that occur within 2 days after another event 
cases <- cases %>% filter(wait > 2 | is.na(wait))

# 3f Change HH for alternative window sensitivity analysis
if(CaseType == "mi" & Timing == "All23"){
  cases <- cases %>% mutate(CaseDateRaw = paste0(str_sub(CaseDateRawOrig, 1, 9), 23))
}

# 3f Keep only necesary variables 
if(CaseType == "MI"){cases$htn <- "NotMeasured"}
cases <- cases %>% 
  dplyr::select(UPID, zcta, CaseDateRaw, 
                YYYY, InOut,
                contains("DX"), contains("stroke"), contains("pr"), TPADM, STEMI, wait, 
                sex, age, race, htn, PublicIns, CaseDateRawOrig)

# 3g Save Results 
# this dataframe should have 1024335 obs of 31 variables
cases %>% fst::write_fst(paste0(intermediate.data.folder, 
                                "c_1_all_cases_unassigned_", CaseType, "_", Timing, ".fst")) 

####********************
#### 4: Data Checks #### 
####********************

# 4a Count of primary outcome
if(CaseType == "mi"){
  cases <- cases %>% 
    mutate(DXA  = if_else(is.na(DXA), "NA", DXA), 
           DX01 = if_else(is.na(DX01), "NA", DX01),
           DX02 = if_else(is.na(DX02), "NA", DX02),
           DX03 = if_else(is.na(DX03), "NA", DX03)) %>% 
    mutate(DXA_410x1 = str_sub(DXA, 1, 3) == "410" & str_sub(DXA,5,5) == "1" ) %>% 
    mutate(DXA_I21 = str_detect(DXA, "I21")) %>% 
    mutate(DX01_410x1 = str_sub(DX01, 1, 3) == "410" & str_sub(DX01,5,5) == "1" ) %>% 
    mutate(DX01_I21 = str_detect(DX01, "I21")) %>% 
    mutate(DX02_410x1 = str_sub(DX02, 1, 3) == "410" & str_sub(DX02,5,5) == "1" ) %>% 
    mutate(DX02_I21 = str_detect(DX02, "I21")) %>% 
    mutate(DX03_410x1 = str_sub(DX03, 1, 3) == "410" & str_sub(DX03,5,5) == "1" ) %>% 
    mutate(DX03_I21 = str_detect(DX03, "I21")) %>% 
    mutate(MI_prim_tf = as.numeric(DXA_410x1) + as.numeric(DX01_410x1) + as.numeric(DX02_410x1) + as.numeric(DX03_410x1) + 
            as.numeric(DXA_I21) + as.numeric(DX01_I21) + as.numeric(DX02_I21) + as.numeric(DX03_I21)) %>% 
    mutate(MI_prim = as.numeric(MI_prim_tf>0)) 
  cases <- cases %>% 
    filter(TPADM !="4" & TPADM != "5") %>% 
    filter(MI_prim == 1)
}
# 4b Calculate insurance percentages
a <- cases %>% 
  group_by(PublicIns) %>% 
  summarize(Count2 = n()) %>% 
  mutate(PercentIns = 100*Count2/nrow(cases))
PPubIns <- a$PercentIns[a$PublicIns=="Public Insurance"]

# 4c Determine number of unique individuals 
b <- cases %>% dplyr::select(UPID) %>% distinct()
NInd <- nrow(b)

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
PNYC <- nrow(filter(cases, zcta %in% nyc.zcta))

# 4e Save results
if(Timing != "All23"){
  data.frame(PercentMissing = PercentMissing,
           NInd = NInd, 
           PPubIns = PPubIns, 
           PNYC = PNYC,
           AgeMean = mean(as.numeric(cases$age), na.rm = TRUE), 
           AgeSD = sd(cases$age, na.rm =TRUE)) %>% 
    write_csv(paste0(final.data.folder, "PercentMissing_", CaseType, ".csv"))
}

# 4c Clean the environment 
#rm(list = ls())


##### Testing 
a <- cases %>% select(UPID, CaseDateRaw)
head(a)
a.main <- cases %>% select(UPID, CaseDateRaw)
head(a)
