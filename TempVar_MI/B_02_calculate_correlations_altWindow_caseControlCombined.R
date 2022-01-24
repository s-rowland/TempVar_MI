# Plot Correlation of lagged metrics with CaseControl data
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/14/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Readin and Organize Data
# 2: Wrangle Data
# 3: Create Plots

####**************
#### N: Notes #### 
####**************

# N.a users Cannot run this codes
# In order to run this script you need the original casedatetime
# because we use that variable to match the case and control periods 
# from the two datasets 
# we removed the casedatetime variable as part of deidentifying the data.

####********************
#### 0: Preparation #### 
####********************

# 0a Declare directories
if (!exists('Ran_0_00')){
  here::i_am('README.md')
  source(here::here('TempVar_mi', 'Scripts',
                    '0_00_create_folder_structure.R'))
  }

####********************************
#### 1: Readin Correlation Data ####
####********************************

# 1a Readin data
dta.main.full <- fst::read.fst(paste0(final.data.folder, "prepared_cohort", "_", "mi",
                                      "_identified", "_", "Pop", "_", "STEMI2hrDelay", ".fst"))

dta.main <- dta.main.full %>% 
  mutate(HourNameOrig = paste0(CaseDateRawOrig, "_", UPID, "_", str_sub(HourName0, -7, -1)))%>%
  dplyr::select(HourNameOrig, 
                MeanAbsFDT_24hr, MaxAbsFDT_24hr, SDT_24hr, 
                DRT_24hr, SDMinMaxT_48hr, MeanFDT_24hr, DayDiffT_48hr) %>% 
  rename(MeanAbsFDT_24hr_Main = MeanAbsFDT_24hr, 
         MaxAbsFDT_24hr_Main = MaxAbsFDT_24hr, 
         SDT_24hr_Main = SDT_24hr, 
         DRT_24hr_Main = DRT_24hr, 
         SDMinMaxT_48hr_Main = SDMinMaxT_48hr, 
         MeanFDT_24hr_Main = MeanFDT_24hr, 
         DayDiffT_48hr_Main = DayDiffT_48hr)

dta.23.full <- bind_rows(fst::read.fst(paste0(final.data.folder, "prepared_cohort", "_", "mi",
                                              "_identified", "_", "Pop", "_", "All23", ".fst")))
dta.23 <- dta.23.full %>% 
  mutate(HourNameOrig = paste0(CaseDateRawOrig, "_", UPID, "_", str_sub(HourName0, -7, -1)))%>%
  dplyr::select(HourNameOrig, 
                MeanAbsFDT_24hr, MaxAbsFDT_24hr, SDT_24hr, 
                DRT_24hr, SDMinMaxT_48hr, MeanFDT_24hr, DayDiffT_48hr) %>% 
  rename(MeanAbsFDT_24hr_All23 = MeanAbsFDT_24hr, 
         MaxAbsFDT_24hr_All23 = MaxAbsFDT_24hr, 
         SDT_24hr_All23 = SDT_24hr, 
         DRT_24hr_All23 = DRT_24hr, 
         SDMinMaxT_48hr_All23 = SDMinMaxT_48hr, 
         MeanFDT_24hr_All23 = MeanFDT_24hr, 
         DayDiffT_48hr_All23 = DayDiffT_48hr)

# 1b Combine Data
dta <- dta.main %>%
  inner_join(dta.23, by = "HourNameOrig") 

####*******************************
#### 2: Calculate Correlations ####
####*******************************

# 2a Calculate correlations for case periods
corr.dta <- data.frame(corr = c(
  cor(dta$MeanAbsFDT_24hr_Main, dta$MeanAbsFDT_24hr_All23),
  cor(dta$MaxAbsFDT_24hr_Main, dta$MaxAbsFDT_24hr_All23),
  cor(dta$SDT_24hr_Main, dta$SDT_24hr_All23),
  cor(dta$DRT_24hr_Main, dta$DRT_24hr_All23),
  cor(dta$SDMinMaxT_48hr_Main, dta$SDMinMaxT_48hr_All23),
  cor(dta$MeanFDT_24hr_Main, dta$MeanFDT_24hr_All23),
  cor(dta$DayDiffT_48hr_Main, dta$DayDiffT_48hr_All23)
), 
TempVarMetric = c("MeanAbsFDT_24hr", "MaxAbsFDT_24hr","SDT_24hr", 
                  "DRT_24hr", "SDMinMaxT_48hr",  
                  "MeanFDT_24hr", "DayDiffT_48hr"), 
StartHour1 = rep("All23", 7)
) 


####*********************
#### 3: Save Results ####
####*********************

# 3c Save results
corr.dta %>% write_csv(paste0(tables.folder, 
                              "Correlations_MultiMetric_AltWindow_CaseControlCombined.csv"))

