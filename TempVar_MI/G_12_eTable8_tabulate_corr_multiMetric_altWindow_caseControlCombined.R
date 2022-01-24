# Plot Correlation of Var Metrics by lag
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/21/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Readin and Organize Data
# 2: Wrangle Correlations

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
library(purrr)
library(forcats)

# 0c Source functions 
source(paste0(scripts.folder, "G_00_set_figure_size.R"))

####********************************
#### 1: Readin Correlation Data ####
####********************************

# 1a Readin Correlations 
corrdf <- read_csv(paste0(tables.folder, 
                          "Correlations_MultiMetric_AltWindow_CaseControlCombined.csv"))

####*****************************
#### 2: Wrangle Correlations ####
####*****************************

# 2a Rename Metrics 
corrdf <- corrdf %>% 
  mutate(TVMetric = 
           case_when(
             TVMetric == "MeanAbsFDT_24hr" ~ "MeanAbsFDT",
             TVMetric == "MaxAbsFDT_24hr" ~ "MaxAbsFDT",
             TVMetric == "SDT_24hr" ~ "SDT",
             TVMetric == "DRT_24hr" ~ "DTR",
             TVMetric == "SDMinMaxT_48hr" ~ "SDMinMaxT",
             TVMetric == "MeanFDT_24hr" ~ "MeanFDT",
             TVMetric == "DayDiffT_48hr" ~ "DiffMeanT"
           )) 

# 2b Order the  columns
corrdf$TVMetric <- factor(corrdf$TVMetric, levels = rev(NamesTVMetricList))

# 2c Save table
corrdf %>% 
  arrange(desc(TVMetric)) %>%
  write_csv(paste0(manuscript.folder, "eTable8_corr_AltWindow.csv"))


