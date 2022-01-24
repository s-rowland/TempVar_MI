# Create etable 1: AIC of Main Models 
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 08/18/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Create Table of AIC

####********************
#### 0: Preparation #### 
####********************

# 0a Load packages
library(survival)
library(readr)
library(dplyr)
library(ggplot2)
library(dlnm)
library(stringr)
library(egg)

####***********************
#### 1: Move AIC Table ####
####***********************

# we already create the AIC table in C_01, 
# so here we are just transporting it ot the manuscripts folder
aic_table <- read_csv(paste0(tables.folder, "AIC_Main_Models.csv"))

# 1i Save table 
aic_table  %>% 
  write_csv(paste0(manuscript.folder, "eTable3_MainModels_AIC.csv"))