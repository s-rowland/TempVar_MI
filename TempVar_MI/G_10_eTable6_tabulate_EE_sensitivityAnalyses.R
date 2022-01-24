# Create eTable 4 - sensitivity analyses
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 08/18/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Set Up Objects
# 2: Create Extract Estimates Function
  # 2A: Readin Estimates
  # 2B: Extract Estimates
# 3: Create eTable

####********************
#### 0: Preparation #### 
####********************

# 0a Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(dlnm)
library(stringr)
library(egg)

# 0b Source functions 
source(paste0(scripts.folder, "G_00_set_figure_size.R"))

####*********************
#### 1: Readin Table #### 
####*********************

# 1a Readin Estimates
TVList.main <- c(rep("MeanAbsFDT_24hr",length(SensitivityList)), 
                 rep("MeanFDT_24hr",length(SensitivityList)))
ERConstraintList <- c(rep("3dfevenknots",length(SensitivityList)), 
                      rep("4dfevenknots",length(SensitivityList)))
SensitivityList2 <- rep(SensitivityList , 2)

TES.list <- mapply(c, TVList.main, ERConstraintList, SensitivityList2, SIMPLIFY=FALSE)

get_est <- function(TES){
  # TVMetric <- TVList.main[1]; ERConstraint <- ERConstraintList[1]
  # Sensitivity <- SensitivityList2[1]
  read_csv(paste0(estimates.folder, "TempVar_mi_", TES[1], "_ER", TES[2],
                  "_fullpop_fullpop_", TES[3], ".csv")) %>% 
    filter(Labels %in% c("per10", "per90"))
}
est.table.list <- map(TES.list, get_est)
est.table <- bind_rows(est.table.list)

####**********************
#### 2: Wrangle Table #### 
####**********************

# 2a Parse out the effect esimates for the two exposure contrasts
est.table10 <- est.table %>% 
  filter(Labels == "per10") %>% 
  mutate(fit.pc10 = convert_to_percent(fit.or), 
         lci.pc10 = convert_to_percent(lci.or),
         uci.pc10 = convert_to_percent(uci.or)) %>%
  dplyr::select(TVMetric, Sensitivity, fit.pc10, lci.pc10, uci.pc10)
est.table90 <- est.table %>% 
  filter(Labels == "per90") %>% 
  mutate(fit.pc90 = convert_to_percent(fit.or), 
         lci.pc90 = convert_to_percent(lci.or),
         uci.pc90 = convert_to_percent(uci.or)) %>%
  dplyr::select(fit.pc90, lci.pc90, uci.pc90)
est.table <- bind_cols(est.table10, est.table90)

# 2b Rearrange data so that we have a column for 10th percentile and one for 90th
# replicates the structure of the table in the supplemental
est.table <- est.table %>% 
  mutate(EE10 = paste0(format(round(fit.pc10, 2), nsmall = 2), " (", 
                       format(round(lci.pc10, 2), nsmall = 2), ", ", 
                       format(round(uci.pc10, 2), nsmall = 2), ")"), 
         EE90 = paste0(format(round(fit.pc90, 2), nsmall = 2), " (", 
                       format(round(lci.pc90, 2), nsmall = 2), ", ", 
                       format(round(uci.pc90, 2), nsmall = 2), ")"))

# 2b Rename Metrics 
est.table <- est.table %>%
  mutate(TVMetric = 
           case_when(
             TVMetric == "MeanAbsFDT_24hr" ~ "MeanAbsFDT",
             TVMetric == "MaxAbsFDT_24hr" ~ "MaxAbsFDT",
             TVMetric == "SDT_24hr" ~ "SDT",
             TVMetric == "DRT_24hr" ~ "DTR",
             TVMetric == "SDMinMaxT_48hr" ~ "SDMinMaxT",
             TVMetric == "MeanFDT_24hr" ~ "MeanFDT",
             TVMetric == "DayDiffT_48" ~ "DiffMeanT"
           ), 
         Sensitivity = 
           case_when(
             Sensitivity == "Main" ~ "Main",
             Sensitivity == "prim_alt_410x1_A" ~ "Outcome Definition B",
             Sensitivity == "prim_alt_410xx_A" ~ "Outcome Definition C",
             Sensitivity == "prim_alt_410x01_A4" ~ "Outcome Definition D",
             Sensitivity == "prim_alt_NoReinfarction" ~ "Outcome Definition E",
             Sensitivity == "prim_6moRecurr" ~ "Outcome Definition F",
             Sensitivity == "NOmeanRH" ~ "No Mean RH Adj.",
             Sensitivity == "NOvarRH" ~ "No RH Variability Adj.",
             Sensitivity == "NORH" ~ "No RH Adj.",
             Sensitivity == "MeanT5df" ~ "5 df for Mean T",
             Sensitivity == "VarRH5df" ~ "5 df for RH Variability",
             Sensitivity == "MinT" ~ "Minimal T Adj.",
             Sensitivity == "MaxT" ~ "Maximal T Adj."))

# 2d Order Sensitivity
est.table$Sensitivity <- factor(est.table$Sensitivity, levels = SensitivityOrder) 

# 3d Save results 
est.table %>% write_csv(paste0(manuscript.folder, "eTable6_EE_SensitivityAnalyses.csv"))
