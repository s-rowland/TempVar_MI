# Create eTable 4: EffectEstimates of alternative variability metrics
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 08/18/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Readin Table
# 2: Wrangle Table

####********************
#### 0: Preparation #### 
####********************

# 0a Load packages
library(readr)
library(dplyr)
library(stringr)

# 0f Source functions 
source(paste0(scripts.folder, "G_00_set_figure_size.R"))

####*********************
#### 1: Readin Table #### 
####*********************

# 1a Readin Estimates
ERConstraintList <- c("3dfevenknots", "3dfevenknots",
                      "3dfevenknots", "3dfevenknots", "4dfevenknots", 
                      "4dfevenknots", "4dfevenknots")
get_est <- function(TempVarMetric, ERConstraint){
  read_csv(paste0(estimates.folder, "TempVar_mi_", TempVarMetric, "_ER", ERConstraint,
                  "_fullpop_fullpop_Main", ".csv")) %>% 
    filter(Labels %in% c("per10", "per90"))
}
est.table.list <- map2(NamesTVListExpW, ERConstraintList, get_est)
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
  dplyr::select(TVMetric, fit.pc10, lci.pc10, uci.pc10)
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
# 2c Wrangle TVMetric variable for manuscript order
# 2c.i Rename Metrics 
est.table <- est.table %>%
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
# 2c.ii Order TempVar Metrics
est.table$TVMetric <- factor(est.table$TVMetric, levels = rev(NamesTVMetricList)) 

# 2d Save results 
est.table %>% 
  dplyr::select(TVMetric, EE10, EE90) %>%
  write_csv(paste0(manuscript.folder, "eTable5_EE_AlternativeMetrics.csv"))
