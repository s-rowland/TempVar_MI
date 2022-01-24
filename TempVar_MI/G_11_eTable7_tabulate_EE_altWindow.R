# Create eTable 7: EffectEstimates of alternative exposure window
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 11/24/2020

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
library(stringr)

# 0b Source functions 
source(paste0(scripts.folder, "G_00_set_figure_size.R"))

####*********************
#### 1: Readin Table #### 
####*********************

# 1a Readin Estimates
TVList2 <- c(rep(NamesTVListExpW,length(ExpWList)))

ERConstraintList <- c("3dfevenknots", "3dfevenknots",
                      "3dfevenknots", "3dfevenknots", "4dfevenknots", 
                      "4dfevenknots", "4dfevenknots")
ERConstraintList2 <- c(rep(ERConstraintList,length(ExpWList)))
ExpWList2 <- rep(ExpWList, length(NamesTVListExpW))

TES.list <- mapply(c, TVList2, ERConstraintList2, ExpWList2, SIMPLIFY=FALSE)

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
             TVMetric == "DayDiffT_48hr" ~ "DiffMeanT"
           ))

# 2d Create exposure window variable 
est.table <- est.table %>% 
  mutate(ExposureWindow = if_else(Sensitivity == "Main", "Time of Event", "Midnight"))

# 2e Order ExposureWindow variable
est.table$ExposureWindow <- factor(est.table$ExposureWindow,levels = ExpWOrder)

# 2c Order TempVar Metrics
est.table$TVMetric <- factor(est.table$TVMetric, levels = rev(NamesTVMetricList)) 

# 2f Save results 
est.table %>% 
  arrange(desc(ExposureWindow)) %>% 
  arrange(desc(TVMetric)) %>%
  write_csv(paste0(manuscript.folder, "eTable7_EE_AlternativeWindow.csv"))
