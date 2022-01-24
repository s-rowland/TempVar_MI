# Function: Analyze association between TempVar and CVD
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/15/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Begin Function
# 2: Prepare Data
# 3: Stratify Data 
# 4: Fit Health Model 
# 5: Save Results

####**************
#### N: Notes #### 
####**************

# Na Description
# This code is the function for fitting all of the models 
# for the analysis 
# except for the negative control exposure sensitivity analysis 
# By using the same code, we can ensure that the exact same dataprocessing 
# and output is applied to each model, 
# without updating many individual codes 
# The code outputs a) the model, b) estimate for the mean to 10th and 90th percentile 
# c) for penalized spline models only, an ER plot.

# Nb OneBasis 
# I am using OneBasis as a convinent wrapper for the natural spline

# Nc ModelName 
# A critical object in this project is the ModelName 
# This identifiers provides all the unique information about a model 
# When you look a file name, you know exactly what the model is about

####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
library(readr)
library(dplyr)
library(lubridate)
library(magrittr)
library(tidyr)
library(survival)
library(stringr)
library(splines)
library(mgcv)
library(nlme)
library(pspline)
library(dlnm)
library(ggplot2)

####***********************
#### 1: Begin Function ####
####***********************

# 1a Declare function
analyze_TempVar_NegContExp <- function(CaseType,  TempVarMetric, ERConstraint, SubPopVar, SubPop, Sensitivity){
  #NLDASWeight <- "Pop"; Timing <- "STEMI2hrDelay"
  #CaseType <- "mi";    TempVarMetric <-"MeanAbsFDT_24hr";   ERConstraint <- "3dfevenknots";  
  #SubPopVar <- "fullpop"; SubPop <- "fullpop"; Sensitivity <- "Main"
  
  # 1b Create ModelName
  ModelName <- paste0("TempVar", "_",CaseType, "_", TempVarMetric, "_ER", ERConstraint,
                      "_",SubPopVar, "_", SubPop, "_", Sensitivity)
  
  # 1c Set dataset parameters 
  NLDASWeight <- "Pop"
  if(Sensitivity != "All23"){Timing <- "STEMI2hrDelay"}
  if(Sensitivity == "All23"){Timing <- "All23"}
  
  ####*********************
  #### 2: Prepare Data ####
  ####*********************
  
  # 2a Readin Data
  dta <- fst::read.fst(paste0(final.data.folder, "prepared_cohort", "_", CaseType,
                              "_deidentified", "_", NLDASWeight, "_", Timing, "_TVOnly_Lead.fst"))  %>% 
    mutate(MI_prim = 1)
  
  # 2b Rename TempVarMetric and RVarMetric 
  # this allows us to use the same analysis code for each variability metric
  # 2b.i Create names
  RHVarMetric <- str_replace(TempVarMetric, "T_", "R_")
  TempVarMetricLead <- paste0(TempVarMetric, "Lead")
  # 2b.ii Rename variable
  dta <- dta %>% 
    rename(TempVar:= !!TempVarMetric, 
           TempVarLead := !!TempVarMetricLead,
           RHVar:= !!RHVarMetric) 
  
  # 2c Create weather variables that correspond to exposure window
  # 2c.i Determine hours of exposure window
  NumHr <- as.numeric(str_remove_all(str_remove_all(TempVarMetric, "[A-z]"), "_") )
  # 2c.ii Create names 
  # we use minT and maxT in sensitivity analyses
  MeanTName <- paste0("MeanT_", NumHr, "hr")
  MeanRName <- paste0("MeanR_", NumHr, "hr")
  MinTName <- paste0("MinT_", NumHr, "hr")
  MaxTName <- paste0("MaxT_", NumHr, "hr")
  # 2c.iii Rename variable
  dta <- dta %>% 
    rename(MeanT := !!MeanTName,
           MeanR := !!MeanRName, 
           MinT := !!MinTName,
           MaxT := !!MaxTName)
  
  # 2d Keep only primary MI admissions according to outcome definition
  # we use the main primary outcome except for sensitivity analyses
  # the use different outcome criteria
  # 2d.i mi version 
  # this logical statement captures all analyses that  
  # use the main outcome definition
  if(!str_detect(Sensitivity, "prim") & CaseType == "mi"){
    dta <- dta %>% filter(MI_prim == 1)
  } else if(str_detect(Sensitivity, "prim_alt_410x1_A") & CaseType == "mi"){
    dta <- dta %>% filter(prim_alt_410x1_A == 1)
  } else if(str_detect(Sensitivity, "prim_alt_410x01_A4") & CaseType == "mi"){
    dta <- dta %>% filter(prim_alt_410x01_A4 == 1)
  } else if(str_detect(Sensitivity, "prim_alt_410xx_A") & CaseType == "mi"){
    dta <- dta %>% filter(prim_alt_410xx_A == 1)
  } else if(str_detect(Sensitivity, "prim_60moRecurr") & CaseType == "mi"){
    dta <- dta %>% filter(MI_prim == 1) %>% filter(SubsequentE != "Subsequent")
  } else if(str_detect(Sensitivity, "prim_alt_NoReinfarction") & CaseType == "mi"){
    dta <- dta %>% filter(prim_alt_NoReinfarction == 1)
  }
  
  # 2e Calculate mean and range of TempVar 
  # There are used for the crosspred() function 
  # although the mean is based on the all-year mean,
  # which can be changed for seasonal analyses
  mean.TempVar <- mean(dta$TempVar, na.rm = TRUE)
  min.TempVar <- min(dta$TempVar, na.rm = TRUE)
  max.TempVar <- max(dta$TempVar, na.rm = TRUE)
  mean.TempVarLead <- mean(dta$TempVarLead, na.rm = TRUE)
  min.TempVarLead <- min(dta$TempVarLead, na.rm = TRUE)
  max.TempVarLead <- max(dta$TempVarLead, na.rm = TRUE)
  
  ####***************************
  #### 4: Fit Health Models  ####
  ####***************************
  
  dta <- dta %>% filter(!is.na(TempVar) & !is.na(TempVarLead))
  
  # 4a Create cross-basis terms for sdt
  # by using cross-basis to represent the nonlinear terms, 
  # we can more easily wrangle results
  # 4a.i Set ER Constraint
  ERDF <- as.numeric(str_remove_all(ERConstraint, "[A-z]"))
  
  # 4a.iii Create crossbasis
  # the negative control exposure should ahve the same flexibility as the main model
  if(str_detect(ERConstraint, "evenknots")){
    ob.TempVar <- onebasis(dta$TempVar, 
                           fun="ns", df = ERDF)
    ob.TempVarLead  <- onebasis(dta$TempVarLead, 
                                fun="ns", df = ERDF)
  }
  
  # 4b Fit Models
  # 4b.i Main model 
  if(Sensitivity == "NegContExp") {
    mod.TempVar <- coxph(Surv(TimetoEvent, Case) ~ ob.TempVar + 
                           ns(TempVarLead, ERDF) + 
                           ns(MeanT,4) + ns(MeanR,4) +ns(RHVar, 4) +
                           strata(EventID),
                         dta, ties = "efron")
    mod.TempVarLead <- coxph(Surv(TimetoEvent, Case) ~ ns(TempVar, ERDF) + 
                               ob.TempVarLead + 
                               ns(MeanT,4) + ns(MeanR,4) +ns(RHVar, 4) +
                               strata(EventID),
                             dta, ties = "efron")
  }
  
  ####**********************
  #### 5: Save Results  ####
  ####**********************
  
  # 5a Save Models 
  mod.TempVar %>% saveRDS(paste0(models.folder, ModelName, "_TempVarLag.RDS"))
  mod.TempVarLead %>% saveRDS(paste0(models.folder, ModelName, "_TempVarLead.RDS"))
  
  ####**********************************************************
  #### 6: Save Estimates of Association with Main Exposure  ####
  ####**********************************************************
  
  # 6a Create predictions
  pred <- crosspred(ob.TempVar, mod.TempVar,
                    cen = mean.TempVar,
                    dataset, 
                    at=seq(min.TempVar, max.TempVar, length.out = 100))
  
  # 6b Extract coefficient fit  
  fit.table <- as.data.frame(pred$matRRfit)  
  colnames(fit.table) <- paste0("fit.or")
  fit.table <- fit.table %>%   mutate(TempVar = as.numeric(row.names(fit.table)))
  
  # 6b Extract 95% CI  
  lci.table <- as.data.frame(pred$matRRlow)  
  colnames(lci.table) <- paste0("lci.or")
  
  uci.table <- as.data.frame(pred$matRRhigh)  
  colnames(uci.table) <- paste0("uci.or")
  
  # 6d Combine fit and se 
  # note that all OR are relative to the mean of that variability metric
  pred.table <- bind_cols(fit.table, lci.table, uci.table)
  
  # 6e Save prediction tables
  pred.table %>% write_csv(paste0(estimates.folder, ModelName, "_TempVarLag.csv"))
  
  ####*****************************************************************
  #### 7: Save Estimates of Association with Neg Control Exposure  ####
  ####*****************************************************************
  
  # 7a Create predictions
  pred <- crosspred(ob.TempVarLead, mod.TempVarLead,
                    cen = mean.TempVarLead,
                    dataset, 
                    at = seq(min.TempVarLead, max.TempVarLead, length.out = 100))
  
  # 7b Extract coefficient fit  
  fit.table <- as.data.frame(pred$matRRfit)  
  colnames(fit.table) <- paste0("fit.or")
  fit.table <- fit.table %>% mutate(TempVar = as.numeric(row.names(fit.table)))
  
  # 7b Extract 95% CI  
  lci.table <- as.data.frame(pred$matRRlow)  
  colnames(lci.table) <- paste0("lci.or")
  uci.table <- as.data.frame(pred$matRRhigh)  
  colnames(uci.table) <- paste0("uci.or")
  
  # 7d Combine fit and se 
  # note that all OR are relative to the mean of that variability metric
  pred.table <- bind_cols(fit.table, lci.table, uci.table)
  
  # 7e Save prediction tables
  pred.table %>% write_csv(paste0(estimates.folder, ModelName, "_TempVarLead.csv"))
}

