# Plot Exposure-Response for individual models
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 06/30/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Create Plotting Function
  # 1A: Prepare Data
  # 1B: Readin Estimates
  # 1C: Plot Estimates
# 2: Create Plots

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
library(lubridate)

# 0f Source functions 
source(paste0(scripts.folder, "G_00_set_figure_size.R"))

####*********************************
#### 1: Create Plotting Function ####
####*********************************

# 1a Name function 
plot_TempVar_ER <- function(CaseType, TempVarMetric,  ERConstraint, SubPopVar, SubPop, Sensitivity, TempVarRange){
  #CaseType <- "mi";   TempVarMetric <- "MeanAbsFDT_24hr";   ERConstraint <- "3dfevenknots";  
  #SubPopVar <- "fullpop"; SubPop <- "fullpop"; Sensitivity <- "Main" ; TempVarRange <- "025to975"
  
  # 1c Declare ModelName
  ModelName <- paste0("TempVar", "_",CaseType, "_", TempVarMetric, "_ER", ERConstraint,
                      "_",SubPopVar, "_", SubPop, "_", Sensitivity)
  
  ####**********************
  #### 1A: Prepare Data ####
  ####**********************
  
  # 1A.a Readin Data
  dta <- fst::read.fst(paste0(final.data.folder, "prepared_cohort", "_", CaseType, 
                              "_identified", "_", "Pop", "_",  "STEMI2hrDelay", "_TVOnly.fst"))
  
  # 1A.b Rename TempVarMetric and RVarMetric 
  # 1A.b.i Determine variable names
  RHVarMetric <- str_replace(TempVarMetric, "T_", "R_")
  # 1A.b.ii 
  dta <- dta %>% 
    rename(TempVar:= !!TempVarMetric) %>% 
    rename(RHVar:= !!RHVarMetric) 
  
  # 1A.c Keep only primary MI admissions according to outcome definition
  # mi version 
  if(str_detect(Sensitivity, "main") & CaseType == "mi") {dta <- dta %>% filter(MI_prim ==1)}
  if(str_detect(Sensitivity, "prim_alt_410x1_A") & CaseType== "mi") {dta <- dta %>% filter(prim_alt_410x1_A ==1)}
  if(str_detect(Sensitivity, "prim_alt_410x01_A4") & CaseType == "mi") {dta <- dta %>% filter(prim_alt_410x01_A4 ==1)}
  if(str_detect(Sensitivity, "prim_alt_410xx_A") & CaseType == "mi") {dta <- dta %>% filter(prim_alt_410xx_A ==1)}
  if(str_detect(Sensitivity, "prim_60moRecurr") & CaseType == "mi") {
    dta <- dta %>% filter(MI_prim ==1) %>% filter(SubsequentE !="Subsequent")
  }
  
  # 1A.d Create exposure vector
  Exposure_vector <- dta %>% 
    select(TempVar) %>% 
    rename(TempVarObs = TempVar)
  
  # 1A.e Determine quantiles of data 
  TempVar.025th <- quantile(Exposure_vector$TempVar, 0.025)
  TempVar.975th <- quantile(Exposure_vector$TempVar, 0.975)
  
  ####**************************
  #### 1B: Readin Estimates ####
  ####**************************
  
  # 1B.a Declare convert to precent function 
  convert_to_percent <- function(x){
    100*(x-1)
  }
  
  # 1B.b Readin lag exposure estimates from main model
  # 1B.b.i Readin main model estimates
  est.table0 <- read_csv(paste0(estimates.folder, ModelName, ".csv"))
  # 1B.b.ii Rename variables 
  est.table0 <- est.table0 %>% 
    rename(TempVar = Contrast) %>% 
    mutate(fit.pc = convert_to_percent(fit.or), 
           lci.pc = convert_to_percent(lci.or), 
           uci.pc = convert_to_percent(uci.or)) %>% 
    dplyr::select(TempVar, fit.pc, lci.pc, uci.pc)
  
  ####************************
  #### 1C: Plot Estimates ####
  ####************************
  
  # 1C.a Create the string of the metricname, for the plot label
  TempVarMetricName <- str_sub(TempVarMetric, 0, -6)
  if(TempVarMetricName == "DayDiffT"){TempVarMetricName <- "DiffMeanT"}
  if(TempVarMetricName == "DRT"){TempVarMetricName <- "DTR"}
  # unused lettering system 
  #if(TempVarMetric == "MeanAbsFDT_24hr"){TempVarMetricName <- "A"}
  #if(TempVarMetric == "MaxAbsFDT_24hr"){TempVarMetricName <- "A"}
  #if(TempVarMetric == "SDT_24hr"){TempVarMetricName <- "B"}
  #if(TempVarMetric == "DRT_24hr"){TempVarMetricName <- "C"}
  #if(TempVarMetric == "SDMinMaxT_48hr"){TempVarMetricName <- "D"}
  #if(TempVarMetric == "MeanFDT_24hr"){TempVarMetricName <- "B"}
  #if(TempVarMetric == "DayDiffT_48hr"){TempVarMetricName <- "E"}
  
  # 1C.b Create variable of the type of infomation each row contains 
  # we will use this variable for faceting 
  est.table0 <- est.table0 %>% mutate(Info = "Change in \n MI Hospitalizations (%)")
  Exposure_vector <- Exposure_vector %>% mutate(Info = "Density")
  
  # 1C.c Combine data 
  est.table <- bind_rows(est.table0, Exposure_vector)
  
  # 1C.d Filter by range
  if(TempVarRange == "025to975"){
    est.table <- est.table %>% 
      filter(TempVar >= TempVar025th) %>%
      filter(TempVar <= TempVar975th)
  } 
  
  # 1C.e Create fake obs to manage plots
  # here we will create two fake obs to help our plotting
  # the first serves as the text label for the plot and maintains a common YMax
  # the second maintains a standard YMin 
  # by setting the Info category to Change in hosp 
  # these points only impact the E-R plot 
  est.table <- est.table %>% 
    mutate(YMaxValue = 0, LabelValue = "a")
  if(TempVarMetric !="DayDiffT_48hr"){YMin <- -10; YMax <- 15}
  if(TempVarMetric =="DayDiffT_48hr"){YMin <- -11; YMax <- 30}
  VAL <- 35
  if(TempVarMetric == "MaxAbsFDT_24hr"){VAL <- 30}
  if(TempVarMetric == "DayDiffT_48hr"){VAL <- 15}
  est.table[(nrow(est.table)+1),] <- list(est.table$TempVar[VAL], est.table$fit.pc[VAL], 
                                            est.table$lci.pc[VAL], est.table$uci.pc[VAL], 
                                            "Change in \n MI Hospitalizations (%)", NA, YMax, TempVarMetricName)
  est.table[(nrow(est.table)+1),] <- list(est.table$TempVar[VAL], est.table$fit.pc[VAL], 
                                            est.table$lci.pc[VAL], est.table$uci.pc[VAL], 
                                            "Change in \n MI Hospitalizations (%)", NA, -10, "a")
  
  est.table <- est.table %>% 
    mutate(YMaxValue = if_else(YMaxValue!= 0 , YMaxValue, NaN))
  est.table$LabelValue[est.table$LabelValue=="a"] <- NA
  
  # 1C.f Set curve color
  if(TempVarMetric %in% NamesAbsChangeOverTimeExpW){
    ActiveColor <- ColAbsChangeOverTime
  } else if(TempVarMetric %in% NamesBagOfValuesExpW){
    ActiveColor <- ColBagOfValues
  } else if(TempVarMetric %in% NamesDirChangeOverTimeExpW){
    ActiveColor <- ColDirChangeOverTime
  }
  
  # 1C.g Make exposure-response plot
  TP <- ggplot(est.table) + 
    geom_blank(aes(x = TempVar, y = YMaxValue)) + 
    geom_text(aes(x=TempVar,y=YMaxValue, label=LabelValue), size = 8)+
    geom_hline(yintercept = 0, color ="grey" ) +
    geom_vline(aes(xintercept = TempVar.025th), color = "grey42", linetype="dashed") + 
    geom_vline(aes(xintercept = TempVar.975th), color = "grey42", linetype="dashed") + 
    geom_line(aes(x = TempVar, y = fit.pc), color = ActiveColor) +    
    geom_ribbon(aes(x = TempVar, ymin= lci.pc,  ymax = uci.pc),
                fill = ActiveColor, color = NA, alpha  = AlphaCI) +
    geom_density(aes(x = TempVarObs), color = ActiveColor) + 
    facet_grid(Info~., scales="free_y", switch = "y") +
    theme(strip.placement = 'outside',
          strip.switch.pad.grid = unit('0.15', "cm"),
          strip.background = element_blank(), 
          strip.text = element_text(size = 15)) +
    xlim(min(Exposure_vector$TempVarObs), 1.05*max(Exposure_vector$TempVarObs))+
    labs(y = paste0(" ")) +
  theme(panel.background = element_rect(fill= NA, color = "black")) + 
    theme(axis.text.x = element_text(size = 17)) + 
    theme(axis.text.y = element_text(size = 17, angle = 0)) +
    theme(axis.title.x = element_text(size = 20, vjust =0, lineheight=0.25)) + 
    theme(strip.text = element_text(size = 20)) + 
    if(TempVarMetric == "MeanAbsFDT_24hr"){
      labs(x = expression(MeanAbsFDT~(degree*C))) 
    } else if(TempVarMetric == "MaxAbsFDT_24hr"){
      labs(x = expression(MaxAbsFDT~(degree*C))) 
    }else if(TempVarMetric == "SDT_24hr"){
      labs(x = expression(SDT~(degree*C))) 
    }else if(TempVarMetric == "DRT_24hr"){
      labs(x = expression(DTR~(degree*C))) 
    }else if(TempVarMetric == "SDMinMaxT_48hr"){
      labs(x = expression(SDMinMaxT~(degree*C))) 
    }else if(TempVarMetric == "MeanFDT_24hr"){
      labs(x = expression(MeanFDT~(degree*C))) 
    }else if(TempVarMetric == "DayDiffT_48hr"){
      labs(x = expression(DiffMeanT~(degree*C))) 
    }
  
  # 1C.h Save image
  tiff(paste0(manuscript.folder, "eFig5_ER_", TempVarMetric, ".tiff"), 
       height = HH.fig, width = WW.fig, res = RR.fig)
  
  print(TP)
  dev.off()
  
}

####*********************
#### 2: Create Plots ####
####*********************

# 2a Create plots
plot_TempVar_ER("mi", "MaxAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "main", "00to100")
plot_TempVar_ER("mi", "SDT_24hr", "3dfevenknots", "fullpop", "fullpop", "main", "00to100")
plot_TempVar_ER("mi", "DRT_24hr", "3dfevenknots", "fullpop", "fullpop", "main", "00to100")
plot_TempVar_ER("mi", "SDMinMaxT_48hr", "4dfevenknots", "fullpop", "fullpop", "main", "00to100")
plot_TempVar_ER("mi", "DayDiffT_48hr", "4dfevenknots", "fullpop", "fullpop", "main", "00to100")
