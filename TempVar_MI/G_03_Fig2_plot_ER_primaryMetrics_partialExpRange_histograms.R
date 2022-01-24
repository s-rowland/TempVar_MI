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
  
  # 1b Declare ModelName
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
  TempVar025th <- quantile(Exposure_vector$TempVar, 0.025)
  TempVar975th <- quantile(Exposure_vector$TempVar, 0.975)
  
  # 1A.f Categorize hourly temperature#
  obsTV <- Exposure_vector %>% 
   # filter(TempVar >= TempVar025th) %>%
   #filter(TempVar <= TempVar975th) %>%
    mutate(TVCate = cut(TempVarObs, 50)) %>% 
    group_by(TVCate) %>%
    summarize(Count = n()) %>%
    mutate(Frequency = 100* Count/nrow(Exposure_vector), 
           CountPerK = Count / 1000, 
           TVCate = str_sub(TVCate, 2)) %>% 
    separate(TVCate, c("TVCate", "nada"), sep =',') %>% 
    select(-nada) %>% 
    mutate(TVCate = as.numeric(TVCate))

  ####**************************
  #### 1B: Readin Estimates ####
  ####**************************
  
  # 1B.a Declare convert to precent function 
  convert_to_percent <- function(x){
    100*(x-1)
  }
  
  # 1B.b Readin lag exposure estimates from main model
  # 1B.b.i Readin main model estimates
  est.table <- read_csv(paste0(estimates.folder, ModelName, ".csv"))
  # 1B.b.ii Rename variables 
  est.table <- est.table %>% 
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

  # 1C.d Filter by range
  if(TempVarRange == "025to975"){
    est.table <- est.table %>% 
      filter(TempVar >= TempVar025th) %>%
      filter(TempVar <= TempVar975th) 
  }
  
  # 1C.f Set curve color
  if(TempVarMetric %in% NamesAbsChangeOverTimeExpW){
    ActiveColor <- ColAbsChangeOverTime
  } else if(TempVarMetric %in% NamesBagOfValuesExpW){
    ActiveColor <- ColBagOfValues
  } else if(TempVarMetric %in% NamesDirChangeOverTimeExpW){
    ActiveColor <- ColDirChangeOverTime
  }
  
  if(TempVarMetric == "MeanAbsFDT_24hr"){
    XLabel <- labs(x = expression(MeanAbsFDT~(degree*C)))
    Xseq <- c(0, 0.5, 1, 1.5); Yseq <- c(-5, -2.5, 0, 2.5, 5, 7.5, 10)
    }
  if(TempVarMetric == "MeanFDT_24hr"){
    XLabel <- labs(x = expression(MeanFDT~(degree*C)))
    Xseq <- c(-1, -0.5, 0, 0.5); Yseq <- c(-10, -5, 0, 5, 10)
    }
  
  
  tema <- theme(panel.background = element_rect(fill= NA, color = "black"),
                    axis.text.x = element_text(size = 10),
                    axis.text.y = element_text(size = 10, angle = 0),
                    axis.title.x = element_text(size = 12, vjust =0, lineheight=0.25),
                    axis.title.y = element_text(size = 12, vjust =0, lineheight=0.25)) 
  # 1C.g Make exposure-response plot
  TP.a <- ggplot(est.table) + 
    geom_hline(yintercept = 0, color ="grey" ) +
    geom_vline(aes(xintercept = TempVar025th), color = "grey42", alpha = 0.5) + 
    geom_vline(aes(xintercept = TempVar975th), color = "grey42", alpha = 0.5) + 
    geom_line(aes(x = TempVar, y = fit.pc), color = ActiveColor) +    
    geom_ribbon(aes(x = TempVar, ymin= lci.pc,  ymax = uci.pc),
                fill = ActiveColor, color = NA, alpha  = AlphaCI) +
    scale_x_continuous(breaks = Xseq) +
    scale_y_continuous(breaks = Yseq) +
    labs(y = expression("Change in MI Hospitalizations (%)"), 
         x = " ") +
    tema
  
  
  TP.b <- obsTV %>% 
    ggplot(aes(TVCate, CountPerK)) + 
    geom_bar(stat = 'identity')  + 
    labs(y = "Count Times Thousand") + 
    scale_x_continuous(breaks = Xseq) +
    XLabel + 
    tema
  
  # 1C.h Return panels
  list(TP.a, TP.b)
  #tiff(paste0(plots.folder, "Fig2_ER_partialExpRange_presentationSpecial_", TempVarMetric, ".tiff"), 
   #    height = HH.fig, width = WW.fig*1.25, res = RR.fig)
  #print(cowplot::plot_grid(TP.a, TP.b, ncol = 1))
  #dev.off()
}

####*********************
#### 2: Create Plots ####
####*********************

# 2a Create plots
TP.ab <- plot_TempVar_ER("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "main", "00to100")
TP.cd <- plot_TempVar_ER("mi", "MeanFDT_24hr", "4dfevenknots", "fullpop", "fullpop", "main", "00to100")
 
 
tiff(paste0(plots.folder, "Fig2_ER_partialExpRange_presentationSpecial.tiff"), 
    height = HH.fig, width = WW.fig*1.25, res = RR.fig)

cowplot::plot_grid(TP.ab[[1]],TP.cd[[1]], TP.ab[[2]],TP.cd[[2]], align = "v", ncol = 2)
dev.off()


