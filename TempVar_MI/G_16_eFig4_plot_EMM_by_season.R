# Create Seasonal
# Temperature Variability- CVD Analysis 
# Sebastian Rowland 
# Updated 16.7.2019
####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Set Up Plotting Objects 
# 2: Create Plotting Function 
  # 2A: Readin Estimates
  # 2B: Plot Estimates
# 3: Create Plots

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
library(ggpattern)

# 0f Source functions 
source(paste0(scripts.folder, "G_00_set_figure_size.R"))

####********************************
#### 1: Set Up Plotting Objects #### 
####********************************

# 1a Create plotting theme 
tema <-   theme_classic() +
  theme(plot.title = element_text(size = 25, vjust = 1, hjust =0.5)) +
  theme(axis.title.x = element_text(size = 17, vjust =0, lineheight=0.25)) + 
  theme(axis.title.y = element_text(size = 17, angle = 90, vjust= 0.5)) + 
  theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.text.y = element_text(size = 15, angle = 0))

# 1b Read in percentile table 
Percentile.Table <- read_csv(paste0(tables.folder, "Supp_10_90_Percentiles_MultiMetric.csv"))

####**********************************
#### 2: Create Plotting Function  ####
####**********************************

# 4a Name function 
plot_TempVar_ER_overlap <- function(CaseType, TempVarMetric,  ERConstraintH, ERConstraintC, ERConstraintM, 
                                    Sensitivity, YMin, YMax, leg.X, leg.Y){
  # CaseType <- "mi";   TempVarMetric <- "MeanAbsFDT_24hr";   ERConstraintH <- "3dfevenknots";  
  # SubPopVar <- "fullpop"; SubPop <- "fullpop"; Sensitivity <- "Main"
  # YMin <- -10; YMax <- 10; leg.X <- 0.25; leg.Y <- 5 ; PlotLabel <- "A"

  ####**************************
  #### 2A: Readin Estimates ####
  ####**************************
  
  # 2A.a Readin Estimates
  est.table <- mutate(read.csv(paste0(estimates.folder, "TempVar", "_",CaseType, "_", 
                                TempVarMetric, "_ER", ERConstraintH,
                                "_","Season4mo", "_", "Hottest4", "_", Sensitivity, ".csv")), 
                       Season = "Hot Months") %>% 
    bind_rows(mutate(read.csv(paste0(estimates.folder, "TempVar", "_",CaseType, "_", 
                                     TempVarMetric, "_ER", ERConstraintC,
                                     "_","Season4mo", "_", "Coldest4", "_", Sensitivity, ".csv")), 
                     Season = "Cold Months")) %>% 
    bind_rows(mutate(read.csv(paste0(estimates.folder, "TempVar", "_",CaseType, "_", 
                                     TempVarMetric, "_ER", ERConstraintM,
                                     "_","Season4mo", "_", "Moderate4", "_", Sensitivity, ".csv")), 
                     Season = "Moderate Months"))

  # 2A.b Convert effect estimates to percent change 
    est.table <- est.table %>% 
      rename(TempVar = Contrast) %>%
    mutate(fit.pc = convert_to_percent(fit.or), 
           lci.pc = convert_to_percent(lci.or), 
           uci.pc = convert_to_percent(uci.or)) 
    
  # 2A.c Set SeasonOrder
  est.table$Season <- factor(est.table$Season, levels = NamesSeasonSea) 
    
  # 2A.d Determine the percentiles of tempvar metric
  TempVar.025th <- Percentile.Table$TempVar.25th[Percentile.Table$TempVarMetric == TempVarMetric]
  TempVar.975th <- Percentile.Table$TempVar.975th[Percentile.Table$TempVarMetric == TempVarMetric]
    
  ####************************
  #### 2B: Plot Estimates ####
  ####************************
    
  # 2B.a Create manuscript-friendly name for TV metric
  TempVarMetricName <- str_sub(TempVarMetric, 0, -6)
  if(TempVarMetricName == "DayDiffT"){TempVarMetricName <- "DiffMeanT"}
  if(TempVarMetricName == "DRT"){TempVarMetricName <- "DTR"}
  
  # 2B.b Set locations for the TV metric names 
  VAL <- 0.15
  if(TempVarMetric == "MeanAbsFDT_24hr"){VAL <- 0.3}
  ActiveX <- min(est.table$TempVar) + VAL * diff(range(est.table$TempVar))
  ActiveY <- min(est.table$lci.pc) + 0.95 * (max(est.table$uci.pc) - min(est.table$lci.pc))
  
  # 2B.c Make Er Plot object
  TP <- ggplot(est.table, aes(x = TempVar)) + 
    geom_hline(yintercept=0, color ="grey" ) + 
    geom_vline(aes(xintercept = TempVar.025th), color = "grey", linetype="dashed") + 
    geom_vline(aes(xintercept = TempVar.975th), color = "grey", linetype="dashed") + 
    geom_ribbon(aes(x = TempVar, ymin= lci.pc,  ymax = uci.pc, fill = Season), 
                color = NA, alpha  = 0.1)+
    geom_line(aes(y = fit.pc, color = Season), size = 2)+ 
    scale_fill_manual(values = ColSeasonSea) + 
    scale_color_manual(values = ColSeasonSea) + 
    labs(y = paste0("Change in \n MI Hospitalizations (%)")) + 
    tema + 
    ylim(YMin, YMax) + 
    xlim(min(est.table$TempVar), 1.05*max(est.table$TempVar))+ 
    scale_y_continuous(breaks = seq(YMin, YMax, by = 5)) +
    annotate(geom = "text", label = TempVarMetricName, x = ActiveX, y = ActiveY, 
             size = 6) +
    theme(strip.background = element_blank(), 
          strip.text.x = element_blank(),
          legend.position = c(leg.X, leg.Y),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11),
          legend.key.size = unit(0.5, "cm"),
          panel.background = element_rect(fill= NA, color = "black"),
          legend.background = element_rect(fill="transparent")) +
    if(TempVarMetric == "MeanAbsFDT_24hr"){
      labs(x = expression(MeanAbsFDT~(degree*C)))  
    } else if(TempVarMetric == "MeanFDT_24hr"){
      labs(x = expression(MeanFDT~(degree*C))) 
    }
  
 # 2B.d Output Plot 
  tiff(paste0(manuscript.folder, "eFig4_Seasonal_EMM_", TempVarMetric, ".tiff"), 
       height = HH.fig, width = WW.fig, res = RR.fig)
  print(TP)
  dev.off()
  
}

####*********************
#### 3: Create Plots ####
####*********************

# 3a Create plots 
plot_TempVar_ER_overlap("mi", "MeanAbsFDT_24hr", "3dfevenknots", "3dfevenknots", 
                        "4dfevenknots","Main",  -15, 15, 2, 2)
plot_TempVar_ER_overlap("mi", "MeanFDT_24hr", "3dfevenknots", "3dfevenknots", 
                        "3dfevenknots", "Main", -15, 40, 0.5, 0.9)
