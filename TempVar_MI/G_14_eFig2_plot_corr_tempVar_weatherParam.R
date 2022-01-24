# Plot Cross Metric Correlations
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/16/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Readin and Organize Data
# 2: Wrangle Data
# 3: Create Plots

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
library(purrr)
library(forcats)
library(ggplot2)
library(viridis)
library(egg)

# 0b Set up environment
# 0b Declare directories
project.folder <- "H:/Temp_CVD_Analysis/"
final.data.folder <- paste0(project.folder, "DataPrep/Data/Final_Data/")
analysis.folder <- paste0(project.folder,"TempVar_mi/")
scripts.folder <- paste0(analysis.folder, "Scripts/")
outputs.folder <- paste0(analysis.folder, "Outputs/")
models.folder   <- paste0(outputs.folder, "Models/")
estimates.folder <- paste0(outputs.folder, "Estimates/")
plots.folder <- paste0(outputs.folder, "Plots/")
tables.folder <- paste0(outputs.folder, "Tables/")
manuscript.folder <- paste0(outputs.folder, "Manuscript/")

# 0c Source functions 
source(paste0(scripts.folder, "G_00_set_figure_size.R"))

####********************************
#### 1: Readin Correlation Data ####
####********************************

# 1a Readin Correlations 
corrdf <- read_csv(paste0(tables.folder, "Correlations_PrimaryMetric_Weather_", "AllYear", ".csv")) %>% 
  bind_rows(read_csv(paste0(tables.folder, "Correlations_PrimaryMetric_Weather_", "Hottest4", ".csv"))) %>% 
  bind_rows(read_csv(paste0(tables.folder, "Correlations_PrimaryMetric_Weather_", "Coldest4", ".csv"))) %>% 
  bind_rows(read_csv(paste0(tables.folder, "Correlations_PrimaryMetric_Weather_", "Moderate4", ".csv")))

####*********************
#### 2: Wrangle Data ####
####*********************

# 2a Rename Metrics 
corrdf <- corrdf %>% 
  mutate(VarName1 = case_when(
    VarName1 == "MeanAbsFDT_24hr" ~ "MeanAbsFDT",
    VarName1 == "MaxAbsFDT_24hr" ~ "MaxAbsFDT",
    VarName1 == "SDT_24hr" ~ "SDT",
    VarName1 == "DRT_24hr" ~ "DTR",
    VarName1 == "SDMinMaxT_48hr" ~ "SDMinMaxT",
    VarName1 == "MeanFDT_24hr" ~ "MeanFDT",
    VarName1 == "DayDiffT_48hr" ~ "DiffMeanT"),
    VarName2 = case_when(
      VarName2 == "MeanT" ~ "Mean T",
      VarName2 == "MinT" ~ "Minimal T",
      VarName2 == "MaxT" ~ "Maximal T",
      VarName2 == "MeanR" ~ "Mean RH",
      VarName2 == "VarR" ~ "RH Variability"), 
    Season = case_when(
      Season == "AllYear" ~ "All Months",
      Season == "Hottest4" ~ "Hot Months",
      Season == "Coldest4" ~ "Cold Months",
      Season == "Moderate4" ~ "Moderate Months"))

# 2b Order the TV Metrics
# we need to add this fake entry with another metric "a" 
# so that we can laterr remvoe it from the plot 
# and have an empty square 
corrdf[141, ] <- list("a", "Mean T", "All Months", 0)
NamesTVMetricList2 <- c(NamesAbsChangeOverTime, "a", NamesBagOfValues, NamesDirChangeOverTime)
corrdf$VarName1 <- factor(corrdf$VarName1, levels = NamesTVMetricList2) 

corrdf$VarName2 <- factor(corrdf$VarName2, 
                          levels = c("RH Variability", "Mean RH","Maximal T","Minimal T","Mean T"))

# 2c Assign TV Categories 
corrdf <- corrdf %>% 
  mutate(Category = case_when(
    VarName1 %in% NamesAbsChangeOverTime  ~ "Abs-Change-Over-Time",
    VarName1 %in% NamesBagOfValues  ~ "Bag-Of-Values",
    VarName1 %in% NamesDirChangeOverTime ~ "Dir-Change-Over-Time"
  ))

# 2d Rename variable 
corrdf <- corrdf %>% 
  rename(Correlation = MeanCorr)


####*********************
#### 3: Create Plots ####
####*********************

# 3a Create plot object 
p <- corrdf %>% 
  ggplot(aes(Season, VarName2)) +
  geom_tile(aes(fill = Correlation), color = "white") +
  geom_text(aes(label = format(round(Correlation, 1), nsmall = 1)), color = "black", size = 3) +
  geom_vline(aes(xintercept = 1.5), color = "black", size = 0.6) + 
  #  scale_fill_gradientn(colours = c("violet", "pink", "white", "limegreen", "deepskyblue"), 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab") +
  theme_minimal(base_size = 15) + # minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, size = 10, hjust = 1),
        axis.text.y = element_text(vjust = 0.3, size = 10, hjust = 1),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), 
        strip.text = element_text(size = 12)) +
  theme(plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm")) +
  coord_fixed() + 
  facet_wrap(~VarName1) + 
  labs(x = "", y = "")  + 
  #theme_classic() + 
  theme(axis.text.x = element_text(angle = 90), 
  axis.ticks = element_blank(), 
  panel.grid = element_blank())


# 3b Wrangle ploting object
# This wrangling puts the TV metrics in the right order. 
# While you could do facet with two variables to manually contol columns and rows 
# it doesn't do the axes correctly for that thirdentry (SDMinMaxT)

library(gridBase)  
library(gridExtra)
g <- ggplotGrob(p)
# get the grobs that must be removed
rm_grobs <- g$layout$name %in% c("panel-1-3", "panel-3-3", "strip-t-3-1", "strip-t-3-3")
# remove grobs
g$grobs[rm_grobs] <- NULL
g$layout <- g$layout[!rm_grobs, ]
## move axis closer to panel
g$layout[g$layout$name == "axis-b-3-3", c("t", "b")] = c(14.5, 14.5)
grid::grid.newpage()

# 3c Create image 
tiff(paste0(manuscript.folder, "eFig2_TempVarMetric_Weather_Correlations.tiff"), 
     height = HH.fig, width = WW.fig, res = RR.fig)

grid::grid.draw(g)

dev.off()


