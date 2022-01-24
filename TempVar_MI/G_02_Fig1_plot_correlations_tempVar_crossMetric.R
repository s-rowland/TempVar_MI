# Plot Cross Metric Correlations
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 06/04/2020

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
library(stringr)
library(purrr)
library(forcats)
library(ggplot2)
library(viridis)
library(egg)
library(RColorBrewer)

# 0b Source functions 
source(paste0(scripts.folder, "G_00_set_figure_size.R"))

####********************************
#### 1: Readin Correlation Data ####
####********************************

# 1a Readin Correlations 
corrdf <- read_csv(paste0(tables.folder, "Correlations_TempVar_CrossMetric_", "AllYear", ".csv")) 

# 1b Keep only the metrics of interest
corrdf <- corrdf %>% 
  filter(VarName1 %in% NamesTVListExpW) %>% 
  filter(VarName2 %in% NamesTVListExpW) 
  
####*********************
#### 2: Wrangle Data ####
####*********************

# 2a Rename Metrics 
corrdf <- corrdf %>% 
  mutate(VarName1 = 
           case_when(
             VarName1 == "MeanAbsFDT_24hr" ~ "MeanAbsFDT",
             VarName1 == "MaxAbsFDT_24hr" ~ "MaxAbsFDT",
             VarName1 == "SDT_24hr" ~ "SDT",
             VarName1 == "DRT_24hr" ~ "DTR",
             VarName1 == "SDMinMaxT_48hr" ~ "SDMinMaxT",
             VarName1 == "MeanFDT_24hr" ~ "MeanFDT",
             VarName1 == "DayDiffT_48hr" ~ "DiffMeanT"
           )) %>% 
  mutate(VarName2 = 
           case_when(
             VarName2 == "MeanAbsFDT_24hr" ~ "MeanAbsFDT",
             VarName2 == "MaxAbsFDT_24hr" ~ "MaxAbsFDT",
             VarName2 == "SDT_24hr" ~ "SDT",
             VarName2 == "DRT_24hr" ~ "DTR",
             VarName2 == "SDMinMaxT_48hr" ~ "SDMinMaxT",
             VarName2 == "MeanFDT_24hr" ~ "MeanFDT",
             VarName2 == "DayDiffT_48hr" ~ "DiffMeanT"
           ))

# 2b Order the TV Metrics
corrdf$VarName1 <- factor(corrdf$VarName1, levels = NamesTVMetricList) 
corrdf$VarName2 <- factor(corrdf$VarName2, levels = rev(NamesTVMetricList))

# 2e Rename variable 
corrdf <- corrdf %>% 
  rename(Correlation = MeanCorr)

# 2f Assign TV Categories 
corrdf <- corrdf %>% 
  mutate(Category = case_when(
    VarName1 %in% NamesAbsChangeOverTime & VarName2 %in% NamesAbsChangeOverTime ~ "Absolute Change Over Time",
    VarName1 %in% NamesBagOfValues & VarName2 %in% NamesBagOfValues ~ "Bag Of Values",
    VarName1 %in% NamesDirChangeOverTime & VarName2 %in% NamesDirChangeOverTime ~ "Directed Change Over Time", 
    TRUE ~ "Bag Of Values"
  ))

# 2g Order the TV Categories
corrdf$Category <- factor(corrdf$Category, levels = NamesTVCatList) 

####*********************
#### 3: Create Plots ####
####*********************
  
# 3a Begin image
tiff(paste0(manuscript.folder, "Fig1_TempVarMetric_Correlations_AllYear.tiff"), 
     height = HH.fig, width = WW.fig*1.25, res = RR.fig)

# 3b Create plot
corrdf %>% 
  ggplot(aes(VarName1, VarName2)) +
  geom_tile(aes(fill = Correlation, color = Category)) +
  geom_tile(aes(fill = Correlation), color = "white") +
  geom_text(aes(label = format(round(Correlation, 2), nsmall = 2)), color = "black", size = 4) +
  geom_rect(aes(xmin=0.5, xmax=2.5, ymin=5.5, ymax=7.5), 
            color = ColAbsChangeOverTime, fill= NA, size = 1.25) + 
  geom_rect(aes(xmin=2.5, xmax=5.5, ymin=2.5, ymax=5.5), 
            color = ColBagOfValues, fill= NA, size = 1.25) + 
  geom_rect(aes(xmin=5.5, xmax=7.5, ymin=0.5, ymax=2.5), 
            color = ColDirChangeOverTime, fill= NA, size = 1.25) + 
  scale_fill_gradientn(colours = CorrColors, 
                       values = c(0, 0.5, 1))+
  scale_color_manual(values = ColTVCatList) +
  theme_minimal(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, size = 14, hjust = 1),
        axis.text.y = element_text(vjust = 0.3, size = 14, hjust = 1),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  guides(color = guide_legend(override.aes = list(fill="white", size =1))) +
  labs(x = "", y = "")   

# 3c End image
dev.off()