# Create fig3 
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 11/24/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Set Up Plotting Objects 
# 2: Prepare Table
# 3: Create Plot

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
library(purrr)

# 0b Source functions 
source(paste0(scripts.folder, "G_00_set_figure_size.R"))

####********************************
#### 1: Set Up Plotting Objects #### 
####********************************

# 1a Set up plotting theme
tema <- theme_classic() +
  theme(plot.title = element_text(size = 30, vjust = 1, hjust =0.5)) +
  theme(axis.title.x = element_text(size = 18, vjust =0, lineheight=0.25)) + 
  theme(axis.title.y = element_text(size = 18, angle = 90, vjust= 1)) + 
  theme(axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16, angle = 0)) + 
  theme(panel.background = element_rect(fill= NA, color = "black"))

####**********************
#### 2: Prepare Table ####
####**********************

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

# 2b Order TempVar Metrics
est.table$TVMetric <- factor(est.table$TVMetric, levels = rev(NamesTVMetricList)) 

# 2c Add subcategory variables 
est.table <- est.table %>% 
  mutate(TVCat = case_when(
    TVMetric %in% NamesAbsChangeOverTime ~ "Abs. Change Over Time", 
    TVMetric %in% NamesBagOfValues ~ "Bag Of Values", 
    TVMetric %in% NamesDirChangeOverTime ~ "Dir. Change Over Time"))
est.table$TVCat <- factor(est.table$TVCat, levels = NamesTVCatList) 

# 2d Wrangle variables 
est.table <- est.table %>% 
  mutate(VarPercentile = if_else(Labels == "per10", 10, 90))

# 2e Convert effect estimate to percent change of rate 
est.table <- est.table %>% 
  mutate(fit.pc = convert_to_percent(fit.or), 
         lci.pc = convert_to_percent(lci.or),
         uci.pc = convert_to_percent(uci.or))

####*********************
#### 3: Create Plot  ####
####*********************

# 3a Begin image
tiff(paste0(manuscript.folder, "Fig3_ER_", "MultiMetric_ForestPlot", ".tiff"), 
     height = HH.fig, width = WW.fig, res = RR.fig)

# 3b Make plot
TempPlot <- ggplot(est.table, aes(TVMetric)) + 
  geom_hline(yintercept = 0, color = "grey" ) + 
  geom_point(aes(y = fit.pc, color = TVCat), size = 3, shape = 18) + 
  geom_errorbar(aes(ymin = lci.pc, ymax = uci.pc, color = TVCat) ) + 
  scale_color_manual(values = ColTVCatList) +
  tema +
  theme(legend.title = element_blank()) +
  guides(color = FALSE) +
  guides(shape = FALSE) +
  guides(alpha = FALSE) +
  coord_flip() + 
  labs(y = paste0("Change in MI Hospitalizations (%)"), 
       x = paste0(" ")) +
  facet_grid(. ~ VarPercentile) + 
  theme(panel.spacing = unit(1, "lines")) + 
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank()) 

# 3c Add panel labels
print(tag_facet(TempPlot, open ="", close = "", tag_pool = c("A", "B"), size = 10, 
                x = 7.5, y = -0.5))

# 3d Finish image
dev.off()
