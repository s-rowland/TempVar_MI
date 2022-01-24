# Create Fig 4
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 11/24/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Set Up Plotting Objects 
# 2: Wrangle Estimates
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

# 0f Source functions 
source(paste0(scripts.folder, "G_00_set_figure_size.R"))

####********************************
#### 1: Set Up Plotting Objects #### 
####********************************

# 1a Set up plotting theme
tema <- theme_classic() +
  theme(plot.title = element_text(size = 30, vjust = 1, hjust =0.5)) +
  theme(axis.title.x = element_text(size = 18, vjust =0, lineheight=0.25)) + 
  theme(axis.title.y = element_text(size = 18, angle = 90, vjust= 1)) + 
  theme(panel.background = element_rect(fill= NA, color = "black"))

####**************************
#### 2: Wrangle Estimates ####
####**************************

# 2a Readin Estimates 
est.table <- read_csv(paste0(manuscript.folder, "eTable7_EE_AlternativeWindow.csv"))

# 2b Drop columns that we no longer need 
est.table <- est.table %>% select(-contains("EE"), -Sensitivity)
# 2c Order TempVar Metrics
# again, we create this fake "a" entry that we will remove later 
# in order to ahve that top right empty box in the plot
#est.table$TVMetric <- factor(est.table$TVMetric, levels = NamesTVMetricList) 
est.table[15, ] <- list("a", 0,0,0, 0,0, 0, "Time of Event")
NamesTVMetricList2 <- c(NamesAbsChangeOverTime, "a", NamesBagOfValues, NamesDirChangeOverTime)
est.table$TVMetric <- factor(est.table$TVMetric, levels = NamesTVMetricList2) 

# 2d Order the TVCategory  
est.table <- est.table %>% 
  mutate(TVCat = case_when(
    TVMetric %in% NamesAbsChangeOverTime ~ "Abs. Change Over Time", 
    TVMetric %in% NamesBagOfValues ~ "Bag Of Values", 
    TVMetric %in% NamesDirChangeOverTime ~ "Dir. Change Over Time"))
est.table$TVCat <- factor(est.table$TVCat, levels = rev(NamesTVCatList)) 

# 2f Order ExposureWindow
est.table$ExposureWindow <- factor(est.table$ExposureWindow, 
                                        levels = c("Midnight", "Time of Event")) 

# 2g Break up table 
est.table10 <- est.table %>% 
  dplyr::select(ExposureWindow, TVMetric, fit.pc10, lci.pc10, uci.pc10, TVCat) %>% 
  rename(fit.pc = fit.pc10, 
         lci.pc = lci.pc10, 
         uci.pc = uci.pc10) %>% 
  mutate(VarPercentile = 10)

est.table90 <- est.table %>% 
  dplyr::select(ExposureWindow, TVMetric, fit.pc90, lci.pc90, uci.pc90, TVCat) %>% 
  rename(fit.pc = fit.pc90, 
         lci.pc = lci.pc90, 
         uci.pc = uci.pc90) %>% 
  mutate(VarPercentile = 90)

# 2h Combine 
est.table <- bind_rows(est.table10, est.table90)

# 2i Keep only 90th percentile 
est.table <- est.table %>% filter(VarPercentile == 90)

####********************
#### 3: Create Plot ####
####********************

# 3a Create plot
TP <- ggplot(est.table90, aes(ExposureWindow)) + 
  geom_hline(yintercept = 0, color = "grey" ) + 
  geom_point(aes(y = fit.pc, color = TVCat), size = 3, shape = 18) + 
  geom_errorbar(aes(ymin = lci.pc, ymax = uci.pc, color = TVCat) ) + 
  tema +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = rev(ColTVCatList)) + 
  guides(color = FALSE) +
  guides(shape = FALSE) +
  guides(alpha = FALSE) +
  coord_flip() + 
  labs(y = paste0("Change in MI Hospitalizations (%)"), 
       x = paste0(" ")) +
  facet_wrap(~TVMetric) + 
  theme(strip.text = element_text(size = 12), 
        axis.text = element_text(size = 12)) 
TP

# 3b Arrange the facets
# we remove the fake facet, leaving behind an empty section
library(gridBase)  
library(gridExtra)
g <- ggplotGrob(TP)
# get the grobs that must be removed
rm_grobs <- g$layout$name %in% c("panel-1-3", "panel-3-3", "strip-t-3-1", "strip-t-3-3")
# remove grobs
g$grobs[rm_grobs] <- NULL
g$layout <- g$layout[!rm_grobs, ]
## move axis closer to panel
g$layout[g$layout$name == "axis-b-3-3", c("t", "b")] = c(14.5, 14.5)
grid::grid.newpage()

# 3c Save Plot
tiff(paste0(manuscript.folder, "Fig4_Forest_", "Sensitivity_AlternateWindow", "_90th.tiff"), 
     height = HH.fig, width = WW.fig, res = RR.fig)

grid::grid.draw(g)

dev.off()
