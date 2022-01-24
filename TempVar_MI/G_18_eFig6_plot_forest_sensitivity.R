# Create fig3 
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 06/04/2020

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
  theme(axis.text.x = element_text(size = 14)) + 
  theme(axis.text.y = element_text(size = 14, angle = 0)) + 
  theme(panel.background = element_rect(fill= NA, color = "black"))

####**************************
#### 2: Wrangle Estimates ####
####**************************

# 2a Readin Estimates 
est.table <- read_csv(paste0(manuscript.folder, "eTable6_EE_SensitivityAnalyses.csv"))

# 2b Order TempVar Metrics
est.table$TVMetric <- factor(est.table$TVMetric, levels = NamesTVMetricList) 

# 2c Order Sensitivity
est.table$Sensitivity <- factor(est.table$Sensitivity, levels = SensitivityOrder) 

# 2d Break up table 
est.table10 <- est.table %>% 
  dplyr::select(Sensitivity, TVMetric, fit.pc10, lci.pc10, uci.pc10) %>% 
  rename(fit.pc = fit.pc10, 
         lci.pc = lci.pc10, 
         uci.pc = uci.pc10) %>% 
  mutate(VarPercentile = 10)

est.table90 <- est.table %>% 
  dplyr::select(Sensitivity, TVMetric, fit.pc90, lci.pc90, uci.pc90) %>% 
  rename(fit.pc = fit.pc90, 
         lci.pc = lci.pc90, 
         uci.pc = uci.pc90) %>% 
  mutate(VarPercentile = 90)

# 2e Combine 
est.table <- bind_rows(est.table10, est.table90)

# 2f Keep only 90th percentile 
est.table <- est.table %>% filter(VarPercentile == 90)

# 2g Extract the CI of the main model
main <- est.table %>% 
  filter(Sensitivity == "Main") %>% 
  arrange(desc(TVMetric))

####********************
#### 3: Create Plot ####
####********************

# 3a Create plot
TP <- ggplot(est.table, aes(Sensitivity)) + 
  geom_blank() + 
  geom_rect(aes(xmin = 0, xmax = 13.5, ymin = main$lci.pc[2], ymax = main$uci.pc[2], 
                alpha = TVMetric), fill = ColTVCatList[1]) + 
  geom_rect(aes(xmin = 0, xmax = 13.5, ymin = main$lci.pc[1], ymax = main$uci.pc[1], 
                fill = TVMetric), alpha = 0.05*AlphaCI) + 
    geom_hline(yintercept = 0, color = "grey" ) + 
  geom_point(aes(y = fit.pc, color = TVMetric), size = 3, shape = 18) + 
  geom_errorbar(aes(ymin = lci.pc, ymax = uci.pc, color = TVMetric) ) + 
  tema +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c(ColTVCatList[1], ColTVCatList[3])) + 
  scale_fill_manual(values = c(NA, ColTVCatList[3])) + 
  scale_alpha_manual(values = c(0.05*AlphaCI, 0)) + 
  guides(color = FALSE) +
  guides(shape = FALSE) +
  guides(alpha = FALSE) +
  guides(fill = FALSE) +
  coord_flip() + 
  labs(y = paste0("Change in MI Hospitalizations (%)"), 
       x = paste0(" ")) +
  facet_grid(. ~ TVMetric) + 
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank()) 

# 3b Save plot
tiff(paste0(manuscript.folder, "eFig6_Forest_", "MainMetric_Sensitivity", ".tiff"), 
     height = HH.fig, width = WW.fig, res = RR.fig)
print(tag_facet(TP, open ="", close = "", tag_pool = c("A", "B"), size = 10, 
                x = 13.5, y = -0.3))
dev.off()

