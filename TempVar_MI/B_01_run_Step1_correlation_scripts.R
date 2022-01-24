# Run Correlation Scripts 
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/14/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Run Correlation Scripts

####**************
#### N: Notes #### 
####**************

# Na Description
# We use this script to run all of the correlation scripts.
# For autocorrelation, we actually need to run the two segments of the 
# medium season separately 
# otherwise there is a 4-month jump between some periods. 

####********************
#### 0: Preparation #### 
####********************

# 0a Declare directories
if (!exists('Ran_0_00')){
  here::i_am('README.md')
  source(here::here('TempVar_mi', 'Scripts',
                    '0_00_create_folder_structure.R'))
}

####********************************
#### 1: Run Correlation Scripts #### 
####********************************

# 1a AllYear correlations
Season <- "AllYear" 
source(paste0(scripts.folder, "B_01a_calculate_correlations_multiMetric_crossMetric.R"))
source(paste0(scripts.folder, "B_01b_calculate_correlations_multiMetric_weatherFeatures.R"))

# 1b Correlations within hottest4 months
Season <- "Hottest4" 
source(paste0(scripts.folder, "B_01a_calculate_correlations_multiMetric_crossMetric.R"))
source(paste0(scripts.folder, "B_01b_calculate_correlations_multiMetric_weatherFeatures.R"))

# 1c Correlations within coldest 4 months
Season <- "Coldest4" 
source(paste0(scripts.folder, "B_01a_calculate_correlations_multiMetric_crossMetric.R"))
source(paste0(scripts.folder, "B_01b_calculate_correlations_multiMetric_weatherFeatures.R"))

# 1d Correlations within medium 4 months
Season <- "Medium4" 
source(paste0(scripts.folder, "B_01a_calculate_correlations_multiMetric_crossMetric.R"))
source(paste0(scripts.folder, "B_01b_calculate_correlations_multiMetric_weatherFeatures.R"))
