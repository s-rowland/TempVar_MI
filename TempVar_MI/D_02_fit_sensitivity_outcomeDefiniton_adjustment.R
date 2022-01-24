# Sensitivity Analyses- model choices
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/29/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Assess Robustness to Outcome Definition 
# 2: Assess Robustness to Adjustment Approach
# 3: Assess Robustness to Temperature Metric

####********************
#### 0: Preparation #### 
####********************

# 0a Declare directories
if (!exists('Ran_0_00')){
  here::i_am('README.md')
  source(here::here('TempVar_mi', 'Scripts',
                    '0_00_create_folder_structure.R'))
}

# 0c Load functions 
source(paste0(scripts.folder, "C_00_Analyze_TempVar_function.R"))

####*************************************************
#### 1: Assess Robustness to Outcome Definition  ####
####*************************************************

# 1a MeanAbsFDT_24hr
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "prim_alt_410x1_A")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "prim_alt_410xx_A")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "prim_alt_410x01_A4")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "prim_alt_NoReinfarction")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "prim_60moRecurr")

# 1b MeanFDT_24hr
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "prim_alt_410x1_A")
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "prim_alt_410xx_A")
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "prim_alt_410x01_A4")
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "prim_alt_NoReinfarction")
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "prim_60moRecurr")

####*************************************************
#### 2: Assess Robustness to Adjustment Approach ####
####*************************************************

# 2a MeanAbsFDT_24hr
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "NOmeanRH")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "NOvarRH")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "NORH")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "VarRH5df")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "MeanRH5df")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "MeanT5df")

# 2b MeanFDT_24hr
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "NOmeanRH")
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "NOvarRH")
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "NORH")
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "VarRH5df")
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "MeanRH5df")
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "MeanT5df")

####************************************************
#### 3: Assess Robustness to Temperature Metric ####
####************************************************

# 3a MeanAbsFDT_24hr
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "MinT")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "MaxT")

# 3b MeanFDT_24hr
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "MinT")
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "MaxT")