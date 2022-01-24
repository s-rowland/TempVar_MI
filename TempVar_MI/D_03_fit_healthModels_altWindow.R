# Fit the Midnight-Based TempVar - MI Health Models
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 06/04/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Fit Models

####**************
#### N: Notes #### 
####**************

# N.a Set constraints 
# we do not use AIC to chose the constraint for the natural spline 
# since we are using the same constraints as in the main models. 

####********************
#### 0: Preparation #### 
####********************

# 0a Declare directories
if (!exists('Ran_0_00')){
  here::i_am('README.md')
  source(here::here('TempVar_mi', 'Scripts',
                    '0_00_create_folder_structure.R'))
}

# 0b Load functions 
source(paste0(scripts.folder, "C_00_Analyze_TempVar_function.R"))

####*******************
#### 1: Fit Models ####
####*******************

analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "All23")
analyze_TempVar("mi", "MaxAbsFDT_24hr", "3dfevenknots", "fullpop", "fullpop", "All23")
analyze_TempVar("mi", "SDT_24hr", "3dfevenknots", "fullpop", "fullpop", "All23")
analyze_TempVar("mi", "DRT_24hr", "3dfevenknots", "fullpop", "fullpop", "All23")
analyze_TempVar("mi", "SDMinMaxT_48hr", "4dfevenknots", "fullpop", "fullpop", "All23")
analyze_TempVar("mi", "MeanFDT_24hr", "4dfevenknots", "fullpop", "fullpop", "All23")
analyze_TempVar("mi", "DayDiffT_48hr", "4dfevenknots", "fullpop", "fullpop", "All23")
