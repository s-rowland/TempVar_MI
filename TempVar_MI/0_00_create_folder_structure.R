# Create Folder Structure
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/30/2020

####***********************
#### Table of Contents ####  
####***********************

# D: Description
# 0: Preparation 
# 1: Create Folder Structure

####********************
#### D: Description ####
####********************

# We use this script to run all of the correlation scripts.
# For autocorrelation, we actually need to run the two segments of the 
# medium season separately 
# otherwise there is a 4-month jump between some periods. 

####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
library(here)
library(readr)
library(dplyr)
library(lubridate)
library(magrittr)
library(tidyr)
library(stringr)
library(purrr)
library(forcats)
library(survival)
library(stringr)
library(splines)
library(mgcv)
library(nlme)
library(pspline)
library(dlnm)
library(ggplot2)
library(viridis)
library(egg)

# 0b Silence dplyr comments
options(dplyr.summarise.inform = FALSE)

# 0c Add marker 
Ran_0_00 <- 'Ran_0_00'

# 0d Set CaseType 
# either mi or stroke
CaseType <- "mi"
# 0e Set dataset parameters
# For TempVar-MI, we always use population-weighted temperature data
NLDASWeight <- "Pop"

####********************************
#### 1: Create Folder Structure #### 
####********************************

# 1a Declare directories
project.folder <- paste0(print(here::here()),'/')
intermediate.data.folder <- paste0(project.folder, "DataPrep/Data/Intermediate_Data/")
final.data.folder <- paste0(project.folder, "DataPrep/Data/Final_Data/")
analysis.folder <- paste0(project.folder,"TempVar_mi/")
scripts.folder <- paste0(analysis.folder, "Scripts/")
outputs.folder <- paste0(analysis.folder, "Outputs/")
models.folder   <- paste0(outputs.folder, "Models/")
estimates.folder <- paste0(outputs.folder, "Estimates/")
plots.folder <- paste0(outputs.folder, "Plots/")
tables.folder <- paste0(outputs.folder, "Tables/")
manuscript.folder <- paste0(outputs.folder, "Manuscript/")

# 1b Identify list of folder locations which have just been created above
folder.names <- grep(".folder",names(.GlobalEnv),value=TRUE)

# 1c Create function to create list of folders
# note that the function will not create a folder if it already exists 
create_folders <- function(name){
  ifelse(!dir.exists(get(name)), dir.create(get(name), recursive=TRUE), FALSE)
}

# 1d Create the folders named above
lapply(folder.names, create_folders)

# 1e source functions
source(paste0(scripts.folder, "C_00_Analyze_TempVar_function.R"))