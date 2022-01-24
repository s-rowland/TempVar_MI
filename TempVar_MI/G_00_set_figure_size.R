# Set Figure Size
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 11/24/2020

####***********************
#### Table of Contents #### 
####***********************

# D: Descripton
# 1: Set Figure Size
# 2: Set Colors for TV Metric Subcategories

####********************
#### D: Description ####
####********************

# By setting common plotting features in a separate script 
# We can ensure consistency across plots 
# It also helps make some choices more explicit

####************************
#### 1: Set Figure Size ####
####************************

# not all figures are this size, but these are common sizes 
HH.fig <- 1000 
WW.fig <- 1000 
RR.fig <- 150

HH.efig <- 600 
WW.efig <- 600 
RR.efig <- 200

####***********************************************
#### 2: Set Colors for TV Metric Subcategories ####
####***********************************************

# 2a Set order of TVmetric variable names
NamesAbsChangeOverTimeExpW <- c("MeanAbsFDT_24hr", "MaxAbsFDT_24hr")
NamesBagOfValuesExpW <- c("SDT_24hr", "DRT_24hr", "SDMinMaxT_48hr")
NamesDirChangeOverTimeExpW <- c("MeanFDT_24hr", "DayDiffT_48hr")
NamesTVListExpW <- c(NamesAbsChangeOverTimeExpW, NamesBagOfValuesExpW, NamesDirChangeOverTimeExpW)

# 2b Set order of TVMetric publication names
NamesAbsChangeOverTime <- c("MeanAbsFDT", "MaxAbsFDT")
NamesBagOfValues <- c("SDT", "DTR", "SDMinMaxT")
NamesDirChangeOverTime <- c("MeanFDT", "DiffMeanT")
NamesTVMetricList <- c(NamesAbsChangeOverTime, NamesBagOfValues, NamesDirChangeOverTime)

# 2c Set order of TV categories
NamesTVCatList <- c("Absolute Change Over Time", "Bag Of Values", "Directed Change Over Time")

# 2d Set colors for TV categories
ColAbsChangeOverTime <- "darkorange"
ColBagOfValues <- "saddlebrown"
ColDirChangeOverTime <- "mediumpurple3"
ColTVCatList <- c(ColAbsChangeOverTime, ColBagOfValues, ColDirChangeOverTime)

####***********************************************
#### 3: Set Values for Other Plotting Features ####
####***********************************************

# 3a Set the level of transparency for confidence intervals 
# not often used
AlphaCI <- 0.35

# 3b Seasonal Stuff
NamesSeasonAllY <- c("All Months", "Hot Months", "Cold Months", "Moderate Months")
NamesSeasonSea <- c("Hot Months", "Cold Months", "Moderate Months")
NamesSeasonSeaVarAllY <- c("AllYear", "Hottest4", "Coldest4", "Moderate4")
NamesSeasonSeaVar <- c("Hottest4", "Coldest4", "Moderate4")
ColAllYear <- "black"
ColHottest4 <- "maroon2"
ColColdest4 <- "dodgerblue2"
ColModerate4 <- "darkgreen"
ColSeasonAllY <- c(ColAllYear, ColHottest4, ColColdest4, ColModerate4)
ColSeasonSea <- c(ColHottest4, ColColdest4, ColModerate4)

# 3c Correlation colors
CorrColors <- c("white", "lightgoldenrod2",  "green3")

# 3d Dataset parameters

Timing <- "STEMI2hrDelay"

# 3e Declare convert to precent function 
convert_to_percent <- function(x){
  100 * (x - 1)
}

# 3f Set order of sensitivity analyses
SensitivityList <- c("Main",
                     "prim_alt_410x1_A", "prim_alt_410xx_A",
                     "prim_alt_410x01_A4", "prim_alt_NoReinfarction",
                     "prim_6moRecurr", 
                     "NOmeanRH", "NOvarRH", "NORH",
                     "MeanT5df", "VarRH5df","MinT", "MaxT"
                      )

SensitivityOrder <- c("Maximal T Adj.","Minimal T Adj.", 
                       "5 df for RH Variability", "5 df for Mean T",
                       "No RH Adj.", "No RH Variability Adj.", "No Mean RH Adj.", 
                       "Outcome Definition F", "Outcome Definition E", "Outcome Definition D", 
                       "Outcome Definition C", "Outcome Definition B", 
                       "Main")

# 3g Set order of sensitivity analyses
ExpWList <- c("Main", "All23")

ExpWOrder <- c("Midnight", "Time of Event")
