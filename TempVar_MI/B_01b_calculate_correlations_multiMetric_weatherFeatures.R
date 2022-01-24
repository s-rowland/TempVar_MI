# Create Correlations for Primary metrics and Weather Metrics
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 06/04/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Readin and Organize Data
# 2: Create Function to Calculate ZCTA-Specific Corrrelations
# 2A: Make Lags and Select Hours
# 2B: Calculate Metrics 
# 2C: Calculate Correlations
# 3: Calculate Correlations for Many ZCTA 
# 4: Save Results

####**************
#### N: Notes #### 
####**************

# B_01a calculates correlations among TV metrics; 
# B_01b calculates correlations between TV metrics and weather metrics

####********************
#### 0: Preparation #### 
####********************

# 0a Declare directories
if (!exists('Ran_0_00')){
  here::i_am('README.md')
  source(here::here('TempVar_mi', 'Scripts',
                    '0_00_create_folder_structure.R'))
}

# 0b Set seed 
set.seed(1234567)

####*********************************
#### 1: Readin and Organize Data ####
####*********************************
# 1a Readin 5 years of weather data
# HourIndex is the number of hours that have passed since
# "1990/01/01 00:00:00" NYC local time
# see a_04 for the details on creation of HourIndex
ActiveYearList <- c(2005, 2006, 2007, 2008, 2009)
ActiveYYYY <- ActiveYearList[1]
wea <-fst::read_fst(paste0(intermediate.data.folder, 
                           "weather_long_", "Pop", "_", ActiveYYYY, ".fst")) %>% 
  as.data.frame() %>%
  mutate(zcta = as.character(zcta)) %>%
  dplyr::select(zcta, HourIndex, temp, rh)

for (i in 2:length(ActiveYearList)){
  ActiveYYYY <- ActiveYearList[i]
  wea <-fst::read_fst(paste0(intermediate.data.folder, 
                             "weather_long_", "Pop", "_", ActiveYYYY, ".fst")) %>% 
    as.data.frame() %>%
    mutate(zcta = as.character(zcta)) %>%
    dplyr::select(zcta, HourIndex, temp, rh) %>% 
    bind_rows(wea)
}

# 1b Stratify data by zcta
wea.ls <- split(wea, wea$zcta)

# 1c Declare function to calculate variability metrics
make_var_ave_data <- function(dta, VarInitial){
  
  dta.e.lag <- dta %>% 
    dplyr::select(contains(paste0(VarInitial, "Lag_")))
  dta.e.lag.24 <- dta.e.lag[,1:24]
  dta.e.lag.48 <- dta.e.lag[,1:48]
  dta.e.lag.25_48 <- dta.e.lag[,25:48]
  # Compute first differences 
  dta.e.fd.24 <- data.frame(a = rep(NA, nrow(dta)))
  for(i in 1:24){
    dta.e.fd.24[,i] <- dta.e.lag[,i] - dta.e.lag[,(i + 1)]
  }
  
  # 1c.iii Create dataframes for intermediate variables 
  # aka, variables we need to subsequently calculate variability metrics
  dta.e.MinMax <- data.frame(a = rep(NA, nrow(dta)))
  dta.e.MinMax[,1] <- apply(dta.e.lag.24, 1, function(x) min(x))
  dta.e.MinMax[,2] <- apply(dta.e.lag.24, 1, function(x) max(x))
  dta.e.MinMax[,3] <- apply(dta.e.lag.25_48, 1, function(x) min(x))
  dta.e.MinMax[,4] <- apply(dta.e.lag.25_48, 1, function(x) max(x))
  
  dta.e.DayMean <- data.frame(a = rep(NA, nrow(dta)))
  dta.e.DayMean$Mean24hr <- apply(dta.e.lag.24, 1, function(x) mean(x))
  dta.e.DayMean$Mean25_48hr <- apply(dta.e.lag.25_48, 1, function(x) mean(x))
  
  # 1c.iv Create dataset of TV metrics and weather parameters
  dta.var <- dta %>% dplyr::select(HourIndex)
  dta.var[,2] <- apply(dta.e.fd.24, 1, function(x) mean(abs(x)))
  dta.var[,3] <- apply(dta.e.fd.24, 1, function(x) max(abs(x)))
  dta.var[,4] <- apply(dta.e.lag.24, 1, function(x) sd(x))
  dta.var[,5] <- apply(dta.e.lag.24, 1, function(x) max(x) - min(x))
  dta.var[,6] <- apply(dta.e.MinMax, 1, function(x) sd(x))
  dta.var[,7] <- apply(dta.e.fd.24, 1, function(x) mean(x))
  dta.var[,8] <- dta.e.DayMean$Mean24hr - dta.e.DayMean$Mean25_48hr 
  dta.var[,9] <- apply(dta.e.lag.24, 1, function(x) mean(x))
  dta.var[,10] <- apply(dta.e.lag.24, 1, function(x) min(x))
  dta.var[,11] <- apply(dta.e.lag.24, 1, function(x) max(x))
  dta.var[,12] <- apply(dta.e.lag.48, 1, function(x) mean(x))
  dta.var[,13] <- apply(dta.e.lag.48, 1, function(x) min(x))
  dta.var[,14] <- apply(dta.e.lag.48, 1, function(x) max(x))
  # 1c.v Rename variables 
  if(VarInitial == "t"){
    colnames(dta.var) <- c("HourIndex", 
                           "MeanAbsFDT_24hr", "MaxAbsFDT_24hr",
                           "SDT_24hr", "DRT_24hr", "SDMinMaxT_48hr",
                           "MeanFDT_24hr", "DayDiffT_48hr", 
                           "MeanT_24hr", "MinT_24hr", "MaxT_24hr", 
                           "MeanT_48hr", "MinT_48hr", "MaxT_48hr")
  }
  if(VarInitial == "r"){
    colnames(dta.var) <- c("HourIndex", 
                           "MeanAbsFDR_24hr", "MaxAbsFDR_24hr",
                           "SDR_24hr", "DRR_24hr", "SDMinMaxR_48hr",
                           "MeanFDR_24hr", "DayDiffR_48hr", 
                           "MeanR_24hr", "MinR_24hr", "MaxR_24hr", 
                           "MeanR_48hr", "MinR_48hr", "MaxR_48hr")
  }
  # 1c.vi Keep only complete cases
  dta.var <- dta.var %>% filter(complete.cases(dta.var))
  dta.var
}

####*****************************************************************
#### 2: Create Function to Calculate ZCTA-Specific Corrrelations ####
####*****************************************************************

# 2a Declare function 
calculate_TempVarMetric_corr <- function(wea.df){
  # wea.df <- wea.ls[[1]]
  
  # 2b silence dplyr's auto warning for summarize() 
  options(dplyr.summarise.inform = FALSE)
  
  ####***********************************
  #### 2A: Make Lags and Select Hours ####
  ####***********************************
  
  # 2A.a Create lags for 47 hours (2 days)
  for(HR in 0:47){ 
    VarNameT <- paste0("T", "Lag", "_", str_pad(HR,2, pad = "0"))
    VarNameR <- paste0("R", "Lag", "_", str_pad(HR,2, pad = "0"))
    
    wea.df <- wea.df %>% mutate(!!VarNameT := lag(temp, HR)) %>% 
      mutate(!!VarNameR := lag(rh, HR))
  }
  
  # 2A.b Get start hour
  # we start with a minimum of 48 hours
  # to ensure that SDMinMaxT and DayDiffT have enough hours for the first period
  Start <- 48 + sample(25, 1)
  
  # 2A.c Create keep variable
  # The keep variable identifies the start of each 24-hour period
  # since the lagged weather is in wide format
  # ie a column for each lag
  # we can drop everything but the rows for the start of each period.
  # We will filter our hours via the keep variable
  wea.df <- wea.df %>% 
    mutate(HourIndex2 = HourIndex + Start) %>% 
    mutate(HourIndex3 = HourIndex2 / 24) %>% 
    mutate(HourIndex4 = HourIndex3 - floor(HourIndex3)) %>% 
    mutate(Keep = if_else(HourIndex4 == 0 , 1, 0))
  
  # 2A.d Keep selected hours 
  wea.df <- wea.df %>% filter(Keep == 1) 
  
  # 2A.e Filter hours within season 
  # 2A.e.i Extract datetime variables 
  wea.df <- wea.df %>% 
    mutate(DateTime = HourIndex * 60 * 60 + 
             parse_date_time("1990/01/01 00:00:00", "ymd HMS", tz="America/New_York")) %>%
    mutate(MM = month(DateTime), DD = day(DateTime)) 
  # 2A.e.ii Set months for seasons
  AllYearMonths <- c(1:12) 
  Hottest4Months <- c(6, 7, 8, 9)
  Coldest4Months <- c(12, 1, 2, 3)
  Medium4Months <- c(4, 5, 10, 11)
  # 2A.e.iv Set ActiveSeasonMonth
  if(Season == "AllYear"){ActiveSeasonMonth <- AllYearMonths}
  if(Season == "Hottest4"){ActiveSeasonMonth <- Hottest4Months}
  if(Season == "Coldest4"){ActiveSeasonMonth <- Coldest4Months}
  if(Season == "Medium4"){ActiveSeasonMonth <- Medium4Months}
  # 2A.e.iii Filter 
  wea.df <- wea.df %>% filter(MM %in% ActiveSeasonMonth)
  
  ####***************************
  #### 2B: Calculate Metrics ####
  ####***************************
  
  # 2B.a Calculate Metrics
  # with inner_join() we combine the two dataframes
  # temp variability and RH variability
  wea.var <- inner_join(make_var_ave_data(wea.df, "t"),
                        make_var_ave_data(wea.df, "r"),
                        by =  "HourIndex")
  
  ####********************************
  #### 2C: Calculate Correlations ####
  ####********************************
  
  # 2C.a Calculate Correlations
  a <- data.frame(corr = c(
    cor(wea.var$MeanAbsFDT_24hr, wea.var$MeanT_24hr), 
    cor(wea.var$MeanAbsFDT_24hr, wea.var$MinT_24hr), 
    cor(wea.var$MeanAbsFDT_24hr, wea.var$MaxT_24hr), 
    cor(wea.var$MeanAbsFDT_24hr, wea.var$MeanR_24hr), 
    cor(wea.var$MeanAbsFDT_24hr, wea.var$MeanAbsFDR_24hr), 
    cor(wea.var$MaxAbsFDT_24hr, wea.var$MeanT_24hr), 
    cor(wea.var$MaxAbsFDT_24hr, wea.var$MinT_24hr), 
    cor(wea.var$MaxAbsFDT_24hr, wea.var$MaxT_24hr), 
    cor(wea.var$MaxAbsFDT_24hr, wea.var$MeanR_24hr), 
    cor(wea.var$MaxAbsFDT_24hr, wea.var$MaxAbsFDR_24hr), 
    cor(wea.var$SDT_24hr, wea.var$MeanT_24hr), 
    cor(wea.var$SDT_24hr, wea.var$MinT_24hr), 
    cor(wea.var$SDT_24hr, wea.var$MaxT_24hr), 
    cor(wea.var$SDT_24hr, wea.var$MeanR_24hr), 
    cor(wea.var$SDT_24hr, wea.var$SDR_24hr), 
    cor(wea.var$DRT_24hr, wea.var$MeanT_24hr), 
    cor(wea.var$DRT_24hr, wea.var$MinT_24hr), 
    cor(wea.var$DRT_24hr, wea.var$MaxT_24hr), 
    cor(wea.var$DRT_24hr, wea.var$MeanR_24hr), 
    cor(wea.var$DRT_24hr, wea.var$DRR_24hr), 
    cor(wea.var$SDMinMaxT_48hr, wea.var$MeanT_48hr), 
    cor(wea.var$SDMinMaxT_48hr, wea.var$MinT_48hr), 
    cor(wea.var$SDMinMaxT_48hr, wea.var$MaxT_48hr), 
    cor(wea.var$SDMinMaxT_48hr, wea.var$MeanR_48hr), 
    cor(wea.var$SDMinMaxT_48hr, wea.var$SDMinMaxR_48hr), 
    cor(wea.var$MeanFDT_24hr, wea.var$MeanT_24hr), 
    cor(wea.var$MeanFDT_24hr, wea.var$MinT_24hr), 
    cor(wea.var$MeanFDT_24hr, wea.var$MaxT_24hr),
    cor(wea.var$MeanFDT_24hr, wea.var$MeanR_24hr), 
    cor(wea.var$MeanFDT_24hr, wea.var$MeanFDR_24hr),
    cor(wea.var$DayDiffT_48hr, wea.var$MeanT_48hr), 
    cor(wea.var$DayDiffT_48hr, wea.var$MinT_48hr), 
    cor(wea.var$DayDiffT_48hr, wea.var$MaxT_48hr),
    cor(wea.var$DayDiffT_48hr, wea.var$MeanR_48hr), 
    cor(wea.var$DayDiffT_48hr, wea.var$DayDiffR_48hr)
  ), 
  VarName1 = c(rep("MeanAbsFDT_24hr", 5), rep("MaxAbsFDT_24hr", 5), 
               rep("SDT_24hr", 5), rep("DRT_24hr", 5), rep("SDMinMaxT_48hr", 5), 
               rep("MeanFDT_24hr", 5), rep("DayDiffT_48hr", 5)), 
  VarName2 = rep(c("MeanT", "MinT", "MaxT", "MeanR", "VarR"), 7)
  ) 
  a <- a %>% mutate(Season := Season)
  
}

####*********************************************
#### 3: Calculate Correlations for Many ZCTA ####
####*********************************************

# 3a Set up mapping
future::plan(multiprocess)

# 3a Calculate corr
cor.results.ls <- future_map(wea.ls, calculate_TempVarMetric_corr)

# 3b Combine results 
cor.results.tot <- bind_rows(cor.results.ls)

# 3c Compute means 
cor.results <- cor.results.tot %>% 
  group_by( VarName1, VarName2, Season) %>% 
  summarize(MeanCorr = mean(corr, na.rm = TRUE))

####********************
### 4: Save Results ####
####********************

# 4a Save results
cor.results %>% write_csv(paste0(tables.folder, "Correlations_PrimaryMetric_Weather_", Season, ".csv"))

