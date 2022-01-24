# Assess Effect Modification by Season
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 06/07/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Assess Nonlinearity 
# 2: Choose df for Natural Splines
# 3: Confirm Overlap Across Seasons

####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
# 0a Declare directories
if (!exists('Ran_0_00')){
  here::i_am('README.md')
  source(here::here('TempVar_mi', 'Scripts',
                    '0_00_create_folder_structure.R'))
}

# 0b Load figure size
source(paste0(scripts.folder, "G_00_set_figure_size.R"))

####*****************************
#### 1: Assess Nonlinearity  ####
####*****************************

# 1a Fit penalized models for MeanAbsFDT
analyze_TempVar("mi", "MeanAbsFDT_24hr", "psp", "Season4mo", "Hottest4", "CheckLinearity")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "psp", "Season4mo", "Coldest4", "CheckLinearity")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "psp", "Season4mo", "Medium4", "CheckLinearity")

# 1b Fit penalized models for MeanFDT
analyze_TempVar("mi", "MeanFDT_24hr", "psp", "Season4mo", "Hottest4", "CheckLinearity")
analyze_TempVar("mi", "MeanFDT_24hr", "psp", "Season4mo", "Coldest4", "CheckLinearity")
analyze_TempVar("mi", "MeanFDT_24hr", "psp", "Season4mo", "Medium4", "CheckLinearity")

# 1c Create function to extract edf
get_edf <- function(TempVarMetric, SubPop, CaseType, ERConstraint, SubPopVar, Sensitivity){
  ModelName <- paste0("TempVar", "_",CaseType, "_", TempVarMetric, "_ER", ERConstraint,
                      "_",SubPopVar, "_", SubPop, "_", Sensitivity)
  mod <- readRDS(paste0(models.folder, ModelName, ".RDS"))
  edf <- mod$df[1]
  c(TempVarMetric, SubPop, edf)
}

# 1d Create edf table 
edf_season_table <- data.frame(
  TempVarMetric = NA, 
  Season = NA, 
  edf = NA
)

# 1e Fill Table 
# 1e.i Assess nonlinearity for MeanAbsFDT
edf_season_table[1, ] <- get_edf("MeanAbsFDT_24hr", "Hottest4", "mi","psp", "Season4mo", "CheckLinearity")
edf_season_table[2, ] <- get_edf("MeanAbsFDT_24hr", "Coldest4", "mi","psp", "Season4mo", "CheckLinearity")
edf_season_table[3, ] <- get_edf("MeanAbsFDT_24hr", "Medium4", "mi","psp", "Season4mo", "CheckLinearity")
# 1e.ii Assess nonlinearity for MeanFDT 
edf_season_table[4, ] <- get_edf("MeanFDT_24hr", "Hottest4", "mi","psp", "Season4mo", "CheckLinearity")
edf_season_table[5, ] <- get_edf("MeanFDT_24hr", "Coldest4", "mi","psp", "Season4mo", "CheckLinearity")
edf_season_table[6, ] <- get_edf("MeanFDT_24hr", "Medium4", "mi","psp", "Season4mo", "CheckLinearity")

# 1f Save table 
edf_season_table %>% write_csv(paste0(tables.folder, "EDF_seasonalEMM_models.csv"))

####**************************************
#### 2: Choose df for Natural Splines ####
####**************************************

# 2a MeanAbsFDT
# 2a.i Hottest4
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "Season4mo", "Hottest4", "Main")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "4dfevenknots", "Season4mo", "Hottest4", "Main")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "5dfevenknots", "Season4mo", "Hottest4", "Main")
# 2a.ii Coldest4
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "Season4mo", "Coldest4", "Main")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "4dfevenknots", "Season4mo", "Coldest4", "Main")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "5dfevenknots", "Season4mo", "Coldest4", "Main")
# 2a.iii Medium4
analyze_TempVar("mi", "MeanAbsFDT_24hr", "3dfevenknots", "Season4mo", "Medium4", "Main")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "4dfevenknots", "Season4mo", "Medium4", "Main")
analyze_TempVar("mi", "MeanAbsFDT_24hr", "5dfevenknots", "Season4mo", "Medium4", "Main")

# 2b MeanFDT
# 2b.i Hottest4
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "Season4mo", "Hottest4", "Main")
analyze_TempVar("mi", "MeanFDT_24hr", "4dfevenknots", "Season4mo", "Hottest4", "Main")
analyze_TempVar("mi", "MeanFDT_24hr", "5dfevenknots", "Season4mo", "Hottest4", "Main")
# 2b.ii Coldest4
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "Season4mo", "Coldest4", "Main")
analyze_TempVar("mi", "MeanFDT_24hr", "4dfevenknots", "Season4mo", "Coldest4", "Main")
analyze_TempVar("mi", "MeanFDT_24hr", "5dfevenknots", "Season4mo", "Coldest4", "Main")
# 2b.iii Medium4
analyze_TempVar("mi", "MeanFDT_24hr", "3dfevenknots", "Season4mo", "Medium4", "Main")
analyze_TempVar("mi", "MeanFDT_24hr", "4dfevenknots", "Season4mo", "Medium4", "Main")
analyze_TempVar("mi", "MeanFDT_24hr", "5dfevenknots", "Season4mo", "Medium4", "Main")

# 2c Create function to extract AIC
get_aic <- function(TempVarMetric, SubPop, CaseType, ERConstraint, SubPopVar, Sensitivity){
  ModelName <- paste0("TempVar", "_",CaseType, "_", TempVarMetric, "_ER", ERConstraint,
                      "_",SubPopVar, "_", SubPop, "_", Sensitivity)
  mod <- readRDS(paste0(models.folder, ModelName, ".RDS"))
  AIC <- AIC(mod)
  c(TempVarMetric, SubPop, AIC)
}

# 2d Create aic table 
aic_season_table <- data.frame(
  TempVarMetric = NA, 
  Season = NA, 
  AIC = NA
)

# 2e Fill Table 
# 2e.i Assess AIC for MeanAbsFDT
aic_season_table[1, ] <- get_aic("MeanAbsFDT_24hr", "Hottest4", "mi","3dfevenknots", "Season4mo", "Main")
aic_season_table[2, ] <- get_aic("MeanAbsFDT_24hr", "Hottest4", "mi","4dfevenknots", "Season4mo","Main")
aic_season_table[3, ] <- get_aic("MeanAbsFDT_24hr", "Hottest4", "mi","5dfevenknots", "Season4mo", "Main")
aic_season_table[4, ] <- get_aic("MeanAbsFDT_24hr", "Coldest4", "mi","3dfevenknots", "Season4mo", "Main")
aic_season_table[5, ] <- get_aic("MeanAbsFDT_24hr", "Coldest4", "mi","4dfevenknots", "Season4mo", "Main")
aic_season_table[6, ] <- get_aic("MeanAbsFDT_24hr", "Coldest4", "mi","5dfevenknots", "Season4mo", "Main")
aic_season_table[7, ] <- get_aic("MeanAbsFDT_24hr", "Medium4", "mi", "3dfevenknots", "Season4mo", "Main")
aic_season_table[8, ] <- get_aic("MeanAbsFDT_24hr", "Medium4", "mi","4dfevenknots", "Season4mo", "Main")
aic_season_table[9, ] <- get_aic("MeanAbsFDT_24hr", "Medium4", "mi","5dfevenknots", "Season4mo", "Main")

# 2e.ii Assess AIC for MeanFDT 
aic_season_table[10, ] <- get_aic("MeanFDT_24hr", "Hottest4", "mi","3dfevenknots", "Season4mo", "Main")
aic_season_table[11, ] <- get_aic("MeanFDT_24hr", "Hottest4", "mi","4dfevenknots", "Season4mo", "Main")
aic_season_table[12, ] <- get_aic("MeanFDT_24hr", "Hottest4", "mi","5dfevenknots", "Season4mo", "Main")
aic_season_table[13, ] <- get_aic("MeanFDT_24hr", "Coldest4", "mi","3dfevenknots", "Season4mo", "Main")
aic_season_table[14, ] <- get_aic("MeanFDT_24hr", "Coldest4", "mi","4dfevenknots", "Season4mo", "Main")
aic_season_table[15, ] <- get_aic("MeanFDT_24hr", "Coldest4", "mi","5dfevenknots", "Season4mo", "Main")
aic_season_table[16, ] <- get_aic("MeanFDT_24hr", "Medium4", "mi", "3dfevenknots", "Season4mo", "Main")
aic_season_table[17, ] <- get_aic("MeanFDT_24hr", "Medium4", "mi","4dfevenknots", "Season4mo", "Main")
aic_season_table[18, ] <- get_aic("MeanFDT_24hr", "Medium4", "mi","5dfevenknots", "Season4mo", "Main")

# 2f Find minAIC
AIC.table2 <- aic_season_table %>%
  group_by(TempVarMetric, Season) %>% 
  summarize(minAIC = min(AIC))

AIC.table3 <- aic_season_table %>%
  left_join(AIC.table2, by = c("TempVarMetric", "Season"))

# 2g Calculate deltaAIC 
AIC.table3 <- AIC.table3 %>% mutate(deltaAIC = as.numeric(AIC) - as.numeric(minAIC))

# 2h Calculate weight denominator
AIC.table4 <- AIC.table3 %>% 
  group_by(TempVarMetric) %>% 
  summarize(denom = sum(exp(-0.5 *deltaAIC)))

AIC.table5 <- AIC.table3 %>%
  left_join(AIC.table4, by = "TempVarMetric")

# 2i calculate AIC weights 
AIC.table5$AkaikeWeight <- exp(-0.5*AIC.table5$deltaAIC) / AIC.table5$denom
# 3e pretty format
AIC.table5$AkaikeWeight <- round(100* AIC.table5$AkaikeWeight, 1)

# 2j Save table 
AIC.table5 %>% 
  mutate(df = rep(c(3, 4, 5), 6)) %>% 
  write_csv(paste0(tables.folder, "AIC_seasonalEMM_models.csv"))

####***************************************
#### 3: Confirm Overlap Across Seasons ####
####***************************************

# 3a Create Season df 
# 3a.i Readin df
dta <- fst::read.fst(paste0(final.data.folder, "prepared_cohort", "_", "mi", 
                            "_identified", "_", NLDASWeight, "_", Timing, "_TVOnly.fst"))
# 3a.ii Remove missing exposure 
dta <- dta %>% 
  filter(!is.na(MeanR_48hr)) %>%  
  filter(!is.na(MeanT_48hr))  %>% 
  filter(MeanAbsFDT_24hr < 11)
# 3a.iii Keep only primary MI
dta <- dta %>% filter(MI_prim == 1 )
# 3a.iv Create Season variables
dta <- dta %>% 
  mutate(CaseDate = parse_date_time(CaseDateRaw, "ymd h")) %>% 
  mutate(MM = month(CaseDate), HH.adt = hour(CaseDate), DD = day(CaseDate)) %>% 
  mutate(Season4mo = case_when(
    MM %in% c(6, 7, 8, 9) ~ "Hottest4",
    MM %in% c(12, 1, 2, 3)  ~ "Coldest4",
    MM %in% c(4, 5, 10, 11)  ~ "Medium4"))
# 3a.v Keep only variables of interest 
dta <- dta %>% dplyr::select(Season4mo, MeanAbsFDT_24hr, MeanFDT_24hr)
# 3a.vi Split into three datasets 
hot  <- dta %>% filter(Season4mo == "Hottest4")
cold <- dta %>% filter(Season4mo == "Coldest4")
med  <- dta %>% filter(Season4mo == "Medium4")

# 3b Create percentile table
percentile.table <- data.frame(
  TVMetric = c("MeanAbsFDT", "MeanAbsFDT","MeanFDT", "MeanFDT"),
  percentile = rep(c(10, 90)),
  allyear.perc = c(quantile(dta$MeanAbsFDT_24hr, 0.10), quantile(dta$MeanAbsFDT_24hr, 0.90), 
                   quantile(dta$MeanFDT_24hr, 0.10), quantile(dta$MeanFDT_24hr, 0.90)),
  hot.perc = c(quantile(hot$MeanAbsFDT_24hr, 0.01), quantile(hot$MeanAbsFDT_24hr, 0.99), 
               quantile(hot$MeanFDT_24hr, 0.01), quantile(hot$MeanFDT_24hr, 0.99)),
  cold.perc = c(quantile(cold$MeanAbsFDT_24hr, 0.01), quantile(cold$MeanAbsFDT_24hr, 0.99), 
                quantile(cold$MeanFDT_24hr, 0.01), quantile(cold$MeanFDT_24hr, 0.99)),
  med.perc = c(quantile(med$MeanAbsFDT_24hr, 0.01), quantile(med$MeanAbsFDT_24hr, 0.99), 
               quantile(med$MeanFDT_24hr, 0.01), quantile(med$MeanFDT_24hr, 0.99))
)

# 3c Compare percentiles
percentile.table <- percentile.table %>% 
  mutate(hot.okay = if_else(percentile==10, hot.perc < allyear.perc, hot.perc > allyear.perc), 
         cold.okay = if_else(percentile==10, cold.perc < allyear.perc, cold.perc > allyear.perc),
         med.okay = if_else(percentile==10, med.perc < allyear.perc, med.perc > allyear.perc))

# 3d Save results 
percentile.table %>% write_csv(paste0(tables.folder, "season_overlap.csv"))
