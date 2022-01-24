# Function: Analyze association between TempVar and CVD
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/15/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Begin Function
# 2: Prepare Data
# 3: Stratify Data 
# 4: Fit Health Model 
# 5: Save Results

####**************
#### N: Notes #### 
####**************

# Na Description
# This code is the function for fitting all of the models 
# for the analysis 
# except for the negative control exposure sensitivity analysis 
# By using the same code, we can ensure that the exact same dataprocessing 
# and output is applied to each model, 
# without updating many individual codes 
# The code outputs a) the model, b) estimate for the mean to 10th and 90th percentile 
# c) for penalized spline models only, an ER plot.

# Nb OneBasis 
# I am using OneBasis as a convinent wrapper for the natural spline

# Nc ModelName 
# A critical object in this project is the ModelName 
# This identifiers provides all the unique information about a model 
# When you look a file name, you know exactly what the model is about


####***********************
#### 1: Begin Function ####
####***********************

# 1a Declare function
analyze_TempVar <- function(CaseType,  TempVarMetric, ERConstraint, SubPopVar, SubPop, Sensitivity){
  # CaseType is the outcome of interest; can be MI or stroke (only MI analyzed for manuscript)
  # TempVarMetric is the temperature variability metric used as the exposure 
  # ERConstraint is the constraint for the non-linearity of the exposure-response curve
  # SubPopVar is the variable used to divide the population; 'fullpop' refers to the whole study population
  # SubPop is the specific group we will examine; 'fullpop' refers to the whole study population 
  # Sensitivity determines any modificaitons ot the model or data for conducting an sensitivity analysis 
  # "Main" refers to the primary analysis.
  # 
  #NLDASWeight <- "Pop"; Timing <- "STEMI2hrDelay"
  #CaseType <- "mi";    TempVarMetric <-"MeanAbsFDT_24hr";   ERConstraint <- "3dfevenknots";  
  #SubPopVar <- "fullpop"; SubPop <- "fullpop"; Sensitivity <- "Main"
  
  # 1b Create ModelName
  ModelName <- paste0("TempVar", "_",CaseType, "_", TempVarMetric, "_ER", ERConstraint,
                      "_",SubPopVar, "_", SubPop, "_", Sensitivity)
  
  # 1c Set dataset parameters
  # For TempVar-MI, we always use population-weighted temperature data
  NLDASWeight <- "Pop"
  # Except for the sensitivity analysis where we set all events to 11:00 pm, we assume 2-hour 
  # delay between MI and hospitalization
  if(Sensitivity != "All23"){Timing <- "STEMI2hrDelay"}
  if(Sensitivity == "All23"){Timing <- "All23"}
  
  ####*********************
  #### 2: Prepare Data ####
  ####*********************
  
  # 2a Readin Data
  # we set MI_prim to 1 because all events in this dataset are primary MI events
  dta <- fst::read.fst(paste0(final.data.folder, "prepared_cohort", "_", CaseType,
                              "_deidentified", "_", NLDASWeight, "_", Timing, "_TVOnly.fst"))  %>% 
    mutate(MI_prim = 1)
  
  # 2b Rename TempVarMetric and RVarMetric 
  # this allows us to use the same analysis code for each variability metric
  # 2b.i create name
  RHVarMetric <- str_replace(TempVarMetric, "T_", "R_")
  # 2b.ii Rename variable
  dta <- dta %>% 
    rename(TempVar:= !!TempVarMetric, 
           RHVar:= !!RHVarMetric) 
  
  # 2c Create weather variables that correspond to exposure window
  # 2c.i Determine hours of exposure window
  NumHr <- as.numeric(str_remove_all(str_remove_all(TempVarMetric, "[A-z]"), "_") )
  # 2c.ii Create names 
  # we use minT and maxT in sensitivity analyses
  MeanTName <- paste0("MeanT_", NumHr, "hr")
  MeanRName <- paste0("MeanR_", NumHr, "hr")
  MinTName <- paste0("MinT_", NumHr, "hr")
  MaxTName <- paste0("MaxT_", NumHr, "hr")
  # 2c.iii Rename variable
  dta <- dta %>% 
    rename(MeanT := !!MeanTName,
           MeanR := !!MeanRName, 
           MinT := !!MinTName,
           MaxT := !!MaxTName)
  
  # 2d Keep only primary MI admissions according to outcome definition
  # we use the main primary outcome except for sensitivity analyses
  # the use different outcome criteria
  # 2d.i mi version 
  # this logical statement captures all analyses that  
  # use the main outcome definition
  # "prim_alt_410x1_A" only includes cases with ICD code 410.x1 in the principle diagnostic position
  # "prim_alt_410x01_A4" only includes cases with ICD code 410.x1 or 410.x0 in the principle diagnostic position to the fourth position
  # "prim_alt_410xx_A" only includes cases with ICD code 410.x1 or 410.x0 in the principle diagnostic position
  # "prim_60moRecurr" main criteria, but exclude any cases where the patient was hospitalized for MI within the previous 6 months
  # "prim_NoReinfarction" main criteria, but no recurrent cases at all
  

  if(!str_detect(Sensitivity, "prim") & CaseType == "mi"){
    dta <- dta %>% filter(MI_prim == 1)
  } else if(str_detect(Sensitivity, "prim_alt_410x1_A") & CaseType == "mi"){
    dta <- dta %>% filter(prim_alt_410x1_A == 1)
  } else if(str_detect(Sensitivity, "prim_alt_410x01_A4") & CaseType == "mi"){
    dta <- dta %>% filter(prim_alt_410x01_A4 == 1)
  } else if(str_detect(Sensitivity, "prim_alt_410xx_A") & CaseType == "mi"){
    dta <- dta %>% filter(prim_alt_410xx_A == 1)
  } else if(str_detect(Sensitivity, "prim_6moRecurr") & CaseType == "mi"){
    dta <- dta %>% filter(MI_prim == 1) %>% filter(SubsequentE != "Subsequent")
  } else if(str_detect(Sensitivity, "prim_alt_NoReinfarction") & CaseType == "mi"){
    dta <- dta %>% filter(prim_alt_NoReinfarction == 1)
  }
  
  # 2e Calculate mean and range of TempVar 
  # There are used for the crosspred() function 
  # although the mean is based on the all-year mean,
  # which can be changed for seasonal analyses
  mean.TempVar <- mean(dta$TempVar, na.rm = TRUE)
  min.TempVar <- min(dta$TempVar, na.rm = TRUE)
  max.TempVar <- max(dta$TempVar, na.rm = TRUE)
  
  ####***************************
  #### 4: Fit Health Models  ####
  ####***************************
  
  # 4b Create one-basis terms for TempVar
  # by using one-basis to represent the nonlinear terms, 
  # we can more easily wrangle results
  # 4b.i Set ER Constraint
  # we remove all of the letters from the  parameter, leaving just the number of df
  ERdf <- as.numeric(str_remove_all(ERConstraint, "[A-z]"))
  # 4b.ii Create crossbasis
  # note that if the constraint is penalized spline 
  # then we do not use onebasis() 
  # since it is harder to extract the edf from the model
  if(str_detect(ERConstraint, "evenknots")){
    ob.TempVar <- onebasis(dta$TempVar, fun = "ns", df = ERdf)
  }
  # 4c Fit Main Models
  # 4c.i Typical non-linear model 
  # a few sensitivity analyses change the regression formula; 
  # these are in the formulaList 
  # coxph() does not like when the formula is an object from the environment
  # 'CheckLinearity' fit with penalized spline to check for non-linearity 
  # 'NOmeanRH' sensitivity analysis without covariate term for mean RH 
  # 'NOvarRH' sensitivity analysis without covariate term for corresponding RH variability
  # 'NORH' sensitivity analysis without either RH term 
  # 'varRH5df' sensivitiy analysis where term for RH variability has 5 df rather than 4 
  # 'meanRH5df' sensivity analysis where term for meanRH had 5 df rather than 4 
  # 'meanT5df' sensivitiy analysis where term for mean temp has 5 df rather than 4 
  # 'MinT' sensivitiy analysis where we adjust for minimal temperature rather than mean
  # 'MaxT' sensivitiy analysis where we adjust for maximal temperature rather than mean
  
    formulaList <- c("CheckLinearity", "NOmeanRH","NOvarRH", "NORH",
                   "varRH5df", "meanRH5df", "MeanT5df", "MinT", "MaxT")
  # here the !() excludes all sensitivities based on the formulaList object
  if(!(Sensitivity %in% formulaList)) {
    mod <- coxph(Surv(TimetoEvent, Case) ~ ob.TempVar + 
                   ns(MeanT,4) + ns(MeanR,4) + ns(RHVar, 4) +
                   strata(EventID),
                 dta, ties = "efron")
  }
  #4c.ii Checking linearity models
  # these arguements for pspline yield a penalized spline with optimized constraints according to conditional AIC.
  # only used in initial exploratory analysis. 
  # Not 
  if(ERConstraint == "psp") {
    mod <- coxph(Surv(TimetoEvent, Case) ~ pspline(TempVar, df=0, caic=T) +
                   ns(MeanT,4) + ns(MeanR,4) +ns(RHVar, 4) +
                   strata(EventID),
                 dta, ties = "efron")
  }
  
  # 4d Sensitivity Models 
  # these models use different adjustment strategies and thus require their own lines 
  if(Sensitivity == "NOmeanRH"){
    mod <- coxph(Surv(TimetoEvent, Case) ~ ob.TempVar + 
                   ns(MeanT,4) + ns(RHVar, 4) +
                   strata(EventID),
                 dta, ties = "efron")
  }
  # 4b.iii NOvarRH
  if(Sensitivity == "NOvarRH"){
    mod <- coxph(Surv(TimetoEvent, Case) ~ ob.TempVar + 
                   ns(MeanT,4) + ns(MeanR,4) +
                   strata(EventID),
                 dta, ties = "efron")
  }
  # 4b.iv NORH
  if(Sensitivity == "NORH"){
    mod <- coxph(Surv(TimetoEvent, Case) ~ ob.TempVar + 
                   ns(MeanT,4) + 
                   strata(EventID),
                 dta, ties = "efron")
  }
  # 4b.iv varRH5df
  if(Sensitivity == "VarRH5df"){
    mod <- coxph(Surv(TimetoEvent, Case) ~ ob.TempVar + 
                   ns(MeanT,4) + ns(MeanR,4) +ns(RHVar, 5) +
                   strata(EventID),
                 dta, ties = "efron")
  }
  # 4b.v RH5df
  if(Sensitivity == "MeanRH5df"){
    mod <- coxph(Surv(TimetoEvent, Case) ~ ob.TempVar + 
                   ns(MeanT, 4) + ns(MeanR, 5) +ns(RHVar, 4) +
                   strata(EventID),
                 dta, ties = "efron")
  }
  
  # 4b.vi MeanT5df
  if(Sensitivity == "MeanT5df"){
    mod <- coxph(Surv(TimetoEvent, Case) ~ ob.TempVar + 
                   ns(MeanT, 5) + ns(MeanR, 5) +ns(RHVar, 4) +
                   strata(EventID),
                 dta, ties = "efron")
  }
  
  # 4b.vii MinT
  if(Sensitivity == "MinT"){
    mod <- coxph(Surv(TimetoEvent, Case) ~ ob.TempVar + 
                   ns(MinT, 4) + ns(MeanR, 5) +ns(RHVar, 4) +
                   strata(EventID),
                 dta, ties = "efron")
  }
  
  # 4b.vii MaxT
  if(Sensitivity == "MaxT"){
    mod <- coxph(Surv(TimetoEvent, Case) ~ ob.TempVar + 
                   ns(MaxT, 4) + ns(MeanR, 5) +ns(RHVar, 4) +
                   strata(EventID),
                 dta, ties = "efron")
  }
  
  ####**********************
  #### 5: Save Results  ####
  ####**********************
  
  # 5a Save Models 
  mod %>% saveRDS(paste0(models.folder, ModelName, ".RDS"))
  
  # 5b Create predictions
  # we need this if() statement to avoid the psp models throwing an error
  if(Sensitivity != "CheckLinearity"){
    
    # the cen argument sets the reference exposure level for our effect estimates
    # crosspred() will yield estimates for 100 exposure levels
    # we center at mean temperature variability so that we can express effect estimates 
    # as increase or decreases in variability relative to the mean
    pred <- crosspred(ob.TempVar,mod,cen = mean.TempVar,dataset, 
                      at = seq(min.TempVar, max.TempVar, length.out = 100))
    
    # 5c Extract coefficient fit  
    fit.table <- as.data.frame(pred$matRRfit)  
    colnames(fit.table) <- paste0("fit.or")
    fit.table <- fit.table %>%  
      mutate(TempVar = as.numeric(row.names(fit.table)))
    
    # 5d Extract 95% CI  
    lci.table <- as.data.frame(pred$matRRlow)  
    colnames(lci.table) <- paste0("lci.or")
    uci.table <- as.data.frame(pred$matRRhigh)  
    colnames(uci.table) <- paste0("uci.or")
    
    # 5e Combine fit and se 
    # note that all OR are relative to the mean of that variability metric
    pred.table <- bind_cols(fit.table, lci.table, uci.table)
    
    # 5f Save prediction tables
    pred.table %>% write_csv(paste0(estimates.folder, ModelName, ".csv"))
  }
  # 5g Plot psplines 
  if(Sensitivity == "CheckLinearity"){
    convert_to_percent <- function(x){100*(x-1)}
    termPSP <- termplot(mod, term = 1, se = T,plot = F)$TempVar
    termPSP <- termPSP %>% 
      rename(TempVar = x) %>% 
      mutate(fit.or = exp(y)) %>% 
      mutate(lci.or = exp(y - 1.96*se), 
             uci.or = exp(y + 1.96*se)) %>% 
      mutate(fit.pc = convert_to_percent(fit.or), 
             lci.pc = convert_to_percent(lci.or),
             uci.pc = convert_to_percent(uci.or)) %>% 
      mutate(Term = "PSP") %>% 
      sample_frac(0.1) %>% 
      write_csv(paste0(estimates.folder, ModelName, ".csv"))
    plot.psp <- ggplot(termPSP) + 
      geom_hline(yintercept = 0, color ="grey" ) + 
      geom_line(aes(x = TempVar, y = fit.pc, color = Term)) +    
      geom_ribbon(aes(x = TempVar, ymin= lci.pc,  ymax = uci.pc, fill = Term),
                  color = NA, alpha  = AlphaCI) +
      labs(y = paste0("Change in \n MI Hospitalizations (%)"), 
           x = TempVarMetric) 

    
    png(paste0(plots.folder, "CheckLinearity_", ModelName, ".png"))
    print(plot.psp)
    dev.off()
  }
  
}

