# Temperature Variability - Myocardial Infarction
Comparison of temperature variability metrics and association with myocardial infarction in NYS 
companion to manuscript: Rowland, Sebastian T., Robbie M. Parks, Amelia K. Boehme, Jeff Goldsmith, Johnathan Rush, Allan C. Just, and Marianthi-Anna Kioumourtzoglou. "The association between ambient temperature variability and myocardial infarction in a New York-State-based case-crossover study: An examination of different variability metrics." Environmental Research 197 (2021): 111207.

Readme 

1. Please run 0_00_create_folder_structure.R first to create folder structure, load packages, etc. 
Also run this script prior to any other script to generate the strings for the folders.

2. Folder Structure 
lvl 1: Project (eg Temp_CVD_Analysis)
lvl 2: Analysis (eg TempVar_mi)
lvl 3: Components (Data, Scripts, Outputs) 

3. Prefix categories
Folder: DataPrep
a_ Prepare hourly weather data from raw gribs from nldas 
* Note some of these steps were done on my personal computer and so file paths refer to my personal laptop
b_ Extract cases from SPARCS data 
c_ Create case-crossover dataset and assign exposure

Folder: TempVar_MI
* A_ Data processing (unused)
* B_ All descriptive analyses and summary (no health models) 
* C_ All main health models (examining df, stratification, declared, planned out) 
* D_ Sensitivity analyses
* E_ Exploratory analyses that may be new inquiries (not necessarily part of paper)
* F_ Plotting models or similar post-modeling processing
* G_ Final versions of plots for manuscript
* H_ Other
