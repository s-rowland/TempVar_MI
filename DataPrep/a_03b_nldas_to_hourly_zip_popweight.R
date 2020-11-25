# Compute Population-Weighted Average Hourly Weather Parameters
# Data Prep
# Temperature-CVD-NYS Project
# Sebastian T. Rowland
# Updated 09/01/2020

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation 
# 1: Prepare zcta Data
# 2: Compute Weights Matrix 
#  2A Prepare weather grid data
#  2B Convert grid centroids to polygons
#  2C Create Voroni or Thiessien Polygons from Centroids 
#  2D Compute Population Within Each Intersection Segment
#  2E Compute Area of Intersection between zctas and Grid Cells
#  2F Create the Weights Matrix
#  2G: Prepare weather Data 
#  2H: Compute Area-Weighted zcta-year weathers
#  2I: Save Results

####**************
#### N: Notes ####
####**************

# Na Description
# Here we calculate population-weighted averages. 
# The core idea is that we first create a matric of weights 
# so that each NLDAS grid has a weight for each ZCTA 
# if the grid does not overlap with a ZCTA, then the weight is zero.
# A more precise method like ABODE would better distinguish
# population density within a census tract 
# but this approach should be sufficient 
# for a feature like ambient temperature that is fairly homogeneous across space
# Once we have the weights, we just matrix multiply the weights by the nldas data.
# and all the contributions add up as they should. 
# by doing matrix multiplication, we only need to compute the weights once.
# This script takes about 3.5 hours to run 

# Nb Loops
# This is currently set up as a loop 
# but it can easily be converted to parallel via purrr 

# Nc Assumption 
# this method is very efficient, but does assume that the weather grid has a measurement 
# for every grid for every time

# Nd Assumption: Planar
# currently step 2d.i Create Intersections 
# treats the coordinates as planar

# Ne Assumption: Constant Population 
# Unlikely to impact results for temperature, sph, or pressure. 
# we assign constant weights across all years. 
# This can easily be updated with more Census data.

####********************
#### 0: Preparation ####
####********************

# 0a Install packages 
library(sf)
library(tidyverse) 
library(fst)
library(deldir)
library(dismo)
library(rgdal)
library(lwgeom)

# 0b Declare directories
project.folder           <- "H:/Temp_CVD_Analysis/"
analysis.folder          <- paste0(project.folder,"DataPrep/")
data.folder              <- paste0(analysis.folder, "Data/")
raw.data.folder          <- paste0(data.folder,"Raw_Data/")
raw.outcome.folder       <- paste0(raw.data.folder, "Outcome_Data/")
intermediate.data.folder <- paste0(data.folder, "Intermediate_Data/")
final.data.folder        <- paste0(data.folder, "Final_Data/")
aggregated.NLDAS.folder <- paste0(intermediate.data.folder, "NLDAS_ZCTA_Data_PopWeighted/")

# 0c Declare active years and variables 
VarList <- c(rep("TMP", 22), rep("SPFH", 22), rep("PRES", 22))
YearList <- c(rep(c(1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 
                    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,2014, 2015, 2016),3))
ComboList <- paste0(YearList, "_", VarList)

# ComboList <- ComboList[1]

####**************************
#### 1: Prepare zcta Data ####
####**************************

# 1a Read in area-tract data 
area.tract <- st_read("H:/Temp_CVD_Analysis/DataPrep/Data/Raw_Data/tl_2010_36_tract10/tl_2010_36_tract10.shp")
area.tract <- area.tract %>% 
  mutate(GEO.id2 = as.character(GEOID10), 
         AreaLand = as.numeric(as.character(ALAND10)), 
         AreaWater = as.numeric(as.character(AWATER10)))

# 1b Convert projection  
st_crs(area.tract) <- "+proj=longlat +datum=WGS84"

# 1c Read in tract-level population data 
pop.tract <- read_csv("H:/Temp_CVD_Analysis/DataPrep/Data/Raw_Data/NYS_Census_raw/ACS_10_5YR_S0101_with_ann.csv")

# 1d Wrangle data
pop.tract <- pop.tract[2:nrow(pop.tract),]
pop.tract <- pop.tract %>% dplyr::select(GEO.id2, HC01_EST_VC01) %>% 
  rename(pop = HC01_EST_VC01)  %>% 
  mutate(GEO.id2 = as.character(GEO.id2), 
         pop = as.numeric(pop))

# 1e Combina area and pop data 
tract <- left_join(area.tract, pop.tract, by = "GEO.id2") 

# 1f Calculate density 
tract <- tract %>% mutate(area = AreaLand + AreaWater) %>%
  mutate(density = pop / area)

# 1g Keep variables of interest 
tract <- tract %>% dplyr::select(density, GEO.id2, geometry)

# 1h Read in Area-zcta data 
area.zcta <- st_read("H:/Temp_CVD_Analysis/DataPrep/Data/Raw_Data/tl_2010_36_zcta510/tl_2010_36_zcta510.shp")
area.zcta<- area.zcta %>% mutate(zcta = as.character(ZCTA5CE10), 
                                AreaLand = as.numeric(as.character(ALAND10)), 
                                AreaWater = as.numeric(as.character(AWATER10)))
# 1i Convert projection 
st_crs(area.zcta) <- "+proj=longlat +datum=WGS84"

# 1j Create intersections
zcta.intersec <- st_intersection(area.zcta, tract)
# the warning message is that we are ignoring the spherical nature of the globe
# for a small area like zcta's and census tracts, 
# this assumption leads to negligible error

# 1k Adjust the sf 
# without this step we get an error in the instersection step
area.zcta <- area.zcta %>% 
  st_set_precision(1e5) %>% 
  st_make_valid()
tract <- tract %>% 
  st_set_precision(1e5) %>% 
  st_make_valid()

# 1l Make intersction 
zcta <- st_intersection(tract, area.zcta)

# 1m Keep variables of interest
zcta <- zcta %>% 
  dplyr::select(geometry, density, zcta, GEO.id2) %>% 
  rename(tract = GEO.id2)

# 1n Clean up environment
rm(area.zcta, area.tract, pop.tract, tract)

####*******************************
#### 2: Compute Weights Matrix ####
####*******************************

# 2a Initiate loop over ComboList
for (i in 1:length(ComboList)){

  ####**********************************
  #### 2A Prepare Weather Grid Data ####
  ####**********************************
  # 2A.a Set the year-parameter combo
  YYYY_VVV <- ComboList[[i]]
  # 2A.b Readin the raw weather data 
  nldas <- read.fst(paste0(raw.data.folder, "NLDAS_raw/", YYYY_VVV, "_NLDAS_NY.fst"))
  # 2A.c Put weather data in long format
  nldas.l <- nldas %>% 
    gather("Time", "temp", 4:NCOL(nldas)) %>% 
    rename(lat = y, lon = x)
  # 2A.d Choose just the first hour of weather data
  # since we first construct a weight matrix relating 
  # each weather grid to each zcta 
  # we do not actually need weather values 
  # we really just want the grid centroid locations 
  zcta2 <- zcta
  zcta2$DateTime <- nldas.l$Time[1]
  nldasgrid <- nldas.l %>% filter(Time == nldas.l$Time[1]) 
  
  ####*******************************************
  #### 2B Convert Grid Centroids to Polygons ####
  ####*******************************************
  # 2B.a Isolate just the lat and lon values
  nldas.coord <- cbind(nldasgrid$lon, nldasgrid$lat)
  nldasgrid2 <- nldasgrid 
  nldasgrid2$lat <- NULL 
  nldasgrid2$lon <- NULL
  pcs <- CRS("+proj=longlat +datum=WGS84")
  # this step creates a spatial dataframe 
  # where the coordinates of that dataframe 
  # are the coordinates of the grid centroids 
  # this step may seem unnecessary- why not convert directly? 
  # but I remember getting errors when I converted directly
  nldasgrid.sp <- SpatialPointsDataFrame(nldas.coord, nldasgrid2, proj4string = pcs, match.ID = FALSE)
 
  ####***********************************************************
  #### 2C Create Voroni or Thiessien Polygons from Centroids ####
  ####***********************************************************
  # 2C.a Create Voroni or Thiessien Polygons from Centroids 
  # each pixel of area is assigned to the nearest controid. 
  vor <- voronoi(nldasgrid.sp)
  # 2C.b Write voroni polygons
  # at the moment, I found writing and then reading the data 
  # avoids certain errors with data structure 
  writeOGR(vor, "H:/Temp_CVD_Analysis/DataPrep/Data/Intermediate_Data/NLDAS_VOR", 
           layer = "NLDAS_VOR", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  # 2C.c Readin voroni polygons
  nldas.vor <- st_read("H:/Temp_CVD_Analysis/DataPrep/Data/Intermediate_Data/NLDAS_VOR/NLDAS_VOR.shp")
  # plot(nldas.vor)
  
  ####************************************************************
  #### 2D Compute Population Within Each Intersection Segment ####
  ####************************************************************
  # we are treating the area as a flat plane 
  # which is probably not valid for whole US
  # butsufficient for New York State & this size grid
  # 2d.a Create Intersections
  zcta.intersec <- st_intersection(zcta2, nldas.vor)
  # 2d.b Compute area of each intersection
  intersec.area <- zcta.intersec %>% 
    mutate(area = st_area(.) %>% as.numeric())
  # 2d.c Compute populations 
  intersec.area <- intersec.area %>% mutate(pop = area * density)
  
  ####**********************************
  #### 2E Create the Weights Matrix ####
  ####**********************************
  # weights are how much each grid cells contributes to each zcta
  # now convert to a matrix 
  # 2E.a Clean up dataframe of interscetions
  dta <- as.data.frame(intersec.area)
  dta <- dta %>% dplyr::select(zcta, nldas_uid, pop)
  # 2E.b Compute the total area for each zcta
  dta.totpop <- dta %>% 
    group_by(zcta) %>% 
    summarize(ZCTA.pop = sum(pop))
  # 2E.c For each zcta, percent the percent of its area contributed by each grid
  # note that it only computes the contribution of grids that contribute non-zero
  dta2 <- dta %>% 
    left_join(dta.totpop, by = "zcta") %>% 
    mutate(percent.pop = pop/ZCTA.pop)
  # this line just test that indeed, the contribute of the relevant grids sums to 100%
  test <- dta2 %>% 
    group_by(zcta) %>% 
    summarize(Tot = sum(percent.pop))
  # 2E.d Keep just what we want 
  dta3 <- dta2 %>% dplyr::select(zcta, nldas_uid, percent.pop)
  # 2E.e Combine spatial units from the same NLDAS grid 
  dta3 <- dta3 %>% 
    group_by(zcta, nldas_uid) %>% 
    summarize(percent.pop = sum(percent.pop)) %>% 
    ungroup()
  # 2E.f Put weights in wide matrix format
  # Spread puts our data in wide format 
  # fill = 0 means that for grid-zcta combinations where the 
  # grid does not contribute to that zcta 
  # the value of the cell will be zero
  dta4 <- dta3 %>% 
    spread(nldas_uid, percent.pop, fill = 0) %>% 
    arrange(zcta)
  weights.matrix <- dta4 %>% 
    dplyr::select(-zcta) %>% 
    as.matrix()
  # 2E.g Identify active grid cells that contribute to NYA
  # often NLDAS data is a rectangle, 
  # so we can just remove the grid cells that do not overlap with NYS
  ActiveGrids <- intersec.area %>% 
    as.data.frame() %>% 
    dplyr::select(nldas_uid) %>% 
    distinct()
  # 2E.h Track order of gridid's
  # since the weights matrix does not have the names of the gridids
  # we make this list to make sure the weather data is 
  # ordered the same way as the weights matrix
  GridOrderList <- names(dta4%>%dplyr::select(-zcta))
  
  ####******************************
  #### 2F: Prepare weather Data ####
  ####******************************
  # 2F.a Once again we readin the weather data 
  nldas2 <- read.fst(paste0(raw.data.folder, "NLDAS_raw/", YYYY_VVV, "_NLDAS_NY.fst"))
  
  # 2F.b Then, we eliminate any gridids that did not contribute to any zctas 
  nldas3 <- nldas2 %>% as.data.frame() %>% mutate(x = as.character(x), y = as.character(y))%>%
    inner_join(ActiveGrids, by = "nldas_uid") #%>%
  
  # 2F.c We then order our weather data according to the order of the weights matrix
  nldas3$nldas_uid <- factor(nldas3$nldas_uid, levels = GridOrderList)
  nldas4 <- nldas3 %>% arrange(nldas_uid)
  
  # 2F.d Convert weather to matrix form
  nldas.clean <- nldas4 %>% dplyr::select(-nldas_uid, -x, -y)  %>% as.matrix()
  
  ####*************************************************
  #### 2G: Compute Area-Weighted zcta-Year Weather ####
  ####*************************************************
  # 2G.a Perform matrix multiplication
  year_zcta <- weights.matrix %*% nldas.clean
  
  ####**********************
  #### 2H: Save Results ####
  ####**********************
  # 2H.a Convert zcta-level weather matrix to dataframe
  yz <- year_zcta %>% as.data.frame() 
  
  # 2H.b Attach the names of the zctas
  yz.withzcta <- yz %>% bind_cols(dta4%>%dplyr::select(zcta),.) 
  
  # 2H.c Save results
  # test to save small amount of data and review
  #nn <- nrow(yz.withzip)
  #nn2 <- floor(nn/2)
  #nn3 <- nn2 + 1
  #yz.withzip %>% slice(1:nn2) %>% write_fst(paste0(final.data.folder, "zip_",YYYY_VVV,"a.fst"))
  #yz.withzip %>% slice(nn2:nn) %>% write_fst(paste0(final.data.folder, "zip_",YYYY_VVV,"b.fst"))
  # option to zip the files
  #setwd("/Users/sebastianrowland/Desktop/assign_grid_zip2/Data/Final Data")
  #zip(zipfile = paste0("zip_",YYYY_VVV,"a"), files = paste0("zip_",YYYY_VVV,"a.fst"))
  #zip(zipfile = paste0("zip_",YYYY_VVV, "b"), files = paste0("zip_",YYYY_VVV,"b.fst"))
  
  yz.withzcta %>% write_fst(paste0(aggregated.NLDAS.folder, "zcta_",YYYY_VVV,".fst"))
  #zip(zipfile = paste0("tot_zcta_",YYYY_VVV), files = paste0("zcta_",YYYY_VVV,".fst"))
}
