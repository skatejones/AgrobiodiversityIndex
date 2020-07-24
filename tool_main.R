### main.R ###

# Sarah needs to fix:
# China has multiple entries for several indicators
# Lao too

### version 2020-04-06 14:00

## set working directory 
setwd("C:/Users/sjones/Documents/02_Bioversity/24_ABD_index/Science/ABDI_Tool/R" )

## load the libraries
suppressPackageStartupMessages({
  library(gdalUtils)
  library(rgeos)
  library(data.table) # for large datasets. For a crash course see https://www.listendata.com/2016/10/r-data-table.html 
  library(readxl)
  library(openxlsx)
  library(raster)
  library(rgdal)
  library(magrittr)
  library(vegan) # for diversity metrics
  library(dplyr) # for data manipulation
  library(foreign) # for reading dbf files
  library(Hmisc) # for statistically describing data
})

## load the file/dir paths and the parameters
# (if the data gets updated, changes will need to be applied in the config.R file)
source("tool_config.R")

## check indicators in master file
#d <- setDT(read_excel(path = path_master_file, sheet = "ABDI_2019", skip =1))
indicators <- setDT(read_excel(path = path_master_file, sheet = "ABDI_2020", skip =1))
indicators[,c("Measurement_category","Sub_indicator_ID","Sub-indicator")]

## load the functions 
source("tool_func_utils.R") # functions for ancillary tasks such as country code formatting, metadata setup, gridding the world
source("tool_func_subindicators.R") # functions to compute each subindicator

## preprocess the spatial data
# to make the computation of the spatial data possible,
# all the spatial dataset is split into multiple files based on a world grid
source("tool_preprocess_spatial.R")

## compute and save the subindicators 
source("tool_compute_indicators.R")

# this step can take a long time so only do if spatial subindicators need updating
source("tool_compute_spatial_indicators.R")

# Combine all sub-indicators #
aggregate_subindicators(path_subindicator_dir=path_subindicator_dir, 
                        output_dir="C:/Users/sjones/Documents/02_Bioversity/24_ABD_index/Science/ABDI_Tool", 
                        name = paste0("all_subindicators_",format(Sys.time(), "%Y%m%d"),".xlsx"))

# Combine all timeseries sub-indicators #
# NOTE that this needs further work so each timestep is saved with timestep identifier
aggregate_subindicators(path_subindicator_dir=path_subindicator_dir, 
                        output_dir="C:/Users/sjones/Documents/02_Bioversity/24_ABD_index/Science/ABDI_Tool", 
                        name = paste0("all_timeseries_subindicators_",format(Sys.time(), "%Y%m%d"),".xlsx"))
