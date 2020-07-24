# config.R

### version 2020-04-12 10:00

user <- "Sarah Jones" 

### path to the directory where the subindicators are to be saved ####
path_subindicator_dir <- file.path("..","subindicator") 
# ".." means move up one directory
# "../.." means move up two directories
  
### path to directory timeseries outputs will be saved ####
path_timeseries_dir <- file.path("..","timeseries")

### path to the data directory ####
path_data_dir <- file.path("..", "data")

### path to the R scripts ####
path_scripts_dir <- file.path("..", "R")

### ABDI metadata for the subindicators ####
path_master_file <- file.path("..","R", "ABDI_indicators_master.xlsx")
master <- setDT(read_excel(path = path_master_file, sheet = "ABDI_2020", skip =1))

### controled vocabulary for country names
path_country_cv <- file.path("..","R", "Country_codes.xlsx") ####
country_cv_raw <- as.data.table(read_excel(path = path_country_cv))

### non spatial data ###

## CPXIXX: Commitment sub-indicators ####
#path_commitments_file <- file.path(path_data_dir,"Commitments", "ABDI_Commitments_ALLDATA.xlsx")
path_commitments_file <- file.path(path_data_dir,"Commitments", "Commitments_nbsaps_20200609.xlsx")

## AP1I01_01: Dietary guidelines ####
path_fbdg_file <- file.path(path_data_dir,"FAO","FAO_dietary_guidelines.xlsx")

## AP1I01_02: Food composition tables ####
path_foodcomp_file <- file.path(path_data_dir,"ILSI","Food composition tables.xlsx")

## AP2I02_01: Nitrogen use efficiency ####
path_nue_file <- file.path(path_data_dir,"SMNI","Zhang_etal_NUE_Supp1.xlsx")

## AP2I02_05: Pesticide ####
#path_pest_file <- file.path(path_data_dir,"FAO",  "FAOSTAT_data_3-30-2018_pesticides.csv")
path_pest_file <- file.path(path_data_dir,"FAO",  "FAOSTAT_data_3-20-2020_pesticides.csv")

## AP2I02_02: SNMI ####
path_snmi_file <- file.path(path_data_dir, "EPI", "epi_2018.csv")

## AP2I02_06: Organic (FAO) ####
#path_fao_organic_file <- file.path(path_data_dir,  "FAO", "FAOSTAT_data_18-06-2018_organic.csv")
path_fao_organic_file <- file.path(path_data_dir,  "FAO", "FAOSTAT_data_3-20-2020_organic.csv")

## AP2I02_08: Conservation Ag (FAO) ####
path_aquastat_ca_file <- file.path(path_data_dir, "FAO", "aquastat_ca_20191205.csv") 

## AP2I03_02: ILIs with pro-ABD investments ####
path_ili_file <- file.path(path_data_dir,  "ILI", "ILIs_All.xlsx")

### AP3I04_06: WIEWS UPDATE AVAILABLE?####
# International reporting on plant genetic resources for food and agriculture - WIEWS
path_wiews_file <- file.path(path_data_dir,"WIEWS","WIEWS indicators.xlsx")

## SP1I11_01: Diversity in supply ####
path_foodbal_file <- file.path(path_data_dir,"FAO",  "FAOSTAT_data_3-11-2020_foodbalance.csv")

#### SP1I14_01: DALYs ####
# Nutritional function diversity (DALYs)
path_daly_file <- file.path(path_data_dir,"DALYs","DALY's Health effects of dietary risks in 195 countries.xlsx")

#### SP1I17_01: Non-staples ####
# Energy from non-staples
#path_energy_file <- file.path(path_data_dir,"FAO","Energy from non-staples.xlsx")
path_energy_file <- file.path(path_data_dir,"FAO","FAOSTAT_data_3-20-2020_energy.csv")

## SP2I12_03: FAO crop production richness ####
path_fao_crops_file <- file.path(path_data_dir,"FAO","FAOSTAT_data_3-12-2020_crop_production.csv") 

## SP2I12_04: Fish richness  ####
path_fish_file <- file.path(path_data_dir,"Tedesco","cty_fish_srichness.dbf") 

#### SP2I12_05: Livestock diversity ####
# Species diversity of livestock in production
path_livestock_shannon_file <- file.path(path_data_dir, "GLW", "Livestock_8_shannons_LSU_cty.dbf")

#### SP2I12_06 Livestock richness  ####
# Species richness of livestock in production
path_livestock_richness_file <- file.path(path_data_dir, "GLW", "Livestock_8_richness_cty.dbf")

## SP2I13_01: Livestock breed richness ####
path_livestock_breeds_file <- file.path(path_data_dir, "FAO", "DADIS_population_data.csv")

### SP3I10_02 & SP3I13_02: Genesys genebank data  ####
## path of the raw accession data downloaded from https://api.genesys-pgr.org/
path_genesys_file <- file.path(path_data_dir, "Genesys", "all_genesys_diversity_20200311.csv")

## SP3I13_01: CWR species ####
path_cwr_file <- file.path(path_data_dir,  "GBIF", "CWR", "GBIF_CWR_20200320.csv")

## SP3I19_01: NUS ####
path_nus_file <- file.path(path_data_dir,"NUS_intertropical","NUS_diversity_results.csv")

### SP3I19_02 and SP3I19_03: In-situ Ex-situ ####
# Conservation representativeness
path_khoury_file <- file.path(path_data_dir,"Khoury","SP3I19_03.xlsx")

### SP3 CWR index NEW ####
path_wep_file <- file.path(path_data_dir,"Khoury","WEP_taxonomy_distributions_uses_final.xlsx")
path_iucn_file <- file.path(path_data_dir,"IUCN","IUCN list of plants.csv")

### Spatial data ####

path_spatial_data_preprocessed_dir <- file.path(path_data_dir, "spatial_preprocessed")
dir.create(path_spatial_data_preprocessed_dir, showWarnings = F)

### world reference grid
path_ref_grid_dir <- file.path(path_spatial_data_preprocessed_dir, "reference_world_grid")
tile_size <- 15 # size of the squares of the grid (in degree) 
# caution: the tile size has to be the same for the entire computation

### country geometries
## source data
path_country_dir <- file.path(path_data_dir,"GADM")
country_layer_name <- "gadm36_countries"

## split on the reference grid
path_country_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "countries")
dir.create(path_country_preprocessed_dir, showWarnings = F)

### AP2I02_04: Agroforestry ####
## source data
path_af_file <- file.path(path_data_dir, "ICRAF", "tc_ag_2010.tif")
## split on the reference grid
path_af_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "af")

### AP2I03_01: Ramunkutty pasture-crop ####
## source data
path_pasture_file <- file.path(path_data_dir,"Ramankutty", "pasture2000_area.tif")
path_cropland_file <- file.path(path_data_dir,  "Ramankutty", "cropland2000_area.tif")
## split on the reference grid
path_ramankutty_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "ramankutty")

## SP2I12_01: SPAM %land >22 crops ####
#path_spam_22crops_file <- file.path(path_data_dir,"SPAM","t_sr_2010_spam_42c_maj22_2.xlsx")
path_spam_22crops_file <- file.path(path_data_dir,  "SPAM", "sr_2010_spam_42c.tif")
## split on the reference grid
path_spam_22crops_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "SPAM_22crops")

#### SP2I21_01: Soil Biodviersity ####
## source data
path_soilbiodiv_file <- file.path(path_data_dir, "ESDAC", "soil_bio_new.tif")
## split on the reference grid
path_soilbiodiv_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "soilbiodiv")

#### SP2I22_01: ESA nat in ag ####
## source data
path_esa_file <- file.path(path_data_dir,  "ESA", "esa2015_natag_1km_gt10.tif")
## split on the reference grid
path_esa_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "ESA_natag")

### SP2I12_02: SPAM diversity ####
## source data
path_spam_shannon_file <- file.path(path_data_dir, "SPAM", "H_2010_spam_42c.tif")
## split on the reference grid
path_spam_shannon_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "SPAM_shannon")

### SP2I12_02: SPAM diversity time series ####
path_spam_shannon_T2000_file <- file.path(path_data_dir, "SPAM", "H_2000_spam_20c.tif")
## split on the reference grid
path_spam_shannon_T2000_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "SPAM_T2000_shannon")

path_spam_shannon_T2005_file <- file.path(path_data_dir, "SPAM", "H_2005_spam_20c.tif")
## split on the reference grid
path_spam_shannon_T2005_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "SPAM_T2005_shannon")

path_spam_shannon_T2010_file <- file.path(path_data_dir, "SPAM", "H_2010_spam_20c.tif")
## split on the reference grid
path_spam_shannon_T2010_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "SPAM_T2010_shannon")

# Time-series indicators not in ABDI ####
### SPAM_N: SPAM richness ####
## source data
path_spam_richness_file <- file.path(path_data_dir,  "SPAM", "sr_2010_spam_42c.tif")
## split on the reference grid
path_spam_richness_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "SPAM_richness")

### SPAM_N : SPAM richness time series ####
## source data
path_spam_richness_T2000_file <- file.path(path_data_dir,  "SPAM", "sr_2000_spam_20c.tif")
## split on the reference grid
path_spam_richness_T2000_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "SPAM_T2000_richness")

## source data
path_spam_richness_T2005_file <- file.path(path_data_dir,  "SPAM", "sr_2005_spam_20c.tif")
## split on the reference grid
path_spam_richness_T2005_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "SPAM_T2005_richness")

## source data
path_spam_richness_T2010_file <- file.path(path_data_dir,  "SPAM", "sr_2010_spam_20c.tif")
## split on the reference grid
path_spam_richness_T2010_preprocessed_dir <- file.path(path_spatial_data_preprocessed_dir, "SPAM_T2010_richness")

