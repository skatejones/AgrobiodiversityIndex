# Compute spatial indicators

### version 2020-03-26 15:30

#### AP2I02_04 Spatial ####

## calculation
out <- get_AP2I02_04(
  dir_af_rasters = path_af_preprocessed_dir,
  dir_countries = path_country_preprocessed_dir,
  country_col = "GID_0",
  #country_index = country_index[grepl("^AF",GID_0)],
  country_index = country_index[GID_0!="ATA"], # exclude Antarctica (it creates an error and we don't need it)
  metadata_master= master,
  user = user,
  country_cv = country_cv,
  SI="AP2I02_04")

## export
write.xlsx(out, file.path(path_subindicator_dir,"AP2I02_04.xlsx"))

#out <- get_AP2I02_04_pixel(
#  dir_af_rasters = path_af_preprocessed_dir,
#  dir_countries = path_country_preprocessed_dir,
#  country_col = "GID_0",
#  country_index = country_index,
#  metadata_master= master,
#  user = user,
#  country_cv = country_cv)
## export
#write.xlsx(out, file.path(path_subindicator_dir,"AP2I02_04_pixel.xlsx"))

#### AP2I03_01 ####
# Diversification through crop-livestock systems

## calculation
#if(file.exists(file.path(path_subindicator_dir,"AP2I03_01.xlsx"))==T){
#  x <- readline("File AP2I03_01 (crop-livestock diversity) already exists. Sure you want to recompute? (y/n)\n")
#  }
out <- get_AP2I03_01(
  input = path_ramankutty_preprocessed_dir, 
  dir_countries = path_country_preprocessed_dir, 
  #country_index = country_index[grepl("^PER",GID_0)],
  country_index = country_index,
  metadata_master= master,
  user = user,
  country_cv = country_cv,
  SI="AP2I03_01")

## export
write.xlsx(out, file.path(path_subindicator_dir,"AP2I03_01.xlsx"))

#out <- get_AP2I03_01_pixel(
#  dir_ramankutty_rasters = path_ramankutty_preprocessed_dir, 
#  dir_countries = path_country_preprocessed_dir, 
#  # country_index = country_index[grepl("^A[NZ]",GID_0)],
#  # country_index = country_index[grepl("^AGO",GID_0)],
#  country_index = country_index,
#  metadata_master= master,
#  user = user,
#  country_cv = country_cv)
## export
#write.xlsx(out, file.path(path_subindicator_dir,"AP2I03_01_pixel.xlsx"))

#### SP2I12_01 ####
# Percentage of agricultural land (pixels) with >= 22 crops
## calculation

out <- get_SP2I12_01(
  input = path_spam_22crops_preprocessed_dir,
  dir_countries = path_country_preprocessed_dir,
  # country_index = country_index[grepl("^AG",GID_0)],
  country_index = country_index,
  metadata_master= master,
  user = user,
  country_cv = country_cv,
  SI="SP2I12_01")

## export
write.xlsx(out, file.path(path_subindicator_dir,"SP2I12_01.xlsx"))

## calculation
#out <- get_SP2I12_01_pixel(
#  dir_spam_rasters = path_spam_harv_preprocessed_dir,
#  dir_countries = path_country_preprocessed_dir,
#  # country_index = country_index[grepl("^A[NZ]",GID_0)],
#  country_index = country_index,
#  metadata_master= master,
#  user = user,
#  country_cv = country_cv)
## export
#write.xlsx(out, file.path(path_subindicator_dir,"SP2I12_01_pixel.xlsx"))

#### SP2I12_02 ####
# country mean Shannon index
## calculation
out <- get_SP2I12_02(
  input = path_spam_shannon_preprocessed_dir, # dir_spam_rasters
  dir_countries = path_country_preprocessed_dir,
  # country_index = country_index[grepl("^A[NZ]",GID_0)],
  country_index = country_index,
  metadata_master= master,
  user = user,
  country_cv = country_cv,
  SI="SP2I12_02")
## export
write.xlsx(out, file.path(path_subindicator_dir,"SP2I12_02.xlsx"))

#### SP2I12_02 time-series ####

# 2000 
out <- get_SP2I12_02(
  input = path_spam_shannon_T2000_preprocessed_dir, # dir_spam_rasters
  dir_countries = path_country_preprocessed_dir,
  # country_index = country_index[grepl("^A[NZ]",GID_0)],
  country_index = country_index,
  metadata_master= master,
  user = user,
  country_cv = country_cv,
  SI="SP2I12_02")

## add timestepidentifier
out$timestep <- c("2000")

## export
write.xlsx(out, file.path(path_timeseries_dir,"SP2I12_02_T2000.xlsx"))

# 2005
out <- get_SP2I12_02(
  input = path_spam_shannon_T2005_preprocessed_dir, # dir_spam_rasters
  dir_countries = path_country_preprocessed_dir,
  # country_index = country_index[grepl("^A[NZ]",GID_0)],
  country_index = country_index,
  metadata_master= master,
  user = user,
  country_cv = country_cv,
  SI="SP2I12_02")

## add timestepidentifier
out$timestep <- c("2005") 

## export
write.xlsx(out, file.path(path_timeseries_dir,"SP2I12_02_T2005.xlsx"))

# 2010
out <- get_SP2I12_02(
  input = path_spam_shannon_T2010_preprocessed_dir, # dir_spam_rasters
  dir_countries = path_country_preprocessed_dir,
  # country_index = country_index[grepl("^A[NZ]",GID_0)],
  country_index = country_index,
  metadata_master= master,
  user = user,
  country_cv = country_cv,
  SI="SP2I12_02")

## add timestepidentifier
out$timestep <- c("2010") 

## export
write.xlsx(out, file.path(path_timeseries_dir,"SP2I12_02_T2010.xlsx"))


#### SPAM_N ####
#Country mean crop species richness
out <- get_SP2I12_03(
  input = path_spam_richness_file,
  dir_countries = path_country_preprocessed_dir,
  country_index = country_index,
  metadata_master= master,
  user = user,
  country_cv = country_cv,
  SI="SP2I12_03")

## export
write.xlsx(out, file.path(path_subindicator_dir,"SP2I12_03.xlsx"))

#### SPAM_N time-series ####

# 2000
out <- get_SP2I12_03(
  input = path_spam_richness_T2000_file,
  dir_countries = path_country_preprocessed_dir,
  country_index = country_index,
  metadata_master= master,
  user = user,
  country_cv = country_cv,
  SI="SP2I12_03")

## add timestepidentifier
out$timestep <- c("2000") 

## export
write.xlsx(out, file.path(path_timeseries_dir,"SP2I12_03_T2000.xlsx"))

# 2005
out <- get_SP2I12_03(
  input = path_spam_richness_T2005_file,
  dir_countries = path_country_preprocessed_dir,
  country_index = country_index,
  metadata_master= master,
  user = user,
  country_cv = country_cv,
  SI="SP2I12_03")

## add timestepidentifier
out$timestep <- c("2005") 

## export
write.xlsx(out, file.path(path_timeseries_dir,"SP2I12_03_T2005.xlsx"))

# 2010
out <- get_SP2I12_03(
  input = path_spam_richness_T2010_file,
  dir_countries = path_country_preprocessed_dir,
  country_index = country_index,
  metadata_master= master,
  user = user,
  country_cv = country_cv,
  SI="SP2I12_03")

## add timestepidentifier
out$timestep <- c("2010") 

## export
write.xlsx(out, file.path(path_timeseries_dir,"SP2I12_03_T2010.xlsx"))


#### SP2I21_01 ####
# mean soil biodiversity index per country
## calculation
out <- get_SP2I21_01(
  dir_soilbiodiv_rasters = path_soilbiodiv_preprocessed_dir,
  dir_countries = path_country_preprocessed_dir,
  country_col = "GID_0",
  country_index = country_index,#[grepl("^A[NZ]",GID_0)],
  metadata_master= master,
  user = user,
  country_cv = country_cv,
  SI="SP2I21_01")
## export
write.xlsx(out, file.path(path_subindicator_dir,"SP2I21_01.xlsx"))

#### SP2I22_01  ####
# Percentage of agricultural landscapes with at least 10% natural land
## calculation
out <- get_SP2I22_01(
  dir_esa_rasters = path_esa_preprocessed_dir,
  dir_countries = path_country_preprocessed_dir,
  country_col = "GID_0",
  country_index = country_index, #[grepl("^A[NZ]",GID_0)],
  metadata_master= master,
  user = user,
  country_cv = country_cv,
  SI="SP2I22_01")
## export
write.xlsx(out, file.path(path_subindicator_dir,"SP2I22_01.xlsx"))

