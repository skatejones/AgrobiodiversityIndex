# compute_indicators.R

### version 2020-04-06 14:00

### CPXIXX_XX ####
# Commitment indicators
## calculation
out <- get_CPXIXX(input = path_commitments_file, 
                  input_sheet="score_final",
                  range="A1:S121",
                  #range="A1:S120",
                  #range="A1:V10",
                  country_col = "ISO3",
                  country_cv = country_cv, 
                  metadata_master = master, 
                  user = user)
## export
write.xlsx(out, file.path(path_subindicator_dir,"CPXIXX_nbsaps.xlsx"))

### AP1I01_01 ####
# Policies or guidelines leading to diverse diets: diet guidelines
out <- get_AP1I01_01(input = path_fbdg_file, input_sheet="FBDG",country_col="Country",country_cv = country_cv, metadata_master = master, user = user,SI="AP1I01_01")
## export
write.xlsx(out, file.path(path_subindicator_dir,"AP1I01_01.xlsx"))

### AP1I01_02 ####
# Resources to facilitate uptake of diverse diets: food composition table
out <- get_AP1I01_02(input = path_foodcomp_file, input_sheet="data",country_col="Country",country_cv = country_cv, metadata_master = master, user = user,SI="AP1I01_02")
## export
write.xlsx(out, file.path(path_subindicator_dir,"AP1I01_02.xlsx"))

### AP2I02_01 ####
# Nitrogen use efficiency
out <- get_AP2I02_01(input = path_nue_file, 
                     input_sheet = "NUE", 
                     input_skip = 1,
                     value_col="value",
                     country_col = 2, 
                     method = "latest year",
                     country_cv = country_cv,
                     metadata_master = master,
                     user=user,
                     SI = "AP2I02_01")

## export
write.xlsx(out, file.path(path_subindicator_dir,"AP2I02_01.xlsx"))

### AP2I02_02 ####
# Integrated Plant Nutrient Management (IPNM)
# Inverse of the Sustainable Nitrogen Management Index: 100/SMNI
## calculation
out <- get_AP2I02_02(input = path_snmi_file, 
                     country_col = "country",
                     country_cv = country_cv, 
                     metadata_master = master, 
                     user = user,
                     SI="AP2I02_02")
## export
write.xlsx(out, file.path(path_subindicator_dir,"AP2I02_02.xlsx"))

#### AP2I02_05 ####
### make indicator
out <- get_AP2I02_05(input = path_pest_file, 
                     value_col="Value",
                     country_col = "Area", 
                     method = "latest year", 
                     metadata_master = master,
                     user = user, 
                     country_cv = country_cv,
                     SI = "AP2I02_05")
## export
write.xlsx(out, file.path(path_subindicator_dir,"AP2I02_05.xlsx"))

### AP2I02_03  ####
# Percentage of agricultural land that is organic, including land under conversion to organic 
## calculation
out <- get_AP2I02_03(input = path_fao_organic_file, country_cv = country_cv, metadata_master = master, user = user,SI="AP2I02_03")
## export
write.xlsx(out, file.path(path_subindicator_dir,"AP2I02_03.xlsx"))


###  AP2I02_08  ####
# % land under CA 
## calculation
out <- get_AP2I02_06(input = path_aquastat_ca_file, value_col="value",country_cv = country_cv, metadata_master = master, user = user,SI="AP2I02_06")

## export
write.xlsx(out, file.path(path_subindicator_dir,"AP2I02_06.xlsx"))

### AP2I03_02 ####
# Maintenance or conservation of landscape complexity
## calculation
out <- get_AP2I03_02(input = path_ili_file, input_sheet = "Performance", input_range = "F3:I147",
                     value_col="ProABD", country_cv = country_cv, metadata_master = master, user = user,SI="AP2I03_02")
## export
write.xlsx(out, file.path(path_subindicator_dir,"AP2I03_02.xlsx"))

### AP3I04_01 ####
# International reporting on plant genetic resources for food and agriculture
out <- get_AP3I04_01(input = path_wiews_file, input_sheet="AP3I04_06",
                               country_col="country",country_cv = country_cv, 
                               metadata_master = master, user = user,SI="AP3I04_01")
## export
write.xlsx(out, file.path(path_subindicator_dir,"AP3I04_01.xlsx"))

#### SP1I11_01  ####
# diversity of food in diets

out <- get_SP1I11_01(input = path_foodbal_file, element_name = "Food supply quantity (kg/capita/yr)", food_col = "Item",
                     country_col = "Area",  value_col="Value", country_cv = country_cv, 
                     metadata_master = master, user = user,SI = "SP1I11_01")
write.xlsx(out, file.path(path_subindicator_dir,"SP1I11_01.xlsx"))

#### SP1I14_01 ####
# Nutritional function diversity (DALYs)

## calculation
out <- get_SP1I14_01(input = path_daly_file, input_sheet="PDFTables.com", country_col = "Location Name", 
                     value_col="DALYs_2017_mean", 
                     country_cv = country_cv, metadata_master = master, user = user,SI = "SP1I14_01")
## export
write.xlsx(out, file.path(path_subindicator_dir,"SP1I14_01.xlsx"))

#### SP1I17_01 ####
# Energy from non-staples

out <- get_SP1I17_01(input = path_energy_file,  country_col = "Area", 
                     country_cv = country_cv, metadata_master = master, user = user,SI = "SP1I17_01")

write.xlsx(out, file.path(path_subindicator_dir,"SP1I17_01.xlsx"))

#### SP2I12_03 ####
# Crop species richness per country (FAO)

## calculation
out <- get_SP2I12_03(input = path_fao_crops_file,
                     country_col = "Area", 
                     value_col = "Value",
                     country_cv = country_cv, 
                     metadata_master = master, 
                     user = user,SI="SP2I12_03")

## export
write.xlsx(out, file.path(path_subindicator_dir,"SP2I12_03.xlsx"))

#### SP2I12_04 ####
# Freshwater fish species richness per major sub-basin
out <- get_SP2I12_04(input = path_fish_file,country_col = "NAME_0",value_col = "Avg_Fish_s",
                     country_cv = country_cv,metadata_master = master,user = user,SI = "SP2I12_04")

## export
write.xlsx(out, file.path(path_subindicator_dir,"SP2I12_04.xlsx"))

#### SP2I12_05 ####
# Species diversity of livestock in production
out <- get_SP2I12_05(input = path_livestock_shannon_file,
                     country_col = "GID_0",value_col = "MEAN",
                     country_cv = country_cv,metadata_master = master,
                     user = user,SI = "SP2I12_05")
## export
write.xlsx(out, file.path(path_subindicator_dir,"SP2I12_05.xlsx"))


#### SP2I13_01 ####
# Livestock breed richness at country level
out <- get_SP2I13_01(input = path_livestock_breeds_file, skip_rows = 2,
                     country_col = "country",value_col="population_median",
                     country_cv = country_cv, metadata_master = master, user = user,
                     SI = "SP2I13_01")
## export
write.xlsx(out, file.path(path_subindicator_dir,"SP2I13_01.xlsx"))

#### SP3I10_01 ####
# Varietal diversity in gene bank accessions
out <- get_SP3I10_01(input = path_genesys_file, country_col = "ISO", value_col = "data.v.shan", 
                     country_cv = country_cv, metadata_master = master, user = user,SI = "SP3I10_01")

## export
write.xlsx(out, file.path(path_subindicator_dir,"SP3I10_01.xlsx"))

#### SP3I13_01 ####
# country gamma CWR species diversity 
## calculation
out <- get_SP3I13_01(input = path_cwr_file, country_col = "countryCode", 
                     country_cv = country_cv, metadata_master = master, user = user,SI="SP3I13_01")
## export
write.xlsx(out, file.path(path_subindicator_dir,"SP3I13_01.xlsx"))

#### SP3I13_02  ####
# Species diversity in gene bank accessions
out <- get_SP3I13_02(
  input = path_genesys_file, 
  country_col = "ISO", 
  value_col = "data.s.shan", 
  country_cv = country_cv, 
  metadata_master = master, 
  user = user,
  SI = "SP3I13_02")

## export
write.xlsx(out, file.path(path_subindicator_dir,"SP3I13_02.xlsx"))

#### SP3I19_01 ####
# NUS species diversity
out <- get_SP3I19_01(input = path_nus_file,value_col = "data.s.shan", country_cv = country_cv, metadata_master = master, user = user, SI = "SP3I19_01")
## export
write.xlsx(out, file.path(path_subindicator_dir,"SP3I19_01.xlsx"))

###SP3I19_02 ####
# In-situ conservation representativeness
## calculation
out <- get_SP3I19_02(input = path_khoury_file, input_sheet = "SP3I19_03", country_col = "ISO2", country_cv = country_cv, metadata_master = master, user = user,SI="SP3I19_02")
## export
write.xlsx(out, file.path(path_subindicator_dir,"SP3I19_02.xlsx"))

###SP3I19_03 ####
# Ex-situ conservation representativeness
## calculation
out <- get_SP3I19_03(input = path_khoury_file,  input_sheet = "SP3I19_03", country_col = "ISO2", country_cv = country_cv, metadata_master = master, user = user,SI="SP3I19_03")
## export
write.xlsx(out, file.path(path_subindicator_dir,"SP3I19_03.xlsx"))

