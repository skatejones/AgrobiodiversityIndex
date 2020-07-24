
### Calculate diversity of accessions in Genesys records per country
### last updated 11 March 2020

library(vegan) # for diversity calculations
library(reshape2) # for formatting data
library(genesysr) # import genesys accessions directly from their site
library(openxlsx) # open excel files

wd <- readline() #at the prompt, copy and paste your filepath and press enter
C:\Users\sjones\Documents\02_Bioversity\24_ABD_index\Science\ABDI_Tool\data\Genesys
setwd(wd)

### Collate all diversity files already saved

diversity <- list.files(pattern=".*_diversity.csv")
diversity <- do.call(rbind,lapply(diversity,read.csv,header=T))

# remove rows with spurious data
diversity <- subset(diversity, nchar(as.character(X)) <= 3)
diversity <- subset(diversity, !is.na(X))
diversity <- subset(diversity, X != "")
diversity <- subset(diversity, !(X %in% c("20", "100","300","400","999")))
names(diversity)[names(diversity) == "X"] <- "ISO"

write.csv(diversity, paste0("all_genesys_diversity_",format(Sys.Date(),"%Y%m%d"), ".csv", sep = ""),row.names = T)

### Import data from genesys site

user_login()
# s.jones@cgiar.org / Bioversity12!

# list of countries for loop
countries <- read.xlsx("C:/Users/sjones/Documents/02_Bioversity/24_ABD_index/Science/ABDI_Tool/R/Country_codes_formatted.xlsx")
countries <- countries[1]
countries <- unique(countries)
countries <- subset(countries,Iso3_Code != c("AUS","USA","IND","PER")) # remove random ones already done


# download files per country
for (i in c(1:nrow(countries))){ 
  print(paste0("Running country ",countries[i,1], " (i=",i,")"))
  data <- genesysr::get_accessions(list(countryOfOrigin = list(code3 = c(countries[i,1])), # note filtering by fields isn't working - all fields are downloaded
                                        fields = c("crop.name",
                                                   "taxonomy.genus",
                                                   "taxonomy.species",
                                                   "taxonomy.subtaxa",
                                                   "taxonomy.taxonName")))# put at.least=2 to get just first two pages per country
  ### Write raw data to file
  
  #Exclude countries that have no data
  if(nrow(data) > 0){
    
    write.csv(data, paste0("genesys_", countries[i,1], ".csv", sep = ""),row.names = T)
  
    ### Prepare data for analysis
  
    # Drop columns that are not needed
    names(data)
    data <- data[,c("taxonomy.genus", "taxonomy.species","taxonomy.subtaxa", 
                  "taxonomy.taxonName", "origCty")]
  
    # Drop rows with strange data
    data <- data[which(data$origCty == countries[i,1]),]
  
    # Make new columns with full species name for each accession
    data$Species <- paste(data$taxonomy.genus,data$taxonomy.species,sep=" ")
    data$Variety <- paste(data$taxonomy.genus,data$taxonomy.species,data$taxonomy.subtaxa,sep=" ")
  
    # Put into wide format 
    data.s <-  dcast(data,origCty~Species,value.var="Species",fun.aggregate=length)
    row.names(data.s) <- data.s$origCty
    data.s <- subset(data.s,select=-c(origCty))

    data.v <-  dcast(data,origCty~Variety,value.var="Variety",fun.aggregate=length)
    row.names(data.v) <- data.v$origCty
    data.v <- subset(data.v,select=-c(origCty))

    ### Calculate richness and diversity indices

    # Richness
    data.s.richness <- specnumber(as.matrix(data.s),MARGIN=1)
    data.v.richness <- specnumber(as.matrix(data.v),MARGIN=1)

    # Shannon's entropy
    data.s.shan <- diversity(data.s,index="shannon",base=exp(1))
    data.v.shan <- diversity(data.v,index="shannon",base=exp(1))

    # Gini-Simpson's index 
    # Both variants of Simpson's index are based on D = sum p_i^2.
    # Choice simpson returns 1-D and invsimpson returns 1/D.
    # Choice simpson is Gini-Simpson index the way R calculates it

    data.s.gini <- diversity(data.s,index="simpson")
    data.v.gini <- diversity(data.v,index="simpson")

    # Effective diversity
    # (The number of equally-common species required to give a particular 
    # value of an index)
    data.s.eff <- 1/(1-data.s.gini) # calculated based on Gini conversion formulas
    data.v.eff <- 1/(1-data.v.gini) # calculated based on Gini conversion formulas

    ### Combine and save outputs

    result.s <- cbind(data.s.richness,data.s.shan,data.s.gini,data.s.eff)
    result.s <- data.frame(result.s)
    result.v <- cbind(data.v.richness,data.v.shan,data.v.gini,data.v.eff)
    result.v <- data.frame(result.v)
    results <- cbind(result.s,result.v)
  
    write.csv(results, paste0("genesys_", countries[i,1], "_diversity.csv", sep = ""),row.names = T)
    print(paste0("Finished running ",countries[i,1]," (i=",i,")"))
    }
  print(paste0("Finished running ",countries[i,1]," (i=",i,"), which had no data"))
  }
