# func_indicators.R

### version 2020-04-12 10:00

#' Get the CPXIXX_XX (commitment) sub-indicators
#' @param input table with the commitment scores from 0 to 3, obtained from manual scoring of text-mined sentences
#' @param input_sheet
#' @param range
#' @param country_col
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation

get_CPXIXX <- function(input, input_sheet, range, country_col,country_cv = country_cv, metadata_master = master, user = user){
  print("computing commitment subindicators")
  #input = path_commitments_file
  #input_sheet="country_score"
  #range="A1:V10"
  #country_col = "ISO3"
  d <- as.data.table(read_xlsx(input, sheet = input_sheet, range = range))
  # transform to long format
  dt <- melt(d,id.vars=c(country_col),variable.name="sub-indicator")
  
  ## add country information
  country_codes_formatted <- read_xlsx(file.path("..","R", "Country_codes_formatted.xlsx"))
  d_std <- merge.data.frame(dt,country_codes_formatted[,c("Iso3_Code","CountrynameISO")],by.x=c("ISO3"),by.y=c("Iso3_Code"))
  d_std <- unique(d_std)
  
  # scale the values from 0 to 100
  value_max <- 3
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  d_std$value <- ifelse(d_std$value>0,round(d_std$value/value_max*100,1),d_std$value) 
  
  ## return standard data output table
  d_std <- d_std[,c("ISO3","CountrynameISO", "value","sub-indicator")]
  colnames(d_std) <- c("Iso3_Code","country","value","subIndicator")
  ### make metadata
  m <- make_metadata_commitments(metadata_master = metadata_master, 
                                 Indicator1 = "CP1I01", 
                                 Indicator2 = "CP2I01",
                                 Indicator3 = "CP3I01",
                                 user = user)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the AP1I01_01 sub-indicator
#' Policies or guidelines leading to diverse diets

get_AP1I01_01 <- function(input,input_sheet,country_col,method,country_cv,metadata_master,user=NA,SI="AP1I01_01"){
  print(paste0("computing ",SI))
  ### import data
  d <-  as.data.table(read_xlsx(input, sheet = input_sheet,skip=1))
  
  # set value column
  d$value <- ifelse(d$FBDG=="Y",1,0)
  
  ### add country information
  setDT(d)
  setDT(country_cv)
  setnames(d, country_col, "country_tmp",skip_absent=T)
  
  # standardize country names (no leading/trailing spaces, no uppercase)
  d[,key:=trimws(tolower(country_tmp))]
  country_cv[,key:=trimws(tolower(country_match))]
  
  # name matching
  setkey(d, key)
  setkey(country_cv, key)
  tt <- merge(country_cv[,-c("country_match")], d,all.x=TRUE)
  
  # handle missing ISO3 codes
  if(dim(tt[is.na(Iso3_Code)])[1]>0){
    warning(paste("No ISO3 codes found for", paste(tt[is.na(Iso3_Code), country_tmp], collapse = ", ")))
  }
  tt[is.na(Iso3_Code), CountrynameISO:=country_tmp]
  setnames(tt, "CountrynameISO","country")
  ### remove unecessary columns 
  tt[,key:=NULL]
  tt[,country_tmp:=NULL]
  tt <- unique(tt)
  tt$value <- ifelse(is.na(tt$value),0,tt$value)
  ttt <- tt %>%
    group_by(Iso3_Code,country) %>%
    summarise(value=max(value))
  #setDT(ttt)
  #setkey[ttt,Iso3_Code]
  #setkey[d,Iso3_Code]
  d <- merge(d[,-c("value")],ttt,by.x="country_tmp",by.y="country", all.y=T)
  # remove unecessary columns 
  d[,country:=country_tmp]
  d[,key:=NULL]
  d[,country_tmp:=NULL]
  
  # scale values from 0 to 100
  d_std <- d
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  d_std$value <- ifelse(d_std$value>0,round(d_std$value/value_max*100,1),d_std$value) 
  
  ### return table with 3 columns: country ISO, country name and value
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F]
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator =  SI, user = user)

  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the AP1I01_02 sub-indicator
#' Resources to facilitate uptake of diverse diets

get_AP1I01_02 <- function(input,input_sheet,country_col,method,country_cv,metadata_master,user=NA,SI="AP1I01_02"){
  print(paste0("computing ",SI))
  ### import data
  d <-  as.data.table(read_xlsx(input, sheet = input_sheet))
  
  # set value column
  d$value <- ifelse(d$`Country-based FCT`=="Y",1,0)
  d$value <- ifelse(is.na(d$value),0,d$value )
  
  ### add country information
  setDT(d)
  setDT(country_cv)
  setnames(d, country_col, "country_tmp",skip_absent=T)
  
  # standardize country names (no leading/trailing spaces, no uppercase)
  d[,key:=trimws(tolower(country_tmp))]
  country_cv[,key:=trimws(tolower(country_match))]
  
  # name matching
  setkey(d, key)
  setkey(country_cv, key)
  tt <- merge(country_cv[,-c("country_match")], d,all.x=TRUE)
  
  # handle missing ISO3 codes
  if(dim(tt[is.na(Iso3_Code)])[1]>0){
    warning(paste("No ISO3 codes found for", paste(tt[is.na(Iso3_Code), country_tmp], collapse = ", ")))
  }
  tt[is.na(Iso3_Code), CountrynameISO:=country_tmp]
  setnames(tt, "CountrynameISO","country")
  ### remove unecessary columns 
  tt[,key:=NULL]
  tt[,country_tmp:=NULL]
  tt <- unique(tt)
  tt$value <- ifelse(is.na(tt$value),0,tt$value)
  ttt <- tt %>%
    group_by(Iso3_Code,country) %>%
    summarise(value=max(value))
  #setDT(ttt)
  #setkey[ttt,Iso3_Code]
  #setkey[d,Iso3_Code]
  d <- merge(d[,-c("value")],ttt,by.x="country_tmp",by.y="country", all.y=T)
  # remove unecessary columns 
  d[,country:=country_tmp]
  d[,key:=NULL]
  d[,country_tmp:=NULL]
  
  # scale values from 0 to 100
  d_std <- d
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  d_std$value <- ifelse(d_std$value>0,round(d_std$value/value_max*100,1),d_std$value) 
  
  ### return table with 3 columns: country ISO, country name and value
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F]
  d_std[,subIndicator := "AP1I01_02"]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = "AP1I01_02", user = user)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the AP2I02_01 sub-indicator: avoided over-use of chemical fertilisers
#'
#' @param input input file
#' @param input_sheet if the input file is an excel file, the index or the name of the spreadsheet
#' @param input_skip the number of rows to skip
#' @param country_columns the index of the column(s) with country values
#' @param method "latest year" or "mean" the calculation method
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_col index or name of the column that contains the country information
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' @detail XXX to be modified
#' @examples
#'
#'
get_AP2I02_01 <- function(input, input_sheet, input_skip, metadata_master, 
                          user=NA, method, value_col,country_col, country_cv,SI="AP2I02_01"){
  print(paste0("computing ",SI))
  ### import data
  d <- setDT(read_xlsx(input, sheet = input_sheet, skip = input_skip))

  ### get indicator
  if(method == "latest year"){
    latest_year <- as.character(max(as.numeric(colnames(d)[grepl("^[12][0-9]{3}$",trimws(colnames(d)))])))
    d[,value:=eval(as.name(latest_year))]
  }else{
    stop("method not defined")
  }

  ### add country information
  d <- get_country(t = d, country_col = country_col, country_cv = country_cv)
  
  ### return table with 2 columns: country and value
  if(is.numeric(country_col)){
    country_col <- colnames(d)[colnames(d)==colnames(d)[country_col]]
  }
  # scale the values from 0 to 100 and cap to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  d_std <- d
  d_std$value <- d_std[,get(value_col)]
  
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(d_std$value>100,100,d_std$value)
  
  # get standard format
  d_std <- d_std[,c("Iso3_Code", country_col, "value"), with = F]
  d_std[,subIndicator := SI]

  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  thresholds <- data.table(thresholds = c("","Actual minimum","Actual maximum"), value = c("",value_min,value_max))
  m <- rbind(m,thresholds,use.names=FALSE)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the AP2I02_02 sub-indicator: Integrated Plant Nutrient Management (IPNM)
#' @param input table with theSustainable Nitrogen Management Index: 100/SMNI
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @details NB the following column names are hardcoded "SNM.rnk.current", "country"
#'
get_AP2I02_02 <- function(input, country_col,country_cv = country_cv, metadata_master, user,SI="AP2I02_02"){
  print(paste0("computing ",SI))
  d <- fread(input, encoding = "UTF-8")
  ### get the sub-indicator
  d[,value := SNM_current]
  d$value <- ifelse(d$value==0,NA,d$value)

  ### add country information
  d <- get_country(t = d, country_col = "country", country_cv = country_cv)

  # scale the values from 0 to 100 and cap to 100

  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  d_std <- d

  value_max <- ifelse(value_max=="data",max(d_std$value,na.rm=T),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value,na.rm=T),as.numeric(value_min))
  
  d_std$value <- ifelse(d_std$value > value_max,value_max,d_std$value)
  d_std$value <- ifelse(d_std$value < value_min,value_min,d_std$value)
  
  d_std$value <- value_max - d_std$value # take max - x so higher values are better
  
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(is.na(d_std$value),100,d_std$value)
  #d_std$value <- ifelse(d_std$value>100,100,d_std$value)

  d_std <- d_std[,c("Iso3_Code", country_col, "value"), with = F]
  d_std[,subIndicator := SI]
  
  ### return table with 3 columns: country code, country name, value
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F]
  d_std[,subIndicator := SI]

  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  thresholds <- data.table(thresholds = c("","Actual minimum","Actual maximum"), value = c("",value_min,value_max))
  m <- rbind(m,thresholds,use.names=FALSE)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the AP2I02_03 sub-indicator: Percentage of agricultural land that is organic, including land under conversion to organic
#' @param input table with the Percentage of agricultural land that is organic, including land under conversion to organic (FAO)
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @details NB the following column names are hardcoded "Item", "Area", "Year", "Value". So are the factors "Agricultural area" and "Agricultural area organic, total" of the "Item" column.
#'
get_AP2I02_03 <- function(input, country_cv, metadata_master, user,SI="AP2I02_03"){
  print(paste0("computing ",SI))
  ### get indicator
  d <- fread(input)
  d <- d[Item == "Agriculture area under organic agric.",.(Area,Year,Value) ]

  ## if more than one year (it was not the case in FAOSTAT_data_18-06-2018_organic.csv), select only the latest available data
  max_year <- d[, .(Year = max(Year, na.rm = T)) , Area]
  setkey(max_year, Area, Year)
  setkey(d, Area, Year)
  d <- d[max_year]

  d[,value :=  Value ]

  ### add country information
  d <- get_country_all(t = d, country_col = "Area", country_cv = country_cv)

  ### return table with 2 columns: country and value
  d_std <- d[,c("Iso3_Code", "country", "value"), with = F]
  d_std[,subIndicator := SI]

  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)

  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Extract the AP2I02_05 sub-indicator from the FAO dataset
#'
#' @param data Inverse of pesticide applications on arable land, as reported via FAOSTAT
#' @param country_col name of the column that contains the country information
#' @param value_col name of the column that contains the value information
#' @param year_col name of the column that contains the year information
#' @param method "latest year" or "mean" the calculation method
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' 
get_AP2I02_05 <- function(input, value_col, country_col = "Area", method = "latest year", 
                          metadata_master, user = NA, country_cv, SI="AP2I02_05"){
  print(paste0("computing ",SI))
  d <- fread(input)
  setnames(d, country_col, "COUNTRY")
  setnames(d, value_col, "PEST")
  #d$YEAR <- d$`Year Code`
  
  # remove subsidaries of China (whole China remains)
    # and remove countries that no longer exist
  d <- d[!(COUNTRY %in% c("China, mainland","Belgium-Luxembourg","Serbia and Montenegro")),]

  ### add country ISO3
  d <- get_country(t = d, country_col = "COUNTRY", country_cv = country_cv)
  
  ### extract the indicators
  if(method=="latest year"){
    # get the latest data available per country
    country_latest_year <- d[, .(latest_year = max(Year)), .(Iso3_Code)]
    setkey(country_latest_year,Iso3_Code,latest_year)
    setkey(d,Iso3_Code,Year)
    d <- d[,.(Iso3_Code, country, Year, PEST)][country_latest_year]
    d <- d[!is.na(Iso3_Code),]
    d[,INV_PEST := 1/(PEST+0.01)] # take the inverse so higher values are better
    d[,MAX_MINUS_PEST := max(PEST)-PEST]# or subtract values from maximum so higher values are better
    d[,value := MAX_MINUS_PEST]
  }
  
  # scale the values from 0 to 100 an cap to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  
  d_std <- d
  d_std$value<- as.numeric(d_std$value)
  d_std <- d_std[which(d_std$value !="Inf"),]
  d_std <- d_std[which(!is.na(d_std$value)),]
  
  value_max <- ifelse(value_max=="data",max(d_std$value,na.rm=T),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value,na.rm=T),as.numeric(value_min))
  
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(d_std$value>100,100,d_std$value)
  
  ## make standard indicator output
  d_std <- d_std[,.(Iso3_Code, country, value, subIndicator = SI)]
  
  ## remove spurious rows (countries that no longer exist)
  d_std <- d_std[which(!is.na(d_std$Iso3_Code)),]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  thresholds <- data.table(thresholds = c("","Actual minimum","Actual maximum"), value = c("",value_min,value_max))
  m <- rbind(m,thresholds,use.names=FALSE)
  
  ### return list
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the AP2I02_06 sub-indicator (Percentage of arable land under conservation agriculture)
#' @param input table with the percentage of arable land under conservation agriculture (FAO)
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @details NB the following column names are hardcoded "Variable Name", "Area", "Year", "Value"
#'
get_AP2I02_06 <- function(input, value_col,country_cv = country_cv, metadata_master, user,SI="AP2I02_06"){
  print(paste0("computing ",SI))
  ### get indicator
  d <- fread(input, sep = ",", skip = 0, blank.lines.skip = T)
  d <- d[!is.na(Value)] # remove metadata at the end of the file
  
  d <- d[`Variable Name` == "Conservation agriculture area as % of arable land area", .(Area, Year, Value)]
  
  ## if more than one year (it was not the case with aquastat_ca_20191205.csv), select only the latest available data
  max_year <- d[, .(Year = max(Year, na.rm = T)) , Area]
  setkey(max_year, Area, Year)
  setkey(d, Area, Year)
  d <- d[max_year]
  
  d[,value := Value] # make the standard "value" column (like for all subindicators)
  
  ### add country information
  d <- get_country_all(t = d, country_col = "Area", country_cv = country_cv)
  
  d_std <- d
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F]
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}


#' Get the AP2I03_02 sub-indicator: Maintenance or conservation of landscape complexity
#' @param input table with the Number of integrated landscape initiatives with pro-ABD investments (ILI)
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @details NB the following column names are hardcoded "ProABD", "Country"
#'
get_AP2I03_02 <- function(input,input_sheet,input_range, value_col,country_cv = country_cv, metadata_master, user,SI="AP2I03_02"){
  print(paste0("computing ",SI))
  d <- as.data.table(read_xlsx(input, sheet=input_sheet, range=input_range))
  #d[,value := ProABD] # make the standard "value" column (like for all subindicators)

  ### add country information
  d <- get_country(t = d, country_col = "Country", country_cv = country_cv)

  # scale the values from 0 to 100 and cap to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  
  d_std <- d
  d_std$value <- d_std[,get(value_col)]
  value_max <- ifelse(value_max=="data",max(d_std$value,na.rm=T),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(d_std$value>100,100,d_std$value)
  
  ### return table with 2 columns: country and value
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F]
  d_std[,subIndicator := SI]
  d_std <- d_std[which(!is.na(d_std$Iso3_Code)),]

  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)

  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the AP3I04_01 sub-indicator (International reporting on plant genetic resources for food and agriculture - WIEWS)

get_AP3I04_01 <- function(input,input_sheet,country_col,method,country_cv,metadata_master,user=NA,SI="AP3I04_01"){
  print(paste0("computing ",SI))
  ### import data
  d <- setDT(read_xlsx(input, sheet = input_sheet,trim_ws=TRUE))
  d <- d[which(!is.na(d$country)),] # remove empty rows
  
  ### add country information
  setDT(d)
  setDT(country_cv)
  setnames(d, country_col, "country_tmp",skip_absent=T)

  # standardize country names (no leading/trailing spaces, no uppercase)
  d[,key:=trimws(tolower(country_tmp))]
  country_cv[,key:=trimws(tolower(country_match))]
  
  # name matching
  setkey(d, key)
  setkey(country_cv, key)
  tt <- merge(country_cv[,-c("country_match")], d,all.x=TRUE)
  
  # handle missing ISO3 codes
  if(dim(tt[is.na(Iso3_Code)])[1]>0){
    warning(paste("No ISO3 codes found for", paste(tt[is.na(Iso3_Code), country_tmp], collapse = ", ")))
  }
  tt[is.na(Iso3_Code), CountrynameISO:=country_tmp]
  setnames(tt, "CountrynameISO","country")
  ### remove unecessary columns 
  tt[,key:=NULL]
  tt[,country_tmp:=NULL]
  tt <- unique(tt)
  tt$value <- ifelse(is.na(tt$value),0,tt$value)
  ttt <- tt %>%
    group_by(Iso3_Code,country) %>%
    summarise(value=max(value))
  #setDT(ttt)
  #setkey[ttt,Iso3_Code]
  #setkey[d,Iso3_Code]
  d <- merge(d[,-c("value")],ttt,by.x="country_tmp",by.y="country", all.y=T)
  # remove unecessary columns 
  d[,country:=country_tmp]
  d[,key:=NULL]
  d[,country_tmp:=NULL]
  
  # scale the values from 0 to 100 and cap to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  
  d_std <- d
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(d_std$value>100,100,d_std$value)
  
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F]
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the SP1I11_01 sub-indicator (Species diversity in diets)
#' @param input input table 
#' @param country_col name of the column that contains the country information
#' @param food_col name of the column that contains the food information
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#'

get_SP1I11_01 <- function(input, country_col, element_name, food_col, value_col,country_cv = country_cv, metadata_master, user,SI="SP1I11_01"){
  print(paste0("computing ",SI))
  ### get indicator
  d <- read.csv(input)
  d <- setDT(d)
  d <- d[!is.na(Value)] # remove metadata at the end of the file
  
  d <- d[`Element` == element_name, .(Area, Item,Year, Value)]
  d <- d[!(Item %in% c("Alcohol Non-Food",
                       "Beer",
                       "Beverages, Alcoholic",
                       "Beverages, Fermented",
                       "Wine")),]
  
  ### add country information
  # remove subsidaries of China (all China is retained)
  d <- d[!(Area %in% c("China, mainland")),]#"China, Hong Kong SAR",China, Macao SAR","China, Taiwan Province of")),]
  #d[which(d$Area=="China" & d$Item == "Wheat and products"),]
  #check <- d[which(d$Item == "Wheat and products"),]
  d <- get_country(t = d, country_col = "Area", country_cv = country_cv)
  d[which(d$country=="China" & d$Item == "Wheat and products"),]

  ## if more than one year, select only the latest available data
  max_year <- d[, .(Year = max(Year, na.rm = T)) , country]
  setkey(max_year, country,Year)
  setkey(d, country, Year)
  d <- d[max_year]
  
  d[,value := Value] 
  country_col <- "country"
  # calculate diversity
  countries <- d[,unique(eval(as.name(country_col)))]
  d <- rbindlist(lapply(countries, function(x){
    # cat(paste0("computing diversity index for ",x,"\n"))
    Iso3_Code <- d[eval(as.name(country_col)) == x,Iso3_Code]
    count_country <- d[eval(as.name(country_col)) == x, .N, eval(as.name(food_col))]
    H_div <- diversity(d[eval(as.name(country_col)) == x,value],index="shannon",base=exp(1))
    gs_div <- diversity(d[eval(as.name(country_col)) == x,value], "simpson")   # Choice simpson returns 1-D and invsimpson returns 1/D (i.e. Gini-simpson)
    eff_div <- 1/(1-gs_div) # effective diversity calculated based on Gini conversion formulas
    return(unique(data.table(country = x, Iso3_Code=Iso3_Code,H_div = H_div, gs_div = gs_div, eff_div = eff_div, N_foods = count_country[,.N])))
  }))

  #d[,value := gs_div]
  d[,value := H_div]
  
  # scale the values from 0 to 100 and cap to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  d_std <- d
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(d_std$value>100,100,d_std$value)
  
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F] 
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  thresholds <- data.table(thresholds = c("","Actual minimum","Actual maximum"), value = c("",value_min,value_max))
  m <- rbind(m,thresholds,use.names=FALSE)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

### SP1I14_01 ###
# Nutritional function diversity (DALYs)

get_SP1I14_01 <- function(input,input_sheet,country_col,value_col,country_cv,metadata_master,user=NA,SI="SP1I14_01 "){
  print(paste0("computing ",SI))
  ### import data
  d <- setDT(read_xlsx(input, sheet = input_sheet,trim_ws=TRUE))
  d <- d[Cause == "All causes",] # filter to only those rows reporting deaths from all causes
  ### add country information
  d <- get_country(t = d, country_col = country_col, country_cv = country_cv)
  
  ### return table with 3 columns: country ISO, country name and value
  if(is.numeric(country_col)){
    country_col <- colnames(d)[colnames(d)==colnames(d)[country_col]]
  }
  # get standard value format
  d_std <- d
  d_std$value <- d_std[,get(value_col)]
  
  # scale values from 0 to 100  
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  
  d_std$value <- ifelse(d_std$value > value_max,value_max,d_std$value)
  d_std$value <- value_max - d_std$value # inverse value so higher values are better and mean fewer deaths
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  
  # put into standard format
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F] 
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  thresholds <- data.table(thresholds = c("","Actual minimum","Actual maximum"), value = c("",value_min,value_max))
  m <- rbind(m,thresholds,use.names=FALSE)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}


### SP1I17_01 
# % energy from non-staples

get_SP1I17_01 <- function(input,country_col,country_cv,metadata_master,user,SI="SP1I17_01"){
  print(paste0("computing ",SI))
  ## data import
  #d <- setDT(read_xlsx(input, sheet = input_sheet,trim_ws=TRUE))
  d <- fread(input)
  #d <- d[which(!is.na(d$country)),] # remove empty rows
  d <- d[Item == "Share of dietary energy supply derived from cereals, roots and tubers (kcal/cap/day) (3-year average)",.(Area,Year,Item, Value)]
  
  # remove subsidaries of China (whole China remains)
  d <- d[!(Area %in% c("China, mainland")),]
  
  # if more than one year, select only the latest available data
  d$Year <- substr(d$Year,6,9)
  d$Year <- as.numeric(d$Year)
  max_year <- d[, .(Year = max(Year, na.rm = T)) , Area]
  setkey(max_year, Area,Year )
  setkey(d, Area, Year)
  d <- d[max_year]
  d[,`Energy from staples,roots,tubers` := Value]
  
  ### add country information
  d <- get_country(t = d, country_col = country_col, country_cv = country_cv)
  
  ### return table with 3 columns: country ISO, country name and value
  if(is.numeric(country_col)){
    country_col <- colnames(d)[colnames(d)==colnames(d)[country_col]]
  }
  
  # calculate % energy not from cereals, roots and tubers
  d[,`Energy from other sources` := 100-Value]
  
  # scale the values from 0 to 100 and cap values to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  
  d_std <- d
  d_std$value <- d_std$`Energy from other sources`
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(d_std$value>100,100,d_std$value)
  
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F] 
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the SP2I12_03 sub-indicator: Crop species richness in production from FAO

get_SP2I12_03 <- function(input,country_col,value_col,country_cv,metadata_master,user=NA,SI="SP2I12_03"){
  print(paste0("computing ",SI))
  ## data import
  d <- fread(input)
  
  ## Calculate unique items per country
  d <- d[Element == "Area harvested", .(Area, Year, Item, Value)]
  
  # remove subsidaries of China (whole China remains)
  # and remove countries that no longer exist
  d <- d[!(Area %in% c("China, mainland","Belgium-Luxembourg","Serbia and Montenegro")),]

  # if more than one year, select only the latest available data
  max_year <- d[, .(Year = max(Year, na.rm = T)) , Area]
  setkey(max_year, Area,Year)
  setkey(d, Area,Year)
  d <- d[max_year]
  
  # group the same foods
  sort(unique(d$Item))
  d <- as.data.frame(d)
  d$Item[d$Item %in% c("Cassava leaves","Cassava")] <- "Cassava"
  d$Item[d$Item %in% c("Cherries, sour","Cherries")] <- "Cherries"
  d$Item[d$Item %in% c("Chillies and peppers, dry","Chillies and peppers, green")] <- "Chillies"
  d$Item[d$Item %in% c("Onions, dry","Onions, shallots, green")] <- "Onions"
  d$Item[d$Item %in% c("Maize, green","Maize")] <- "Maize"
  d$Item[d$Item %in% c("Peas, dry","Peas, green")] <- "Peas" 
  d$Item[d$Item %in% c("Sugar beet","Sugar cane")] <- "Sugar" 
  
  # get count of unique food items per country
  d <- dplyr::summarise(group_by(d,Area),
                     value=n())
  
  ### add country information
  setDT(d)
  d <- get_country(t = d, country_col = country_col, country_cv = country_cv)
  
  if(is.numeric(country_col)){
    country_col <- colnames(d)[colnames(d)==colnames(d)[country_col]]
  }
  # scale the values from 0 to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  d_std <- d
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(d_std$value>100,100,d_std$value)
  
  ### make standard indicator output
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F]
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  thresholds <- data.table(thresholds = c("","Actual minimum","Actual maximum"), value = c("",value_min,value_max))
  m <- rbind(m,thresholds,use.names=FALSE)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get SP2I12_04: Fish richness 
get_SP2I12_04 <- function(input,country_col,value_col,country_cv,metadata_master,user,SI="SP2I12_04"){
  print(paste0("computing ",SI))
  #input= path_fish_file
  ## data import
  d <- foreign::read.dbf(input)

  ### add country information
  d <- get_country(t = d, country_col = country_col, country_cv = country_cv)
  
  if(is.numeric(country_col)){
    country_col <- colnames(d)[colnames(d)==colnames(d)[country_col]]
  }
  # scale the values from 0 to 100 and cap to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  
  d_std <- d
  d_std$value <- d_std[,get(value_col)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(d_std$value>100,100,d_std$value)
  
  ### make standard indicator output
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F]
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the SP2I12_05 sub-indicator: livestock diversity

get_SP2I12_05 <- function(input, country_col,value_col,country_cv,metadata_master,user,SI="SP2I12_05"){
  print(paste0("computing ",SI))
  ## data import
  d <- read.dbf(input)
  setDT(d)

  ### add country information
  country_codes_formatted <- read_xlsx(file.path("..","R", "Country_codes_formatted.xlsx"))
  d_std <- merge.data.frame(d,country_codes_formatted[,c("Iso3_Code","CountrynameISO")],by.x=c(country_col),by.y=c("Iso3_Code"),all.x=T,all.y=F)
  setDT(d_std)
  d_std$Iso3_Code <- d_std$GID_0
  d_std <- unique(d_std)
  
  # scale the values from 0 to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  d_std$value <- d_std[,get(value_col)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(d_std$value>100,100,d_std$value)
  
  ### make standard indicator output
  d_std$country <- d_std$CountrynameISO
  d_std <- d_std[,c("Iso3_Code", "country", "value")]
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  thresholds <- data.table(thresholds = c("","Actual minimum","Actual maximum"), value = c("",value_min,value_max))
  m <- rbind(m,thresholds,use.names=FALSE)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the SP2I13_01 sub-indicator: livestock breed diversity

get_SP2I13_01 <- function(input, skip_rows,country_col, value_col,country_cv = country_cv, metadata_master, user,SI="SP2I13_01"){
  print(paste0("computing ",SI))
  ### get indicator
  d <- read.csv(input, skip = skip_rows, header = T)
  d <- setDT(d)
  
  # remove unofficial countries
  d <- d[(d$country != "Northern Macedonia"),]

  ## if more than one year, select only the latest available data
  #d[,country := get(country_col)]
  #d[,breed := get("breed")]
  d$country_breed <- paste(d$country,d$breed,sep="-")
  max_year <- d[, .(year = max(year, na.rm = T)) , country_breed]
  setkey(max_year, country_breed,year)
  setkey(d, country_breed, year)
  d <- d[max_year]
  
  d$value <- d[, get(value_col)] # make the standard "value" column (like for all subindicators)
  d <- d[!is.na(d$value),]
  d <- d[!(d$value == "#NUM!"),]
  d$value <- as.numeric(as.character(d$value))
  
  # calculate diversity
  countries <- d[,unique(eval(as.name(country_col)))]
  d <- rbindlist(lapply(countries, function(x){
    # cat(paste0("computing diversity index for ",x,"\n"))
    count_country <- d[eval(as.name(country_col)) == x, .N, eval(as.name("breed"))]
    H_div <- diversity(d[country==x,value],index="shannon",base=exp(1))
    gs_div <- diversity(d[country==x,value], "simpson")   # Choice simpson returns 1-D and invsimpson returns 1/D (i.e. Gini-simpson)
    eff_div <- 1/(1-gs_div) # effective diversity calculated based on Gini conversion formulas
    return(data.table(country = x, H_div = H_div, gs_div = gs_div, eff_div = eff_div, N_breeds = count_country[,.N]))
  }))
  d[,value := H_div]
  
  ### add country information
  d <- get_country(t = d, country_col = country_col, country_cv = country_cv)
  
  # scale the values from 0 to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  d_std <- d
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F] 
  d_std[,subIndicator := SI]
  
  ## remove spurious rows (old countries or sub-national zones)
  d_std <- d_std[which(!is.na(d_std$Iso3_Code)),]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  thresholds <- data.table(thresholds = c("","Actual minimum","Actual maximum"), value = c("",value_min,value_max))
  m <- rbind(m,thresholds,use.names=FALSE)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}


#' Get the SP3I10_01 sub_indicator: varietal diversity in gene banks
get_SP3I10_01 <- function(input, country_col, value_col, country_cv, metadata_master, user, SI="SP3I10_01"){
  print(paste0("computing ",SI))
  d <- fread(input) 
  
  # get standard country names
  d <- merge(d,country_cv[,c(1,2)],by.x="ISO",by.y="Iso3_Code")
  d <- unique(d)
  d$country <- d[,get("CountrynameISO")]
  d$Iso3_Code <- d[,get("ISO")]
  
  # scale the values from 0 to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  d_std <- d
  d_std$value <- d_std[,get(value_col)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F] 
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  thresholds <- data.table(thresholds = c("","Actual minimum","Actual maximum"), value = c("",value_min,value_max))
  m <- rbind(m,thresholds,use.names=FALSE)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}


#' Get the SP3I13_01 sub-indicator: country gamma CWR species diversity 
#' @param input table with the column country_col and species_col
#' @param country_col name of the column that contains the country information
#' @param species_col name of the column that contains the species information
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' 
#'
get_SP3I13_01 <- function(input, country_col, country_cv = country_cv, metadata_master, user,SI="SP3I13_01"){
  print(paste0("computing ",SI," GBIF CWR data"))
  d <- fread(input,quote="") 

  # remove spurious rows
  d <- d[which(!is.na(d$countryCode)),]
  d <- d[which(!(d$countryCode %in% c("", " ","NA","ZZ"))),]
  
  # compute diversity metrics
  countries <- d[,unique(eval(as.name(country_col)))]
  d$species_all <- ifelse(d$taxonRank %in% c("VARIETY","FORM","SUBSPECIES"),d$species,d$scientificName)
  
  d <- rbindlist(lapply(countries, function(x){
    cwr_count_country <- d[countryCode == x, .N, eval(as.name("species_all"))]
    H_div <- diversity(cwr_count_country[,N],index="shannon",base=exp(1))
    # Choice simpson returns 1-D and invsimpson returns 1/D (i.e. Gini-simpson)
    gs_div <- diversity(cwr_count_country[,N], "simpson") 
    eff_div <- 1/(1-gs_div) # effective diversity calculated based on Gini conversion formulas
    return(data.table(country_name = x, gs_div = gs_div, H_div = H_div, eff_div = eff_div,N_species = cwr_count_country[,.N]))
  }))
  d[,value := H_div]
  
  ### add country information
  d <- get_country(t = d, country_col = "country_name", country_cv = country_cv)
  
  # scale the values from 0 to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  d_std <- d
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F] 
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  thresholds <- data.table(thresholds = c("","Actual minimum","Actual maximum"), value = c("",value_min,value_max))
  m <- rbind(m,thresholds,use.names=FALSE)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the SP3I13_02 sub_indicator: species diversity in gene banks

get_SP3I13_02 <- function(input, country_col, value_col, country_cv, metadata_master, user, SI="SP3I13_02"){
  print(paste0("computing ",SI))
  d <- fread(input) 
  
  # get standard country names
  d <- merge(d,country_cv[,c(1,2)],by.x="ISO",by.y="Iso3_Code")
  d <- unique(d)
  d$country <- d[,get("CountrynameISO")]
  d$Iso3_Code <- d[,get("ISO")]
  
  # scale the values from 0 to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  d_std <- d
  d_std$value <- d_std[,get(value_col)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F] 
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  thresholds <- data.table(thresholds = c("","Actual minimum","Actual maximum"), value = c("",value_min,value_max))
  m <- rbind(m,thresholds,use.names=FALSE)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}


#' Get the SP3I19_01 sub-indicator: NUS diversity
get_SP3I19_01 <- function(input, country_col, value_col, country_cv, metadata_master, user, SI="SP3I19_01"){
  #input = path_nus_file
  print(paste0("computing ",SI))
  d <- fread(input) 
  
  # remove spurious rows
  d <- d[d$V1 !=""]
  
  # get standard country names
  d <- merge(d,country_cv,by.x="V1",by.y="country_match")
  d <- unique(d)
  d$country <- d[,get("CountrynameISO")]
  
  # scale the values from 0 to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  d_std <- d
  d_std$value <- d_std[,get(value_col)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F] 
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  thresholds <- data.table(thresholds = c("","Actual minimum","Actual maximum"), value = c("",value_min,value_max))
  m <- rbind(m,thresholds,use.names=FALSE)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the SP3I19_02 sub-indicator: In-situ conservation representativeness
get_SP3I19_02 <- function(input, input_sheet, country_col="ISO2",country_cv = country_cv, metadata_master = master, user = user,SI="SP3I19_02"){
  print("computing SP3I19_02")
  d <- as.data.table(read_xlsx(input, sheet = input_sheet))

  ### add country information
  d <- get_country(t = d, country_col = country_col, country_cv = country_cv)
  if(is.numeric(country_col)){
    country_col <- colnames(d)[colnames(d)==colnames(d)[country_col]]
  }
  
  ### get values
  d_std <- d
  colnames(d_std)[colnames(d_std)=="Indicator  (in situ)"] <- "value"

  ### return table with 3 columns: country ISO, country name and value
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F]
  d_std[,subIndicator := "SP3I19_02"]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = "SP3I19_02", user = user)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the SP3I19_03 sub-indicator: Ex-situ conservation representativeness

get_SP3I19_03 <- function(input, input_sheet, country_col="ISO2",country_cv = country_cv, metadata_master = master, user = user,SI="SP3I19_03"){
  #input = path_khoury_file
  #input_sheet="SP3I19_03"
  #country_col = "country"
  print(paste0("computing ",SI))
  d <- as.data.table(read_xlsx(input, sheet = input_sheet))
  
  ### add country information
  d <- get_country(t = d, country_col = country_col, country_cv = country_cv)
  if(is.numeric(country_col)){
    country_col <- colnames(d)[colnames(d)==colnames(d)[country_col]]
  }
  
  ### get values
  d_std <- d
  colnames(d_std)[colnames(d_std)=="Indicator  (ex situ)"] <- "value"
  
  ### return table with 3 columns: country ISO, country name and value
  d_std <- d_std[,c("Iso3_Code", "country", "value"), with = F]
  d_std[,subIndicator := SI]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = "SP3I19_03", user = user)
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the SP3 crop wild relative index NEW indicator

get_SP3_CWR_Index <- function()

#### Spatial indicators ####
#' Get the AP2I02_04 sub-indicator by taking pixel area
#'
#' @param dir_af_rasters path of the directory that contains the agroforestry rasters (splitted on grid)
#' @param dir_countries path of the directory that contains the country shapes (splitted on grid)
#' @param country_col name of the column that contains the country ISO3 code in the data slot of country_sp (e.g. "GID_O" for gadm)
#' @param country_index table of the links between tiles and countries
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @description agricultural land under agroforestry (%)
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' @examples
#'
#'
get_AP2I02_04 <- function(dir_af_rasters, dir_countries, country_col = "GID_0", 
                          country_index, metadata_master, user = NA, country_cv=country_cv,SI="AP2I02_04"){
  k <-1
  print("processing AP2I02_04 % tree cover on agricultural land\n")
  ## compute the indicators
  d <- rbindlist(lapply(country_index[,unique(GID_0)], function(country_id,n=length(country_index[,unique(GID_0)])){
    ## clip the raster to the shape of the country (tile by tile) and retruns values for computing the metrics
    area_tiles_values <- rbindlist(lapply(country_index[GID_0==country_id,tile_id],function(tile_id){
      cat(paste0("processing ",country_id," (",k,"/", n,") - masking tile ", tile_id,"\n"))
      country_tile <- readOGR(dsn = dir_countries, layer = paste0("countries_split_",tile_id), verbose = F)
      af_tile <- raster(file.path(dir_af_rasters, paste0("tile_",tile_id,".tif")))
      af_tile_masked <- mask(af_tile, country_tile[country_tile@data$GID_0 == country_id,])
      ## from the tile, extract the data needed for computing the metrics at the country level 
      # indicator formula is (F): 
      # mean pct of agroforestery = (sum(pixel area * pixel value) of all tiles) / (sum (sum( pixel area) of all tiles)
      ## get the area (in km2) of each pixel
      pixel_area <- area(af_tile_masked, na.rm=TRUE, weights=FALSE)
      af_tile_masked_area <- af_tile_masked * pixel_area # In (F), doing: pixel area * pixel value
      return(data.table(
        area_all = sum(pixel_area[], na.rm = T), # In (F), doing sum(pixel area)
        area_times_value = sum(af_tile_masked_area[], na.rm = T), # In (F), doing: sum(pixel area * pixel value)
        n_pixel = sum(!is.na(af_tile_masked)[])
        ))
      }))
    ## combine the country information from different tiles
    area_tiles_values <- area_tiles_values[,lapply(.SD,sum)] # sums the columns
    
    ## compute the metrics
    cat(paste0("processing ",country_id," (",k,"/", n,") - computing metrics\n"))
    metrics <- data.table(
      Iso3_Code=country_id,
      n_pixels=area_tiles_values[,n_pixel],
      mean_pct_af=area_tiles_values[,area_times_value]/area_tiles_values[,area_all]
      )
    k <<- k + 1
    return(metrics)
    }), use.names = T)
    d[,value:=mean_pct_af]
    
    ## add standard country label to the IsoCode
    setkey(d, Iso3_Code)
    setkey(country_cv, Iso3_Code)
    d <- unique(country_cv[,.(Iso3_Code,CountrynameISO)])[d]
    if(d[is.na(CountrynameISO),.N]>0){
      warning(paste("ISO3 codes ", paste(d[is.na(CountrynameISO),Iso3_Code], collapse = ", "), "had no equivalent"))
      }
    setnames(d, "CountrynameISO","country")
  
    ## make standard indicator output
    d_std <- d
    d_std <- d_std[,.(Iso3_Code, country, value, subIndicator = SI)]
  
    # scale the values from 0 to 100 and cap to 100
    value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
    value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
    
    value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
    value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
    
    d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
    d_std$value <- ifelse(d_std$value>100,100,d_std$value)
    d_std$value  <- ifelse( d_std$value >=0, d_std$value ,NA) # convert NULL values to NA
    
    ## make metadata
    m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  
    rm(k)
    print("finished processing AP2I02_04 % tree cover on agricultural land\n")
    return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}
  

#' Get the AP2I02_04 sub-indicator by counting pixels 
#'
#' @param dir_af_rasters path of the directory that contains the agroforestry rasters (splitted on grid)
#' @param dir_countries path of the directory that contains the country shapes (splitted on grid)
#' @param country_col name of the column that contains the country ISO3 code in the data slot of country_sp (e.g. "GID_O" for gadm)
#' @param country_index table of the links between tiles and countries
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' @details Computation is based on pixel count (regardless of pixel area)
#' @examples
#'
#'
get_AP2I02_04_pixel <- function(dir_af_rasters, dir_countries, country_col = "GID_0", country_index, metadata_master, user = NA, country_cv){
  
  k <-1
  ### compute the indicators
  d <- rbindlist(lapply(country_index[,unique(GID_0)], function(country_id,n=length(country_index[,unique(GID_0)])){
    
    ## clip the raster to the shape of the country (tile by tile)
    af_tiles_masked <- lapply(country_index[GID_0==country_id,tile_id],function(tile_id){
      
      cat(paste0("processing ",country_id," (",k,"/", n,") - masking tile ", tile_id,"\n"))
      
      country_tile <- readOGR(dsn = dir_countries, layer = paste0("countries_split_",tile_id), verbose = F)
      af_tile <- raster(file.path(dir_af_rasters, paste0("tile_",tile_id,".tif")))
      af_tile_masked <- mask(af_tile, country_tile[country_tile@data$GID_0 == country_id,])
      return(af_tile_masked)
    })
    
    ## merge the tiles
    if(length(af_tiles_masked)>1){
      cat(paste0("processing ",country_id," (",k,"/", n,") - aggregating tiles\n"))
      # Note: it is not necessary to merge the rasters
      # af_masked <- do.call(merge,af_tiles_masked)
      # this operation is costly in memory
      # instead, it is enough to bind only the pixel values (regardless of the pixel coordinates)
      af_masked_vals <- unlist(lapply(af_tiles_masked, function(x){
        vals <- values(x)
        vals <- vals[!is.na(vals)]
      }))
    }else{
      af_masked_vals <- af_tiles_masked[[1]][]
      af_masked_vals <- af_masked_vals[!is.na(af_masked_vals)]
    }
    
    ## compute the metrics
    if(length(af_masked_vals)>0){
      cat(paste0("processing ",country_id," (",k,"/", n,") - computing metrics\n"))
      af_masked_t <- table(af_masked_vals)
      af_masked_q <- quantile(af_masked_vals, probs = c(0, .25, .5, .75, 1), na.rm=T)
      metrics <- data.table(
        Iso3_Code=country_id,
        MEAN=mean(af_masked_vals, na.rm = T),
        SD=sd(af_masked_vals, na.rm = T),
        MAJORITY=as.numeric(names(which.max(af_masked_t))),
        MINORITY=as.numeric(names(which.min(af_masked_t))),
        MIN=af_masked_q[1],
        q1=af_masked_q[2],
        MEDIAN=af_masked_q[3],
        q3=af_masked_q[4],
        MAX=af_masked_q[5],
        N = length(af_masked_vals)
      )
    }else{
      cat(paste0("processing ",country_id," (",k,"/", n,") - no data for this country\n"))
      metrics <- data.table(
        Iso3_Code=country_id, MEAN=NA, SD=NA, MAJORITY=NA, MINORITY=NA, MIN=NA, q1=NA, MEDIAN=NA, q3=NA, MAX=NA, N = NA
      )
    }
    k <<- k + 1
    return(metrics)
  }))
  
  ### add standard country label to the IsoCode
  setkey(d, Iso3_Code)
  setkey(country_cv, Iso3_Code)
  d <- unique(country_cv[,.(Iso3_Code,CountrynameISO)])[d]
  if(d[is.na(CountrynameISO),.N]>0){
    warning(paste("ISO3 codes ", paste(d[is.na(CountrynameISO),Iso3_Code], collapse = ", "), "had no equivalent"))
  }
  setnames(d, "CountrynameISO","country")
  
  ### make standard indicator output
  d_std <- d[,.(Iso3_Code, country, value = MEAN, subIndicator = "AP2I02_04_pixel")]
  d[,value:=MEAN]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = "AP2I02_04", user = user)
  
  rm(k)
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the AP2I03_01 sub-indicator by taking pixel area
#'
#' @param dir_ramankutty_rasters path of the directory that contains the rasters (splitted on grid)
#' @param dir_countries path of the directory that contains the country shapes (splitted on grid)
#' @param country_col name of the column that contains the country ISO3 code in the data slot of country_sp (e.g. "GID_O" for gadm)
#' @param country_index table of the links between tiles and countries
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @description Computes the country mean percentage of agricultural land containing both cropland and pasture. This dataset was made by overlaying Ramunkutty (2008) 1 x 1 km pasture and cropland extent datasets to identify pixels containing both cropland and pasture, and representing these "diversified" areas as a percentage of total agricultural land.
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' @examples
#'
#'
get_AP2I03_01 <- function(input, dir_countries, country_col = "GID_0", country_index, metadata_master, user = NA, country_cv,SI="AP2I03_01"){
  
  k <-1
  print("processing AP2I03_01 crop-livestock diversity\n")
  ### compute the indicators
  d <- rbindlist(lapply(country_index[,unique(GID_0)], function(country_id,n=length(country_index[,unique(GID_0)])){
    
    ## clip the raster to the shape of the country (tile by tile)
    area_tiles_values <- rbindlist(lapply(country_index[GID_0==country_id,tile_id],function(tile_id){
      
      cat(paste0("processing ",country_id," (",k,"/", n,") - masking tile ", tile_id,"\n"))
      
      country_tile <- readOGR(dsn = dir_countries, layer = paste0("countries_split_",tile_id), verbose = F)
      ram_tile <- raster(file.path(path_ramankutty_preprocessed_dir, paste0("tile_",tile_id,".tif")))
      ram_tile_masked <- mask(ram_tile, country_tile[country_tile@data$GID_0 == country_id,])
      
      ## get the area (in km2) of each pixel
      pixel_area <- area(ram_tile_masked, na.rm=TRUE, weights=FALSE)
      
      ## get the area of pixel with either pct pasture > 0 or pct cropland > 0
      pixel_1 <- ram_tile_masked == 1 # transform tile to assign 1 if pixel == 1, else 0
      area_pixel_1 <- pixel_1 * pixel_area
      
      ## get the area of pixel with diversified agroecosystem, ie pct pasture > 0 and pct cropland > 0
      pixel_2 <- ram_tile_masked == 2 # transform tile to assign 1 if pixel == 2, else 0
      area_pixel_2 <- pixel_2 * pixel_area
      
      r <- data.table(
        area_pasture_or_cropland = sum(area_pixel_1[], na.rm = T),
        area_pasture_and_cropland = sum(area_pixel_2[], na.rm = T)
      )
      r[,area_agland:=area_pasture_or_cropland+area_pasture_and_cropland]
      return(r)
    }))
    
    ## combine the country information from different tiles
    area_tiles_values <- area_tiles_values[,lapply(.SD,sum)] # sums the columns
    
    ## compute the metrics
    cat(paste0("processing ",country_id," (",k,"/", n,") - computing metrics\n"))
    
    metrics <- data.table(
      Iso3_Code=country_id,
      area_all=area_tiles_values[,area_agland],
      area_pasture_or_cropland=area_tiles_values[,area_pasture_or_cropland],
      area_pasture_and_cropland=area_tiles_values[,area_pasture_and_cropland],
      proportion_area_pasture_or_cropland=area_tiles_values[,area_pasture_and_cropland]/area_tiles_values[,area_agland]
    )
    metrics[,value := proportion_area_pasture_or_cropland*100] # convert to %
    metrics$value <- ifelse(metrics$value>=0,metrics$value,NA) # convert NULL values to NA
    setDT(metrics)
    
    k <<- k + 1
    return(metrics)
  }), use.names = T)
  
  ### add standard country label to the IsoCode
  setkey(d, Iso3_Code)
  setkey(country_cv, Iso3_Code)
  d <- unique(country_cv[,.(Iso3_Code,CountrynameISO)])[d]
  if(d[is.na(CountrynameISO),.N]>0){
    warning(paste("ISO3 codes ", paste(d[is.na(CountrynameISO),Iso3_Code], collapse = ", "), "had no equivalent"))
  }
  setnames(d, "CountrynameISO","country")
  
  ## make standard indicator output
  d_std <- d
  d_std <- d_std[!is.na(d_std$value),]
  d_std <- d_std[!(d_std$value ==""),]
  setDT(d_std)
  d_std <- d_std[,.(Iso3_Code, country, value, subIndicator = SI)]
  
  # scale the values from 0 to 100 and cap to 100
  
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(d_std$value>100,100,d_std$value)
  d_std$value  <- ifelse( d_std$value >=0, d_std$value ,NA) # convert NULL values to NA
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = "AP2I03_01", user = user)
  
  rm(k)
  print("finished processing AP2I03_01 crop-livestock diversity\n")
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}


#' Get the AP2I03_01 sub-indicator by counting pixels
#'
#' @param dir_ramankutty_rasters path of the directory that contains the rasters (splitted on grid)
#' @param dir_countries path of the directory that contains the country shapes (splitted on grid)
#' @param country_col name of the column that contains the country ISO3 code in the data slot of country_sp (e.g. "GID_O" for gadm)
#' @param country_index table of the links between tiles and countries
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @description Computes the country mean percentage of agricultural land containing both cropland and pasture (not weighing pixels by pixel area). This dataset was made by overlaying Ramunkutty (2008) 1 x 1 km pasture and cropland extent datasets to identify pixels containing both cropland and pasture, and representing these "diversified" areas as a percentage of total agricultural land.
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' @examples
#'
#'
get_AP2I03_01_pixel <- function(dir_ramankutty_rasters, dir_countries, country_col = "GID_0", country_index, metadata_master, user = NA, country_cv){
  
  k <-1
  ### compute the indicators
  d <- rbindlist(lapply(country_index[,unique(GID_0)], function(country_id,n=length(country_index[,unique(GID_0)])){
    
    ## clip the raster to the shape of the country (tile by tile)
    ram_tiles_masked <- lapply(country_index[GID_0==country_id,tile_id],function(tile_id){
      
      cat(paste0("processing ",country_id," (",k,"/", n,") - masking tile ", tile_id,"\n"))
      
      country_tile <- readOGR(dsn = dir_countries, layer = paste0("countries_split_",tile_id), verbose = F)
      ram_tile <- raster(file.path(dir_ramankutty_rasters, paste0("tile_",tile_id,".tif")))
      ram_tile_masked <- mask(ram_tile, country_tile[country_tile@data$GID_0 == country_id,])
      return(ram_tile_masked)
    })
    
    ## merge the tiles
    if(length(ram_tiles_masked)>1){
      cat(paste0("processing ",country_id," (",k,"/", n,") - aggregating tiles\n"))
      # Note: it is not necessary to merge the rasters
      # ram_masked <- do.call(merge,ram_tiles_masked)
      # this operation is costly in memory
      # instead, it is enough to bind only the pixel values (regardless of the pixel coordinates)
      ram_masked_vals <- unlist(lapply(ram_tiles_masked, function(x){
        vals <- values(x) 
        vals <- vals[!is.na(vals)]
      }))
    }else{
      ram_masked_vals <- values(ram_tiles_masked[[1]])
      ram_masked_vals <- ram_masked_vals[!is.na(ram_masked_vals)]
    }
    
    ## compute the metrics
    cat(paste0("processing ",country_id," (",k,"/", n,") - computing metrics\n"))
    ram_masked_vals_dt <- as.data.table(ram_masked_vals)
    ram_masked_vals_t <- ram_masked_vals_dt[,.N,ram_masked_vals] # when converted ram_masked_vals by as.data.table(), the function names "ram_masked_vals" the unique variable of the ram_masked_vals vector
    
    N_pasture_or_cropland <- ram_masked_vals_t[ram_masked_vals==1,N] # "1", by construction, means pixel with either pct pasture > 0 or pct cropland > 0
    N_pasture_and_cropland <- ram_masked_vals_t[ram_masked_vals==2,N] # "2", by construction, means pixel with diversified agroecosystem, ie pct pasture > 0 and pct cropland > 0
    metrics <- data.table(
      Iso3_Code=country_id,
      N_pasture_or_cropland=ifelse(length(N_pasture_or_cropland)==0,0,N_pasture_or_cropland), # if no pasture or cropland put 0, else put sum of pasture and cropland
      N_pasture_and_cropland=ifelse(length(N_pasture_and_cropland)==0,0,N_pasture_and_cropland)# if no pasture and cropland together put 0, else put sum of pasture and cropland together
    )
    metrics[,N_agland:=N_pasture_or_cropland + N_pasture_and_cropland]
    metrics[,ratio_diversified:=ifelse(N_agland==0,NA,N_pasture_and_cropland/N_agland)]
    
    k <<- k + 1
    return(metrics)
  }), use.names = T)
  
  ### add standard country label to the IsoCode
  setkey(d, Iso3_Code)
  setkey(country_cv, Iso3_Code)
  d <- unique(country_cv[,.(Iso3_Code,CountrynameISO)])[d]
  if(d[is.na(CountrynameISO),.N]>0){
    warning(paste("ISO3 codes ", paste(d[is.na(CountrynameISO),Iso3_Code], collapse = ", "), "had no equivalent"))
  }
  setnames(d, "CountrynameISO","country")
  
  ### make standard indicator output
  d_std <- d[,.(Iso3_Code, country, value = ratio_diversified, subIndicator = "AP2I03_01_pixel")]
  d[,value:=ratio_diversified]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = "AP2I03_01", user = user)
  
  rm(k)
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

### Get the SP2I12_01 sub-indicator by taking pixel area

#' @param dir_spam_rasters path of the directory that contains the rasters (splitted on grid)
#' @param dir_countries path of the directory that contains the country shapes (splitted on grid)
#' @param country_index table of the links between tiles and countries
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @description Computes the Percentage of agricultural land with >= 22 crops
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' @examples
#'
#'
get_SP2I12_01 <- function(input, dir_countries, country_index, metadata_master, user = NA, country_cv,SI="SP2I12_01"){
  
  print("processing SP2I12_01 % cropland with >22 crops\n")
  k <-1
  ### compute the indicators
  d <- rbindlist(lapply(country_index[,unique(GID_0)], function(country_id,n=length(country_index[,unique(GID_0)])){
    
    area_tiles_values <- rbindlist(lapply(country_index[GID_0==country_id,tile_id],function(tile_id){
      ## clip the raster to the shape of the country (tile by tile)
      
      cat(paste0("processing ",country_id," (",k,"/", n,") - masking tile ", tile_id,"\n"))
      
      country_tile <- readOGR(dsn = dir_countries, layer = paste0("countries_split_",tile_id), verbose = F)
      spam_tile <- raster(file.path(input, paste0("tile_",tile_id,".tif")))
      
      spam_tile_masked <- mask(spam_tile, country_tile[country_tile@data$GID_0 == country_id,])
      
      ## get the area (in km2) of each pixel
      pixel_area <- area(spam_tile_masked, na.rm=TRUE, weights=FALSE)
      
      ## transform tile to assign 1 if pixel >= 22, else 0
      pixel_22 <- spam_tile_masked >= 22
      ## transform tile to assign 1 if the pixel is an agricultural land
      pixel_agland <- spam_tile_masked >= 0
      
      ## get the area of agricultural land
      area_pixel_agland <- pixel_agland * pixel_area
      ## get the area of agricultural land with 22 or more harvested crops
      area_pixel_22 <- pixel_22 * pixel_area
      
      return(data.table(
        area_agland = sum(area_pixel_agland[], na.rm = T),
        area_more_22 = sum(area_pixel_22[], na.rm = T)
      ))
    }))
    
    ## combine the country information from different tiles
    area_tiles_values <- area_tiles_values[,lapply(.SD,sum)] # sums the columns
    
    ## compute the metrics
    cat(paste0("processing ",country_id," (",k,"/", n,") - computing metrics\n"))
    
    metrics <- data.table(
      Iso3_Code=country_id,
      area_all=area_tiles_values[,area_agland],
      area_more_22=area_tiles_values[,area_more_22],
      percent_more_22=area_tiles_values[,area_more_22]/area_tiles_values[,area_agland]*100
    )
    metrics[,value := percent_more_22]
    metrics$value <- ifelse(metrics$value>=0,metrics$value,NA) # convert NULL values to NA
    
    k <<- k + 1
    return(metrics)
  }), use.names = T)
  
  ### add standard country label to the IsoCode
  setkey(d, Iso3_Code)
  setkey(country_cv, Iso3_Code)
  d <- unique(country_cv[,.(Iso3_Code,CountrynameISO)])[d]
  if(d[is.na(CountrynameISO),.N]>0){
    warning(paste("ISO3 codes ", paste(d[is.na(CountrynameISO),Iso3_Code], collapse = ", "), "had no equivalent"))
  }
  setnames(d, "CountrynameISO","country")
  
  ### make standard indicator output
  d_std <- d[,.(Iso3_Code, country, value, subIndicator = SI)]
  
  # scale the values from 0 to 100
  
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  
  rm(k)
  print("finished processing SP2I12_01 % cropland with >22 crops\n")
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}


#' Get the SP2I12_01 sub-indicator by counting pixels
#'
#' @param dir_spam_rasters path of the directory that contains the rasters (splitted on grid)
#' @param dir_countries path of the directory that contains the country shapes (splitted on grid)
#' @param country_index table of the links between tiles and countries
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @description Computes the Percentage of agricultural land with >= 22 crops
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' @examples
#'
#'
get_SP2I12_01_pixel <- function(dir_spam_rasters, dir_countries, country_index, metadata_master, user = NA, country_cv){
  
  k <-1
  ### compute the indicators
  d <- rbindlist(lapply(country_index[,unique(GID_0)], function(country_id,n=length(country_index[,unique(GID_0)])){
    
    N_pixel_tiles_values <- rbindlist(lapply(country_index[GID_0==country_id,tile_id],function(tile_id){
      ## clip the raster to the shape of the country (tile by tile)
      
      cat(paste0("processing ",country_id," (",k,"/", n,") - masking tile ", tile_id,"\n"))
      
      country_tile <- readOGR(dsn = dir_countries, layer = paste0("countries_split_",tile_id), verbose = F)
      spam_tile <- raster(file.path(dir_spam_rasters, paste0("tile_",tile_id,".tif")))
      
      spam_tile_masked <- mask(spam_tile, country_tile[country_tile@data$GID_0 == country_id,])
      
      ## transform tile to assign 1 if pixel >= 22, else 0
      pixel_22 <- spam_tile_masked >= 22
      ## transform tile to assign 1 if the pixel is an agricultural land
      pixel_agland <- spam_tile_masked >= 0
      
      return(data.table(
        N_pixel_agland = sum(pixel_agland[], na.rm = T),
        N_pixel_more_22 = sum(pixel_22[], na.rm = T)
      ))
    }))
    
    ## combine the country information from different tiles
    N_pixel_tiles_values <- N_pixel_tiles_values[,lapply(.SD,sum)] # sums the columns
    
    ## compute the metrics
    cat(paste0("processing ",country_id," (",k,"/", n,") - computing metrics\n"))
    
    metrics <- data.table(
      Iso3_Code=country_id,
      N_pixel_agland=N_pixel_tiles_values[,N_pixel_agland],
      N_pixel_more_22_crops=N_pixel_tiles_values[,N_pixel_more_22],
      proportion_more_22_crops=N_pixel_tiles_values[,N_pixel_more_22]/N_pixel_tiles_values[,N_pixel_agland]
    )
    metrics[,value := proportion_more_22_crops]
    
    k <<- k + 1
    return(metrics)
  }), use.names = T)
  
  ### add standard country label to the IsoCode
  setkey(d, Iso3_Code)
  setkey(country_cv, Iso3_Code)
  d <- unique(country_cv[,.(Iso3_Code,CountrynameISO)])[d]
  if(d[is.na(CountrynameISO),.N]>0){
    warning(paste("ISO3 codes ", paste(d[is.na(CountrynameISO),Iso3_Code], collapse = ", "), "had no equivalent"))
  }
  setnames(d, "CountrynameISO","country")
  
  ### make standard indicator output
  d_std <- d[,.(Iso3_Code, country, value, subIndicator = "SP2I12_01_pixel")]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = "SP2I12_01", user = user)
  
  rm(k)
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}


#' Get the SP2I12_02 sub-indicator
#'
#' @param dir_spam_rasters path of the directory that contains the rasters (splitted on grid)
#' @param dir_countries path of the directory that contains the country shapes (splitted on grid)
#' @param country_index table of the links between tiles and countries
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @description Computes the country mean Shannon index - based on proportion of pixel areas
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' @examples
#'
#'
get_SP2I12_02 <- function(input, dir_countries, country_index, metadata_master, user = NA, country_cv,SI="SP2I12_02"){
  
  k <-1
  print("processing SP2I12_02 crop shannon diversity\n")
  ### compute the indicators
  d <- rbindlist(lapply(country_index[,unique(GID_0)], function(country_id,n=length(country_index[,unique(GID_0)])){
    
    ## clip the raster to the shape of the country (tile by tile)
    tiles_shannon_index_values <- rbindlist(lapply(country_index[GID_0==country_id,tile_id],function(tile_id){
      
      cat(paste0("processing ",country_id," (",k,"/", n,") - masking tile ", tile_id,"\n"))
      
      country_tile <- readOGR(dsn = dir_countries, layer = paste0("countries_split_",tile_id), verbose = F)
      #tile_id = 1
      spam_tile <- raster(file.path(input, paste0("tile_",tile_id,".tif")))
      
      spam_tile_masked <- mask(spam_tile, country_tile[country_tile@data$GID_0 == country_id,])
      
      ## from the tile, extract the data needed for computing the metrics at the country level 
      # indicator formula is (F): 
      # country mean shannon index = (sum(pixel area * pixel value) of all tiles) / (sum(sum pixel area of a tile) of all tiles)
      
      ## get the area (in km2) of each pixel
      pixel_area <- area(spam_tile_masked, na.rm=TRUE, weights=FALSE)
      
      ## get the shannon index times the pixel area
      spam_tile_masked_area <- spam_tile_masked * pixel_area # In (F), doing: pixel area * pixel value
      
      return(data.table(
        area_all = sum(pixel_area[], na.rm = T), # In (F), doing sum(sum pixel area of a tile)
        area_times_value = sum(spam_tile_masked_area[], na.rm = T), # In (F), doing: sum(pixel area * pixel value)
        n_pixels = sum(!is.na(spam_tile_masked)[])
      ))
    }))
    
    
    ## compute the metrics
    cat(paste0("processing ",country_id," (",k,"/", n,") - computing metrics\n"))
    
    # combine the country information from different tiles
    tiles_shannon_index_values <- tiles_shannon_index_values[,lapply(.SD,sum)] # sums the columns
    # In (F), doing (sum(pixel area * pixel value) of all tiles)
    # and
    # In (F), doing (sum(sum pixel area of a tile) of all tiles)
    
    metrics <- data.table(
      Iso3_Code=country_id,
      n_pixels=tiles_shannon_index_values[,n_pixels],
      area_all = tiles_shannon_index_values[,area_all],
      mean_shannon=tiles_shannon_index_values[,area_times_value] / tiles_shannon_index_values[,area_all] # In (F), doing (sum(pixel area * pixel value) of all tiles) / (sum(sum pixel area of a tile) of all tiles)
    )
    k <<- k + 1
    return(metrics)
  }), use.names = T)
  d[,value:=mean_shannon]
  d$value <- ifelse(d$value>=0,d$value,NA) # convert NULL values to NA
  
  ### add standard country label to the IsoCode
  setkey(d, Iso3_Code)
  setkey(country_cv, Iso3_Code)
  d <- unique(country_cv[,.(Iso3_Code,CountrynameISO)])[d]
  if(d[is.na(CountrynameISO),.N]>0){
    warning(paste("ISO3 codes ", paste(d[is.na(CountrynameISO),Iso3_Code], collapse = ", "), "had no equivalent"))
  }
  setnames(d, "CountrynameISO","country")
  
  ### make standard indicator output
  d_std <- d[,.(Iso3_Code, country, value, subIndicator = SI)]
  
  # scale the values from 0 to 100
  
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  thresholds <- data.table(thresholds = c("","Actual minimum","Actual maximum"), value = c("",value_min,value_max))
  m <- rbind(m,thresholds,use.names=FALSE)
  
  rm(k)
  print("finished processing SP2I12_02 crop shannon diversity\n")
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the SP2I12_02 sub-indicator
#'
#' @param dir_spam_rasters path of the directory that contains the rasters (splitted on grid)
#' @param dir_countries path of the directory that contains the country shapes (splitted on grid)
#' @param country_index table of the links between tiles and countries
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @description Computes the country mean Shannon index - based on propoertion of number of pixels (less accurate than proportion of pixel areas)
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' @examples
#'
#'
get_SP2I12_02_pixel <- function(dir_spam_rasters, dir_countries, country_index, metadata_master, user = NA, country_cv){
  
  k <-1
  ### compute the indicators
  d <- rbindlist(lapply(country_index[,unique(GID_0)], function(country_id,n=length(country_index[,unique(GID_0)])){
    
    ## clip the raster to the shape of the country (tile by tile)
    spam_tiles_masked <- lapply(country_index[GID_0==country_id,tile_id],function(tile_id){
      
      cat(paste0("processing ",country_id," (",k,"/", n,") - masking tile ", tile_id,"\n"))
      
      country_tile <- readOGR(dsn = dir_countries, layer = paste0("countries_split_",tile_id), verbose = F)
      spam_tile <- raster(file.path(dir_spam_rasters, paste0("tile_",tile_id,".tif")))
      
      spam_tile_masked <- mask(spam_tile, country_tile[country_tile@data$GID_0 == country_id,])
      
      return(spam_tile_masked)
    })
    
    ## merge the tiles
    if(length(spam_tiles_masked)>1){
      cat(paste0("processing ",country_id," (",k,"/", n,") - aggregating tiles\n"))
      # Note: it is not necessary to merge the rasters
      # spam_masked <- do.call(merge,spam_tiles_masked)
      # this operation is costly in memory
      # instead, it is enough to bind only the pixel values (regardless of the pixel coordinates)
      spam_masked_vals <- unlist(lapply(spam_tiles_masked, function(x){
        vals <- values(x)
        vals <- vals[!is.na(vals)]
      }))
    }else{
      spam_masked_vals <- spam_tiles_masked[[1]][]
      spam_masked_vals <- spam_masked_vals[!is.na(spam_masked_vals)]
    }
    
    ## compute the metrics
    cat(paste0("processing ",country_id," (",k,"/", n,") - computing metrics\n"))
    spam_masked_q <- quantile(spam_masked_vals, probs = c(0, .25, .5, .75, 1), na.rm=T)
    
    metrics <- data.table(
      Iso3_Code=country_id,
      MEAN=mean(spam_masked_vals, na.rm = T), 
      SD=sd(spam_masked_vals, na.rm = T), 
      MIN=spam_masked_q[1],
      q1=spam_masked_q[2],
      MEDIAN=spam_masked_q[3],
      q3=spam_masked_q[4],
      MAX=spam_masked_q[5],
      N = length(spam_masked_vals)
    )
    metrics[,value:=MEAN]
    
    k <<- k + 1
    return(metrics)
  }), use.names = T)
  
  ### add standard country label to the IsoCode
  setkey(d, Iso3_Code)
  setkey(country_cv, Iso3_Code)
  d <- unique(country_cv[,.(Iso3_Code,CountrynameISO)])[d]
  if(d[is.na(CountrynameISO),.N]>0){
    warning(paste("ISO3 codes ", paste(d[is.na(CountrynameISO),Iso3_Code], collapse = ", "), "had no equivalent"))
  }
  setnames(d, "CountrynameISO","country")
  
  ### make standard indicator output
  d_std <- d[,.(Iso3_Code, country, value, subIndicator = "SP2I12_02_pixel")]
  
  # scale the values from 0 to 100
  
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = "SP2I12_02", user = user)
  
  rm(k)
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}



#' Get the SP2I21_01 sub-indicator by taking pixel area
#'
#' @param dir_soilbiodiv_rasters path of the directory that contains the rasters (splitted on grid)
#' @param dir_countries path of the directory that contains the country shapes (splitted on grid)
#' @param country_col name of the column that contains the country ISO3 code in the data slot of country_sp (e.g. "GID_O" for gadm)
#' @param country_index table of the links between tiles and countries
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @description Computes the mean soil biodiversity index per country
#' @details Computation is based on pixel value * pixel area
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' @examples
#'
#'
get_SP2I21_01 <- function(dir_soilbiodiv_rasters, dir_countries, country_col = "GID_0", country_index, metadata_master, user = NA, country_cv,SI){
  
  k <-1
  print("processing SP2I21_01 soil biodiversity\n")
  ### compute the indicators
  d <- rbindlist(lapply(country_index[,unique(GID_0)], function(country_id,n=length(country_index[,unique(GID_0)])){
    
    ## clip the raster to the shape of the country (tile by tile)
    tiles_soil_biodiv_index_values <- rbindlist(lapply(country_index[GID_0==country_id,tile_id],function(tile_id){
      
      cat(paste0("processing ",country_id," (",k,"/", n,") - masking tile ", tile_id,"\n"))
      
      country_tile <- readOGR(dsn = dir_countries, layer = paste0("countries_split_",tile_id), verbose = F)
      soil_tile <- raster(file.path(dir_soilbiodiv_rasters, paste0("tile_",tile_id,".tif")))
      
      soil_tile_masked <- mask(soil_tile, country_tile[country_tile@data$GID_0 == country_id,])
      
      ## from the tile, extract the data needed for computing the metrics at the country level 
      # indicator formula is (F): 
      # mean soilbiodiversityindex = (sum(pixel area * pixel value) of all tiles) / (sum(sum pixel area of a tile) of all tiles)
      
      ## get the area (in km2) of each pixel
      pixel_area <- area(soil_tile_masked, na.rm=TRUE, weights=FALSE)
      
      ## get the area of each agricultural land pixel
      pixel_soil <- soil_tile_masked >= 0 # tranform tile to assign 1 if the pixel is soil
      area_pixel_soil <- pixel_soil * pixel_area
      soil_tile_masked_area <- soil_tile_masked * area_pixel_soil # In (F), doing: pixel area * pixel value
      
      return(data.table(
        area = sum(area_pixel_soil[], na.rm = T), # In (F), doing sum(sum pixel area of a tile)
        area_times_value = sum(soil_tile_masked_area[], na.rm = T) # In (F), doing: sum(pixel area * pixel value)
      ))
    }))
    
    
    ## compute the metrics
    cat(paste0("processing ",country_id," (",k,"/", n,") - computing metrics\n"))
    
    # combine the country information from different tiles
    tiles_soil_biodiv_index_values <- tiles_soil_biodiv_index_values[,lapply(.SD,sum)] # sums the columns
    # In (F), doing (sum(pixel area * pixel value) of all tiles)
    # and
    # In (F), doing (sum(sum pixel area of a tile) of all tiles)
    
    metrics <- data.table(
      Iso3_Code=country_id,
      area_soil = tiles_soil_biodiv_index_values[,area],
      mean_SoilBiodiversityIndex=tiles_soil_biodiv_index_values[,area_times_value] / tiles_soil_biodiv_index_values[,area] # In (F), doing (sum(pixel area * pixel value) of all tiles) / (sum(sum pixel area of a tile) of all tiles)
    )
    k <<- k + 1
    return(metrics)
  }), use.names = T)
  d[,value:=mean_SoilBiodiversityIndex]
  d$value <- ifelse(d$value>=0,d$value,NA) # convert NULL values to NA
  
  ### add standard country label to the IsoCode
  setkey(d, Iso3_Code)
  setkey(country_cv, Iso3_Code)
  d <- unique(country_cv[,.(Iso3_Code,CountrynameISO)])[d]
  if(d[is.na(CountrynameISO),.N]>0){
    warning(paste("ISO3 codes ", paste(d[is.na(CountrynameISO),Iso3_Code], collapse = ", "), "had no equivalent"))
  }
  setnames(d, "CountrynameISO","country")
  
  ### make standard indicator output
  d_std <- d[,.(Iso3_Code, country, value, subIndicator = SI)]
  
  # scale the values from 0 to 100 and cap to 100
  
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(d_std$value>100,100,d_std$value)
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  
  rm(k)
  print("finished processing SP2I21_01 soil biodiversity\n")
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}

#' Get the SP2I21_01 sub-indicator by counting pixels
#'
#' @param dir_soilbiodiv_rasters path of the directory that contains the rasters (splitted on grid)
#' @param dir_countries path of the directory that contains the country shapes (splitted on grid)
#' @param country_col name of the column that contains the country ISO3 code in the data slot of country_sp (e.g. "GID_O" for gadm)
#' @param country_index table of the links between tiles and countries
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @description Computes the mean soil biodiversity index per country
#' @details Computation is based on pixel count (regardless of pixel area) 
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' @examples
#'
#'
get_SP2I21_01_pixel <- function(dir_soilbiodiv_rasters, dir_countries, country_col = "GID_0", country_index, metadata_master, user = NA, country_cv){
  
  k <-1
  ### compute the indicators
  d <- rbindlist(lapply(country_index[,unique(GID_0)], function(country_id,n=length(country_index[,unique(GID_0)])){
    
    ## clip the raster to the shape of the country (tile by tile)
    soil_tiles_masked <- lapply(country_index[GID_0==country_id,tile_id],function(tile_id){
      
      cat(paste0("processing ",country_id," (",k,"/", n,") - masking tile ", tile_id,"\n"))
      
      country_tile <- readOGR(dsn = dir_countries, layer = paste0("countries_split_",tile_id), verbose = F)
      soil_tile <- raster(file.path(dir_soilbiodiv_rasters, paste0("tile_",tile_id,".tif")))
      
      soil_tile_masked <- mask(soil_tile, country_tile[country_tile@data$GID_0 == country_id,])
      
      return(soil_tile_masked)
    })
    
    ## merge the tiles
    if(length(soil_tiles_masked)>1){
      cat(paste0("processing ",country_id," (",k,"/", n,") - aggregating tiles\n"))
      # Note: it is not necessary to merge the rasters
      # soil_masked <- do.call(merge,soil_tiles_masked)
      # this operation is costly in memory
      # instead, it is enough to bind only the pixel values (regardless of the pixel coordinates)
      soil_masked_vals <- unlist(lapply(soil_tiles_masked, function(x){
        vals <- values(x)
        vals <- vals[!is.na(vals)]
      }))
    }else{
      soil_masked_vals <- soil_tiles_masked[[1]][]
      soil_masked_vals <- soil_masked_vals[!is.na(soil_masked_vals)]
    }
    
    ## compute the metrics
    cat(paste0("processing ",country_id," (",k,"/", n,") - computing metrics\n"))
    soil_masked_q <- quantile(soil_masked_vals, probs = c(0, .25, .5, .75, 1), na.rm=T)
    
    metrics <- data.table(
      Iso3_Code=country_id,
      MEAN=mean(soil_masked_vals, na.rm = T), 
      SD=sd(soil_masked_vals, na.rm = T), 
      MIN=soil_masked_q[1],
      q1=soil_masked_q[2],
      MEDIAN=soil_masked_q[3],
      q3=soil_masked_q[4],
      MAX=soil_masked_q[5],
      N = length(soil_masked_vals)
    )
    
    k <<- k + 1
    return(metrics)
  }), use.names = T)
  
  ### add standard country label to the IsoCode
  setkey(d, Iso3_Code)
  setkey(country_cv, Iso3_Code)
  d <- unique(country_cv[,.(Iso3_Code,CountrynameISO)])[d]
  if(d[is.na(CountrynameISO),.N]>0){
    warning(paste("ISO3 codes ", paste(d[is.na(CountrynameISO),Iso3_Code], collapse = ", "), "had no equivalent"))
  }
  setnames(d, "CountrynameISO","country")
  
  ### make standard indicator output
  d_std <- d[,.(Iso3_Code, country, value = MEAN, subIndicator = "SP2I21_01_pixel")]
  d_std[,value:=MEAN]
  
  # scale the values from 0 to 100 and cap to 100
  
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(d_std$value>100,100,d_std$value)
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = "SP2I21_01", user = user)
  
  rm(k)
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}


#' Get the SP2I22_01 sub-indicator by taking pixel area
#'
#' @param dir_esa_rasters path of the directory that contains the rasters (splitted on grid). Pixels have 2 possible values: 1 = agricultural land with at least 10% natural land, 0 = agricultural land with less than 10% of natural land
#' @param dir_countries path of the directory that contains the country shapes (splitted on grid)
#' @param country_col name of the column that contains the country ISO3 code in the data slot of country_sp (e.g. "GID_O" for gadm)
#' @param country_index table of the links between tiles and countries
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @description Computes the proportion of pixels of agricultural land that contain natural land per country
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' @examples
#'
#'

get_SP2I22_01 <- function(dir_esa_rasters, dir_countries, country_col = "GID_0", country_index, metadata_master, user = NA, country_cv,SI){
  
  k <- 1
  print("Processing SP2I22_01 % nat in ag\n")
  ### compute the indicators
  
  d <- rbindlist(lapply(country_index[,unique(GID_0)], function(country_id,n=length(country_index[,unique(GID_0)])){
    
    ## clip the raster to the shape of the country (tile by tile) and retruns values for computing the metrics
    area_tiles_values <- rbindlist(lapply(country_index[GID_0==country_id,tile_id],function(tile_id){
      
      cat(paste0("processing ",country_id," (",k,"/", n,") - masking tile ", tile_id,"\n"))
      
      country_tile <- readOGR(dsn = dir_countries, layer = paste0("countries_split_",tile_id), verbose = F)
      esa_tile <- raster(file.path(dir_esa_rasters, paste0("tile_",tile_id,".tif")))
      # data has two values: 0 (cropland with <=10% nat) or 1 (cropland with >10% nat)
      
      esa_tile_masked <- mask(esa_tile, country_tile[country_tile@data$GID_0 == country_id,])
      
      ## from the tile, extract the data needed for computing the metrics at the country level 
      ## get the area (in km2) of each pixel
      pixel_area <- area(esa_tile_masked, na.rm=TRUE, weights=FALSE)
      
      ## get the area of cropand
      pixel_agland <- esa_tile_masked %in% c(0,1) # pixel is agland if its value is 0 or 1
      area_pixel_agland <- pixel_agland * pixel_area
      
      ## get the area of cropland with 10% natural land
      area_pixel_natland <- esa_tile_masked * pixel_area # works because only esa_tile cells with value 1 will be counted
      
      return(data.table(
        area_agland = sum(area_pixel_agland[], na.rm = T),
        area_natland = sum(area_pixel_natland[], na.rm = T)
      ))
    }))
    
    ## combine the country information from different tiles
    area_tiles_values <- area_tiles_values[,lapply(.SD,sum)] # sums the columns
    
    ## compute the metrics
    cat(paste0("processing ",country_id," (",k,"/", n,") - computing metrics\n"))
    
    metrics <- data.table(
      Iso3_Code=country_id,
      area_agland=area_tiles_values[,area_agland],
      area_natland=area_tiles_values[,area_natland],
      percent_natland=area_tiles_values[,area_natland]/area_tiles_values[,area_agland]*100
    )
    
    k <<- k + 1
    return(metrics)
  }), use.names = T)
  d[,value:=percent_natland]
  d$value <- ifelse(d$value>=0,d$value,NA) # convert NULL values to NA
  
  ### add standard country label to the IsoCode
  setkey(d, Iso3_Code)
  setkey(country_cv, Iso3_Code)
  d <- unique(country_cv[,.(Iso3_Code,CountrynameISO)])[d]
  if(d[is.na(CountrynameISO),.N]>0){
    warning(paste("ISO3 codes ", paste(d[is.na(CountrynameISO),Iso3_Code], collapse = ", "), "had no equivalent"))
  }
  setnames(d, "CountrynameISO","country")
  
  
  ### make standard indicator output
  d_std <- d[,.(Iso3_Code, country, value, subIndicator = SI)]
  
  # scale the values from 0 to 100 and cap to 100
  value_max <- master[Sub_indicator_ID == SI, c(Maximum)]
  value_min <- master[Sub_indicator_ID == SI, c(Minimum)]
  value_max <- ifelse(value_max=="data",max(d_std$value),as.numeric(value_max))
  value_min <- ifelse(value_min=="data",min(d_std$value),as.numeric(value_min))
  d_std$value <- ifelse(d_std$value>0,round((d_std$value-value_min)/(value_max-value_min)*100,1),d_std$value) 
  d_std$value <- ifelse(d_std$value>100,100,d_std$value)
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = SI, user = user)
  
  rm(k)
  
  print("finished processing SP2I22_01 % nat in ag\n")
  
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
  
}

#' Get the SP2I22_01 sub-indicator by counting pixels
#'
#' @param dir_esa_rasters path of the directory that contains the rasters (splitted on grid). Pixels have 2 possible values: 1 = agricultural land with at least 10% natural land, 0 = agricultural land with less than 10% of natural land
#' @param dir_countries path of the directory that contains the country shapes (splitted on grid)
#' @param country_col name of the column that contains the country ISO3 code in the data slot of country_sp (e.g. "GID_O" for gadm)
#' @param country_index table of the links between tiles and countries
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param user name of the user who runs the computation
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO", "value")}
#' @description Computes the proportion of pixels of agricultural land that contain natural land per country
#' @details Computation is based on pixel count (regardless of pixel area)
#' @return a list of 3 tables: standard subindicator, exhaustive subindicator and metadata.
#' @examples
#'
#'
get_SP2I22_01_pixel <- function(dir_esa_rasters, dir_countries, country_col = "GID_0", country_index, metadata_master, user = NA, country_cv){
  
  k <-1
  ### compute the indicators
  d <- rbindlist(lapply(country_index[,unique(GID_0)], function(country_id,n=length(country_index[,unique(GID_0)])){
    
    ## clip the raster to the shape of the country (tile by tile)
    esa_tiles_masked <- lapply(country_index[GID_0==country_id,tile_id],function(tile_id){
      
      cat(paste0("processing ",country_id," (",k,"/", n,") - masking tile ", tile_id,"\n"))
      
      country_tile <- readOGR(dsn = dir_countries, layer = paste0("countries_split_",tile_id), verbose = F)
      esa_tile <- raster(file.path(dir_esa_rasters, paste0("tile_",tile_id,".tif")))
      
      esa_tile_masked <- mask(esa_tile, country_tile[country_tile@data$GID_0 == country_id,])
      
      return(esa_tile_masked)
    })
    
    ## merge the tiles
    if(length(esa_tiles_masked)>1){
      cat(paste0("processing ",country_id," (",k,"/", n,") - aggregating tiles\n"))
      # Note: it is not necessary to merge the rasters
      # esa_masked <- do.call(merge,esa_tiles_masked)
      # this operation is costly in memory
      # instead, it is enough to bind only the pixel values (regardless of the pixel coordinates)
      esa_masked_vals <- unlist(lapply(esa_tiles_masked, function(x){
        vals <- values(x)
        vals <- vals[!is.na(vals)]
      }))
    }else{
      esa_masked_vals <- esa_tiles_masked[[1]][]
      esa_masked_vals <- esa_masked_vals[!is.na(esa_masked_vals)]
    }
    
    ## compute the metrics
    cat(paste0("processing ",country_id," (",k,"/", n,") - computing metrics\n"))
    esa_masked_vals_dt <- as.data.table(esa_masked_vals)
    esa_masked_vals_t <- esa_masked_vals_dt[,.N,esa_masked_vals] # when converted esa_masked_vals by as.data.table(), the function names "esa_masked_vals" the unique variable of the esa_masked_vals vector
    
    N_agland_nat <- esa_masked_vals_t[esa_masked_vals==1,N]
    N_agland_not_nat <- esa_masked_vals_t[esa_masked_vals==0,N]
    
    metrics <- data.table(
      Iso3_Code=country_id,
      N_agland_not_nat=ifelse(length(N_agland_not_nat)==0,0,N_agland_not_nat),
      N_agland_nat=ifelse(length(N_agland_nat)==0,0,N_agland_nat)
    )
    metrics[,N_agland:=N_agland_not_nat + N_agland_nat]
    metrics[,percent_agland_nat:=ifelse(N_agland==0,NA,N_agland_nat/N_agland*100)]
    metrics[,value:=percent_agland_nat]
    
    k <<- k + 1
    return(metrics)
  }), use.names = T)
  
  ### add standard country label to the IsoCode
  setkey(d, Iso3_Code)
  setkey(country_cv, Iso3_Code)
  d <- unique(country_cv[,.(Iso3_Code,CountrynameISO)])[d]
  if(d[is.na(CountrynameISO),.N]>0){
    warning(paste("ISO3 codes ", paste(d[is.na(CountrynameISO),Iso3_Code], collapse = ", "), "had no equivalent"))
  }
  setnames(d, "CountrynameISO","country")
  
  ### make standard indicator output
  d_std <- d[,.(Iso3_Code, country, value, subIndicator = "SP2I22_01_pixel")]
  
  ### make metadata
  m <- make_metadata(metadata_master = metadata_master, subIndicator = "SP2I22_01", user = user)
  
  rm(k)
  return(list(subIndicator = d_std, subIndicator_all = d, metadata = m))
}
