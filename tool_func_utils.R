# func_utils.R

### version 2020-03-26 15:30

#' Format the country controlled vocabulary table
#' 
#' @param x cv table with columns "Iso3_Code", "Iso2_Code", "CountrynameISO", "Country.otherX"
#' @return cv table with \code{c("Iso3_Code", " CountrynameISO" , "value")} columns
format_country_cv <- function(x){
  xx <- copy(x)
  setDT(xx)
  xx[,CountrynameISO2:=CountrynameISO]
  xx <- melt(xx, id.vars = c("Iso3_Code","CountrynameISO"), value.name = "country_match")[!is.na(country_match)]
  xx[,variable:=NULL]
  xx <- xx[!(duplicated(Iso3_Code) & duplicated(country_match))]
  return(xx)
}

## format the controlled vocabulary
country_cv <- format_country_cv(x = country_cv_raw)
write.xlsx(country_cv, file.path(path_scripts_dir,"Country_codes_formatted.xlsx"))

#' Add ISO country codes to a data.table with a country column
#' 
#' @param t table with the country column
#' @param country_col the index or name of the column that contains the country
#' @param country_cv controlled vocabulary table \code{colnames(country_cv)} are \code{c("Iso3_Code", "CountrynameISO" , "country_match")} 
#' @return 
#' @details Idea for improvement: use a gazetteer like geonames http://download.geonames.org/export/dump/ 
get_country <- function(t, country_col, country_cv){
  setDT(t)
  setDT(country_cv)
  ### identify the country_col
  if(is.numeric(country_col)){
    country_col <- colnames(t)[colnames(t)==colnames(t)[country_col]]
  }
  setnames(t, country_col, "country_tmp",skip_absent=T)
  ### standardize country names (no leading/trailing spaces, no uppercase)
  t[,key:=trimws(tolower(country_tmp))]
  country_cv[,key:=trimws(tolower(country_match))]
  ### name matching
  setkey(t, key)
  setkey(country_cv, key)
  tt <- country_cv[,-c("country_match"), with = F][t]
  ### handle missing ISO3 codes
  if(dim(tt[is.na(Iso3_Code)])[1]>0){
    warning(paste("No ISO3 codes found for", paste(tt[is.na(Iso3_Code), country_tmp], collapse = ", ")))
  }
  tt[is.na(Iso3_Code), CountrynameISO:=country_tmp]
  setnames(tt, "CountrynameISO","country")
  ### remove unecessary columns 
  tt[,key:=NULL]
  tt[,country_tmp:=NULL]
  # country_cv[,key:=NULL]
  return(tt)
}

#' Add ISO country codes to a data.table with a country column adding on missing countries
#' 
get_country_all <- function(t, country_col, country_cv){
  setDT(t)
  setDT(country_cv)
  ### identify the country_col
  if(is.numeric(country_col)){
    country_col <- colnames(t)[colnames(t)==colnames(t)[country_col]]
  }
  setnames(t, country_col, "country_tmp",skip_absent=T)
  ### standardize country names (no leading/trailing spaces, no uppercase)
  t[,key:=trimws(tolower(country_tmp))]
  country_cv[,key:=trimws(tolower(country_match))]
  ### name matching
  setkey(t, key)
  setkey(country_cv, key)
  tt <- merge(country_cv[,-c("country_match")], t,all.x=TRUE)
  ### handle missing ISO3 codes
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
  tt <- tt %>%
    group_by(Iso3_Code,country) %>%
    summarise(Year = median(Year,na.rm=T),Value=median(Value,na.rm=T),value=max(value))
  setDT(tt)
  # country_cv[,key:=NULL]
  return(tt)
}

#' Make metadata table based on the metadata master file
#' 
#' @param metadata_master master file with indicator information (ABDI_indicators_master.xlsx)
#' @param subIndicator subIndicator code
#' @param user name of the user who runs the computation
#' @return a metadata table for the sub-indicator
#' 
make_metadata <- function(metadata_master, subIndicator, user = NA){
  m <- master[Sub_indicator_ID == subIndicator, 
              .(Indicator_ID, Indicator, Sub_indicator_ID, `Sub-indicator`, Description, Minimum, Maximum, `Justification for thresholds`, `Data collection year`, Citation_short, ## that's where you can add metadata information from the 
                "computed on"= format(Sys.time(), "%x"),
                'computed by'=user)]
  mm <- t(m)
  mm <- data.table(metadata = rownames(mm), value = mm[,1])
  return(mm)
}

#' Make metadata table for commitment indicators
make_metadata_commitments <- function(metadata_master,Indicator1,Indicator2,Indicator3,user=NA){
  m1 <- master[Indicator_ID == Indicator1, 
              .(Indicator_ID,Indicator, Description,Minimum, Maximum, `Justification for thresholds`, `Data collection year`, Citation_short,
                "computed on"= format(Sys.time(), "%x"),
                'computed by'=user)]
  m2 <-  master[Indicator_ID == Indicator2, 
                .(Indicator_ID,Indicator, Description,Minimum, Maximum, `Justification for thresholds`, `Data collection year`, Citation_short,
                  "computed on"= format(Sys.time(), "%x"),
                  'computed by'=user)]
  m3 <- master[Indicator_ID == Indicator3, 
               .(Indicator_ID,Indicator, Description,Minimum, Maximum, `Justification for thresholds`, `Data collection year`, Citation_short,
                 "computed on"= format(Sys.time(), "%x"),
                 'computed by'=user)]
  m123 <- rbind(m1,m2,m3)
  m123 <- unique(m123)
  mm <- t(m123)
  mm <- data.table(metadata = rownames(mm), `indicator 1` = mm[,1],`indicator 2`=mm[,2],`indicator 3`=mm[,3])
  return(mm)
}

#' Create squared spatial polygons that cover the world
#' 
#' @param tile_size the size of the tile in degree
#' @return spatial polygons projected in long/lat with the WGS84 datum and ellipsoid
make_world_grid <- function(tile_size){
  ## xmins of the longitudes
  if(floor(360/tile_size)==(360/tile_size)){
    longs <- seq(-180,180, tile_size)
  }else{
    longs <- c(seq(-180,180, tile_size),180)
  }
  longs_spacing <- diff(longs) # xmax - xmin of each tile (on the edges, tiles are not necessarilly square)
  longs <- longs[1:(length(longs)-1)]
  ## xmins of the latitudes
  if(floor(90/tile_size)==(90/tile_size)){
    lats <- seq(-90,90, tile_size)
  }else{
    lats <- c(seq(-90,90, tile_size),90)
  }
  lats_spacing <- diff(lats) # ymax - ymin of each tile (on the edges, squares are not necessarilly square)
  lats <- lats[1:(length(lats)-1)]

  ## make the grid
  grid_tiles <- unlist(lapply(1:length(lats), function(lat_index){
    lapply(1:length(longs), function(long_index){
      rgeos::bbox2SP(
        s=lats[lat_index], n = lats[lat_index] + lats_spacing[lat_index], 
        w = longs[long_index], e = longs[long_index] + longs_spacing[long_index],
        proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
    })
  }))
  grid_tiles <- SpatialPolygonsDataFrame(
    Sr = do.call(bind, grid_tiles),
    data = data.table(tile_id = 1:length(grid_tiles))
  )
  return(grid_tiles)
}

#' Split the countries on the reference grid and saves the subsets in directory 
#' 
#' @param countries country spatial objects
#' @param ref_grid the world reference grid (spatialPolygonDataframe object)
#' @param output_dir path of the directory where the objects are saved
#' @return an index of the tiles and countries
split_countries_on_grid <- function(countries, ref_grid, output_dir){
  grid_index <- 1:length(ref_grid)
  index_countries_tiles <- rbindlist(
    l = lapply(grid_index, function(tile_id){
      ## Note: this lapply takes a long time to compute. However, I would not recommend parellelizing (with parallel::parlapply() for instance)
      # because duplicating the R environment which includes the GADM36 object for each core of the processor may saturate the RAM 
      countries_split <- crop(x = countries, y = extent(ref_grid[tile_id,]))
      if(!is.null(countries_split)){
        writeOGR(obj = countries_split, dsn = output_dir, driver = "ESRI Shapefile", 
                 layer = paste0("countries_split_",tile_id), overwrite_layer = T)
        index_countries_tile <- data.table(countries_split@data)
        index_countries_tile[,tile_id := tile_id]
        return(index_countries_tile)
      }
    })
  )
  return(index_countries_tiles)
}

#' Split the raster on the reference grid and saves the subsets in directory 
#' 
#' @param path_to_raster path of the raster file
#' @param ref_grid the world reference grid (spatialPolygonDataframe object)
#' @param output_dir path of the directory where the objects are saved
#' @param NA_val value of the nodatavalue after R import
#' @details split_raster_on_grid is an alternative to split_raster_on_grid_gdal_wrappers. It The raster object is not loaded into R.
#' @return nothing
split_raster_on_grid <- function(path_to_raster, ref_grid, output_dir, NA_val = NULL){
  
  ## load the raster to split
  ras <- raster(path_to_raster)
  
  ## for some reason, the raster gets loaded into R, but the ras@file@nodatavalue does not get applied to the R object
  # it was the case with ESA_2015_ag_nat_per_for_indicator.tif, NA values where assigned 128 at the import
  if(!is.null(NA_val)){
    NAvalue(ras) <- NA_val
  }
  
  ## select a subset of the raster file based on the extent of each tile
  out <- lapply(ref_grid@data$tile_id, function(tile_id){
    cat(paste0("processing tile ", tile_id,"\n"))
    ## get tile
    tile <- ref_grid[ref_grid$tile_id==tile_id,]

    ## make sure projection is the same
    tile <- spTransform(tile, ras@crs)
    
    ## make the raster tile
    raster_tile <- crop(ras, extent(tile))
    writeRaster(x = raster_tile, filename = file.path(output_dir, paste0("tile_", tile_id, ".tif")), overwrite = T) # (*)
  })
}

#' Split the raster on the reference grid and saves the subsets in directory (using gdal wrapper functions)
#' 
#' @param path_to_raster path of the raster file
#' @param ref_grid the world reference grid (spatialPolygonDataframe object)
#' @param output_dir path of the directory where the objects are saved
#' @details split_raster_on_grid_gdal_wrappers uses wrapper functions of the gdal library. The raster object is not loaded into R.
#' @return nothing
split_raster_on_grid_gdal_wrappers <- function(path_to_raster, ref_grid, output_dir){
  
  ## get raster info (proj4, extent, resolution)
  raster_info <- gdalUtils::gdalinfo(path_to_raster, raw_output = F, proj4 = T) 
  
  ## select a subset of the raster file based on the extent of each tile
  out <- lapply(ref_grid@data$tile_id, function(tile_id){
    cat(paste0("processing tile ", tile_id,"\n"))
    
    ## get tile
    tile <- ref_grid[ref_grid$tile_id==tile_id,]
    ## make sure projection is the same
    tile <- spTransform(tile, raster_info$proj4)
    bbox_tile <- bbox(tile)
    
    raster_tile <- gdal_translate(
      src_dataset = path_to_raster,
      dst_dataset = file.path(tmpDir(), paste0("tile_", tile_id, ".tif")),
      projwin = c(bbox_tile[1,1],bbox_tile[2,2],bbox_tile[1,2],bbox_tile[2,1]), # c(xmin, ymax, xmax, ymin)
      output_Raster = T # not outputing the raster here would create a larger file than if generated with writeRaster (*)
    )
    writeRaster(x = raster_tile, filename = file.path(output_dir, paste0("tile_", tile_id, ".tif")), overwrite = T) # (*)
  })
}

### Mosaic the split tiles

mosaic_tiles <- function(path_to_tiles,tile_pattern,output_dir,output_file){
  ## get the tiles (proj4, extent, resolution)
  tiles.list <- list.files(path=path_to_tiles,pattern=tile_pattern,full.names=T)
  tiles <- lapply(c(1:length(tiles.list)), #1:6,8:289
                  function(x){
                    raster(tiles.list[x])})

  ## get the original raster extent
  #original <- raster(path_to_raster) 
  
  ## make a template for the mosaic
  #e <- extent(original)
  #template <- raster(e)
  #proj4string(template) <- crs(original)
  
  #writeRaster(template, file=output_file, format="GTiff")
  
  ## mosaic the tiles
  tiles$fun <- median
  tiles$overwrite <- TRUE
  out <- do.call(mosaic,tiles)
  writeRaster(out, file=output_file, format="GTiff")
  #out <- mosaic(tiles,paste0(output_file,".tif", of="GTiff"))
  }

#' aggregate the (already computed and saved) subindicators into one single file
#' 
#' @param subindicator_dir_path the path to the directory that contains the computed indicators
#' @param output_file output file location (optional)
#' @return a 3 column datatable with Iso3_Code, country, value, subIndicator
#' 
aggregate_subindicators <- function(path_subindicator_dir, output_dir, name){
  output_file <- paste0(output_dir,"/",name)
  f <- list.files(path = path_subindicator_dir, pattern = ".xlsx")
  data <- rbindlist(lapply(f, function(x){
    read_xlsx(path = file.path(path_subindicator_dir, x), sheet = "subIndicator")
  }), use.names = T,fill=TRUE)
  data <- data[which(!is.na(data$value)),]
  data <- data[which(data$value !="NA"),]
  if(is.na(output_file)){
    return(data)
  }else{
    write.xlsx(data=data,output_file)
  }
}

