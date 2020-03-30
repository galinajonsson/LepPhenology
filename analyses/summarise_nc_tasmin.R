summarise_nc_tasmin <-  function(input_dir, verbose = TRUE) {
  
  library(reshape2)
  library(dplyr)
  library(ncdf4)
  library(BRCmap)
  
  # get files from the input directory
  files <- list.files(path = paste(input_dir), pattern = "\\.nc$", full.names = TRUE, 
                      recursive = FALSE)
  
  # sense check these file names
  if(length(files) == 0) stop('No .nc files found in ', input_dir)
  if(length(files) < length(list.files(path = input_dir))) warning('Not all files in ', input_dir, ' are .nc files, other file types have been ignored')
  
  
  # create a function to read in the data we want from these .rds files
  
  read_ncdf <- function(fileName) {
    
    nc_data <- nc_open(fileName)
    
    
    # get time
    time <- ncvar_get(nc_data,"time")
    
    tunits <- ncatt_get(nc_data,"time","units")
    nt <- dim(time)
    
    dname <- "tasmin"
    tmp_array <- ncvar_get(nc_data,dname)
    dlname <- ncatt_get(nc_data,dname,"long_name")
    dunits <- ncatt_get(nc_data,dname,"units")
    fillvalue <- ncatt_get(nc_data,dname,"_FillValue")
    
    
    ##########################################
    
    latname <- "latitude"
    lat_array <- ncvar_get(nc_data,latname)
    latname <- ncatt_get(nc_data,latname,"standard_name")
    
    longname <- "longitude"
    long_array <- ncvar_get(nc_data,longname)
    longname <- ncatt_get(nc_data,longname,"standard_name")
    
    long_list <- as.list(long_array)
    lat_list <- as.list(lat_array)
    
    ##########################################
    
    # get global attributes
    title <- ncatt_get(nc_data,0,"title")
    institution <- ncatt_get(nc_data,0,"institution")
    datasource <- ncatt_get(nc_data,0,"source")
    references <- ncatt_get(nc_data,0,"references")
    history <- ncatt_get(nc_data,0,"history")
    Conventions <- ncatt_get(nc_data,0,"Conventions")
    
    
    
    # replace netCDF fill values with NA's
    tmp_array[tmp_array==fillvalue$value] <- NA
    
    length(na.omit(as.vector(tmp_array[,,1])))
    
    
    # create dataframe -- reshape data
    # matrix (nlon*nlat rows by 2 cols) of lons and lats
    lonlat <- as.matrix(cbind(long_list, lat_list))
    
    # get a single slice or layer (January)
    m <- 1
    tmp_slice <- tmp_array[,,m]
    
    tmp_slice
    
    tmp_vec <- as.vector(tmp_slice)
    length(tmp_vec)
    
    
    # create dataframe and add names
    tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
    names(tmp_df01) <- c("xCoord","yCoord",paste(dname,as.character(m), sep="_"))
    head(na.omit(tmp_df01), 10)
    
    
    #### Fing grid refrences
    require(BRCmap)
    tmp_df01$km10grid <- gps_latlon2gr(as.numeric(tmp_df01$yCoord), as.numeric(tmp_df01$xCoord), out_projection = "OSGB", return_type = "gr")
    tmp_df01$km10grid <- reformat_gr(tmp_df01$km10grid, prec_out = 10000, precision = NULL, pad_gr = FALSE)
    
    # Unlist yCoord, xCoord and tasmin
    tmp_df01$yCoord <- unlist(tmp_df01$yCoord)
    tmp_df01$xCoord <- unlist(tmp_df01$xCoord)
    tmp_df01$tasmin_1 <- unlist(tmp_df01$tasmin_1)
    
    # Remove empty data
    tmp_df01 <- tmp_df01[complete.cases(tmp_df01), ]
    
    tasmin1900 <- tmp_df01[,3:4] # 1708
    
    
    for(m in 2:12){
      # get a single slice or layer
      tmp_slice <- tmp_array[,,m]
      
      tmp_vec <- as.vector(tmp_slice)
      
      # create dataframe and add names
      tmp_df <- data.frame(cbind(lonlat,tmp_vec))
      names(tmp_df) <- c("xCoord","yCoord",
                         paste(dname,as.character(m), sep="_"))
      
      
      #### Fing grid refrences
      require(BRCmap)
      tmp_df$km10grid <- gps_latlon2gr(as.numeric(tmp_df$yCoord), as.numeric(tmp_df$xCoord), out_projection = "OSGB", return_type = "gr")
      tmp_df$km10grid <- reformat_gr(tmp_df$km10grid, prec_out = 10000, precision = NULL, pad_gr = FALSE)
      
      # Unlist yCoord, xCoord and tasmin
      tmp_df$yCoord <- unlist(tmp_df$yCoord)
      tmp_df$xCoord <- unlist(tmp_df$xCoord)
      tmp_df[,3] <- unlist(tmp_df[,3])
      
      # Remove empty data
      tmp_df <- tmp_df[,3:4]
      
      tmp_df <- tmp_df[complete.cases(tmp_df), ]
      
      tasmin1900 <- merge(tasmin1900, tmp_df, by ="km10grid")
      
    }
    
    
    tasmin1900$year <- substr(fileName, 91, 94)
    
    return(tasmin1900)
  }
  
  if(verbose) cat('Loading data...')
  
  # Use lapply to run this function on all files
  result <- lapply(files, function(x) { read_ncdf(x) })
  # rbind all together 
  result_df <- do.call("rbind", result)
  
  if(verbose) cat('done\n')
  
  return(result_df)
}