
load_et_by_filepath<-function(x,process){
  #https://gis.stackexchange.com/questions/386628/working-with-irregularly-spaced-gridded-netcdf-data-in-r
  
  #vector memory limit gets reached with the expand grid approach for the ET data but not the climate data.
  #can use the expand grid approach to get the climate data on a regular grid because it is simpler
  # and works betetr than the dataframe-based approach with the T data.
  
  #open netcdf file
  nc_data <- nc_open('./../../../Data/climate/aridity.nc')
  #nc_data <- nc_open('./../../../Data/ET/9km_smap_purdy_2015_2017/9km_monthly/PTJPL_SMAP_ET_201504_mean.nc')
  
  raster_from_nc_expand_grid <- function(file,variable){
  
  #load file
  nc_data <- nc_open(file)
    
  # Longitude 
  lon <- ncvar_get(nc_data ,"lon")
  dim(lon)
  # Latitude 
  lat <- ncvar_get(nc_data ,"lat")
  dim(lat)
  # Variable
  var <- ncvar_get(nc_data,variable)
  
  #convert to raster
  latlong = expand.grid(long=lon, lat=lat)
  latlong = data.frame(cbind(latlong, aridity = c(var)))
  latlong = na.exclude(latlong)
  colnames(latlong) <- c('x','y',variable)
  
  latlong <- fix_grid(latlong)
  
  return(latlong)
  
  }
  
  #re-gridded aridity 
  mean_aridity <- raster_from_nc_expand_grid('./../../../Data/climate/aridity.nc',
                                     'aridity20yrs')
  #save
  writeRaster(mean_aridity,'./../../../Data/Derived_data/Climate/mean_aridity.tif')
  
  
  plot(test)
  
  
  nc_data <- nc_open('./../../../Data/climate/aridity.nc')
  
  # Longitude 
  lon <- ncvar_get(nc_data ,"lon")
  
  # Latitude 
  lat <- ncvar_get(nc_data ,"lat")
  
  # Variable
  var <- ncvar_get(nc_data,'aridity20yrs')
  
  #convert to raster
  latlong = expand.grid(long=lon, lat=lat)
  latlong = data.frame(cbind(latlong, aridity = c(var)))
  
  dim(latlong)
  
  dim(latlong)
  
  plot(latlong)
  
  
  
  
  fix_grid_2<-function(x){
    
    e<-extent(x[c(1:2)])
    r<-raster(e,ncol=1000,nrow=1000,crs='+proj=longlat +datum=WGS84')
    r_new<-rasterize(x[,1:2],r,x[3],fun=mean)
    return(r_new)
    
  }
  
  
  
  
  
  # old ----
  rasterFromXYZ(latlong)
  
  latlong = expand.grid(long=lon, lat=lat)
  latlong = data.frame(cbind(latlong, aridity = c(ET)))
  colnames(latlong) <- c('x','y','z')
  latlong<-na.omit(latlong)
  latlong <- fix_grid(latlong)
  r_new<-rasterize(latlong[,1:2],latlong[3],fun=mean)
  plot(latlong$long,latlong$lat)
  extent(latlong[c(1:2)])
  aridity_raster <- fix_grid(latlong)
  aridity_raster <-resample(aridity_raster,biomass)
  plot(aridity_raster)
  
  coordinates(latlong)=~long+lat
  crs(latlong) <- '+proj=longlat +datum=WGS84'
  plot(raster(latlong))
  spplot(latlong,"PM25")
  
  # get longitude and latitude
  
  # Longitude 
  lon <- ncvar_get(nc_data ,"lon")
  nlon <- dim(lon)
  lon.df<-as.data.frame(lon)
  #head(lon.df)
  
  lon.df$ID <- rownames(lon.df)
  #summary(lon.df)
  
  lon.df.melted <- reshape2::melt(lon.df, 
                                  id.vars = c("ID"),
                                  variable.name = "lon")
  
  #head(lon.df.melted)
  
  #make unique column ID
  lon.df.melted$RegionSite <- paste0(lon.df.melted$ID,lon.df.melted$lon)
  
  #check no duplicates
  # dim(lon.df.melted)
  # length(unique(lon.df.melted$RegionSite))
  
  lon.df.melted <- lon.df.melted[-c(1,2)]
  colnames(lon.df.melted) <- c('x','ID')
  
  #head(lon.df.melted)
  
  # Latitude #
  lat <- ncvar_get(nc_data ,"lat")
  nlat <- dim(lat)
  
  # Covert to data frame
  lat.df<-as.data.frame(lat)
  head(lat.df)
  #head(lat.df)
  
  lat.df$ID <- rownames(lat.df)
  #summary(lat.df)
  
  lat.df.melted <- reshape2::melt(lat.df, 
                                  id.vars = c("ID"),
                                  variable.name = "lat")
  
  
  #head(lat.df.melted)
  
  #make unique column ID
  lat.df.melted$RegionSite <- paste0(lat.df.melted$ID,lat.df.melted$lat)
  
  lat.df.melted <- lat.df.melted[-c(1,2)]
  colnames(lat.df.melted) <- c('y','ID')
  
  
  lat.df.melted$ID <- gsub('lat','',lat.df.melted$ID)
  lon.df.melted$ID <- gsub('lon','',lon.df.melted$ID)
  
  head(lat.df.melted)
  head(lon.df.melted)
  
  #merge lat and long
  lat_lon <- merge(lat.df.melted,lon.df.melted,by=c('ID'))
  #head(lat_lon)
  
  #re-order columns
  lat_lon_order <- c("x", "y", "ID")
  lat_lon <- lat_lon[, lat_lon_order]
  #head(lat_lon)
  
  lat_lon$ID <- 1
  lat_long_raster <- fix_grid(lat_lon)
  plot(lat_long_raster)
  
  # Get ET #
  ET <- ncvar_get(nc_data,'SMAPcanopy_transpiration')
  # fix.aridity<- fix_grid(data.frame(rasterToPoints(ET)))
  # plot(fix.aridity)
  # biomass<-raster('./../../../Data/Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')
  # fix.aridity <- resample(fix.aridity,biomass)
  
  #head(ET)
  #summary(ET)
  #str(ET)
  
  ET.df<-as.data.frame(ET)
  #head(ET.df)
  
  ET.df$ID <- rownames(ET.df)
  #summary(ET.df)
  
  ET.df.melted <- reshape2::melt(ET.df, 
                                 id.vars = c("ID"),
                                 variable.name = "ET")
  #head(ET.df.melted)
  hist(ET.df.melted$value)
  
  #make unique column ID
  ET.df.melted$RegionSite <- paste0(ET.df.melted$ID,ET.df.melted$ET)
  
  ET.df.melted <- ET.df.melted[-c(1,2)]
  colnames(ET.df.melted) <- c('ET','ID')
  ET.df.melted$ID <- gsub('V1','',ET.df.melted$ID)
  #head(ET.df.melted)
  
  # Merge with lat/lon data frame
  
  lat_lon_et <- merge (lat_lon,ET.df.melted,by=c('ID'))
  summary(lat_lon_et)
  
  # Turn into raster
  lat_lon_et_na_rm <- na.omit(lat_lon_et)
  lat_lon_et_na_rm <- data.frame(lat_lon_et_na_rm[c(2,3,4)])
  lat_lon_et_raster <-fix_grid(lat_lon_et_na_rm)
  
  plot(lat_lon_et$x,lat_lon_et$y)
  # cleanup
  rm(lat,lon,lat.df,lon.df,lat.df.melted,lon.df.melted,
     lat_lon_et,lat_lon,ET.df,ET.df.melted,ET,nc_data)
  
  
  return(lat_lon_et_na_rm)
  
}

