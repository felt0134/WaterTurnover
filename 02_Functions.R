# in-house functions to do various tasks


# Load on 36/9km data sets individually ------

load_et_by_filepath<-function(x,process){
  
  #open netcdf file
  nc_data <- nc_open(x)
  
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
  head(lat.df.melted)
  head(lon.df.melted)
  
  #merge lat and long
  lat_lon <- merge(lat.df.melted,lon.df.melted,by=c('ID'))
  #head(lat_lon)
  
  #re-order columns
  lat_lon_order <- c("x", "y", "ID")
  lat_lon <- lat_lon[, lat_lon_order]
  #head(lat_lon)
  
  # Get ET #
  ET <- ncvar_get(nc_data,process)
  #head(ET)
  #summary(ET)
  
  ET.df<-as.data.frame(ET)
  #head(ET.df)
  
  ET.df$ID <- rownames(ET.df)
  #summary(lat.df)
  
  ET.df.melted <- reshape2::melt(ET.df, 
                                 id.vars = c("ID"),
                                 variable.name = "ET")
  #head(ET.df.melted)
  
  #make unique column ID
  ET.df.melted$RegionSite <- paste0(ET.df.melted$ID,ET.df.melted$ET)
  
  ET.df.melted <- ET.df.melted[-c(1,2)]
  colnames(ET.df.melted) <- c('ET','ID')
  #head(ET.df.melted)
  
  # Merge with lat/lon data frame
  
  lat_lon_et <- merge (lat_lon,ET.df.melted,by=c('ID'))
  head(lat_lon_et)
  
  # Turn into raster
  lat_lon_et_na_rm <- na.omit(lat_lon_et)
  lat_lon_et_na_rm <- data.frame(lat_lon_et_na_rm[c(2,3,4)])
  #lat_lon_et_raster <-raster::rasterFromXYZ(lat_lon_et_na_rm,digits=.01)
  
  
  # cleanup
  rm(lat,lon,lat.df,lon.df,lat.df.melted,lon.df.melted,
     lat_lon_et,lat_lon,ET.df,ET.df.melted,ET,nc_data)
  
  
  return(lat_lon_et_na_rm)
  
}


#-----------------------------------------------
# calculate data range (maximum minus minimum) ------

min_max <- function(x){
  
  minmax <- max(x) - min(x)
  
  return(minmax)
}

#-----------------------------------------------
# getting biomass for a region-------

get_region_biomass<-function(region){
  
  region.2 <- sp::spTransform(region, CRS('+proj=longlat +datum=WGS84 +no_defs'))
  region.2_abg<-crop(aboveground_biomass,extent(region.2))
  region_abg.2<-mask(region.2_abg,region.2)
  
  return(region_abg.2)
  
}


#-----------------------------------------------
# EASE to regular grid  ------

#Get the EASE grid to a regular grid. outputs a raster
#do this after converting to a dataframe 

fix_grid<-function(x){
  
  e<-extent(x[c(1:2)]) #x is an x,y,z, dataframe (lon,lat,value) of irregular EASE grid
  r<-raster(xmn=e[1],xmx=e[2],ymn=e[3],ymx=e[4],crs='+proj=longlat +datum=WGS84') #create toy raster to later populate
  r_new<-rasterize(x[,1:2],r,x[3],fun=mean)
  return(r_new)
  
}

# test.2<-fix_grid(test)
# plot(test.2)

#-----------------------------------------------
# Resample according to region of interest (X2 CHECK IF USED)--------------------------


region_resample<-function(region_shapefile,region_raster,et_raster){
  
  transp.region<-crop(et_raster,extent(region_shapefile)) 
  transp.region<-mask(transp.region,region_shapefile)
  #plot(transp.mexico)
  resample_test<-resample(transp.region,region_raster)
  resample_test<-mask(resample_test,region_shapefile)
  #plot(resample_test) #works
  
  #turn into dataframe
  resample_test<-(rasterToPoints(resample_test))
  resample_test<-data.frame(resample_test)
  return(resample_test)
  
}

# test<-region_resample(region_shapefile=Mexico,region_raster=Mexico_raster,et_raster=r_new)
# head(test)

#mexico wood density = 0.68

#------------------------------------------------
# Resample according to biome of interest--------------------------


biome_resample<-function(region_shapefile,region_raster,et_raster){
  
  transp.region<-crop(et_raster,extent(region_shapefile)) 
  transp.region<-mask(transp.region,region_shapefile)
  resample_test<-resample(transp.region,region_raster)
  
  #turn into dataframe
  resample_test<-(rasterToPoints(resample_test))
  resample_test<-data.frame(resample_test)
  return(resample_test)
  
}

# test<-region_resample(region_shapefile=Mexico,region_raster=Mexico_raster,et_raster=r_new)
# head(test)

#mexico wood density = 0.68

#------------------------------------------------
# setup to delineate year and month (using 9km ET data)-----------

get_year_month_column_smap_et_9km<-function(data){
  
  data$id <- rownames(data)
  rownames(data) <-NULL
  
  data$id<-gsub('ET/9km_smap_purdy_2015_2017/9km_monthly//PTJPL_SMAP_ET_',
                '', data$id)
  #head(transp_monthly_9km_2015_2017_smap_enhanced)
  
  data$id<-gsub('./../../../Data/','', data$id)
  
  # get year
  data$year <- substr(data$id, 1, 4)
  # get month
  data$month <- substr(data$id, 5, 6)
  
  return(data)
  
}
#------------------------------------------------
# change LE/m^2 to cumulative mm per month (30.5 days)----
le_to_cumulative_monthly_mm<-function(x){
  
  LE_Wm2 = x
  
  lambda_e = 2.460*10**6      # J kg^-1
  roe_w = 1000                # kg m^-3
  m_2_mm = 1000               # convert m to mm
  s_2_mon = 60*30*48*30.5 
  
  ET_mm = (LE_Wm2*(m_2_mm*s_2_mon)/(lambda_e*roe_w))
  
  return(ET_mm)
  
}
#------------------------------------------------
# Get land cover turnover ------
get_land_cover_turnover_from_water_content<-function(region,x,veg){
  
  turnover.list<-list() 
  ncpath_9_km_2015_2017_smap_enhanced <- "./../../../Data/ET/9km_smap_purdy_2015_2017/9km_monthly/"
  et_9km_monthly_2002_2017_length <- dir(ncpath_9_km_2015_2017_smap_enhanced, full.names = T)
  
  for(i in et_9km_monthly_2002_2017_length[x]){
    
    # Get ET data
    et.data.9km<-load_et_by_filepath(x= i,process = "SMAPcanopy_transpiration")
    
    # Try to normalize the EASE grid
    et.data.9km<-fix_grid(et.data.9km)
    #plot(et.data.9km)
    
    # #wood density database
    # data('wdData')
    # density_region_mean<-aggregate(wd ~ region,mean,data=wdData)
    # 
    # #shapefile data source
    # data(wrld_simpl)
    
    if(region =='Grasslands'){
      
      #get water content data
      
      #just poa
      #wc<-read.csv('./../../../Data/Derived_data/Land_Cover_Water_Content/grassland_water_content.csv')
      
      #all herb
      # wc<-read.csv('./../../../Data/Derived_data/Land_Cover_Water_Content/grassland_water_content_all_herb_families.csv')
      
      # #mostly herb
      # wc<-read.csv('./../../../Data/Derived_data/Land_Cover_Water_Content/grassland_water_content_mostly_herb_families.csv')
      
      # poa an dherb X2 checked
      wc<-read.csv('./../../../Data/Derived_data/Land_Cover_Water_Content/grassland_water_content_poa_herbX2.csv')
      
      wc<-mean(wc$average.water.content)
      wc<-round(wc,2)
      #summary(wc$average.water.content)
      #min = 0.59 mean=2 max = 4.3
      
      #upload region shapefile and raster
      
      shapefile<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Grassland.tif')
      shapefile<-rasterToPolygons(shapefile)
      
      #biomass raster
      region.raster<-raster('./../../../Data/Derived_data/Biomass/Land_Cover/Grasslands.tif')
      
      #Canopy Transpiration
      ET<-biome_resample(shapefile,region.raster,et.data.9km) #bottleneck
      #head(ET)
      #plot(rasterFromXYZ(Mexico_ET))
      
      #now get Biomass
      region_biomass<-rasterToPoints(region.raster)
      region_biomass<-data.frame(region_biomass)
      #head(mexico_biomass)
      
      #re-scale
      region_biomass$Grasslands <- 0.1*region_biomass$Grasslands
      region_biomass$AGB<-region_biomass$Grasslands
      
    }else if(region=='Forest'){
      
      #wood density database
      data('wdData')
      wd<-mean(wdData$wd)
      wd <- round(wd,2)
      
      #upload region shapefile and raster
      
      shapefile<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Forest.tif')
      shapefile<-rasterToPolygons(shapefile)
      
      #biomass raster
      region.raster<-raster('./../../../Data/Derived_data/Biomass/Land_Cover/Forest.tif')
      
      #Canopy Transpiration
      ET<-biome_resample(shapefile,region.raster,et.data.9km) #bottleneck
      #head(ET)
      #plot(rasterFromXYZ(Mexico_ET))
      
      #now get Biomass
      region_biomass<-rasterToPoints(region.raster)
      region_biomass<-data.frame(region_biomass)
      #head(region_biomass)
      
      #re-scale
      region_biomass$Forest <- 0.1*region_biomass$Forest
      region_biomass$AGB<-region_biomass$Forest
      
      
    }else if(region=='Tundra'){
      
      #wood density database
      data('wdData')
      wd<-mean(wdData$wd)
      wd <- round(wd,2)
      
      #get water content data
      wc<-read.csv('./../../../Data/Derived_data/Land_Cover_Water_Content/grassland_water_content.csv')
      wc<-mean(wc$average.water.content)
      wc<-round(wc,2)
      
      #upload region shapefile and raster
      shapefile<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Tundra.tif')
      shapefile<-rasterToPolygons(shapefile)
      
      #biomass raster
      region.raster<-raster('./../../../Data/Derived_data/Biomass/Land_Cover/Tundra.tif')
      
      #Canopy Transpiration
      ET<-biome_resample(shapefile,region.raster,et.data.9km) #bottleneck
      #head(ET)
      #plot(rasterFromXYZ(Mexico_ET))
      
      #now get Biomass
      region_biomass<-rasterToPoints(region.raster)
      region_biomass<-data.frame(region_biomass)
      #head(region_biomass)
      
      #re-scale
      region_biomass$Tundra <- 0.1*region_biomass$Tundra
      region_biomass$AGB<-region_biomass$Tundra
      
      
    }else if(region=='Shrubland'){
      
      #wood density database
      data('wdData')
      wd<-mean(wdData$wd)
      wd <- round(wd,2)
      
      #upload region shapefile and raster
      
      shapefile<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Shrubland.tif')
      shapefile<-rasterToPolygons(shapefile)
      
      #biomass raster
      region.raster<-raster('./../../../Data/Derived_data/Biomass/Land_Cover/Shrubland.tif')
      
      #Canopy Transpiration
      ET<-biome_resample(shapefile,region.raster,et.data.9km) #bottleneck
      #head(ET)
      #plot(rasterFromXYZ(Mexico_ET))
      
      #now get Biomass
      region_biomass<-rasterToPoints(region.raster)
      region_biomass<-data.frame(region_biomass)
      #head(region_biomass)
      
      #re-scale
      region_biomass$Shrubland <- 0.1*region_biomass$Shrubland
      region_biomass$AGB<-region_biomass$Shrubland
      
    }else if(region=='Cropland'){
      
      # poa and herb X2 checked
      wc<-read.csv('./../../../Data/Derived_data/Land_Cover_Water_Content/grassland_water_content_poa_herbX2.csv')
      
      wc<-mean(wc$average.water.content)
      wc<-round(wc,2)
      #summary(wc$average.water.content)
      #min = 0.59 mean=2 max = 4.3
      
      #upload region shapefile and raster
      
      shapefile<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Cropland.tif')
      shapefile<-rasterToPolygons(shapefile)
      
      #biomass raster
      region.raster<-raster('./../../../Data/Derived_data/Biomass/Land_Cover/Cropland.tif') #need to make this!
      
      #Canopy Transpiration
      ET<-biome_resample(shapefile,region.raster,et.data.9km) #bottleneck
      #head(ET)
      #plot(rasterFromXYZ(Mexico_ET))
      
      #now get Biomass
      region_biomass<-rasterToPoints(region.raster)
      region_biomass<-data.frame(region_biomass)
      #head(mexico_biomass)
      
      #re-scale
      region_biomass$Cropland <- 0.1*region_biomass$Cropland
      region_biomass$AGB<-region_biomass$Cropland}else if(region=='xxx'){}
    
    #these data are in mega grams per hectare
    
    #1000000 grams = 1 megagram
    region_biomass$AGB<-(region_biomass$AGB)*1000000
    
    #1 hectare = 10000 square meters
    region_biomass$AGB<-(region_biomass$AGB)/10000
    region_biomass$AGB<-round(region_biomass$AGB,2)
    # now it is in g/m^2
    
    if(veg =='herb'){
      
      #calculate water content (wc in is in grams of water per grams of dry biomass)
      region_biomass$plant_water_grams_m2<- wc*region_biomass$AGB
      region_biomass$plant_water_grams_m2<-round(region_biomass$plant_water_grams_m2,2)
      
    }else if(veg=='woody'){
      
      #calculate wood water content
      region_biomass$plant_water_grams_m2<- ((1-wd)/wd)*region_biomass$AGB
      region_biomass$plant_water_grams_m2<-round(region_biomass$plant_water_grams_m2,2)
      
    }else if(veg=='mixed'){
      
      #calculate water content (wc in is in grams of water per grams of dry biomass)
      region_biomass$plant_water_grams_m2_herb<- wc*region_biomass$AGB
      region_biomass$plant_water_grams_m2_herb<-round(region_biomass$plant_water_grams_m2_herb,2)
      
      #calculate wood water content
      region_biomass$plant_water_grams_m2_woody<- ((1-wd)/wd)*region_biomass$AGB
      region_biomass$plant_water_grams_m2_woody<-round(region_biomass$plant_water_grams_m2_woody,2)
      
      #now average woody an dherb estimates
      region_biomass$plant_water_grams_m2 <- (region_biomass$plant_water_grams_m2_herb + region_biomass$plant_water_grams_m2_woody)/2
      
      #simplify
      region_biomass <- region_biomass[c(1,2,3,4,7)]
      #head(region_biomass)
      
    }else if(veg=='none'){xxx}
    
    #convert to mm in depth per m^2
    
    # The logic:
    #   
    # 1 gram = 1 ml = 1000 mm^3
    # That is a volume
    # 1 m^2=1000000 mm^2 = 1000mm*1000mm
    # Volume = Length*Width*Height
    # Therefore
    # 1000 mm3 volume = 1000 length*1000width*XXHeight
    # 1000/1000000 = .001 mm of water on a m2
    
    region_biomass$plant_water_mm <-region_biomass$plant_water_grams_m2*0.001
    region_biomass$plant_water_mm <- round(region_biomass$plant_water_mm,2)
    
    # remove excess files from memory
    rm(region.raster,shapefile,et.data.9km)
    
    #merge with ET data
    region_biomass_water_transpiration<-merge(region_biomass,ET,by=c('x','y'))
    
    rm(ET,region_biomass)
    
    #get rid of excess colums
    region_biomass_water_transpiration<-region_biomass_water_transpiration[c(1,2,6,7)]
    
    #rename columns
    colnames(region_biomass_water_transpiration) <-c('x','y','water_storage_mm_m2','canopy_transpiration_mm_m2')
    
    #convert latent energy to mm to provide CUMULATIVE monthly T
    region_biomass_water_transpiration<-aggregate(canopy_transpiration_mm_m2~x+y+water_storage_mm_m2,
                                                  le_to_cumulative_monthly_mm,data=region_biomass_water_transpiration)
    
    #filter our zero values
    region_biomass_water_transpiration <- region_biomass_water_transpiration %>%
      dplyr::filter(water_storage_mm_m2 > 0)
    
    #store in list
    turnover.list[[i]] <- region_biomass_water_transpiration
    
  }
  
  return(turnover.list)
  
}


#------------------------------------------------
# coefficient of variation ----

cv <- function(x){
  
  cv_x <- (sd(x)/mean(x))*100

   return(cv_x)
  
}

#------------------------------------------------
# get 2016 annual turnover data -----

#estimates the turnover time for 2016 by water storage by 
# dividing cumulative annual canopy transpiration 

get_2016_annual_turnover<-function(vegetation,wc){
  
  regions<-c(vegetation)
  
  for(i in 1:length(regions)){
    
    ecoregion <- regions[i]
    
    if(wc=='mean'){
      
      outfile <- paste0('./../../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/")
      
    }else if(wc=='low'){
      
      outfile <- paste0('./../../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/wc_3/") 
      
    }else if(wc=='high'){
      
      outfile <- paste0('./../../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/wc_4/") 
      
    }
    
    ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
    region.list <- list()
    
    for(j in ecoregion_dir[1:12]){
      
      test<-fread(j)
      region.list[[j]] <- test
      
    }}
  
  #make into data frame
  test<- do.call("rbind", region.list)
  
  # Look at data 
  
  rm(region.list)
  
  #remove row ID
  rownames(test) <-NULL
  
  #focus on year 2016
  test<- test %>%
    dplyr::filter(year == 2016)
  
  # Get annual turnover 
  
  #get cumulative annual T
  test.annual.cumulative.T <- aggregate(canopy_transpiration_mm_m2~x+y + water_storage_mm_m2,sum,data=test)
  
  #estimate T per day
  test.annual.cumulative.T$canopy_transpiration_mm_m2 <- (test.annual.cumulative.T$canopy_transpiration_mm_m2)/365
  test.annual.cumulative.T$canopy_transpiration_mm_m2 <- round(test.annual.cumulative.T$canopy_transpiration_mm_m2,2)
  
  # get rid of pixels where T is zero
  test.annual.cumulative.T <- test.annual.cumulative.T  %>%
    dplyr::filter(canopy_transpiration_mm_m2 > 0)
  
  #estimate annual turnover
  test.annual.cumulative.T$cumulative.annual.turnover <- 
    test.annual.cumulative.T$water_storage_mm_m2/test.annual.cumulative.T$canopy_transpiration_mm_m2
  
  
  # filter out extreme values
  high<-as.numeric(quantile(test.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.95)))
  low<-as.numeric(quantile(test.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.05)))
  
  test.filter.cumulative <- test.annual.cumulative.T %>%
    dplyr::filter(cumulative.annual.turnover < high) %>%
    dplyr::filter(cumulative.annual.turnover > low)
  
  return(test.filter.cumulative)
  
}

# test<-get_2016_annual_turnover(vegetation = 'grasslands')
# head(test)

#------------------------------------------------
# get 2016 intra-annual CV of turnover -----

get_2016_CV_turnover<-function(vegetation){
  
  regions<-c(vegetation)
  
  for(i in 1:length(regions)){
    
    ecoregion <- regions[i]
    #ecoregion <- 'Grasslands'
    outfile <- paste0('./../../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/")
    ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
    region.list <- list()
    
    for(j in ecoregion_dir[1:12]){
      
      test<-fread(j)
      region.list[[j]] <- test
      
    }}
  
  #make into data frame
  test<- do.call("rbind", region.list)
  
  # Look at data 
  
  rm(region.list)
  
  #remove row ID
  rownames(test) <-NULL
  
  #focus on year 2016
  test<- test %>%
    dplyr::filter(year == 2016)
  
  # Get annual turnover 
  
  #get cumulative annual T
  test.annual.cumulative.T <- aggregate(canopy_transpiration_mm_m2~x+y + water_storage_mm_m2,sum,data=test)
  
  #estimate T per day
  test.annual.cumulative.T$canopy_transpiration_mm_m2 <- (test.annual.cumulative.T$canopy_transpiration_mm_m2)/365
  test.annual.cumulative.T$canopy_transpiration_mm_m2 <- round(test.annual.cumulative.T$canopy_transpiration_mm_m2,2)
  
  # get rid of pixels where T is zero
  test.annual.cumulative.T <- test.annual.cumulative.T  %>%
    dplyr::filter(canopy_transpiration_mm_m2 > 0)
  
  #estimate annual turnover
  test.annual.cumulative.T$cumulative.annual.turnover <- 
    test.annual.cumulative.T$water_storage_mm_m2/test.annual.cumulative.T$canopy_transpiration_mm_m2
  
  
  # filter out extreme values
  high<-as.numeric(quantile(test.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.95)))
  low<-as.numeric(quantile(test.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.05)))
  
  test.filter.cumulative <- test.annual.cumulative.T %>%
    dplyr::filter(cumulative.annual.turnover < high) %>%
    dplyr::filter(cumulative.annual.turnover > low)
  
  #get CV
  
  sd_turnover <- merge(test,test.filter.cumulative[c(1,2,5)],by=c('x','y'))
  
  sd_turnover$monthly_turnover <- 
    (sd_turnover$water_storage_mm_m2/sd_turnover$canopy_transpiration_mm_m2)
  
  cv_turnover<-aggregate(monthly_turnover~x+y,cv,data=sd_turnover)
  
  # filter out extreme values
  high.cv<-as.numeric(quantile(cv_turnover$monthly_turnover,probs=c(0.95),na.rm=TRUE))
  low.cv<-as.numeric(quantile(cv_turnover$monthly_turnover,probs=c(0.05),na.rm=TRUE))
  
  cv_turnover <- cv_turnover %>%
    dplyr::filter(monthly_turnover < high.cv) %>%
    dplyr::filter(monthly_turnover > low.cv)
  
  return(cv_turnover)
  
}
#------------------------------------------------
# Get seasonal turnover (X2 CHECK IF USED) ----


get_seasonal_turnover <- function(test){
  
  # Get January - March 
  
  jan_march <- test %>%
    dplyr::filter(month < 4)
  
  #summary(jan_march)
  
  #get cumulative jan_march T
  test.jan_march.cumulative.T <- aggregate(canopy_transpiration_mm_m2~x+y + water_storage_mm_m2,sum,data=jan_march)
  #head(test.jan_march.cumulative.T)
  
  # get rid of pixels where T is zero
  test.jan_march.cumulative.T <- test.jan_march.cumulative.T  %>%
    dplyr::filter(canopy_transpiration_mm_m2 > 0)
  
  #estimate T per day
  test.jan_march.cumulative.T$canopy_transpiration_mm_m2 <- (test.jan_march.cumulative.T$canopy_transpiration_mm_m2)/90
  test.jan_march.cumulative.T$canopy_transpiration_mm_m2 <- round(test.jan_march.cumulative.T$canopy_transpiration_mm_m2,2)
  
  #estimate jan_march turnover
  test.jan_march.cumulative.T$turnover <- 
    (test.jan_march.cumulative.T$water_storage_mm_m2/test.jan_march.cumulative.T$canopy_transpiration_mm_m2)
  
  # filter out extreme values
  high_jan_march<-as.numeric(quantile(test.jan_march.cumulative.T$turnover,probs=c(0.95)))
  low_jan_march<-as.numeric(quantile(test.jan_march.cumulative.T$turnover,probs=c(0.05)))
  
  test.filter.jan.march <- test.jan_march.cumulative.T %>%
    dplyr::filter(turnover < high_jan_march) %>%
    dplyr::filter(turnover > low_jan_march)
  
  test.filter.jan.march$season <- 'January-March'
  
  # April-June
  
  # head(test)
  # summary(test)
  april_june <- test %>%
    dplyr::filter(month < 7) %>%
    dplyr::filter(month > 3)
  
  #summary(april_june)
  
  
  #get cumulative april_june T
  test.april_june.cumulative.T <- aggregate(canopy_transpiration_mm_m2~x+y + water_storage_mm_m2,sum,data=april_june)
  #head(test.april_june.cumulative.T)
  
  # get rid of pixels where T is zero
  test.april_june.cumulative.T <- test.april_june.cumulative.T  %>%
    dplyr::filter(canopy_transpiration_mm_m2 > 0)
  
  #estimate T per day
  test.april_june.cumulative.T$canopy_transpiration_mm_m2 <- (test.april_june.cumulative.T$canopy_transpiration_mm_m2)/90
  test.april_june.cumulative.T$canopy_transpiration_mm_m2 <- round(test.april_june.cumulative.T$canopy_transpiration_mm_m2,2)
  
  #estimate april_june turnover
  test.april_june.cumulative.T$turnover <- 
    (test.april_june.cumulative.T$water_storage_mm_m2/test.april_june.cumulative.T$canopy_transpiration_mm_m2)
  
  # filter out extreme values
  high_april_june<-as.numeric(quantile(test.april_june.cumulative.T$turnover,probs=c(0.95)))
  low_april_june<-as.numeric(quantile(test.april_june.cumulative.T$turnover,probs=c(0.05)))
  
  test.filter.april_june<- test.april_june.cumulative.T %>%
    dplyr::filter(turnover < high_april_june) %>%
    dplyr::filter(turnover > low_april_june)
  
  test.filter.april_june$season <- 'April-June'
  
  # july-September 
  
  # head(test)
  # summary(test)
  july_sep <- test %>%
    dplyr::filter(month < 10) %>%
    dplyr::filter(month > 6)
  
  #get cumulative july_sep T
  test.july_sep.cumulative.T <- aggregate(canopy_transpiration_mm_m2~x+y + water_storage_mm_m2,sum,data=july_sep)
  #head(test.july_sep.cumulative.T)
  
  # get rid of pixels where T is zero
  test.july_sep.cumulative.T <- test.july_sep.cumulative.T  %>%
    dplyr::filter(canopy_transpiration_mm_m2 > 0)
  
  test.july_sep.cumulative.T$canopy_transpiration_mm_m2 <- (test.july_sep.cumulative.T$canopy_transpiration_mm_m2)/90
  test.july_sep.cumulative.T$canopy_transpiration_mm_m2 <- round(test.july_sep.cumulative.T$canopy_transpiration_mm_m2,2)
  
  #estimate july_sep turnover
  test.july_sep.cumulative.T$turnover <- 
    (test.july_sep.cumulative.T$water_storage_mm_m2/test.july_sep.cumulative.T$canopy_transpiration_mm_m2)
  
  # filter out extreme values
  high<-as.numeric(quantile(test.july_sep.cumulative.T$turnover,probs=c(0.95)))
  low<-as.numeric(quantile(test.july_sep.cumulative.T$turnover,probs=c(0.05)))
  
  test.filter.july_sep <- test.july_sep.cumulative.T %>%
    dplyr::filter(turnover < high) %>%
    dplyr::filter(turnover > low)
  
  test.filter.july_sep$season <- 'July-September'
  
  # Oct- Dec 
  
  oct_dec <- test %>%
    dplyr::filter(month > 9) 
  
  #get cumulative oct_dec T
  test.oct_dec.cumulative.T <- aggregate(canopy_transpiration_mm_m2~x+y + water_storage_mm_m2,sum,data=oct_dec)
  
  # get rid of pixels where T is zero
  test.oct_dec.cumulative.T <- test.oct_dec.cumulative.T  %>%
    dplyr::filter(canopy_transpiration_mm_m2 > 0)
  
  #get daily T
  test.oct_dec.cumulative.T$canopy_transpiration_mm_m2 <- (test.oct_dec.cumulative.T$canopy_transpiration_mm_m2)/90
  test.oct_dec.cumulative.T$canopy_transpiration_mm_m2 <- round(test.oct_dec.cumulative.T$canopy_transpiration_mm_m2,2)
  
  
  #estimate oct_dec turnover
  test.oct_dec.cumulative.T$turnover <- 
    (test.oct_dec.cumulative.T$water_storage_mm_m2/test.oct_dec.cumulative.T$canopy_transpiration_mm_m2)
  
  # filter out extreme values
  high<-as.numeric(quantile(test.oct_dec.cumulative.T$turnover,probs=c(0.95)))
  low<-as.numeric(quantile(test.oct_dec.cumulative.T$turnover,probs=c(0.05)))
  
  test.filter.oct_dec <- test.oct_dec.cumulative.T %>%
    dplyr::filter(turnover < high) %>%
    dplyr::filter(turnover > low)
  
  test.filter.oct_dec$season <- 'October-December'
  
  rbind_seasons <- rbind(test.filter.oct_dec,test.filter.jan.march,test.filter.april_june,
                         test.filter.july_sep)
  
  
  return(rbind_seasons)
  
  
}



#------------------------------------------------
# import VOD (converted to VWC) data -----------

# VOD is linearly proportional to VWC: VOD = VWC*beta
# we assume beta here is 0.11, as derived from lookup tabl
# from SMAP algoritihim development product. beta varies from 0.10-0.11 but
# is surprisingly constant across land cover types. 
# This needs additional scrutiney. 
# https://smap.jpl.nasa.gov/documents/

get_vwc <-function(x,y,filepath){
  
  april_vector <-c(x:y)
  april_list<-list()
  
  lat_lon <- './../../../Data/VWC/SMAPCenterCoordinates9KM.mat'
  #h5read(lat_lon, read.attributes = TRUE)
  lat_lon<-readMat(lat_lon)
  
  #lat
  lat<-lat_lon[1]
  lat<-as.data.frame(lat)
  
  lat$ID <- rownames(lat)
  lat <- reshape2::melt(lat, id.vars = c("ID"),variable.name = "lat")
  lat <- lat[c(1,3)]
  colnames(lat) <- c('ID','y')
  lat$ID <- rownames(lat)
  #lat$ID <- as.numeric(as.character(lat$ID))
  
  #lon
  lon<-lat_lon[2]
  lon<-as.data.frame(lon)
  
  lon$ID <- rownames(lon)
  lon <- reshape2::melt(lon, id.vars = c("ID"),variable.name = "lon")
  lon <- lon[c(1,3)]
  colnames(lon) <- c('ID','x')
  lon$ID <- rownames(lon)
  #lon$ID <- as.numeric(as.character(lon$ID))
  
  lat_lon_df <- merge(lon,lat,by=c('ID'))
  rm(lat,lon,lat_lon)
  
  
  for(i in april_vector){
    
    # read in tau file 
    tst.2 <-h5read(filepath,"MTDCA_TAU")
    # dim(tst.2) # 1624 3856   91 (days)
    # class(tst.2) # array
    tst.2 <- tst.2[,,c(i)] # subset to day 1
    
    
    #head(tst.2)
    
    tst.2<-as.data.frame(tst.2)
    tst.2 <- reshape2::melt(tst.2,variable.name = "tau")
    # head(tst.2)
    # summary(tst.2)
    # hist(tst.2$value)
    
    tst.2$ID <- rownames(tst.2)
    #head(tst.2)
    tst.2$vwc <- tst.2$value/0.11
    tst.2 <- tst.2[c(3,4)]
    #head(tst.2)
    
    #merge with coordinates
    tst.2 <- merge(lat_lon_df,tst.2,by=c('ID'))
    tst.2 <- tst.2[c(2,3,4)]
    
    #plot(lat_lon_df$x,lat_lon_df$y)
    
    tst.2 <- tst.2 %>%
      dplyr::filter(!vwc=='NaN')
    #head(vwc.coordinates)
    #head(vwc.coord.jan1)
    
    april_list[[i]]<-tst.2
    
  }
  
  #make to df
  april_df <- do.call('rbind',april_list)
  rm(april_list)
  #head(april_df)
  
  #average across pixels
  april_ag<-aggregate(vwc~x+y,mean,data=april_df)
  rm(april_df)
  #head(april_ag)
  
  return(april_ag)
  
  # *kg/mm^2 and mm^m2 (height of water over m^2) workout to be the same, so that conversion is not done.
  
}

#------------------------------------------------
# Filter out extreme values for turnover for script (08) based on tails (CHECK) -----


filter_extremes_turnover<-function(df){
  
  # filter out extreme values
  high<-as.numeric(quantile(df$turnover,probs=c(0.99)))
  low<-as.numeric(quantile(df$turnover,probs=c(0.01)))
  
  df <- df %>%
    dplyr::filter(turnover < high) %>%
    dplyr::filter(turnover > low)
  
  return(df)
  
}
#------------------------------------------------
# Get seasonal turnover estimates for each land cover for VWC-based turnover----

# unique(test.vwc$month)
# test.vwc.winte.2r<- test.transp %>%
#   dplyr::filter(month == c('12','1','2'))
# unique(test.vwc.winte.2r$month)

get_seasonal_turnover_VWC <- function(season,land_cover){
  
  
  
  if(season=='december_february'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('12','1','2'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('12','1','2')) 
    
    
  }else if(season=='march_may'){
    
    test.vwc <- test.vwc %>%
      dplyr::filter(month < 5) %>%
      dplyr::filter(month > 2)
    
    
    test.transp <- test.transp %>%
      dplyr::filter(month < 5) %>%
      dplyr::filter(month > 2)
    
  }else if(season=='june_august'){
    
    
    test.vwc <- test.vwc %>%
      dplyr::filter(month < 9) %>%
      dplyr::filter(month > 5)
    
    test.transp <- test.transp %>%
      dplyr::filter(month < 9) %>%
      dplyr::filter(month > 5)
    
    
  }else if(season=='september_november'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month > 8) %>% 
      dplyr::filter(month < 12)
    
    test.transp <- test.transp %>%
      dplyr::filter(month > 8) %>% 
      dplyr::filter(month < 12)
    
    
  }else if(season=='xxx'){}
  
  
  # Average across months 
  test.vwc <-aggregate(vwc~x+y,mean,data=test.vwc)
  
  #sum across months
  test.transp<-aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.transp)
  test.transp$canopy_transpiration_mm_m2 <- test.transp$canopy_transpiration_mm_m2/90
  
  # re-grid
  test.vwc <- fix_grid(test.vwc)
  
  #load reference raster
  transit.all.raster<-raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_global_unfiltered.tif')
  
  #resample to reference raster
  test.vwc <- resample(test.vwc,transit.all.raster)
  
  
  if(land_cover=='grassland'){
    
    test.grassland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.grassland)
    
    # get rid of pixels where T is zero
    test.grassland.cumulative.transp <- test.grassland.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.grassland.cumulative.transp$canopy_transpiration_mm_m2 <- test.grassland.cumulative.transp$canopy_transpiration_mm_m2/365
    
    land_cover_raster<-rasterFromXYZ(test.grassland.cumulative.transp)
    rm(test.grassland.cumulative.transp)
    
    
  }else if(land_cover=='forest'){
    
    
    test.forest.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.forest)
    
    # get rid of pixels where T is zero
    test.forest.cumulative.transp <- test.forest.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.forest.cumulative.transp$canopy_transpiration_mm_m2 <- test.forest.cumulative.transp$canopy_transpiration_mm_m2/365
    
    land_cover_raster<-rasterFromXYZ(test.forest.cumulative.transp)
    rm(test.forest.cumulative.transp)
    
    
  }else if(land_cover=='shrubland'){
    
    
    test.shrubland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.shrubland)
    
    # get rid of pixels where T is zero
    test.shrubland.cumulative.transp <- test.shrubland.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.shrubland.cumulative.transp$canopy_transpiration_mm_m2 <- test.shrubland.cumulative.transp$canopy_transpiration_mm_m2/365
    
    land_cover_raster<-rasterFromXYZ(test.shrubland.cumulative.transp)
    rm(test.shrubland.cumulative.transp)
    
  }else if(land_cover=='cropland'){
    
    
    
    test.cropland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.cropland)
    
    # get rid of pixels where T is zero
    test.cropland.cumulative.transp <- test.cropland.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.cropland.cumulative.transp$canopy_transpiration_mm_m2 <- test.cropland.cumulative.transp$canopy_transpiration_mm_m2/365
    
    #summary(test.cropland.cumulative.transp)
    
    land_cover_raster<-rasterFromXYZ(test.cropland.cumulative.transp)
    rm(test.cropland.cumulative.transp)
    
    
  }else if(land_cover=='tundra'){
    
    
    test.tundra.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.tundra)
    
    # get rid of pixels where T is zero
    test.tundra.cumulative.transp <- test.tundra.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.tundra.cumulative.transp$canopy_transpiration_mm_m2 <- test.tundra.cumulative.transp$canopy_transpiration_mm_m2/365
    
    #summary(test.tundra.cumulative.transp)
    
    land_cover_raster<-rasterFromXYZ(test.tundra.cumulative.transp)
    rm(test.tundra.cumulative.transp)
    
    
  }else if(land_cover=='xxx'){}
  
  
  test.transp <- rasterFromXYZ(test.transp)
  test.transp<- resample(test.transp,transit.all.raster)
  
  land_cover_raster <-resample(land_cover_raster,transit.all.raster)
  test.vwc <-mask(test.vwc,land_cover_raster)
  
  test.vwc<-
    merge(rasterToPoints(test.transp),rasterToPoints(test.vwc),
          by=c('x','y'))
  
  test.vwc$turnover <- 
    test.vwc$layer/test.vwc$canopy_transpiration_mm_m2
  
  colnames(test.vwc) <- c('x','y','canopy_transpiration_mm_m2','storage_mm',
                          'turnover_days','season')
  # bound the data by the 1st and 99th percentiles
  #test.vwc <- filter_extremes_turnover(test.vwc)
  
  return(test.vwc)
  
  
  
}
#------------------------------------------------
# Filter out extreme values for turnover for script (08) based on IQR outliers (X2 CHECK IF USED) -----


filter_extremes_turnover_IQR<-function(df){
  
  # filter out extreme values
  high<-quantile(df$turnover)[4] + (1.5*IQR(df$turnover))
  low<-quantile(df$turnover)[2] - (1.5*IQR(df$turnover))
  
  df <- df %>%
    dplyr::filter(turnover < high) %>%
    dplyr::filter(turnover > low)
  
  return(df)
  
}

#------------------------------------------------
# get monthly VWC-based estimates of transit time ------

get_monthly_turnover_VWC <- function(month,land_cover){
  
  
  
  if(month=='january'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('1'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('1'))
    
    
  }else if(month=='february'){
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('2'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('2'))
    
    
  }else if(month=='march'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('3'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('3'))
    
    
  }else if(month=='april'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('4'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('4'))
    
    
  }else if(month=='may'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('5'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('5'))
    
    
  }else if(month=='june'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('6'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('6'))
    
    
  }else if(month=='july'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('7'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('7'))
    
    
  }else if(month=='august'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('8'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('8'))
    
    
  }else if(month=='september'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('9'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('9'))
    
    
  }else if(month=='october'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('10'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('10'))
    
    
  }else if(month=='november'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('11'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('11'))
    
    
  }else if(month=='december'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('12'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('12'))
    
    
  }
  
  
  # Average across months 
  test.vwc <-aggregate(vwc~x+y,mean,data=test.vwc)
  
  #sum across months
  test.transp<-aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.transp)
  test.transp$canopy_transpiration_mm_m2 <- test.transp$canopy_transpiration_mm_m2/30
  
  # re-grid
  test.vwc <- fix_grid(test.vwc)
  
  #load reference raster
  transit.all.raster<-raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_global_unfiltered.tif')
  
  #resample to reference raster
  test.vwc <- resample(test.vwc,transit.all.raster)
  
  
  if(land_cover=='grassland'){
    
    test.grassland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.grassland)
    
    # get rid of pixels where T is zero
    test.grassland.cumulative.transp <- test.grassland.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.grassland.cumulative.transp$canopy_transpiration_mm_m2 <- 
      test.grassland.cumulative.transp$canopy_transpiration_mm_m2/365
    
    land_cover_raster<-rasterFromXYZ(test.grassland.cumulative.transp)
    rm(test.grassland.cumulative.transp)
    
    
  }else if(land_cover=='forest'){
    
    
    test.forest.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.forest)
    
    # get rid of pixels where T is zero
    test.forest.cumulative.transp <- test.forest.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.forest.cumulative.transp$canopy_transpiration_mm_m2 <- 
      test.forest.cumulative.transp$canopy_transpiration_mm_m2/365
    
    land_cover_raster<-rasterFromXYZ(test.forest.cumulative.transp)
    rm(test.forest.cumulative.transp)
    
    
  }else if(land_cover=='shrubland'){
    
    
    test.shrubland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.shrubland)
    
    # get rid of pixels where T is zero
    test.shrubland.cumulative.transp <- test.shrubland.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.shrubland.cumulative.transp$canopy_transpiration_mm_m2 <- 
      test.shrubland.cumulative.transp$canopy_transpiration_mm_m2/365
    
    land_cover_raster<-rasterFromXYZ(test.shrubland.cumulative.transp)
    rm(test.shrubland.cumulative.transp)
    
  }else if(land_cover=='cropland'){
    
    
    
    test.cropland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.cropland)
    
    # get rid of pixels where T is zero
    test.cropland.cumulative.transp <- test.cropland.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.cropland.cumulative.transp$canopy_transpiration_mm_m2 <- 
      test.cropland.cumulative.transp$canopy_transpiration_mm_m2/365
    
    #summary(test.cropland.cumulative.transp)
    
    land_cover_raster<-rasterFromXYZ(test.cropland.cumulative.transp)
    rm(test.cropland.cumulative.transp)
    
    
  }else if(land_cover=='tundra'){
    
    
    test.tundra.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.tundra)
    
    # get rid of pixels where T is zero
    test.tundra.cumulative.transp <- test.tundra.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.tundra.cumulative.transp$canopy_transpiration_mm_m2 <- 
      test.tundra.cumulative.transp$canopy_transpiration_mm_m2/365
    
    #summary(test.tundra.cumulative.transp)
    
    land_cover_raster<-rasterFromXYZ(test.tundra.cumulative.transp)
    rm(test.tundra.cumulative.transp)
    
    
  }else if(land_cover=='xxx'){}
  
  #resample storage and transp, and mask to the specific land cover type
  test.transp <- rasterFromXYZ(test.transp)
  test.transp<- resample(test.transp,transit.all.raster)
  
  land_cover_raster <-resample(land_cover_raster,transit.all.raster)
  test.vwc <-mask(test.vwc,land_cover_raster)
  
  test.vwc<-
    merge(rasterToPoints(test.transp),rasterToPoints(test.vwc),
          by=c('x','y'))
  
  test.vwc$turnover <- 
    test.vwc$layer/test.vwc$canopy_transpiration_mm_m2
  
  
  # bound the data by the 1st and 99th percentiles
  #test.vwc <- filter_extremes_turnover(test.vwc)
  
  return(test.vwc)
  
  
  
}
#------------------------------------------------
# 95% confidence interval (X2 CHECK IF USED)-----

error.95 <-function(x) {
  n = length(x)
  se = sd(x)/sqrt(n)
  error <- qnorm(0.975)*se
  
  return(error)
}
#------------------------------------------------
# get monthly VWC-based estimates of storage (X2 IF USED) ------

get_monthly_storage_VWC <- function(month,land_cover){
  
  
  
  if(month=='january'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('1'))
    
    
    
  }else if(month=='february'){
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('2'))
    
  }else if(month=='march'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('3'))
    
    
  }else if(month=='april'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('4'))
    

  }else if(month=='may'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('5'))
    
    
  }else if(month=='june'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('6'))
    
    
  }else if(month=='july'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('7'))
    
    
  }else if(month=='august'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('8'))
    

  }else if(month=='september'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('9'))
    

  }else if(month=='october'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('10'))
    
    
  }else if(month=='november'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('11'))
    
    
  }else if(month=='december'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('12'))
    
    
  }
  
  
  
  # re-grid
  test.vwc <- fix_grid(test.vwc)
  
  #load reference raster
  storage.all.raster<-raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_global_unfiltered.tif')
  
  #resample to reference raster
  test.vwc <- resample(test.vwc,storage.all.raster)

  
  if(land_cover=='grassland'){
    
   
    land_cover_raster<- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_grassland_unfiltered.tif')
    
    
  }else if(land_cover=='forest'){
    
    
  
    land_cover_raster<- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_forest_unfiltered.tif')
    
    
  }else if(land_cover=='shrubland'){
    
    
    land_cover_raster<- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_shrubland_unfiltered.tif')
    
  }else if(land_cover=='cropland'){
    
    land_cover_raster<- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_cropland_unfiltered.tif')
    
    
  }else if(land_cover=='tundra'){
    
    
    land_cover_raster<- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_tundra_unfiltered.tif')
    
  }else if(land_cover=='xxx'){}
  

  #land_cover_raster <-resample(land_cover_raster,storage.all.raster)
  land_cover_raster <-resample(land_cover_raster,test.vwc)
  test.vwc <-mask(test.vwc,land_cover_raster)
  
  #convert to data frame
  test.vwc <- data.frame(rasterToPoints(test.vwc))
  
  colnames(test.vwc) <- c('x','y','canopy_transpiration_mm_m2','storage_mm',
                          'turnover_days','season')

  # bound the data by the 1st and 99th percentiles
  #test.vwc <- filter_extremes_turnover(test.vwc)
  
  return(test.vwc)
  
  
  
}

#------------------------------------------------
# go from mm/m^2 to cubic km ----


get_km_cubed_3 <- function(x){
  
  x <- x/1000 #convert mm/m^2 to m/m^2
  x <- x*(9200^2) #convert to cubic m (multiply by length and width of each pixel: 9 by 9 km)
  x <- x*1e-9 # multiply by this value to get cubic km from cubic m.
  
  
  return(x)
  
}


#------------------------------------------------
# error propagation ------

stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}


# num = numerator = storage
# den = denominator = transpiration

# error_prop_division <- function(num,den){
#   
#   #get standard error and relativze by the mean
#   se.storage <- (stderr(num))/mean(num)
#   se.transp <- (stderr(den))/mean(den)
#   
#   se.storage.square
#   
#   uncert <- sqrt((se.storage^2) + (se.transp(2)))
#   
#   return(uncert)
#   
#   
# }



#standard error divided by mean
stderr_relative <- function(x) {
  
  rel<-stderr(x)
  rel <- rel/mean(x)
  return(rel)
  
}

#standard deviation divided by mean
sd_relative <- function(x) {
  
  rel<-sd(x)
  rel <- rel/mean(x)
  return(rel)
  
}

error_prop_division <- function(dataset){
  
  #df <- as.data.frame(dataset)
  
  #get standard error and divide by the mean
  # se.storage.ag <- aggregate(layer~x+y,stderr_relative,data=dataset)
  # se.transp <- aggregate(canopy_transpiration_mm_m2~x+y,stderr_relative,data=dataset)
  
  #get standard deviation and divide by the mean
  se.storage.ag <- aggregate(storage_mm~x+y,sd_relative,data=dataset)
  
  se.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sd_relative,data=dataset)
  
  merge.num.den <- merge(se.storage.ag,se.transp,by=c('x','y'))
  
  merge.num.den <- merge.num.den[!is.na(merge.num.den$canopy_transpiration_mm_m2), ]
  
  merge.num.den$uncertainty <- sqrt(((merge.num.den$storage_mm)^2)) + 
    (((merge.num.den$canopy_transpiration_mm_m2)^2))
  
  merge.num.den<-rasterFromXYZ(merge.num.den[c(1,2,5)])
  crs(merge.num.den) <- '+proj=longlat +datum=WGS84'
  
  return(merge.num.den)
  
  
}
#------------------------------------------------
# create truncated distribution for each land cover type (X2 check if used) ----

get_turncated_dist <- function(land_cover,annual=T){
  
  #get filepath
  
  if(annual==T){
    
    filepath<-paste0("./../../../Data/Derived_data/Turnover/Annual/annual_transit_vwc_",land_cover,"_unfiltered.tif")
    
  }else{
    
    
    filepath<-paste0("./../../../Data/Derived_data/Turnover/Minimum/VWC_",land_cover,"_minimum_transit.tif")
    
  }
  
  #load raster
  grasslands_turnover <- raster(filepath)
  
  #convert to dataframe
  grasslands_turnover_df <- data.frame(rasterToPoints(grasslands_turnover))
  colnames(grasslands_turnover_df) <- c('x','y','turnover')
  #head(grasslands_turnover_df)
  
  #turncate right (top 1%)
  high<-round(quantile(grasslands_turnover_df$turnover,
                       probs=0.99),2)
  
  grasslands_turnover_df_truncate <- grasslands_turnover_df %>%
    dplyr::filter(turnover < high)
  grasslands_turnover_df_truncate$cover <- land_cover
  
  return(grasslands_turnover_df_truncate)
  
}

#------------------------------------------------
# Transit uncertainty by comparing minimum and maximum bounds for each pixel-----

#uncertainty max numerator min denominator:
transit_uncert_max <- function(x){
  
  #if(max==T){
  
  maximum_storage <- aggregate(layer~x+y,max,data=x)
  minimum_transp <- aggregate(canopy_transpiration_mm_m2~x+y,min,data=x)
  merged <- merge(maximum_storage,minimum_transp,by=c('x','y'))
  
  merged$uncertainty <- merged$layer/merged$canopy_transpiration_mm_m2
  
  merged <- merged %>%
    dplyr::filter(!uncertainty=='Inf')
  
  merged<-rasterFromXYZ(merged[c(1,2,5)])
  
  crs(merged) <- '+proj=longlat +datum=WGS84'
  
  return(merged)
  
  
}

#uncertainty min numerator max denominator:
transit_uncert_min <- function(x){
  
  minimum_storage <- aggregate(layer~x+y,min,data=x)
  maximum_transp <- aggregate(canopy_transpiration_mm_m2~x+y,max,data=x)
  merged <- merge(minimum_storage,maximum_transp,by=c('x','y'))
  
  
  merged$uncertainty <- merged$layer/merged$canopy_transpiration_mm_m2
  
  merged <- merged %>%
    dplyr::filter(!uncertainty=='Inf')
  
  merged<-rasterFromXYZ(merged[c(1,2,5)])
  
  crs(merged) <- '+proj=longlat +datum=WGS84'
  
  return(merged)
  
  
}

#------------------------------------------------
# Turn an irregular grid into a regular grid using expand grid approach -----


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

#------------------------------------------------
# get slopes of climate effects on transit time for different land cover types----

get_climate_model_coefs <- function(list,x,veg){
  
  list_coef <- list()
  list_r_squared <- list()
  
  test <- data.frame(list[x])
  colnames(test) <- c('x', 'y', 'cover', 'transit', 'climate_mean')
  test<- test %>%
    dplyr::filter(transit > 0)  
  
  #subset to veg
  test <- subset(test,cover==veg)
  
  #turn to spatial points df
  coordinates(test) <- ~x+y
  
  #run loop to get resampled coefs
  for( i in 1:1000){
    
    #distance constrained subsampling
    test_subsampled <- subsample.distance(test,size=100,d=50,replacement = T,
                                          latlong = T,echo = F)
    
    #turn back into df and run model
    test.df <- as.data.frame(test_subsampled)
    transit_lm<-lm(log(transit)~climate_mean,data=test.df)
    summary(transit_lm)
    
    #make into df so easier to ID
    slope_df <- coef(transit_lm)[2]
    slope_df$coef <- 'slope'
    slope_df$id <- i
    slope_df$veg <- veg
    
    r.squared_df <- data.frame(summary(transit_lm)$r.squared)
    r.squared_df$coef <- 'r.squared'
    r.squared_df$id <- i
    r.squared_df$veg <- veg
    
    #store key output in lists
    list_coef[[i]] <- slope_df
    list_r_squared[[i]] <- r.squared_df
    
    
  }
  
  #output lists
  return(list(list_coef,list_r_squared))
  
}

#------------------------------------------------
# function to generate monthly transp data by land cover type (X2 CHECK IF USED) -----


get_land_cover_transp<-function(region,x){
  
  turnover.list<-list() 
  ncpath_9_km_2015_2017_smap_enhanced <- "./../../../Data/ET/9km_smap_purdy_2015_2017/9km_monthly/"
  et_9km_monthly_2002_2017_length <- dir(ncpath_9_km_2015_2017_smap_enhanced, full.names = T)
  
  for(i in et_9km_monthly_2002_2017_length[x]){
    
    i = et_9km_monthly_2002_2017_length[1]
    
    # Get ET data
    et.data.9km<-load_et_by_filepath(x= i,process = "SMAPcanopy_transpiration")
    
    # Try to normalize the EASE grid
    et.data.9km<-fix_grid(et.data.9km)
    #plot(et.data.9km)
    
    if(region =='Grassland'){
      
      #upload region shapefile and raster
      shapefile<-readOGR('./../../../Data/Derived_data/Land_Cover_Distributions/shapefiles/grassland',
                         layer='grassland')
      
      #biomass raster
      region.raster<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Grassland.tif')
      
      
    }else if(region=='Forest'){
      
      
      #upload region shapefile and raster
      shapefile<-readOGR('./../../../Data/Derived_data/Land_Cover_Distributions/shapefiles/forest',
                         layer='forest')
      
      #biomass raster
      region.raster<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Forest.tif')
      
      
    }else if(region=='Tundra'){
      
      
      #upload region shapefile and raster
      shapefile<-readOGR('./../../../Data/Derived_data/Land_Cover_Distributions/shapefiles/tundra',
                         layer='tundra')
      
      #biomass raster
      region.raster<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Tundra.tif')
      
      
    }else if(region=='Shrubland'){
      
      #upload region shapefile and raster
      shapefile<-readOGR('./../../../Data/Derived_data/Land_Cover_Distributions/shapefiles/shrubland',
                         layer='shrubland')
      
      #biomass raster
      region.raster<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Shrubland.tif')
      
      
    }else if(region=='Cropland'){
      
      #upload region shapefile and raster
      shapefile<-readOGR('./../../../Data/Derived_data/Land_Cover_Distributions/shapefiles/cropland',
                         layer='cropland')
      
      #biomass raster
      region.raster<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Cropland.tif')
      
    }else if(region=='xxx'){}
    
    
    #Canopy Transpiration
    ET<-biome_resample(shapefile,region.raster,et.data.9km) #bottleneck
    colnames(ET) <- c('x','y','canopy_transpiration_mm_m2')
    
    #convert latent energy to mm to provide CUMULATIVE monthly T
    region_biomass_water_transpiration<-aggregate(canopy_transpiration_mm_m2~x+y,
                                                  le_to_cumulative_monthly_mm,data=ET)
    rm(ET)
    
    region_biomass_water_transpiration <- rasterFromXYZ(region_biomass_water_transpiration)
    crs(region_biomass_water_transpiration) <- '+proj=longlat +datum=WGS84 +no_defs'
    region_biomass_water_transpiration <- crop(region_biomass_water_transpiration,extent(region.raster))
    
    #store in list
    #turnover.list[[i]] <- region_biomass_water_transpiration
    
  }
  
  #return(turnover.list)
  
  return(region_biomass_water_transpiration)
  
}


#------------------------------------------------
# import and produce cumulative transpiration raster ------


import_cumulative_transp = function(x){
  
  regions <- x
  
  for(i in 1:length(regions)){
    
    ecoregion <- regions[i]
    outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/")
    ecoregion_dir <- dir(outfile, full.names = T)
    ecoregion_dir <- ecoregion_dir[-13] #remove December 2016 (X2 check before you run this)
    region.list <- list()
    ecoregion_raster <- raster(paste0('./../../../Data/Derived_data/Land_Cover_Distributions/',ecoregion,'.tif'))
    
    
    for(j in ecoregion_dir[1:12]){
      
      test = raster(j)
      test = extend(test,extent(ecoregion_raster))
      
      region.list[[j]] <- test
      
    }
    
    
  }
  
  stack <- stack(region.list)
  summed_stack <- calc(stack,sum,na.rm = TRUE)
  
  return(summed_stack)
  
}


#------------------------------------------------
# Import and produce seasonal estimates of storage and turnover (X2 CHECK IF USED) -----


import_monthly_transp <- function(x){
  
  regions <- x
  
  for(i in 1:length(regions)){
    
    #load in data
    ecoregion <- regions[i]
    outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/")
    ecoregion_dir <- dir(outfile, full.names = T)
    ecoregion_dir <- ecoregion_dir[-13] #remove December 2016 (X2 check before you run this)
    region.list <- list()
    ecoregion_raster <- raster(paste0('./../../../Data/Derived_data/Land_Cover_Distributions/',ecoregion,'.tif'))
    
    for(j in ecoregion_dir[1:12]){
      
      test = raster(j)
      test = extend(test,extent(ecoregion_raster))
      test = data.frame(rasterToPoints(test))
      
      #add year and month columns
      df <-data.frame(colnames(test)[3])
      colnames(df) <- 'val'
      df$val <- gsub(paste0(ecoregion,'_'),'',df$val)
      # get year
      year_val <- substr(df$val, 1, 4)
      # get month
      month_val <- substr(df$val, 6, 7)
      test$year <- year_val
      test$month <- month_val
      
      #rename columns and store in list
      colnames(test) <- c('x','y','canopy_transpiration_mm_m2','year','month')
      region.list[[j]] <- test
      
      
      
    }}
  
  
  #collapse list to a dataframe
  test.monthly.transp<- do.call("rbind", region.list)
  rm(region.list,test)
  rownames(test.monthly.transp)<-NULL
  
  #fix so they are numeric
  test.monthly.transp$year =  as.numeric(as.character(test.monthly.transp$year)) 
  test.monthly.transp$month =  as.numeric(as.character(test.monthly.transp$month)) 
  
  return(test.monthly.transp)
  
  
  
}

#------------------------------------------------
# version 2 of getting seasonal storage and turnover -----

get_seasonal_turnover_2 <- function(season){
  
  
  if(season=='december_february'){
    
    
    transp <- transp.stack[[1:3]]
    
    vwc = vwc.list[c(1:3)]
    
    
  }else if(season=='march_may'){
    
    transp <- transp.stack[[4:6]]
    
    vwc = vwc.list[c(4:6)]
    
  }else if(season=='june_august'){
    
    
    transp <- transp.stack[[7:9]]
    
    vwc = vwc.list[c(7:9)]
    
    
  }else if(season=='september_november'){
    
    
    transp <- transp.stack[[10:12]]
    
    vwc = vwc.list[c(10:12)]}
  
  
  #daily T for three month period
  transp = calc(transp,sum,na.rm=TRUE)
  transp = transp$layer/90
  
  #get average storage
  vwc = do.call('rbind',vwc)
  vwc <-aggregate(vwc~x+y,mean,data=vwc)
  
  #load reference raster
  transit.all.raster <- raster(paste0('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_',ecoregion,'_unfiltered.tif'))
  #fix grid and resample to reference raster to its consistent
  vwc <- fix_grid(vwc)
  vwc <- resample(vwc,transit.all.raster)
  vwc <- mask(vwc,transit.all.raster)
  plot(vwc,main='vwc')
  transp <- resample(transp,transit.all.raster)
  transp <- mask(transp,transit.all.raster)
  plot(transp,main='transp')
  
  # stack.test <- stack(transp,vwc)
  # stack.test$turnover <- stack.test$layer.2/stack.test$layer.1
  # turnover <- stack.test$turnover
  
  #merge and turn to dataframe so can more easily to summary stats
  test<-
    merge(data.frame(rasterToPoints(transp)),data.frame(rasterToPoints(vwc)),
          by=c('x','y'))
  
  test$turnover <- 
    test$layer.y/test$layer.x
  
  colnames(test) <- c('x','y','canopy_transpiration_mm_m2','storage_mm',
                      'turnover_days')
  
  return(test)
  
}

#------------------------------------------------
# version 2 of importing monthly transpiration data -----

import_monthly_transp_2 = function(x){
  
  regions <- x
  
  for(i in 1:length(regions)){
    
    ecoregion <- regions[i]
    outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/")
    ecoregion_dir <- dir(outfile, full.names = T)
    ecoregion_dir <- ecoregion_dir[-13] #remove December 2016 (X2 check before you run this)
    region.list <- list()
    ecoregion_raster <- raster(paste0('./../../../Data/Derived_data/Land_Cover_Distributions/',ecoregion,'.tif'))
    
    
    for(j in ecoregion_dir[1:12]){
      
      test = raster(j)
      test = extend(test,extent(ecoregion_raster))
      
      region.list[[j]] <- test
      
    }
    
    
  }
  
  stack_list = stack(region.list)
  
}


#------------------------------------------------
# Get monthly turnover version 2------



get_monthly_turnover_2 <- function(month){
  
  #remember the months are offset by 1
  
  if(month=='december'){
    
    
    transp <- transp.stack[[1]]
    
    vwc = vwc.list[c(1)]
    
    
  }else if(month=='january'){
    
    transp <- transp.stack[[2]]
    
    vwc = vwc.list[c(2)]
    
  }else if(month=='february'){
    
    
    transp <- transp.stack[[3]]
    
    vwc = vwc.list[c(3)]
    
    
  }else if(month=='march'){
    
    
    transp <- transp.stack[[4]]
    
    vwc = vwc.list[c(4)]
    
    
  }else if(month=='april'){
    
    
    transp <- transp.stack[[5]]
    
    vwc = vwc.list[c(5)]
    
  }else if(month=='may'){
    
    
    transp <- transp.stack[[6]]
    
    vwc = vwc.list[c(6)]
    
  }else if(month=='june'){
    
    
    transp <- transp.stack[[7]]
    
    vwc = vwc.list[c(7)]
    
  }else if(month=='july'){
    
    
    transp <- transp.stack[[8]]
    plot(transp)
    vwc = vwc.list[c(8)]
    
  }else if(month=='august'){
    
    
    transp <- transp.stack[[9]]
    
    vwc = vwc.list[c(9)]
    
  }else if(month=='september'){
    
    
    transp <- transp.stack[[10]]
    
    vwc = vwc.list[c(10)]
    
  }else if(month=='october'){
    
    
    transp <- transp.stack[[11]]
    
    vwc = vwc.list[c(11)]
    
  }else if(month=='november'){
    
    
    transp <- transp.stack[[12]]
    
    vwc = vwc.list[c(12)]
    
  }
  
  
  #daily T for given month 
  transp <- calc(transp,sum)
  transp = transp$layer/30
  
  #get average storage
  vwc = do.call('rbind',vwc)
  vwc <-aggregate(vwc~x+y,mean,data=vwc)
  
  #load reference raster
  # transit.all.raster<-
  #   raster(paste0('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_',ecoregion,'_unfiltered.tif'))
  # 
  
  transit.all.raster <- raster(paste0('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_',ecoregion,'_unfiltered.tif'))
  #fix grid and resample to reference raster to its consistent
  vwc <- fix_grid(vwc)
  vwc <- resample(vwc,transit.all.raster)
  vwc <- mask(vwc,transit.all.raster)
  plot(vwc,main='vwc')
  transp <- resample(transp,transit.all.raster)
  transp <- mask(transp,transit.all.raster)
  plot(transp,main='transp')
  
  # stack.test <- stack(transp,vwc)
  # stack.test$turnover <- stack.test$layer.2/stack.test$layer.1
  # turnover <- stack.test$turnover
  
  #merge and turn to dataframe so can more easily to summary stats
  test<-
    merge(data.frame(rasterToPoints(transp)),data.frame(rasterToPoints(vwc)),
          by=c('x','y'))
  
  test$turnover <- 
    test$layer.y/test$layer.x
  
  test$month <- month
  
  colnames(test) <- c('x','y','canopy_transpiration_mm_m2','storage_mm',
                      'turnover_days','month')
  
  return(test)
  
}

#------------------------------------------------
# revamped script to generate monthly transp rasters for each land cover -----

get_land_cover_transp_2<-function(ecoregion,x){
  
  ncpath_9_km_2015_2017_smap_enhanced <- "./../../../Data/ET/9km_smap_purdy_2015_2017/9km_monthly/"
  et_9km_monthly_2002_2017_length <- dir(ncpath_9_km_2015_2017_smap_enhanced, full.names = T)
  
  
  # Get ET data (works)
  et.data.9km<-load_et_by_filepath(x= et_9km_monthly_2002_2017_length[x],process = "SMAPcanopy_transpiration")
  summary(et.data.9km)
  
  # Try to normalize the EASE grid
  et.data.9km<-fix_grid(et.data.9km)
  #plot(et.data.9km)
  
  
  # #upload region shapefile and raster
  # shapefile<-readOGR(paste0('./../../../Data/Derived_data/Land_Cover_Distributions/shapefiles/',ecoregion),
  #                    layer=ecoregion)
  
  #biomass raster
  region.raster<-raster(paste0('./../../../Data/Derived_data/Land_Cover_Distributions/',ecoregion,'.tif'))
  #plot(region.raster)
  
  # #Canopy Transpiration
  # ET<-biome_resample(shapefile,region.raster,et.data.9km) #bottleneck
  # colnames(ET) <- c('x','y','canopy_transpiration_mm_m2')
  
  resample_test<-resample(et.data.9km,region.raster)
  resample_test<-mask(resample_test,region.raster)
  #plot(resample_test)
  
  #Canopy Transpiration
  ET<-data.frame(rasterToPoints(resample_test))
  colnames(ET) <- c('x','y','canopy_transpiration_mm_m2')
  
  #convert latent energy to mm to provide CUMULATIVE monthly T
  region_biomass_water_transpiration<-aggregate(canopy_transpiration_mm_m2~x+y,
                                                le_to_cumulative_monthly_mm,data=ET)
  rm(ET)
  
  region_biomass_water_transpiration <- rasterFromXYZ(region_biomass_water_transpiration)
  crs(region_biomass_water_transpiration) <- '+proj=longlat +datum=WGS84 +no_defs'
  #region_biomass_water_transpiration <- crop(region_biomass_water_transpiration,extent(region.raster))
  
  
  
  return(region_biomass_water_transpiration)
  
}

#------------------------------------------------
# truncate to help with mapping with long tails ------

truncate_for_mapping <- function(df,col_number){
  
  # df <- var_df
  # col_number = 3
  # 
  #roundabout way of getting the 95th quantile
  test_vec <- df[col_number]
  colnames(test_vec) <- 'vec'
  quantile_95 = quantile(test_vec$vec,prob=0.95)
  
  #directly change column name
  colnames(df) <- c('x','y','value')
  
  #subset to pixels above 95th quantile and then make them all that value
  var_df_above_95 <- df %>%
    dplyr::filter( value > quantile_95) %>%
    dplyr::mutate(value = quantile_95)
  summary(var_df_above_95)
  
  #subset to value below 95th quantile
  var_df_below_95 <- df %>%
    dplyr::filter(value < quantile_95)
  
  #bind the two together into one dataframe
  var_df_2 <- rbind(var_df_below_95,var_df_above_95)
  #head(var_df_2)
  
  return(var_df_2)
  
}

#------------------------------------------------



