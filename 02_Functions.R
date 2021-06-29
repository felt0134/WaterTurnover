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
# ET figure function (NEEDS WORK) --------

# Update projection

# need to check this so it can be used mroe broadly

plot_et <- function(x,y,title,range.x){
  
  #aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  #x <- spTransform(x, CRS(aea.proj))
  
  et= c("red", "orange", "yellow",'green','cyan3','purple')
  bks_et<- quantile(x$y, probs=seq(0, 1, by=0.05), na.rm = TRUE)
  bkcols.et <- colorRampPalette(et)(length(bks_et)-1)
  #proj4string(x)<-CRS(aea.proj)
  r.range <- round(c(minValue(x), maxValue(x)))
  
  # Update projection
  #proj4string(x) <- CRS("+proj=longlat")
  #x<-projectRaster(x, crs=aea.proj)
  #plot(mean_mm_raster_2)
  
  
  # png(file='./../../../Figures/preliminary/my_first_figure.png',
  #     width=1500,height=1200,res=150)
  
  # Plot it
  plot(x,breaks = bks_et,axes=F,box=F,col = bkcols.et,
       legend.width=0.80,legend.shrink=1,main=title,cex.main=0.75,
       axis.args=list(at=seq(r.range[1], r.range[2], range.x),
                      labels=seq(r.range[1], r.range[2], range.x),
                      cex.axis=0.75),
       legend.args=list(expression(paste('')), side=4, font=2,adj=0.5, line=2.5, cex=0.9))
  
  #dev.off()
  
  
  #return()
  
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
# fix grid  ------

#Get the EASE grid to a regular grid. outputs a raster
#do this after converting to a dataframe 

fix_grid<-function(x){
  
  e<-extent(x[c(1:2)])
  r<-raster(e,ncol=500,nrow=500,crs='+proj=longlat +datum=WGS84')
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
  #plot(transp.mexico)
  resample_test<-resample(transp.region,region_raster)
  #resample_test<-mask(resample_test,region_shapefile)
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
# Plot maps of water turnover (NEEDS WORK)--------

plot_turnover<-function(x,y,title,range.x){
  
  et= c("lightblue","red")
  #et= c("red", "orange", "yellow",'green','cyan3','purple')
  bks_et<- quantile(x, probs=seq(0.05, .95, by=0.1), na.rm = TRUE)
  bkcols.et <- colorRampPalette(et)(length(bks_et)-1)
  #proj4string(x)<-CRS(aea.proj)
  r.range <- round(c(minValue(x), maxValue(x)))
  
  # Plot it
  plot(x,breaks = bks_et,axes=F,box=F,col = bkcols.et,
       legend.width=0.80,legend.shrink=1,main=title,cex.main=0.75,
       axis.args=list(at=seq(r.range[1], r.range[2], range.x),
                      labels=seq(r.range[1], r.range[2], range.x),
                      cex.axis=0.75),
       legend.args=list(expression(paste('')), side=4, font=2,adj=0.5, line=2.5, cex=0.9))
  
  
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
      
      #mostly herb
      wc<-read.csv('./../../../Data/Derived_data/Land_Cover_Water_Content/grassland_water_content_mostly_herb_families.csv')
      
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
      
    }else if(region=='none'){x}
    
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
# color bar (NEEDS WORK) -----

# function for color bar legend in maps
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), ticLABS=NULL,title='') {
  scale = (length(lut))
  ifelse(is.null(ticLABS), ticLABELS <- prettyNum(signif(ticks, 4)), ticLABELS <- ticLABS) 
  
  #dev.new(width=1.75, height=5)
  plot(c(0,1), c(0,1), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  #axis(2, ticks, las=1)
  axis(2, at=seq(0, 1, len=nticks), labels=ticLABELS, las=2)
  
  for (i in 1:(length(lut))) {
    y = (i-1)/scale #+ min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}

#------------------------------------------------
# coefficient of variation ----

cv <- function(x){
  
  cv.x<-sd(x)/mean(x)
  return(cv.x)
  
}

#------------------------------------------------
#get 2016 annual turnover data -----

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
# Get seasonal turnover ----


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


