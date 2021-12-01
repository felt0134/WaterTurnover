
# script generate and saves to file monthly transpiration raster. It calls on
# another script from source.

#grassland ------

ecoregion = 'grassland'
source('0X_generate_monthly_LC_Rasters.R')

# forest ----

ecoregion = 'forest'
source('0X_generate_monthly_LC_Rasters.R')

# shrubland ----

ecoregion = 'shrubland'
source('0X_generate_monthly_LC_Rasters.R')

# tundra ----

ecoregion = 'tundra'
source('0X_generate_monthly_LC_Rasters.R')

# cropland ----

ecoregion = 'cropland'
source('0X_generate_monthly_LC_Rasters.R')



ncpath_9_km_2015_2017_smap_enhanced <- "./../../../Data/ET/9km_smap_purdy_2015_2017/9km_monthly/"
et_9km_monthly_2002_2017_length <- dir(ncpath_9_km_2015_2017_smap_enhanced, full.names = T)
file.id<-c(10:21)
ecoregion <-c('grassland') 
test_list <- list()
#10:21 = all months in year 2016
#9 = December 2015-December 2016 (offset by one)
for(i in file.id[c(1,7)]){
    
    file.number <- file.id[i]  
  
    #get data to proper format
    test<-get_land_cover_transp_2(ecoregion=ecoregion,x=i)
    
    #save as raster

    #first get year and month for filename
    df<-data.frame(et_9km_monthly_2002_2017_length[file.number])
    df
    df$id<-gsub('ET/9km_smap_purdy_2015_2017/9km_monthly//PTJPL_SMAP_ET_',
                '', df$et_9km_monthly_2002_2017_length.file.number.)
    df$id<-gsub('./../../../Data/','', df$id)
    # get year
    year <- substr(df$id, 1, 4)
    # get month
    month <- substr(df$id, 5, 6)
    
    outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/",ecoregion,"_",year,"_",month,".tif")
    # writeRaster(test, outfile)
    test_list[[i]] <- test
    print(i)
    
  }
  

plot(test_list[[10]])





